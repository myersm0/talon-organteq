% rules.pl - Rule application logic
% This is the rule ENGINE, not user-defined rules

:- module(rules, [
	rule_elements_at_level/4,
	rule_elements_cumulative/4,
	apply_rule_impl/6,
	resolve_divisions/2
]).

:- use_module(state, [
	element/4, engaged/2, rule/2, max_level/2, antonym/2,
	rule_selector/3, rule_selector/4,
	do_engage/2, do_disengage/2,
	claim/3, release/3, still_owned_after_release/3,
	get_rule_level/2, set_rule_level/2,
	manuals/1, auxiliaries/1, all_divisions/1,
	json_to_atom/2
]).
:- use_module(selectors, [resolve_selector/3]).

% ============================================================================
% Rule element computation
% ============================================================================

% rule_selector can be:
%   rule_selector(RuleId, Level, Division, Selector) - specific division
%   rule_selector(RuleId, Level, Selector) - applies to any targeted division (3-arg form)

rule_elements_at_level(RuleId, Level, Division, Elements) :-
	findall(E, (
		(   rule_selector(RuleId, Level, Div, Selector), (Div = Division ; Div = all)
		;   rule_selector(RuleId, Level, Selector)
		),
		resolve_selector(Division, Selector, LevelElements),
		member(E, LevelElements)
	), All),
	sort(All, Elements).

rule_elements_cumulative(RuleId, MaxLevel, Division, Elements) :-
	findall(E, (
		between(1, MaxLevel, Level),
		rule_elements_at_level(RuleId, Level, Division, LevelElements),
		member(E, LevelElements)
	), All),
	sort(All, Elements).

% ============================================================================
% Division resolution
% ============================================================================

resolve_divisions(all, Divs) :- all_divisions(Divs), !.
resolve_divisions(manuals, Divs) :- manuals(Divs), !.
resolve_divisions(auxiliaries, Divs) :- auxiliaries(Divs), !.
resolve_divisions(List, Divs) :-
	is_list(List),
	maplist(json_to_atom, List, Divs), !.
resolve_divisions(Single, [Div]) :-
	json_to_atom(Single, Div).

% ============================================================================
% Rule application implementation
% ============================================================================

apply_rule_impl(RuleId, mute, _, _, Divisions, Actions) :-
	rule(RuleId, Type),
	(Type = persistent ->
		apply_persistent_rule_to_level(RuleId, 0, Divisions, Actions)
	;   set_rule_level(RuleId, 0),
		Actions = []
	).

apply_rule_impl(RuleId, maximize, _, _, Divisions, Actions) :-
	max_level(RuleId, MaxLevel),
	rule(RuleId, Type),
	(Type = persistent ->
		apply_persistent_rule_to_level(RuleId, MaxLevel, Divisions, Actions)
	;   apply_transient_rule_to_level(RuleId, MaxLevel, Divisions, Actions)
	).

apply_rule_impl(RuleId, minimize, _, _, Divisions, Actions) :-
	rule(RuleId, Type),
	(Type = persistent ->
		apply_persistent_rule_to_level(RuleId, 1, Divisions, Actions)
	;   apply_transient_rule_to_level(RuleId, 1, Divisions, Actions)
	).

apply_rule_impl(RuleId, solo, _, _, Divisions, Actions) :-
	get_rule_level(RuleId, CurrentLevel),
	CurrentLevel > 0,
	solo_rule_impl(RuleId, CurrentLevel, Divisions, Actions).

apply_rule_impl(RuleId, reassert, _, _, Divisions, Actions) :-
	get_rule_level(RuleId, CurrentLevel),
	CurrentLevel > 0,
	reassert_rule_impl(RuleId, CurrentLevel, Divisions, Actions).

apply_rule_impl(RuleId, none, Delta, none, Divisions, Actions) :-
	Delta \= none,
	rule(RuleId, Type),
	get_rule_level(RuleId, CurrentLevel),
	max_level(RuleId, MaxLevel),
	NewLevel is max(0, min(CurrentLevel + Delta, MaxLevel)),
	(Type = persistent ->
		apply_persistent_rule_to_level(RuleId, NewLevel, Divisions, Actions)
	;   apply_transient_rule_delta(RuleId, Delta, Divisions, Actions)
	).

apply_rule_impl(RuleId, none, none, Level, Divisions, Actions) :-
	Level \= none,
	rule(RuleId, Type),
	max_level(RuleId, MaxLevel),
	ClampedLevel is max(0, min(Level, MaxLevel)),
	(Type = persistent ->
		apply_persistent_rule_to_level(RuleId, ClampedLevel, Divisions, Actions)
	;   apply_transient_rule_to_level(RuleId, ClampedLevel, Divisions, Actions)
	).

apply_rule_impl(RuleId, none, none, none, Divisions, Actions) :-
	apply_rule_impl(RuleId, none, 1, none, Divisions, Actions).

% ============================================================================
% Persistent rule application
% ============================================================================

apply_persistent_rule_to_level(RuleId, TargetLevel, Divisions, Actions) :-
	get_rule_level(RuleId, CurrentLevel),
	(TargetLevel = CurrentLevel -> Actions = []
	;   compute_persistent_delta(RuleId, CurrentLevel, TargetLevel, Divisions, Actions),
		set_rule_level(RuleId, TargetLevel)
	).

compute_persistent_delta(RuleId, OldLevel, NewLevel, Divisions, Actions) :-
	findall(Div-OldElements, (
		member(Div, Divisions),
		(OldLevel > 0 -> rule_elements_cumulative(RuleId, OldLevel, Div, OldElements)
		;   OldElements = [])
	), OldPairs),
	findall(Div-NewElements, (
		member(Div, Divisions),
		(NewLevel > 0 -> rule_elements_cumulative(RuleId, NewLevel, Div, NewElements)
		;   NewElements = [])
	), NewPairs),
	compute_delta_actions(RuleId, OldPairs, NewPairs, Actions).

compute_delta_actions(_, [], [], []).
compute_delta_actions(RuleId, [Div-Old|OldRest], [Div-New|NewRest], Actions) :-
	subtract(New, Old, ToAdd),
	subtract(Old, New, ToRemove),
	findall(Action, (
		member(N, ToAdd),
		claim(RuleId, Div, N),
		do_engage(Div, N),
		rpc_action(Div, N, 1.0, Action)
	), AddActions),
	findall(Action, (
		member(N, New),
		\+ member(N, ToAdd),
		do_engage(Div, N),
		rpc_action(Div, N, 1.0, Action)
	), ReassertActions),
	findall(Action, (
		member(N, ToRemove),
		release(RuleId, Div, N),
		(\+ still_owned_after_release(RuleId, Div, N) ->
			(do_disengage(Div, N), rpc_action(Div, N, 0.0, Action))
		;   fail
		)
	), RemoveActions),
	compute_delta_actions(RuleId, OldRest, NewRest, RestActions),
	append([AddActions, ReassertActions, RemoveActions, RestActions], Actions).

% RPC action (local copy, uses state module's element/4)
rpc_action(Division, Number, Value, Action) :-
	element(Division, Number, _, Type),
	rpc_action_for_type(Type, Division, Number, Value, Action).

rpc_action_for_type(stop, Division, Number, Value, set_stop(Division, Number, Value)).
rpc_action_for_type(coupler, _, Number, Value, set_coupler(Number, Value)).
rpc_action_for_type(mono_coupler, _, Number, Value, set_mono_coupler(Number, Value)).
rpc_action_for_type(tremulant, _, Number, Value, set_tremulant(Number, Value)).

% ============================================================================
% Transient rule application
% ============================================================================

apply_transient_rule_to_level(RuleId, TargetLevel, Divisions, Actions) :-
	get_rule_level(RuleId, CurrentLevel),
	(TargetLevel > CurrentLevel ->
		findall(Action, (
			between(1, TargetLevel, L),
			L > CurrentLevel,
			member(Div, Divisions),
			rule_elements_at_level(RuleId, L, Div, Elements),
			member(N, Elements),
			do_engage(Div, N),
			rpc_action(Div, N, 1.0, Action)
		), Actions)
	;   Actions = []
	),
	set_rule_level(RuleId, TargetLevel).

apply_transient_rule_delta(RuleId, Delta, Divisions, Actions) :-
	(antonym(RuleId, Antonym) ->
		apply_transient_with_antonym(RuleId, Antonym, Delta, Divisions, Actions)
	;   get_rule_level(RuleId, Current),
		max_level(RuleId, MaxLevel),
		NewLevel is max(0, min(Current + Delta, MaxLevel)),
		apply_transient_rule_to_level(RuleId, NewLevel, Divisions, Actions)
	).

apply_transient_with_antonym(RuleId, Antonym, Delta, Divisions, Actions) :-
	get_rule_level(RuleId, RuleLevel),
	get_rule_level(Antonym, AntonymLevel),
	Combined is RuleLevel - AntonymLevel,
	NewCombined is Combined + Delta,
	max_level(RuleId, RuleMax),
	max_level(Antonym, AntonymMax),
	(NewCombined > 0 ->
		TargetLevel is min(NewCombined, RuleMax),
		apply_transient_rule_to_level(RuleId, TargetLevel, Divisions, Actions),
		set_rule_level(Antonym, 0)
	; NewCombined < 0 ->
		TargetLevel is min(abs(NewCombined), AntonymMax),
		apply_transient_rule_to_level(Antonym, TargetLevel, Divisions, Actions),
		set_rule_level(RuleId, 0)
	;   set_rule_level(RuleId, 0),
		set_rule_level(Antonym, 0),
		Actions = []
	).

% ============================================================================
% Solo and reassert
% ============================================================================

solo_rule_impl(RuleId, Level, Divisions, Actions) :-
	findall(Div-Elements, (
		member(Div, Divisions),
		rule_elements_cumulative(RuleId, Level, Div, Elements)
	), RulePairs),
	findall(Action, (
		member(Div-RuleElements, RulePairs),
		element(Div, N, _, _),
		(member(N, RuleElements) ->
			(do_engage(Div, N), rpc_action(Div, N, 1.0, Action))
		;   (do_disengage(Div, N), rpc_action(Div, N, 0.0, Action))
		)
	), Actions).

reassert_rule_impl(RuleId, Level, Divisions, Actions) :-
	findall(Action, (
		member(Div, Divisions),
		rule_elements_cumulative(RuleId, Level, Div, Elements),
		member(N, Elements),
		do_engage(Div, N),
		rpc_action(Div, N, 1.0, Action)
	), Actions).
