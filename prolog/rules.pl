% rules.pl - Rule application logic
% This is the rule ENGINE, not user-defined rules

:- module(rules, [
	rule_elements_at_level/4,
	rule_elements_cumulative/4,
	rule_divisions/2,
	apply_rule_impl/5
]).

:- use_module(state, [
	element/4, engaged/2, rule/2, max_level/2, antonym/2,
	rule_selector/3, rule_selector/4, rule_selector/5,
	do_engage/2, do_disengage/2,
	claim/3, release/3, still_owned_after_release/3,
	get_rule_level/2, set_rule_level/2,
	manuals/1, auxiliaries/1, all_divisions/1,
	json_to_atom/2
]).
:- use_module(selectors, [resolve_selector/3]).

% ============================================================================
% Rule divisions inference
% ============================================================================

% Infer target divisions from rule selectors:
% - 3-arg selectors (no division) -> all manuals
% - 4-arg and 5-arg selectors -> explicit divisions (expand 'all')
rule_divisions(RuleId, Divisions) :-
	findall(Div, rule_selector_division(RuleId, Div), AllDivs),
	sort(AllDivs, Divisions).

rule_selector_division(RuleId, Div) :-
	rule_selector(RuleId, _, _),
	manuals(Manuals),
	member(Div, Manuals).

rule_selector_division(RuleId, Div) :-
	rule_selector(RuleId, _, DivSpec, _),
	expand_division(DivSpec, Div).

rule_selector_division(RuleId, Div) :-
	rule_selector(RuleId, _, DivSpec, _, _),
	expand_division(DivSpec, Div).

expand_division(all, Div) :-
	manuals(Manuals),
	member(Div, Manuals).
expand_division(Div, Div) :-
	Div \= all.

% ============================================================================
% Rule element computation
% ============================================================================

% rule_selector forms:
%   rule_selector(RuleId, Level, Selector) - all divisions, engage (default)
%   rule_selector(RuleId, Level, Division, Selector) - specific division, engage (default)
%   rule_selector(RuleId, Level, Division, Selector, Action) - full form with action

% For persistent rules, we just need the elements (always engage)
rule_elements_at_level(RuleId, Level, Division, Elements) :-
	findall(E, (
		get_selector_for_level(RuleId, Level, Division, Selector, _),
		resolve_selector(Division, Selector, LevelElements),
		member(E, LevelElements)
	), All),
	sort(All, Elements).

% For transient rules, we need element-action pairs
rule_element_actions_at_level(RuleId, Level, Division, ElementActions) :-
	findall(E-Action, (
		get_selector_for_level(RuleId, Level, Division, Selector, Action),
		resolve_selector(Division, Selector, LevelElements),
		member(E, LevelElements)
	), All),
	sort(All, ElementActions).

% Unified selector lookup - returns Selector and Action for a given rule/level/division
get_selector_for_level(RuleId, Level, Division, Selector, Action) :-
	rule_selector(RuleId, Level, Div, Selector, Action),
	(Div = Division ; Div = all).
get_selector_for_level(RuleId, Level, Division, Selector, Action) :-
	rule_selector(RuleId, Level, Div, Selector),
	(Div = Division ; Div = all),
	Action = engage.
get_selector_for_level(RuleId, Level, _, Selector, engage) :-
	rule_selector(RuleId, Level, Selector).

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

apply_rule_impl(RuleId, mute, _, _, Actions) :-
	rule(RuleId, Type),
	rule_divisions(RuleId, Divisions),
	(Type = persistent ->
		apply_persistent_rule_to_level(RuleId, 0, Divisions, Actions)
	;   set_rule_level(RuleId, 0),
		Actions = []
	).

apply_rule_impl(RuleId, maximize, _, _, Actions) :-
	max_level(RuleId, MaxLevel),
	rule(RuleId, Type),
	rule_divisions(RuleId, Divisions),
	(Type = persistent ->
		apply_persistent_rule_to_level(RuleId, MaxLevel, Divisions, Actions)
	;   apply_transient_rule_to_level(RuleId, MaxLevel, Divisions, Actions)
	).

apply_rule_impl(RuleId, minimize, _, _, Actions) :-
	rule(RuleId, Type),
	rule_divisions(RuleId, Divisions),
	(Type = persistent ->
		apply_persistent_rule_to_level(RuleId, 1, Divisions, Actions)
	;   apply_transient_rule_to_level(RuleId, 1, Divisions, Actions)
	).

apply_rule_impl(RuleId, solo, _, _, Actions) :-
	get_rule_level(RuleId, CurrentLevel),
	CurrentLevel > 0,
	rule_divisions(RuleId, Divisions),
	solo_rule_impl(RuleId, CurrentLevel, Divisions, Actions).

apply_rule_impl(RuleId, reassert, _, _, Actions) :-
	get_rule_level(RuleId, CurrentLevel),
	CurrentLevel > 0,
	rule_divisions(RuleId, Divisions),
	reassert_rule_impl(RuleId, CurrentLevel, Divisions, Actions).

apply_rule_impl(RuleId, none, Delta, none, Actions) :-
	Delta \= none,
	rule(RuleId, Type),
	rule_divisions(RuleId, Divisions),
	get_rule_level(RuleId, CurrentLevel),
	max_level(RuleId, MaxLevel),
	NewLevel is max(0, min(CurrentLevel + Delta, MaxLevel)),
	(Type = persistent ->
		apply_persistent_rule_to_level(RuleId, NewLevel, Divisions, Actions)
	;   apply_transient_rule_delta(RuleId, Delta, Divisions, Actions)
	).

apply_rule_impl(RuleId, none, none, Level, Actions) :-
	Level \= none,
	rule(RuleId, Type),
	rule_divisions(RuleId, Divisions),
	max_level(RuleId, MaxLevel),
	ClampedLevel is max(0, min(Level, MaxLevel)),
	(Type = persistent ->
		apply_persistent_rule_to_level(RuleId, ClampedLevel, Divisions, Actions)
	;   apply_transient_rule_to_level(RuleId, ClampedLevel, Divisions, Actions)
	).

apply_rule_impl(RuleId, none, none, none, Actions) :-
	apply_rule_impl(RuleId, none, 1, none, Actions).

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

% Transient rules are stateless - just apply all selectors once
apply_transient_rule(RuleId, Divisions, Actions) :-
	max_level(RuleId, MaxLevel),
	findall(Action, (
		between(1, MaxLevel, L),
		member(Div, Divisions),
		rule_element_actions_at_level(RuleId, L, Div, ElementActions),
		member(N-SelectorAction, ElementActions),
		apply_element_action(Div, N, SelectorAction, Action)
	), Actions).

% Legacy level-based entry points now just apply the rule
apply_transient_rule_to_level(RuleId, TargetLevel, Divisions, Actions) :-
	(TargetLevel > 0 ->
		apply_transient_rule(RuleId, Divisions, Actions)
	;   Actions = []
	).

apply_transient_rule_delta(RuleId, Delta, Divisions, Actions) :-
	(Delta > 0 ->
		apply_transient_rule(RuleId, Divisions, Actions)
	;   Actions = []
	).

% Apply the action specified in the selector
apply_element_action(Div, N, engage, Action) :-
	do_engage(Div, N),
	rpc_action(Div, N, 1.0, Action).
apply_element_action(Div, N, disengage, Action) :-
	do_disengage(Div, N),
	rpc_action(Div, N, 0.0, Action).

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
