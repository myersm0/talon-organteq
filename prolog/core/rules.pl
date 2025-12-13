% Rule application engine

:- module(rules, [
	rule_elements_at_level/4,
	rule_elements_cumulative/4,
	rule_divisions/2,
	apply_rule/5,
	make_rpc_action/4
]).

:- use_module(library(apply), [partition/4]).

:- use_module(state, [
	element/4,
	engaged/2,
	current_preset/1,
	rule/2,
	rule_predicate/1,
	rule_action/2,
	rule_selector/3,
	rule_selector/4,
	rule_selector/5,
	do_engage/2,
	do_disengage/2,
	claim/3,
	release/3,
	is_owned/2,
	still_owned_after_release/3,
	get_rule_level/2,
	set_rule_level/2,
	division/1,
	manuals/1,
	all_divisions/1,
	auxiliaries/1,
	json_to_atom/2
]).

:- use_module(selectors, [
	resolve_selector/3,
	preset_matches/2,
	uses_for_preset/1,
	selector_matches_preset/2,
	computed_max_level/3,
	effective_rule_selector/3,
	effective_rule_selector/4,
	effective_rule_selector/5
]).

% ============================================================================
% Rule divisions inference
% ============================================================================

rule_divisions(RuleId, []) :-
	rule_predicate(RuleId), !.

rule_divisions(RuleId, Divisions) :-
	findall(Div, rule_selector_division(RuleId, Div), AllDivs),
	sort(AllDivs, Divisions).

rule_selector_division(RuleId, Div) :-
	effective_rule_selector(RuleId, _, _),
	manuals(Manuals),
	member(Div, Manuals).

rule_selector_division(RuleId, Div) :-
	effective_rule_selector(RuleId, _, DivSpec, _),
	expand_division(DivSpec, Div).

rule_selector_division(RuleId, Div) :-
	effective_rule_selector(RuleId, _, DivSpec, _, _),
	expand_division(DivSpec, Div).

expand_division(all, Div) :-
	manuals(Manuals),
	member(Div, Manuals).
expand_division(Div, Div) :-
	Div \= all.

% ============================================================================
% Rule element computation
% ============================================================================

rule_elements_at_level(RuleId, Level, Division, Elements) :-
	get_prioritized_selectors(RuleId, Level, Division, Selectors),
	findall(E, (
		member(Selector-_, Selectors),
		resolve_selector(Division, Selector, LevelElements),
		member(E, LevelElements)
	), All),
	sort(All, Elements).

rule_element_actions_at_level(RuleId, Level, Division, ElementActions) :-
	get_prioritized_selectors(RuleId, Level, Division, Selectors),
	findall(E-Action, (
		member(Selector-Action, Selectors),
		resolve_selector(Division, Selector, LevelElements),
		member(E, LevelElements)
	), All),
	sort(All, ElementActions).

get_prioritized_selectors(RuleId, Level, Division, Selectors) :-
	findall(Sel-Act, get_selector_for_level(RuleId, Level, Division, Sel, Act), AllSelectors),
	current_preset(Preset),
	partition(is_matching_preset_selector(Preset), AllSelectors, PresetSpecific, Universal),
	(PresetSpecific \= [] ->
		Selectors = PresetSpecific
	;   Selectors = Universal
	).

is_matching_preset_selector(Preset, Selector-_) :-
	uses_for_preset(Selector),
	selector_matches_preset(Selector, Preset).

get_selector_for_level(RuleId, Level, Division, Selector, Action) :-
	effective_rule_selector(RuleId, Level, Div, Selector, Action),
	(Div = Division ; Div = all).
get_selector_for_level(RuleId, Level, Division, Selector, Action) :-
	effective_rule_selector(RuleId, Level, Div, Selector),
	(Div = Division ; Div = all),
	Action = engage.
get_selector_for_level(RuleId, Level, _, Selector, engage) :-
	effective_rule_selector(RuleId, Level, Selector).

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
% Rule application - main entry point
% ============================================================================
% apply_rule(RuleId, Action, Delta, Level, RPCActions)
% Action: none | mute | maximize | minimize | solo | reassert
% Delta: none | integer
% Level: none | integer

apply_rule(RuleId, _, _, _, _) :-
	\+ rule(RuleId, _),
	throw(error(unknown_rule(RuleId), context(apply_rule/5, 'Rule not defined'))).

apply_rule(RuleId, _, _, _, Actions) :-
	rule_predicate(RuleId),
	!,
	apply_predicate_rule(RuleId, Actions).

apply_rule(RuleId, mute, _, _, Actions) :-
	rule(RuleId, _),
	get_rule_level(RuleId, Level),
	rule_divisions(RuleId, Divisions),
	mute_impl(RuleId, Level, Divisions, Actions).

apply_rule(RuleId, maximize, _, _, Actions) :-
	current_preset(Preset),
	computed_max_level(RuleId, Preset, MaxLevel),
	rule(RuleId, Type),
	rule_divisions(RuleId, Divisions),
	(Type = persistent ->
		apply_persistent_to_level(RuleId, MaxLevel, Divisions, Actions)
	;   apply_transient(RuleId, Divisions, Actions)
	).

apply_rule(RuleId, minimize, _, _, Actions) :-
	rule(RuleId, Type),
	rule_divisions(RuleId, Divisions),
	(Type = persistent ->
		apply_persistent_to_level(RuleId, 1, Divisions, Actions)
	;   apply_transient(RuleId, Divisions, Actions)
	).

apply_rule(RuleId, solo, _, _, Actions) :-
	get_rule_level(RuleId, CurrentLevel),
	CurrentLevel > 0,
	rule_divisions(RuleId, Divisions),
	solo_impl(RuleId, CurrentLevel, Divisions, Actions).

apply_rule(RuleId, reassert, _, _, Actions) :-
	get_rule_level(RuleId, CurrentLevel),
	CurrentLevel > 0,
	rule_divisions(RuleId, Divisions),
	reassert_impl(RuleId, CurrentLevel, Divisions, Actions).

apply_rule(RuleId, none, Delta, none, Actions) :-
	Delta \= none,
	rule(RuleId, Type),
	rule_divisions(RuleId, Divisions),
	get_rule_level(RuleId, CurrentLevel),
	current_preset(Preset),
	computed_max_level(RuleId, Preset, MaxLevel),
	NewLevel is max(0, min(CurrentLevel + Delta, MaxLevel)),
	(Type = persistent ->
		apply_persistent_to_level(RuleId, NewLevel, Divisions, Actions)
	;   (Delta > 0 ->
			apply_transient(RuleId, Divisions, Actions)
		;   Actions = []
		)
	).

apply_rule(RuleId, none, none, Level, Actions) :-
	Level \= none,
	rule(RuleId, Type),
	rule_divisions(RuleId, Divisions),
	current_preset(Preset),
	computed_max_level(RuleId, Preset, MaxLevel),
	ClampedLevel is max(0, min(Level, MaxLevel)),
	(Type = persistent ->
		apply_persistent_to_level(RuleId, ClampedLevel, Divisions, Actions)
	;   (ClampedLevel > 0 ->
			apply_transient(RuleId, Divisions, Actions)
		;   Actions = []
		)
	).

apply_rule(RuleId, none, none, none, Actions) :-
	apply_rule(RuleId, none, 1, none, Actions).

% ============================================================================
% Persistent rule application
% ============================================================================

apply_persistent_to_level(RuleId, TargetLevel, Divisions, Actions) :-
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
		make_rpc_action(Div, N, 1.0, Action)
	), AddActions),
	findall(Action, (
		member(N, New),
		\+ member(N, ToAdd),
		do_engage(Div, N),
		make_rpc_action(Div, N, 1.0, Action)
	), ReassertActions),
	findall(Action, (
		member(N, ToRemove),
		release(RuleId, Div, N),
		\+ still_owned_after_release(RuleId, Div, N),
		do_disengage(Div, N),
		make_rpc_action(Div, N, 0.0, Action)
	), RemoveActions),
	compute_delta_actions(RuleId, OldRest, NewRest, RestActions),
	append([AddActions, ReassertActions, RemoveActions, RestActions], Actions).

make_rpc_action(Div, N, Value, Action) :-
	element(Div, N, _, Type),
	make_rpc_action_for_type(Type, Div, N, Value, Action).

make_rpc_action_for_type(stop, Div, N, Value, set_stop(Div, N, Value)).
make_rpc_action_for_type(coupler, _, N, Value, set_coupler(N, Value)).
make_rpc_action_for_type(mono_coupler, _, N, Value, set_mono_coupler(N, Value)).
make_rpc_action_for_type(tremulant, _, N, Value, set_tremulant(N, Value)).

% ============================================================================
% Transient rule application
% ============================================================================

apply_transient(RuleId, Divisions, Actions) :-
	current_preset(Preset),
	computed_max_level(RuleId, Preset, MaxLevel),
	findall(Action, (
		between(1, MaxLevel, L),
		member(Div, Divisions),
		rule_element_actions_at_level(RuleId, L, Div, ElementActions),
		member(N-SelectorAction, ElementActions),
		apply_element_action(Div, N, SelectorAction, Action)
	), Actions).

apply_element_action(Div, N, engage, Action) :-
	do_engage(Div, N),
	make_rpc_action(Div, N, 1.0, Action).
apply_element_action(Div, N, disengage, Action) :-
	do_disengage(Div, N),
	make_rpc_action(Div, N, 0.0, Action).

% ============================================================================
% Predicate-based rules
% ============================================================================

apply_predicate_rule(RuleId, RPCActions) :-
	rule_action(RuleId, ActionSpec),
	(callable(ActionSpec), \+ is_list(ActionSpec) ->
		call(ActionSpec, RPCActions)
	;	findall(RPCAction, (
			member(Action, ActionSpec),
			execute_rule_action(Action, RPCAction)
		), RPCActions)
	).

execute_rule_action(engage(Div, N), RPCAction) :-
	do_engage(Div, N),
	make_rpc_action(Div, N, 1.0, RPCAction).
execute_rule_action(disengage(Div, N), RPCAction) :-
	do_disengage(Div, N),
	make_rpc_action(Div, N, 0.0, RPCAction).
execute_rule_action(toggle(Div, N), RPCAction) :-
	(engaged(Div, N) ->
		do_disengage(Div, N),
		make_rpc_action(Div, N, 0.0, RPCAction)
	;
		do_engage(Div, N),
		make_rpc_action(Div, N, 1.0, RPCAction)
	).

% ============================================================================
% Solo and reassert
% ============================================================================

mute_impl(_, Level, _, []) :-
	Level =< 0,
	!.

mute_impl(RuleId, Level, Divisions, Actions) :-
	findall(Action, (
		member(Div, Divisions),
		rule_elements_cumulative(RuleId, Level, Div, Elements),
		member(N, Elements),
		release(RuleId, Div, N),
		\+ is_owned(Div, N),
		do_disengage(Div, N),
		make_rpc_action(Div, N, 0.0, Action)
	), Actions).

solo_impl(_, Level, _, []) :-
	Level =< 0,
	!.

solo_impl(RuleId, Level, _, Actions) :-
	findall(ClearAction, (
		division(D),
		element(D, N, _, _),
		do_disengage(D, N),
		make_rpc_action(D, N, 0.0, ClearAction)
	), ClearActions),
	forall((
		rule(OtherId, _),
		OtherId \= RuleId,
		get_rule_level(OtherId, OtherLevel),
		OtherLevel > 0,
		rule_divisions(OtherId, OtherDivs)
	), mute_impl(OtherId, OtherLevel, OtherDivs, _)),
	rule_divisions(RuleId, Divisions),
	reassert_impl(RuleId, Level, Divisions, ReassertActions),
	append(ClearActions, ReassertActions, Actions).

reassert_impl(RuleId, Level, Divisions, Actions) :-
	findall(Action, (
		member(Div, Divisions),
		rule_elements_cumulative(RuleId, Level, Div, Elements),
		member(N, Elements),
		do_engage(Div, N),
		make_rpc_action(Div, N, 1.0, Action)
	), Actions).

% ============================================================================
% Rules as selectors
% ============================================================================
%
% Allows rule IDs to be used directly in selectors:
%   resolve_selector(great, my_rule, Els).       % current level
%   resolve_selector(great, my_rule:2, Els).     % explicit level
%
% This enables compound selectors like:
%   difference(my_rule, family(principal, 8))

:- multifile selectors:resolve_selector/3.

selectors:resolve_selector(Division, RuleId, Elements) :-
	atom(RuleId),
	RuleId \= engaged,
	RuleId \= disengaged,
	RuleId \= all,
	RuleId \= stops,
	rule(RuleId, _),
	!,
	get_rule_level(RuleId, Level),
	(Level > 0 ->
		rule_elements_cumulative(RuleId, Level, Division, Elements)
	;	Elements = []
	).

selectors:resolve_selector(Division, RuleId:Level, Elements) :-
	atom(RuleId),
	integer(Level),
	rule(RuleId, _),
	!,
	(Level > 0 ->
		rule_elements_cumulative(RuleId, Level, Division, Elements)
	;	Elements = []
	).
