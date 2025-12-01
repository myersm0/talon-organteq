% state.pl - Dynamic state and core state manipulation
% All dynamic predicates and basic state operations

:- module(state, [
	% Dynamic predicates
	element/4,
	engaged/2,
	current_preset/1,
	rule_level/2,
	owns/3,
	snapshot/2,
	snapshot_counter/1,
	current_snapshot/1,
	coupler_mapping/6,
	% Rule predicates (multifile for user rules)
	rule/2,
	max_level/2,
	antonym/2,
	rule_predicate/1,
	rule_action/2,
	rule_selector/3,
	rule_selector/4,
	rule_selector/5,
	% Divisions
	manual/1,
	auxiliary/1,
	division/1,
	manuals/1,
	auxiliaries/1,
	all_divisions/1,
	% State manipulation
	do_engage/2,
	do_disengage/2,
	claim/3,
	release/3,
	is_owned/2,
	still_owned_after_release/3,
	get_rule_level/2,
	set_rule_level/2,
	% History
	reset_history/0,
	save_snapshot/1,
	restore_snapshot/2,
	get_current_state/1,
	% Helpers
	json_to_atom/2,
	get_dict/4
]).

% Helper: get_dict with default value
get_dict(Key, Dict, Value, Default) :-
	(get_dict(Key, Dict, Value) -> true ; Value = Default).

json_to_atom(Value, Atom) :-
	(atom(Value) -> Atom = Value ; atom_string(Atom, Value)).

% ============================================================================
% Dynamic predicates
% ============================================================================

:- dynamic element/4.           % element(Division, Number, Name, Type)
:- dynamic engaged/2.           % engaged(Division, Number)
:- dynamic current_preset/1.    % current_preset(Name)

:- dynamic rule/2.              % rule(Id, Type) - persistent | transient
:- dynamic max_level/2.         % max_level(RuleId, MaxLevel)
:- dynamic antonym/2.           % antonym(RuleId, AntonymId)
:- dynamic rule_predicate/1.    % rule_predicate(RuleId) - marks predicate-based rules
:- dynamic rule_action/2.       % rule_action(RuleId, Actions) - predicate-based rule implementation
:- dynamic rule_selector/3.     % rule_selector(RuleId, Level, Selector) - all divisions, engage
:- dynamic rule_selector/4.     % rule_selector(RuleId, Level, Division, Selector) - specific division, engage
:- dynamic rule_selector/5.     % rule_selector(RuleId, Level, Division, Selector, Action) - full form

:- multifile rule/2.
:- multifile max_level/2.
:- multifile antonym/2.
:- multifile rule_predicate/1.
:- multifile rule_action/2.
:- multifile rule_selector/3.
:- multifile rule_selector/4.
:- multifile rule_selector/5.

:- dynamic rule_level/2.        % rule_level(RuleId, CurrentLevel)
:- dynamic owns/3.              % owns(RuleId, Division, Number)

:- dynamic snapshot/2.          % snapshot(Id, State)
:- dynamic snapshot_counter/1.  % snapshot_counter(NextId)
:- dynamic current_snapshot/1.  % current_snapshot(Id)

:- dynamic coupler_mapping/6.   % coupler_mapping(Preset, Index, Source, Dest, Trans, Dir)
:- multifile coupler_mapping/6.

snapshot_counter(0).
current_snapshot(-1).

% ============================================================================
% Divisions
% ============================================================================

manual(pedal).
manual(choir).
manual(great).
manual(swell).

auxiliary(coupler).
auxiliary(mono_coupler).
auxiliary(tremulant).

division(D) :- manual(D).
division(D) :- auxiliary(D).

manuals(Ms) :- findall(M, manual(M), Ms).
auxiliaries(As) :- findall(A, auxiliary(A), As).
all_divisions(Ds) :- findall(D, division(D), Ds).

% ============================================================================
% State manipulation
% ============================================================================

do_engage(Division, Number) :-
	(engaged(Division, Number) -> true ; assertz(engaged(Division, Number))).

do_disengage(Division, Number) :-
	retractall(engaged(Division, Number)).

% ============================================================================
% Ownership tracking
% ============================================================================

is_owned(Division, Number) :-
	owns(_, Division, Number).

still_owned_after_release(RuleId, Division, Number) :-
	owns(OtherRule, Division, Number),
	OtherRule \= RuleId.

claim(RuleId, Division, Number) :-
	(owns(RuleId, Division, Number) -> true ; assertz(owns(RuleId, Division, Number))).

release(RuleId, Division, Number) :-
	retractall(owns(RuleId, Division, Number)).

get_rule_level(RuleId, Level) :-
	(rule_level(RuleId, L) -> Level = L ; Level = 0).

set_rule_level(RuleId, Level) :-
	retractall(rule_level(RuleId, _)),
	(Level > 0 -> assertz(rule_level(RuleId, Level)) ; true).

% ============================================================================
% History / Snapshots
% ============================================================================

reset_history :-
	retractall(snapshot(_, _)),
	retractall(snapshot_counter(_)),
	retractall(current_snapshot(_)),
	assertz(snapshot_counter(0)),
	assertz(current_snapshot(-1)).

save_snapshot(Label) :-
	get_current_state(State),
	retract(snapshot_counter(Id)),
	NextId is Id + 1,
	assertz(snapshot_counter(NextId)),
	retract(current_snapshot(OldCurrent)),
	forall((snapshot(SId, _), SId > OldCurrent), retract(snapshot(SId, _))),
	assertz(snapshot(Id, snapshot_data(Label, State))),
	assertz(current_snapshot(Id)).

restore_snapshot(Id, Actions) :-
	snapshot(Id, snapshot_data(_, State)),
	restore_state(State, Actions),
	retract(current_snapshot(_)),
	assertz(current_snapshot(Id)).

get_current_state(State) :-
	findall(engaged(D, N), engaged(D, N), EngagedList),
	findall(rule_level(R, L), rule_level(R, L), RuleLevels),
	findall(owns(R, D, N), owns(R, D, N), Ownership),
	State = state(EngagedList, RuleLevels, Ownership).

restore_state(state(EngagedList, RuleLevels, Ownership), Actions) :-
	findall(engaged(D, N), engaged(D, N), CurrentEngaged),
	findall(Action, (
		member(engaged(D, N), EngagedList),
		\+ member(engaged(D, N), CurrentEngaged),
		rpc_action(D, N, 1.0, Action)
	), EngageActions),
	findall(Action, (
		member(engaged(D, N), CurrentEngaged),
		\+ member(engaged(D, N), EngagedList),
		rpc_action(D, N, 0.0, Action)
	), DisengageActions),
	append(EngageActions, DisengageActions, Actions),
	retractall(engaged(_, _)),
	forall(member(E, EngagedList), assertz(E)),
	retractall(rule_level(_, _)),
	forall(member(R, RuleLevels), assertz(R)),
	retractall(owns(_, _, _)),
	forall(member(O, Ownership), assertz(O)).

% RPC action generation (needed by restore_state)
rpc_action(Division, Number, Value, Action) :-
	element(Division, Number, _, Type),
	rpc_action_for_type(Type, Division, Number, Value, Action).

rpc_action_for_type(stop, Division, Number, Value, set_stop(Division, Number, Value)).
rpc_action_for_type(coupler, _, Number, Value, set_coupler(Number, Value)).
rpc_action_for_type(mono_coupler, _, Number, Value, set_mono_coupler(Number, Value)).
rpc_action_for_type(tremulant, _, Number, Value, set_tremulant(Number, Value)).
