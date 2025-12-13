% Dynamic state and basic state operations

:- module(state, [
	% Dynamic state predicates
	element/4,
	engaged/2,
	current_preset/1,
	rule_level/2,
	owns/3,
	% Rule definition predicates (multifile, for user rules)
	rule/2,
	max_level/2,
	antonym/2,
	rule_predicate/1,
	rule_action/2,
	rule_selector/3,
	rule_selector/4,
	rule_selector/5,
	% History
	snapshot/2,
	snapshot_counter/1,
	current_snapshot/1,
	% Division queries
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
	% History operations
	reset_history/0,
	save_snapshot/1,
	restore_snapshot/2,
	get_snapshot_state/1,
	% Utilities
	json_to_atom/2
]).

:- use_module(config(divisions), [manual/1, auxiliary/1]).

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
:- dynamic rule_action/2.       % rule_action(RuleId, Actions)
:- dynamic rule_selector/3.     % rule_selector(RuleId, Level, Selector)
:- dynamic rule_selector/4.     % rule_selector(RuleId, Level, Division, Selector)
:- dynamic rule_selector/5.     % rule_selector(RuleId, Level, Division, Selector, Action)

:- multifile rule/2.
:- multifile max_level/2.
:- multifile antonym/2.
:- multifile rule_predicate/1.
:- multifile rule_action/2.
:- multifile rule_selector/3.
:- multifile rule_selector/4.
:- multifile rule_selector/5.

:- discontiguous rule/2.
:- discontiguous max_level/2.
:- discontiguous antonym/2.
:- discontiguous rule_selector/3.
:- discontiguous rule_selector/4.
:- discontiguous rule_selector/5.

:- dynamic rule_level/2.        % rule_level(RuleId, CurrentLevel)
:- dynamic owns/3.              % owns(RuleId, Division, Number)

:- dynamic snapshot/2.          % snapshot(Id, State)
:- dynamic snapshot_counter/1.  % snapshot_counter(NextId)
:- dynamic current_snapshot/1.  % current_snapshot(Id)

snapshot_counter(0).
current_snapshot(-1).

% ============================================================================
% Utilities
% ============================================================================

json_to_atom(Value, Atom) :-
	(atom(Value) -> Atom = Value ; atom_string(Atom, Value)).

% ============================================================================
% Division queries
% ============================================================================

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
% Ownership tracking (for persistent rules)
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
% History
% ============================================================================

reset_history :-
	retractall(snapshot(_, _)),
	retractall(snapshot_counter(_)),
	retractall(current_snapshot(_)),
	assertz(snapshot_counter(0)),
	assertz(current_snapshot(-1)).

get_snapshot_state(State) :-
	findall(engaged(D, N), engaged(D, N), EngagedList),
	findall(rule_level(R, L), rule_level(R, L), RuleLevels),
	findall(owns(R, D, N), owns(R, D, N), Ownership),
	State = state(EngagedList, RuleLevels, Ownership).

save_snapshot(Label) :-
	get_snapshot_state(State),
	retract(snapshot_counter(Id)),
	NextId is Id + 1,
	assertz(snapshot_counter(NextId)),
	retract(current_snapshot(OldCurrent)),
	forall((snapshot(SId, _), SId > OldCurrent), retract(snapshot(SId, _))),
	assertz(snapshot(Id, snapshot_data(Label, State))),
	assertz(current_snapshot(Id)).

restore_snapshot(Id, RestoredState) :-
	snapshot(Id, snapshot_data(_, State)),
	State = state(EngagedList, RuleLevels, Ownership),
	retractall(engaged(_, _)),
	forall(member(E, EngagedList), assertz(E)),
	retractall(rule_level(_, _)),
	forall(member(R, RuleLevels), assertz(R)),
	retractall(owns(_, _, _)),
	forall(member(O, Ownership), assertz(O)),
	retract(current_snapshot(_)),
	assertz(current_snapshot(Id)),
	RestoredState = State.
