% Rules for testing

:- multifile state:rule/2.
:- multifile state:max_level/2.
:- multifile state:antonym/2.
:- multifile state:rule_selector/3.
:- multifile state:rule_selector/4.
:- multifile state:rule_selector/5.
:- multifile state:rule_predicate/1.
:- multifile state:rule_action/2.

% ============================================================================
% Persistent rules for testing
% ============================================================================

state:rule(foundation, persistent).
state:max_level(foundation, 3).

state:rule_selector(foundation, 1, great, numbers([1, 2, 3])).
state:rule_selector(foundation, 1, swell, numbers([1, 2])).
state:rule_selector(foundation, 2, pedal, numbers([1, 2])).
state:rule_selector(foundation, 2, great, numbers([4])).
state:rule_selector(foundation, 3, great, family(reed, any, 1, first)).

state:rule(reeds_combo, persistent).
state:max_level(reeds_combo, 2).

state:rule_selector(reeds_combo, 1, great, numbers([1, 2])).
state:rule_selector(reeds_combo, 1, swell, family(reed, any, 1, first)).
state:rule_selector(reeds_combo, 2, great, family(reed)).

state:rule(full_organ, persistent).
state:max_level(full_organ, 4).

state:rule_selector(full_organ, 1, great, numbers([1, 2, 3])).
state:rule_selector(full_organ, 1, pedal, numbers([1, 2])).
state:rule_selector(full_organ, 2, swell, numbers([1, 2, 3])).
state:rule_selector(full_organ, 2, coupler, numbers([1])).
state:rule_selector(full_organ, 3, great, family(reed)).
state:rule_selector(full_organ, 3, swell, family(reed)).
state:rule_selector(full_organ, 3, coupler, numbers([2])).
state:rule_selector(full_organ, 4, coupler, numbers([3])).
state:rule_selector(full_organ, 4, tremulant, numbers([1])).

% ============================================================================
% Transient rules - selector-based
% ============================================================================

state:rule(add_reeds, transient).
state:max_level(add_reeds, 2).
state:rule_selector(add_reeds, 1, family(reed, any, 1, first)).
state:rule_selector(add_reeds, 2, family(reed)).

% ============================================================================
% Transient rules - predicate-based
% ============================================================================

state:rule(brighten, transient).
state:rule(darken, transient).
state:antonym(brighten, darken).
state:antonym(darken, brighten).
state:rule_predicate(brighten).
state:rule_predicate(darken).

state:rule_action(brighten, Actions) :-
	findall(Action, (
		state:division(Div),
		brighten_one(Div, Action)
	), Actions).

brighten_one(Div, engage(Div, N)) :-
	helpers:first_disengaged_in_family(Div, mixture, N), !.
brighten_one(Div, engage(Div, N)) :-
	helpers:first_disengaged_in_family(Div, mutation, N), !.

state:rule_action(darken, Actions) :-
	findall(Action, (
		state:division(Div),
		darken_one(Div, Action)
	), Actions).

darken_one(Div, disengage(Div, N)) :-
	helpers:last_engaged_in_family(Div, mutation, N), !.
darken_one(Div, disengage(Div, N)) :-
	helpers:last_engaged_in_family(Div, mixture, N), !.
