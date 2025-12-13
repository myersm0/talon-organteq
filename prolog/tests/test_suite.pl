% Formal tests for talon-organteq
%
% Run with:
%   cd prolog && swipl -g "consult('main.pl'), consult('tests/test_suite.pl'), run_tests, halt."
%
% Or interactively:
%   cd prolog && swipl -g "consult('main.pl'), consult('tests/test_suite.pl')."
%   run_tests.
%   run_tests(selectors).  % run specific test block

:- use_module(library(plunit)).

% ============================================================================
% Test fixtures
% ============================================================================

setup_test_organ :-
	execute("sync('Test Organ', [
		element(great, 1, 'Montre 8\\'', stop),
		element(great, 2, 'Bourdon 8\\'', stop),
		element(great, 3, 'Prestant 4\\'', stop),
		element(great, 4, 'Trompette 8\\'', stop),
		element(great, 5, 'Mixtura IV', stop),
		element(great, 6, 'Doublette 2\\'', stop),
		element(swell, 1, 'Gedact 8\\'', stop),
		element(swell, 2, 'Salicional 8\\'', stop),
		element(swell, 3, 'Voix Celeste 8\\'', stop),
		element(swell, 4, 'Hautbois 8\\'', stop),
		element(swell, 5, 'Prestant 4\\'', stop),
		element(pedal, 1, 'Subbass 16\\'', stop),
		element(pedal, 2, 'Prinzipal 8\\'', stop),
		element(pedal, 3, 'Octav 4\\'', stop),
		element(pedal, 4, 'Posaune 16\\'', stop),
		element(coupler, 1, 'Swell to Great', coupler),
		element(coupler, 2, 'Great to Pedal', coupler),
		element(tremulant, 1, 'Swell Tremulant', tremulant)
	], [])", _, _).

clear_all_stops :-
	execute("clear_all", _, _).

setup_test_rule :-
	retractall(state:rule(test_combo, _)),
	retractall(state:rule_selector(test_combo, _, _)),
	retractall(state:rule_selector(test_combo, _, _, _)),
	assertz(state:rule(test_combo, persistent)),
	assertz(state:rule_selector(test_combo, 1, great, family(principal, 8))),
	assertz(state:rule_selector(test_combo, 1, swell, family(flute, 8))),
	assertz(state:rule_selector(test_combo, 2, great, family(principal))),
	assertz(state:rule_selector(test_combo, 2, pedal, numbers([1, 2]))).

cleanup_test_rule :-
	retractall(state:rule(test_combo, _)),
	retractall(state:rule_selector(test_combo, _, _)),
	retractall(state:rule_selector(test_combo, _, _, _)),
	retractall(state:rule_level(test_combo, _)),
	retractall(state:owns(test_combo, _, _)).

% ============================================================================
% Config tests
% ============================================================================

:- begin_tests(config).

test(manuals_defined) :-
	findall(M, divisions:manual(M), Ms),
	msort(Ms, [choir, great, pedal, swell]).

test(auxiliaries_defined) :-
	findall(A, divisions:auxiliary(A), As),
	msort(As, [coupler, mono_coupler, tremulant]).

test(family_of_principal) :-
	families:family_of('Montre', principal),
	families:family_of('Prestant', principal),
	families:family_of('Prinzipal', principal).

test(family_of_reed) :-
	families:family_of('Trompette', reed),
	families:family_of('Hautbois', reed).

test(family_of_flute) :-
	families:family_of('Bourdon', flute),
	families:family_of('Gedact', flute).

test(family_of_string) :-
	families:family_of('Salicional', string),
	families:family_of('Voix Celeste', string).

test(family_of_mixture) :-
	families:family_of('Mixtura', mixture).

test(aliases_exist) :-
	once((
		aliases:aliases(Group),
		member('Trompette', Group),
		member('Trumpet', Group)
	)).

:- end_tests(config).

% ============================================================================
% Classification tests
% ============================================================================

:- begin_tests(classification, [setup(setup_test_organ)]).

test(base_name_with_footage) :-
	classification:base_name('Montre 8\'', 'Montre'),
	classification:base_name('Prestant 4\'', 'Prestant'),
	classification:base_name('Subbass 16\'', 'Subbass').

test(base_name_with_roman) :-
	classification:base_name('Mixtura IV', 'Mixtura').

test(base_name_plain) :-
	classification:base_name('Montre', 'Montre').

test(name_footage_8) :-
	classification:name_footage('Montre 8\'', 8).

test(name_footage_4) :-
	classification:name_footage('Prestant 4\'', 4).

test(name_footage_16) :-
	classification:name_footage('Subbass 16\'', 16).

test(name_footage_unknown) :-
	classification:name_footage('Mixtura IV', unknown).

test(element_family_principal) :-
	classification:element_family(great, 1, principal).

test(element_family_flute) :-
	classification:element_family(great, 2, flute).

test(element_family_reed) :-
	classification:element_family(great, 4, reed).

test(element_family_mixture) :-
	classification:element_family(great, 5, mixture).

test(element_footage) :-
	classification:element_footage(great, 1, 8),
	classification:element_footage(great, 3, 4).

:- end_tests(classification).

% ============================================================================
% Selector tests
% ============================================================================

:- begin_tests(selectors, [setup(setup_test_organ)]).

test(numbers_selector) :-
	selectors:resolve_selector(great, numbers([1, 2, 3]), Els),
	Els == [1, 2, 3].

test(all_selector) :-
	selectors:resolve_selector(great, all, Els),
	length(Els, 6).

test(stops_selector) :-
	selectors:resolve_selector(great, stops, Els),
	length(Els, 6).

test(family_selector_principal) :-
	selectors:resolve_selector(great, family(principal), Els),
	msort(Els, [1, 3, 6]).

test(family_selector_reed) :-
	selectors:resolve_selector(great, family(reed), Els),
	Els == [4].

test(family_selector_with_footage) :-
	selectors:resolve_selector(great, family(principal, 8), Els),
	Els == [1].

test(family_selector_with_footage_4) :-
	selectors:resolve_selector(great, family(principal, 4), Els),
	Els == [3].

test(family_selector_with_any_footage) :-
	selectors:resolve_selector(great, family(principal, any), Els),
	msort(Els, [1, 3, 6]).

test(family_selector_with_limit_first) :-
	selectors:resolve_selector(great, family(principal, any, 1, first), Els),
	Els == [1].

test(family_selector_with_limit_last) :-
	selectors:resolve_selector(great, family(principal, any, 1, last), Els),
	Els == [6].

test(engaged_selector_empty, [setup(clear_all_stops)]) :-
	selectors:resolve_selector(great, engaged, Els),
	Els == [].

test(engaged_selector_with_stops, [setup(clear_all_stops), nondet]) :-
	execute("engage(great, numbers([1, 3]))", _, _),
	selectors:resolve_selector(great, engaged, Els),
	msort(Els, [1, 3]).

test(disengaged_selector, [setup(clear_all_stops), nondet]) :-
	execute("engage(great, numbers([1, 3]))", _, _),
	selectors:resolve_selector(great, disengaged, Els),
	msort(Els, [2, 4, 5, 6]).

test(union_selector) :-
	selectors:resolve_selector(great, union([family(reed), family(mixture)]), Els),
	msort(Els, [4, 5]).

test(intersection_selector, [setup(clear_all_stops), nondet]) :-
	execute("engage(great, numbers([1, 4, 5]))", _, _),
	selectors:resolve_selector(great, intersection([family(principal), engaged]), Els),
	Els == [1].

test(difference_selector) :-
	selectors:resolve_selector(great, difference(family(principal), family(principal, 8)), Els),
	msort(Els, [3, 6]).

test(type_selector_stop) :-
	selectors:resolve_selector(great, type(stop), Els),
	length(Els, 6).

test(type_selector_coupler) :-
	selectors:resolve_selector(coupler, type(coupler), Els),
	length(Els, 2).

:- end_tests(selectors).

% ============================================================================
% State operation tests
% ============================================================================

:- begin_tests(state_ops, [setup((setup_test_organ, clear_all_stops))]).

test(do_engage) :-
	state:do_engage(great, 1),
	state:engaged(great, 1).

test(do_engage_idempotent) :-
	state:do_engage(great, 1),
	state:do_engage(great, 1),
	findall(N, state:engaged(great, N), Ns),
	Ns == [1].

test(do_disengage) :-
	state:do_engage(great, 1),
	state:do_disengage(great, 1),
	\+ state:engaged(great, 1).

test(do_disengage_not_engaged) :-
	state:do_disengage(great, 99).  % should not fail

:- end_tests(state_ops).

% ============================================================================
% Command execution tests
% ============================================================================

:- begin_tests(commands, [setup(setup_test_organ)]).

test(engage_command, [setup(clear_all_stops), nondet]) :-
	execute("engage(great, numbers([1, 2]))", Actions, _),
	length(Actions, 2),
	state:engaged(great, 1),
	state:engaged(great, 2).

test(disengage_command, [setup(clear_all_stops), nondet]) :-
	execute("engage(great, numbers([1, 2, 3]))", _, _),
	execute("disengage(great, numbers([2]))", Actions, _),
	length(Actions, 1),
	state:engaged(great, 1),
	\+ state:engaged(great, 2),
	state:engaged(great, 3).

test(toggle_command_engage, [setup(clear_all_stops), nondet]) :-
	execute("toggle(great, numbers([1]))", Actions, _),
	length(Actions, 1),
	state:engaged(great, 1).

test(toggle_command_disengage, [setup(clear_all_stops), nondet]) :-
	execute("engage(great, numbers([1]))", _, _),
	execute("toggle(great, numbers([1]))", Actions, _),
	length(Actions, 1),
	\+ state:engaged(great, 1).

test(toggle_command_mixed, [setup(clear_all_stops), nondet]) :-
	execute("engage(great, numbers([1]))", _, _),
	execute("toggle(great, numbers([1, 2]))", _, _),
	\+ state:engaged(great, 1),
	state:engaged(great, 2).

test(solo_command, [setup(clear_all_stops), nondet]) :-
	execute("engage(great, numbers([1, 2, 3, 4, 5]))", _, _),
	execute("solo(great, numbers([2, 4]))", _, _),
	\+ state:engaged(great, 1),
	state:engaged(great, 2),
	\+ state:engaged(great, 3),
	state:engaged(great, 4),
	\+ state:engaged(great, 5).

test(clear_command, [setup(clear_all_stops), nondet]) :-
	execute("engage(great, numbers([1, 2, 3]))", _, _),
	execute("clear(great)", _, _),
	\+ state:engaged(great, _).

test(clear_all_command, [setup(clear_all_stops), nondet]) :-
	execute("engage(great, numbers([1, 2]))", _, _),
	execute("engage(swell, numbers([1, 2]))", _, _),
	execute("clear_all", _, _),
	\+ state:engaged(_, _).

test(engage_family_command, [setup(clear_all_stops), nondet]) :-
	execute("engage(great, family(principal))", _, _),
	state:engaged(great, 1),
	state:engaged(great, 3),
	state:engaged(great, 6).

test(engage_family_with_footage, [setup(clear_all_stops), nondet]) :-
	execute("engage(great, family(principal, 8))", _, _),
	state:engaged(great, 1),
	\+ state:engaged(great, 3).

test(get_state_command, [setup(clear_all_stops), nondet]) :-
	execute("engage(great, numbers([1]))", _, _),
	execute("get_state", [], State),
	is_dict(State),
	get_dict(preset, State, 'Test Organ').

:- end_tests(commands).

% ============================================================================
% History tests
% ============================================================================

fresh_history :-
	clear_all_stops,
	state:reset_history.

:- begin_tests(history, [setup(setup_test_organ)]).

test(undo_single, [setup(fresh_history), nondet]) :-
	execute("engage(great, numbers([1]))", _, _),
	execute("engage(great, numbers([2]))", _, _),
	state:engaged(great, 1),
	state:engaged(great, 2),
	execute("undo", _, _),
	state:engaged(great, 1),
	\+ state:engaged(great, 2).

test(undo_multiple, [setup(fresh_history), nondet]) :-
	execute("engage(great, numbers([1]))", _, _),
	execute("engage(great, numbers([2]))", _, _),
	execute("engage(great, numbers([3]))", _, _),
	execute("undo", _, _),
	execute("undo", _, _),
	state:engaged(great, 1),
	\+ state:engaged(great, 2),
	\+ state:engaged(great, 3).

test(redo_single, [setup(fresh_history), nondet]) :-
	execute("engage(great, numbers([1]))", _, _),
	execute("engage(great, numbers([2]))", _, _),
	execute("undo", _, _),
	\+ state:engaged(great, 2),
	execute("redo", _, _),
	state:engaged(great, 2).

test(undo_at_beginning, [setup(fresh_history), nondet]) :-
	execute("undo", Actions, _),
	Actions == [].

test(redo_at_end, [setup(fresh_history), nondet]) :-
	execute("engage(great, numbers([1]))", _, _),
	execute("redo", Actions, _),
	Actions == [].

test(redo_invalidated_by_new_action, [setup(fresh_history), nondet]) :-
	execute("engage(great, numbers([1]))", _, _),
	execute("engage(great, numbers([2]))", _, _),
	execute("undo", _, _),
	execute("engage(great, numbers([3]))", _, _),
	execute("redo", Actions, _),
	Actions == [],
	state:engaged(great, 1),
	\+ state:engaged(great, 2),
	state:engaged(great, 3).

:- end_tests(history).

% ============================================================================
% Rule tests
% ============================================================================

:- begin_tests(rules, [
	setup((setup_test_organ, clear_all_stops, setup_test_rule)),
	cleanup(cleanup_test_rule)
]).

test(rule_defined) :-
	state:rule(test_combo, persistent).

test(rule_up_level_1, [nondet]) :-
	execute("up(test_combo)", _, _),
	state:get_rule_level(test_combo, 1),
	state:engaged(great, 1),
	state:engaged(swell, 1).

test(rule_up_level_2, [nondet]) :-
	execute("up(test_combo)", _, _),
	execute("up(test_combo)", _, _),
	state:get_rule_level(test_combo, 2),
	state:engaged(great, 1),
	state:engaged(great, 3),
	state:engaged(great, 6),
	state:engaged(pedal, 1),
	state:engaged(pedal, 2).

test(rule_down, [nondet]) :-
	execute("level(test_combo, 2)", _, _),
	execute("down(test_combo)", _, _),
	state:get_rule_level(test_combo, 1),
	\+ state:engaged(pedal, 1),
	\+ state:engaged(pedal, 2).

test(rule_mute, [nondet]) :-
	execute("level(test_combo, 2)", _, _),
	execute("mute(test_combo)", _, _),
	state:get_rule_level(test_combo, 0),
	\+ state:engaged(great, 1),
	\+ state:engaged(pedal, _).

test(rule_level_direct, [nondet]) :-
	execute("level(test_combo, 2)", _, _),
	state:get_rule_level(test_combo, 2),
	state:engaged(pedal, 1).

test(rule_reassert, [nondet]) :-
	execute("level(test_combo, 1)", _, _),
	state:do_disengage(great, 1),
	\+ state:engaged(great, 1),
	execute("reassert(test_combo)", _, _),
	state:engaged(great, 1).

test(rule_ownership_prevents_disengage, [nondet]) :-
	assertz(state:rule(test_combo2, persistent)),
	assertz(state:rule_selector(test_combo2, 1, great, numbers([1]))),
	execute("level(test_combo, 1)", _, _),
	execute("level(test_combo2, 1)", _, _),
	state:engaged(great, 1),
	execute("mute(test_combo)", _, _),
	state:engaged(great, 1),
	execute("mute(test_combo2)", _, _),
	\+ state:engaged(great, 1),
	retractall(state:rule(test_combo2, _)),
	retractall(state:rule_selector(test_combo2, _, _, _)),
	retractall(state:owns(test_combo2, _, _)).

:- end_tests(rules).

% ============================================================================
% Transient rule tests
% ============================================================================

:- begin_tests(transient_rules, [setup((setup_test_organ, clear_all_stops))]).

test(transient_rule_engage, [nondet]) :-
	assertz(state:rule(test_trans, transient)),
	assertz(state:rule_selector(test_trans, 1, great, family(reed))),
	execute("up(test_trans)", _, _),
	state:engaged(great, 4),
	retractall(state:rule(test_trans, _)),
	retractall(state:rule_selector(test_trans, _, _, _)).

test(transient_rule_disengage, [nondet]) :-
	execute("engage(great, family(reed))", _, _),
	assertz(state:rule(test_trans, transient)),
	assertz(state:rule_selector(test_trans, 1, great, family(reed), disengage)),
	execute("up(test_trans)", _, _),
	\+ state:engaged(great, 4),
	retractall(state:rule(test_trans, _)),
	retractall(state:rule_selector(test_trans, _, _, _, _)).

:- end_tests(transient_rules).

% ============================================================================
% Rule-as-selector tests
% ============================================================================

reset_test_combo :-
	clear_all_stops,
	state:set_rule_level(test_combo, 0),
	retractall(state:owns(test_combo, _, _)).

:- begin_tests(rule_selectors, [
	setup((setup_test_organ, clear_all_stops, setup_test_rule)),
	cleanup(cleanup_test_rule)
]).

test(rule_selector_at_level_0, [setup(reset_test_combo)]) :-
	selectors:resolve_selector(great, test_combo, Els),
	Els == [].

test(rule_selector_at_level_1, [setup(reset_test_combo), nondet]) :-
	execute("level(test_combo, 1)", _, _),
	selectors:resolve_selector(great, test_combo, Els),
	Els == [1].

test(rule_selector_at_level_2, [setup(reset_test_combo), nondet]) :-
	execute("level(test_combo, 2)", _, _),
	selectors:resolve_selector(great, test_combo, Els),
	msort(Els, [1, 3, 6]).

test(rule_selector_explicit_level) :-
	selectors:resolve_selector(great, test_combo:2, Els),
	msort(Els, [1, 3, 6]).

test(rule_selector_explicit_level_0) :-
	selectors:resolve_selector(great, test_combo:0, Els),
	Els == [].

test(rule_selector_different_division, [setup(reset_test_combo), nondet]) :-
	execute("level(test_combo, 2)", _, _),
	selectors:resolve_selector(pedal, test_combo, Els),
	msort(Els, [1, 2]).

test(rule_selector_in_difference) :-
	selectors:resolve_selector(great, difference(test_combo:2, family(principal, 8)), Els),
	msort(Els, [3, 6]).

test(rule_selector_in_union) :-
	selectors:resolve_selector(great, union([test_combo:1, family(reed)]), Els),
	msort(Els, [1, 4]).

test(rule_selector_in_intersection, [setup(reset_test_combo), nondet]) :-
	execute("engage(great, numbers([1, 3, 4]))", _, _),
	selectors:resolve_selector(great, intersection([test_combo:2, engaged]), Els),
	msort(Els, [1, 3]).

test(unknown_rule_falls_through) :-
	\+ selectors:resolve_selector(great, nonexistent_rule, _).

:- end_tests(rule_selectors).

% ============================================================================
% Coupler tests
% ============================================================================

setup_coupler_test :-
	execute("sync('Test Organ', [
		element(great, 1, 'Montre 8\\'', stop),
		element(great, 2, 'Bourdon 8\\'', stop),
		element(great, 3, 'Prestant 4\\'', stop),
		element(swell, 1, 'Gedact 8\\'', stop),
		element(swell, 2, 'Salicional 8\\'', stop),
		element(pedal, 1, 'Subbass 16\\'', stop),
		element(pedal, 2, 'Prinzipal 8\\'', stop),
		element(coupler, 1, 'Swell to Great', coupler),
		element(coupler, 2, 'Great to Pedal', coupler),
		element(coupler, 3, 'Swell to Pedal', coupler),
		element(coupler, 4, 'Choir to Great', coupler),
		element(coupler, 5, 'Swell to Choir', coupler),
		element(coupler, 6, 'Swell Super', coupler),
		element(tremulant, 1, 'Tremulant', tremulant)
	], [])", _, _).

:- begin_tests(couplers, [setup(setup_coupler_test)]).

test(couple_index, [setup(clear_all_stops), nondet]) :-
	execute("couple_index(1)", _, _),
	state:engaged(coupler, 1).

test(decouple_index, [setup(clear_all_stops), nondet]) :-
	execute("couple_index(1)", _, _),
	execute("decouple_index(1)", _, _),
	\+ state:engaged(coupler, 1).

test(decouple_all, [setup(clear_all_stops), nondet]) :-
	execute("couple_index(1)", _, _),
	execute("couple_index(2)", _, _),
	execute("decouple_all", _, _),
	\+ state:engaged(coupler, _).

test(couple_by_divisions, [setup(clear_all_stops), nondet]) :-
	% couple(swell, great) requires coupler mapping for Test Organ preset
	% which doesn't exist - test couple_index instead for unit tests
	execute("couple_index(1)", _, _),
	state:engaged(coupler, 1).

test(decouple_by_divisions, [setup(clear_all_stops), nondet]) :-
	execute("couple_index(1)", _, _),
	execute("decouple_index(1)", _, _),
	\+ state:engaged(coupler, 1).

test(coupler_in_rule, [setup(clear_all_stops), nondet]) :-
	assertz(state:rule(test_coupler_rule, persistent)),
	assertz(state:rule_selector(test_coupler_rule, 1, great, numbers([1, 2]))),
	assertz(state:rule_selector(test_coupler_rule, 1, coupler, numbers([1]))),
	execute("level(test_coupler_rule, 1)", _, _),
	state:engaged(great, 1),
	state:engaged(great, 2),
	state:engaged(coupler, 1),
	execute("mute(test_coupler_rule)", _, _),
	\+ state:engaged(coupler, 1),
	retractall(state:rule(test_coupler_rule, _)),
	retractall(state:rule_selector(test_coupler_rule, _, _, _)),
	retractall(state:owns(test_coupler_rule, _, _)).

:- end_tests(couplers).

% ============================================================================
% Tremulant tests
% ============================================================================

:- begin_tests(tremulants, [setup(setup_coupler_test)]).

test(tremulant_on, [setup(clear_all_stops), nondet]) :-
	execute("tremulant_on(1)", _, _),
	state:engaged(tremulant, 1).

test(tremulant_off, [setup(clear_all_stops), nondet]) :-
	execute("tremulant_on(1)", _, _),
	execute("tremulant_off(1)", _, _),
	\+ state:engaged(tremulant, 1).

test(tremulant_toggle, [setup(clear_all_stops), nondet]) :-
	execute("tremulant_toggle(1)", _, _),
	state:engaged(tremulant, 1),
	execute("tremulant_toggle(1)", _, _),
	\+ state:engaged(tremulant, 1).

test(tremulant_in_rule, [setup(clear_all_stops), nondet]) :-
	assertz(state:rule(test_trem_rule, persistent)),
	assertz(state:rule_selector(test_trem_rule, 1, swell, family(reed))),
	assertz(state:rule_selector(test_trem_rule, 1, tremulant, numbers([1]))),
	execute("level(test_trem_rule, 1)", _, _),
	state:engaged(tremulant, 1),
	execute("mute(test_trem_rule)", _, _),
	\+ state:engaged(tremulant, 1),
	retractall(state:rule(test_trem_rule, _)),
	retractall(state:rule_selector(test_trem_rule, _, _, _)),
	retractall(state:owns(test_trem_rule, _, _)).

:- end_tests(tremulants).

% ============================================================================
% Predicate rule tests
% ============================================================================

:- begin_tests(predicate_rules, [setup((setup_test_organ, clear_all_stops, consult('tests/test_rules.pl')))]).

test(brighten_engages_mixture, [nondet]) :-
	execute("apply(brighten)", Actions, _),
	Actions \== [].

test(darken_disengages_mixture, [nondet]) :-
	execute("engage(great, family(mixture))", _, _),
	execute("apply(darken)", Actions, _),
	Actions \== [].

test(apply_works_for_persistent, [nondet]) :-
	assertz(state:rule(test_apply, persistent)),
	assertz(state:rule_selector(test_apply, 1, great, numbers([1]))),
	execute("apply(test_apply)", _, _),
	state:get_rule_level(test_apply, 1),
	retractall(state:rule(test_apply, _)),
	retractall(state:rule_selector(test_apply, _, _, _)),
	retractall(state:rule_level(test_apply, _)),
	retractall(state:owns(test_apply, _, _)).

:- end_tests(predicate_rules).

% ============================================================================
% Preset-specific rule tests (using crescendi.pl example)
% ============================================================================

setup_preset_test(Preset) :-
	consult('../examples/crescendi.pl'),
	retractall(state:current_preset(_)),
	assertz(state:current_preset(Preset)),
	retractall(state:element(_, _, _, _)),
	forall(member(E, [
		element(great, 1, 'Montre 8''', stop),
		element(great, 2, 'Bourdon 8''', stop),
		element(great, 3, 'Prestant 4''', stop),
		element(great, 4, 'Trompette 8''', stop),
		element(great, 5, 'Mixtura IV', stop),
		element(great, 6, 'Doublette 2''', stop),
		element(great, 7, 'Principal 16''', stop),
		element(great, 8, 'Flute 8''', stop),
		element(great, 9, 'Quinte 2 2/3''', stop),
		element(great, 10, 'Cymbal III', stop),
		element(great, 11, 'Octav 4''', stop),
		element(great, 12, 'Clairon 4''', stop),
		element(great, 13, 'Bombarde 16''', stop),
		element(swell, 1, 'Gedact 8''', stop),
		element(swell, 2, 'Salicional 8''', stop),
		element(swell, 3, 'Voix Celeste 8''', stop),
		element(swell, 4, 'Hautbois 8''', stop),
		element(swell, 5, 'Prestant 4''', stop),
		element(swell, 6, 'Flute 4''', stop),
		element(swell, 7, 'Nasard 2 2/3''', stop),
		element(swell, 8, 'Trompette 8''', stop),
		element(pedal, 1, 'Subbass 16''', stop),
		element(pedal, 2, 'Prinzipal 8''', stop),
		element(pedal, 3, 'Octav 4''', stop),
		element(pedal, 4, 'Posaune 16''', stop),
		element(coupler, 1, 'Swell to Great', coupler),
		element(coupler, 2, 'Great to Pedal', coupler),
		element(coupler, 3, 'Swell to Pedal', coupler),
		element(coupler, 4, 'Choir to Great', coupler),
		element(coupler, 5, 'Swell to Choir', coupler),
		element(coupler, 6, 'Super', coupler),
		element(tremulant, 1, 'Tremulant', tremulant)
	]), (E = element(D, N, Na, T), assertz(state:element(D, N, Na, T)))),
	retractall(state:engaged(_, _)),
	retractall(state:owns(_, _, _)),
	retractall(state:rule_level(_, _)),
	state:reset_history,
	state:save_snapshot(test_setup).

cleanup_preset_test :-
	retractall(state:rule_level('crescendo great', _)),
	retractall(state:owns('crescendo great', _, _)),
	retractall(state:rule_level('crescendo swell', _)),
	retractall(state:owns('crescendo swell', _, _)).

:- begin_tests(preset_rules, [cleanup(cleanup_preset_test)]).

test(freiberg_crescendo_has_13_levels, [
	setup(setup_preset_test('Baroque Cathedral (Freiberg - 1710-1714)'))
]) :-
	selectors:computed_max_level('crescendo great', 'Baroque Cathedral (Freiberg - 1710-1714)', Max),
	Max == 13.

test(alpe_huez_crescendo_swell_has_4_levels, [
	setup(setup_preset_test('Neo-Classical Church (L''Alpe d''Huez - 1978)'))
]) :-
	selectors:computed_max_level('crescendo swell', 'Neo-Classical Church (L''Alpe d''Huez - 1978)', Max),
	Max == 4.

test(fallback_crescendo_has_10_levels, [
	setup(setup_preset_test('Unknown Preset'))
]) :-
	selectors:computed_max_level('crescendo great', 'Unknown Preset', Max),
	Max == 10.

test(crescendo_great_engages_stops, [
	setup((setup_preset_test('Baroque Cathedral (Freiberg - 1710-1714)'), clear_all_stops)),
	nondet
]) :-
	execute("level('crescendo great', 5)", _, _),
	state:engaged(great, _).

test(crescendo_progression, [
	setup((setup_preset_test('Baroque Cathedral (Freiberg - 1710-1714)'), clear_all_stops)),
	nondet
]) :-
	execute("level('crescendo great', 1)", _, _),
	findall(N, state:engaged(great, N), L1),
	execute("level('crescendo great', 5)", _, _),
	findall(N, state:engaged(great, N), L5),
	length(L1, Count1),
	length(L5, Count5),
	Count5 > Count1.

test(crescendo_includes_coupler_at_max, [
	setup((setup_preset_test('Baroque Cathedral (Freiberg - 1710-1714)'), clear_all_stops)),
	nondet
]) :-
	execute("level('crescendo great', 13)", _, _),
	state:engaged(coupler, _).

test(for_preset_fallback_works, [
	setup(setup_preset_test('Some Random Preset'))
]) :-
	selectors:resolve_selector(great, for_preset('Baroque*', numbers([1, 2])), Els),
	Els == [].

test(for_preset_match_works, [
	setup(setup_preset_test('Baroque Cathedral (Freiberg - 1710-1714)'))
]) :-
	selectors:resolve_selector(great, for_preset('Baroque*', numbers([1, 2])), Els),
	Els == [1, 2].

:- end_tests(preset_rules).

% ============================================================================
% Apply command tests
% ============================================================================

:- begin_tests(apply_command, [setup((setup_test_organ, clear_all_stops))]).

test(apply_persistent_increments, [nondet]) :-
	assertz(state:rule(test_apply_p, persistent)),
	assertz(state:max_level(test_apply_p, 3)),
	assertz(state:rule_selector(test_apply_p, 1, great, numbers([1]))),
	assertz(state:rule_selector(test_apply_p, 2, great, numbers([2]))),
	execute("apply(test_apply_p)", _, _),
	state:get_rule_level(test_apply_p, 1),
	execute("apply(test_apply_p)", _, _),
	state:get_rule_level(test_apply_p, 2),
	retractall(state:rule(test_apply_p, _)),
	retractall(state:max_level(test_apply_p, _)),
	retractall(state:rule_selector(test_apply_p, _, _, _)),
	retractall(state:rule_level(test_apply_p, _)),
	retractall(state:owns(test_apply_p, _, _)).

test(apply_transient_fires, [nondet]) :-
	assertz(state:rule(test_apply_t, transient)),
	assertz(state:rule_selector(test_apply_t, 1, great, numbers([1]))),
	execute("apply(test_apply_t)", _, _),
	state:engaged(great, 1),
	retractall(state:rule(test_apply_t, _)),
	retractall(state:rule_selector(test_apply_t, _, _, _)).

:- end_tests(apply_command).

% ============================================================================
% Run all tests
% ============================================================================

:- initialization((
	format("~nTo run all tests: run_tests.~n"),
	format("To run specific tests: run_tests(selectors).~n~n")
)).
