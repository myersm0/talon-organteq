% Integration tests requiring Organteq
%
% These tests require:
%   - Organteq running with --serve flag
%   - Prolog server NOT running (tests run directly)
%
% Run with:
%   cd prolog && swipl -g "consult('main.pl'), consult('tests/test_integration.pl'), run_tests, halt."

:- use_module(library(plunit)).
:- use_module(core(organteq), [organteq_available/0, organteq_call/3, organteq_sync/0]).
:- use_module(core(server), [execute/3]).
:- use_module(core(selectors), [computed_max_level/3]).

% ============================================================================
% Integration test fixtures
% ============================================================================

load_baroque_i :-
	organteq_call(loadPreset, ['Baroque Cathedral I', ''], _),
	sleep(0.5),
	organteq_sync.

load_freiberg :-
	organteq_call(loadPreset, ['Baroque Cathedral (Freiberg - 1710-1714)', ''], _),
	sleep(0.5),
	organteq_sync.

load_alpe_huez :-
	organteq_call(loadPreset, ['Neo-Classical Church (L''Alpe d''Huez - 1978)', ''], _),
	sleep(0.5),
	organteq_sync.

clear_and_sync :-
	execute("clear_all", _, _),
	execute("decouple_all", _, _).

load_crescendi_rules :-
	consult('../examples/crescendi.pl').

% ============================================================================
% Live preset tests
% ============================================================================

:- begin_tests(live_preset, [
	condition(organteq:organteq_available),
	setup(load_baroque_i)
]).

test(preset_loaded) :-
	state:current_preset('Baroque Cathedral I').

test(stops_synced) :-
	findall(N, state:element(great, N, _, stop), Stops),
	length(Stops, Count),
	Count > 0.

test(engage_updates_organteq, [setup(clear_and_sync), nondet]) :-
	execute("engage(great, numbers([1, 2]))", _, _),
	state:engaged(great, 1),
	state:engaged(great, 2).

test(clear_updates_organteq, [setup(clear_and_sync), nondet]) :-
	execute("engage(great, numbers([1, 2, 3]))", _, _),
	execute("clear(great)", _, _),
	\+ state:engaged(great, _).

:- end_tests(live_preset).

% ============================================================================
% Live coupler tests
% ============================================================================

:- begin_tests(live_couplers, [
	condition(organteq:organteq_available),
	setup(load_baroque_i)
]).

test(couple_by_divisions, [setup(clear_and_sync), nondet]) :-
	execute("couple(swell, great)", _, _),
	state:engaged(coupler, _).

test(couple_updates_organteq, [setup(clear_and_sync), nondet]) :-
	execute("couple_index(1)", _, _),
	state:engaged(coupler, 1),
	execute("decouple_index(1)", _, _),
	\+ state:engaged(coupler, 1).

:- end_tests(live_couplers).

% ============================================================================
% Preset-specific crescendo tests (live)
% ============================================================================

:- begin_tests(live_crescendo, [
	condition(organteq:organteq_available),
	setup(load_crescendi_rules)
]).

test(freiberg_crescendo_max_level, [setup(load_freiberg)]) :-
	computed_max_level('crescendo great', 'Baroque Cathedral (Freiberg - 1710-1714)', Max),
	Max == 13.

test(freiberg_crescendo_engages, [setup((load_freiberg, clear_and_sync)), nondet]) :-
	execute("level('crescendo great', 5)", _, _),
	findall(N, state:engaged(great, N), Engaged),
	length(Engaged, Count),
	Count > 0.

test(freiberg_crescendo_max_includes_coupler, [setup((load_freiberg, clear_and_sync)), nondet]) :-
	execute("level('crescendo great', 13)", _, _),
	state:engaged(coupler, _).

test(alpe_huez_crescendo_swell, [setup(load_alpe_huez)]) :-
	computed_max_level('crescendo swell', 'Neo-Classical Church (L''Alpe d''Huez - 1978)', Max),
	Max == 4.

test(baroque_i_fallback_crescendo, [setup(load_baroque_i)]) :-
	computed_max_level('crescendo great', 'Baroque Cathedral I', Max),
	Max == 10.

:- end_tests(live_crescendo).

% ============================================================================
% Run integration tests
% ============================================================================

:- initialization((
	format("~n=== Integration Tests ===~n"),
	format("These tests require Organteq running with --serve~n"),
	format("Run: run_tests.~n~n")
)).
