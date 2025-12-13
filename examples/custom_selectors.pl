% custom_selectors.pl - Custom selector examples
%
% Demonstrates defining your own selectors for use with engage, disengage,
% toggle, solo, and in rule definitions.
%
% Usage:
%   engage(great, evens).
%   solo(swell, odds).
%   engage(great, range(3, 8)).
%   engage(swell, random_n(3)).
%
% Compound selectors (built-in):
%   engage(swell, intersection([range(1, 5), evens])).
%   solo(great, union([odds, family(reed)])).

:- multifile selectors:resolve_selector/3.

% ============================================================================
% Positional selectors
% ============================================================================

% Even-numbered stops
selectors:resolve_selector(Division, evens, Stops) :-
	findall(N, (state:element(Division, N, _, stop), N mod 2 =:= 0), Unsorted),
	sort(Unsorted, Stops).

% Odd-numbered stops
selectors:resolve_selector(Division, odds, Stops) :-
	findall(N, (state:element(Division, N, _, stop), N mod 2 =:= 1), Unsorted),
	sort(Unsorted, Stops).

% Range of stop numbers: range(Low, High) inclusive
selectors:resolve_selector(Division, range(Low, High), Stops) :-
	findall(N, (
		state:element(Division, N, _, stop),
		N >= Low, N =< High
	), Unsorted),
	sort(Unsorted, Stops).

% First N stops (by number)
selectors:resolve_selector(Division, first_n(N), Stops) :-
	findall(Num, state:element(Division, Num, _, stop), All),
	sort(All, Sorted),
	helpers:take_first(Sorted, N, Stops).

% Last N stops (by number)
selectors:resolve_selector(Division, last_n(N), Stops) :-
	findall(Num, state:element(Division, Num, _, stop), All),
	sort(All, Sorted),
	helpers:take_last(Sorted, N, Stops).

% Random N stops from division
selectors:resolve_selector(Division, random_n(N), Stops) :-
	findall(Num, state:element(Division, Num, _, stop), All),
	random_permutation(All, Shuffled),
	helpers:take_first(Shuffled, N, Unsorted),
	sort(Unsorted, Stops).

% ============================================================================
% State-based selectors
% ============================================================================

% Complement of currently engaged (everything that's off)
selectors:resolve_selector(Division, complement, Stops) :-
	findall(N, (
		state:element(Division, N, _, stop),
		\+ state:engaged(Division, N)
	), Unsorted),
	sort(Unsorted, Stops).

% Stops not owned by any active rule
selectors:resolve_selector(Division, unowned, Stops) :-
	findall(N, (
		state:element(Division, N, _, stop),
		\+ state:owns(_, Division, N)
	), Unsorted),
	sort(Unsorted, Stops).

% Stops that ARE owned by some rule
selectors:resolve_selector(Division, owned, Stops) :-
	findall(N, (
		state:element(Division, N, _, stop),
		state:owns(_, Division, N)
	), Unsorted),
	sort(Unsorted, Stops).

% ============================================================================
% Name-based selectors
% ============================================================================

% Stops whose name contains a substring (case-insensitive)
selectors:resolve_selector(Division, name_contains(Sub), Stops) :-
	downcase_atom(Sub, SubLower),
	findall(N, (
		state:element(Division, N, Name, stop),
		Name \= '',
		downcase_atom(Name, NameLower),
		sub_atom(NameLower, _, _, _, SubLower)
	), Unsorted),
	sort(Unsorted, Stops).

% Stops with non-empty names (filters out unused stop slots)
selectors:resolve_selector(Division, named, Stops) :-
	findall(N, (
		state:element(Division, N, Name, stop),
		Name \= ''
	), Unsorted),
	sort(Unsorted, Stops).

