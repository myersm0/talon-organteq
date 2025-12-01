% helpers.pl - Helper predicates for writing custom rules
%
% This module provides convenient predicates for querying and manipulating
% organ state when writing predicate-based rules.

:- module(helpers, [
	% Element queries
	elements_in_family/3,
	elements_with_footage/3,
	engaged_elements/2,
	disengaged_elements/2,
	engaged_in_family/3,
	disengaged_in_family/3,
	% Conditions
	all_engaged_in_family/2,
	any_engaged_in_family/2,
	none_engaged_in_family/2,
	count_engaged_in_family/3,
	% Selection
	first/2,
	take_first/3,
	take_last/3,
	% Combined helpers
	first_disengaged_in_family/3,
	last_disengaged_in_family/3,
	first_engaged_in_family/3,
	last_engaged_in_family/3
]).

:- use_module(state, [element/4, engaged/2, manual/1]).
:- use_module(classification, [element_family/3, element_footage/3]).

% ============================================================================
% Element queries
% ============================================================================

elements_in_family(Division, Family, Elements) :-
	findall(N, element_family(Division, N, Family), Elements).

elements_with_footage(Division, Footage, Elements) :-
	findall(N, element_footage(Division, N, Footage), Elements).

engaged_elements(Division, Elements) :-
	findall(N, engaged(Division, N), Elements).

disengaged_elements(Division, Elements) :-
	findall(N, (element(Division, N, _, _), \+ engaged(Division, N)), Elements).

engaged_in_family(Division, Family, Elements) :-
	findall(N, (element_family(Division, N, Family), engaged(Division, N)), Elements).

disengaged_in_family(Division, Family, Elements) :-
	findall(N, (element_family(Division, N, Family), \+ engaged(Division, N)), Elements).

% ============================================================================
% Conditions
% ============================================================================

all_engaged_in_family(Division, Family) :-
	elements_in_family(Division, Family, All),
	All \= [],
	forall(member(N, All), engaged(Division, N)).

any_engaged_in_family(Division, Family) :-
	element_family(Division, N, Family),
	engaged(Division, N), !.

none_engaged_in_family(Division, Family) :-
	\+ any_engaged_in_family(Division, Family).

count_engaged_in_family(Division, Family, Count) :-
	engaged_in_family(Division, Family, Elements),
	length(Elements, Count).

% ============================================================================
% Selection
% ============================================================================

first([H|_], H).

take_first(List, N, Result) :-
	length(Result, N),
	append(Result, _, List), !.
take_first(List, _, List).

take_last(List, N, Result) :-
	length(List, Len),
	(Len >= N ->
		Skip is Len - N,
		length(Prefix, Skip),
		append(Prefix, Result, List)
	;	Result = List
	).

% ============================================================================
% Combined helpers - common patterns
% ============================================================================

first_disengaged_in_family(Division, Family, N) :-
	disengaged_in_family(Division, Family, [N|_]).

last_disengaged_in_family(Division, Family, N) :-
	disengaged_in_family(Division, Family, Elements),
	Elements \= [],
	last(Elements, N).

first_engaged_in_family(Division, Family, N) :-
	engaged_in_family(Division, Family, [N|_]).

last_engaged_in_family(Division, Family, N) :-
	engaged_in_family(Division, Family, Elements),
	Elements \= [],
	last(Elements, N).
