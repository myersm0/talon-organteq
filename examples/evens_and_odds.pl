:- multifile state:rule/2.
:- multifile state:rule_selector/3.
:- multifile selectors:resolve_selector/3.

state:rule(evens, persistent).
state:rule_selector(evens, 1, evens).

state:rule(odds, persistent).
state:rule_selector(odds, 1, odds).

selectors:resolve_selector(Division, evens, Stops) :-
	findall(N, (state:element(Division, N, _, stop), N mod 2 =:= 0), Unsorted),
	sort(Unsorted, Stops).

selectors:resolve_selector(Division, odds, Stops) :-
	findall(N, (state:element(Division, N, _, stop), N mod 2 =:= 1), Unsorted),
	sort(Unsorted, Stops).
