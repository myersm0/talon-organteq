% server.pl - Prolog HTTP server for Organteq registration control
% Core predicates only - rules are loaded from separate files

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_parameters)).

% ============================================================================
% Dynamic predicates - all state managed by Python client
% ============================================================================

:- dynamic stop/4.              % stop(Manual, Number, Name, Type)
:- dynamic engaged/2.           % engaged(Manual, Number)
:- dynamic combination_level/2. % combination_level(RuleId, Level)
:- dynamic owns/3.              % owns(RuleId, Manual, Number)
:- dynamic rule_level/2.        % rule_level(RuleId, Level)
:- dynamic current_preset/1.    % current_preset(Name)

% Rule definitions - loaded from rule files
:- dynamic rule/2.              % rule(RuleId, Type) - Type: persistent | transient
:- dynamic max_level/2.         % max_level(RuleId, MaxLevel)
:- dynamic antonym/2.           % antonym(RuleId, AntonymId)
:- dynamic rule_level_selector/4. % rule_level_selector(RuleId, Level, Manual, Selector)

% Classification - loaded from classification files
:- dynamic family_of/2.         % family_of(BaseName, Family)

% User-defined constraints and violations
:- dynamic violation_rule/1.    % violation_rule(Head :- Body) - user can define these

% Allow resolve_selector clauses to be non-contiguous (for readability)
% (also allow multifile to allow users to specify their own)
:- multifile resolve_selector/3.
:- discontiguous resolve_selector/3.

% ============================================================================
% Stop classification predicates
% ============================================================================

% Extract base name: everything before the footage pattern
% Matches: "Name 8'", "Name 16'", "Name 2'2/3", "Name III", "Name 3f.", etc.
base_name(FullName, BaseName) :-
	atom_string(FullName, FullStr),
	(   re_matchsub("^(.+?)\\s+\\d+'?.*$"/i, FullStr, Sub, [])
	->  get_dict(1, Sub, BaseStr)
	;   re_matchsub("^(.+?)\\s+[IVX]+\\.?\\s*$"/i, FullStr, Sub, [])
	->  get_dict(1, Sub, BaseStr)
	;   re_matchsub("^(.+?)\\s+\\d+f\\.?\\s*$"/i, FullStr, Sub, [])
	->  get_dict(1, Sub, BaseStr)
	;   BaseStr = FullStr
	),
	atom_string(BaseName, BaseStr).

stop_family(Manual, Number, Family) :-
	stop(Manual, Number, Name, stop),
	base_name(Name, BaseName),
	family_of(BaseName, Family).

stop_footage(Manual, Number, Footage) :-
	stop(Manual, Number, Name, stop),
	atom_string(Name, NameStr),
	(   sub_string(NameStr, _, _, _, "32'") -> Footage = 32
	;   sub_string(NameStr, _, _, _, "16'") -> Footage = 16
	;   sub_string(NameStr, _, _, _, "8'") -> Footage = 8
	;   sub_string(NameStr, _, _, _, "4'") -> Footage = 4
	;   sub_string(NameStr, _, _, _, "2'") -> Footage = 2
	;   sub_string(NameStr, _, _, _, "1'") -> Footage = 1
	;   Footage = unknown
	).

matches_footage(_, _, any) :- !.
matches_footage(Manual, Number, Footage) :-
	stop_footage(Manual, Number, Footage).

% ============================================================================
% Stop selection predicates
% ============================================================================

stops_by_family(Manual, Family, Stops) :-
	findall(Number, stop_family(Manual, Number, Family), Stops).

stops_by_family(Manual, Family, Footage, Stops) :-
	findall(Number, (
		stop_family(Manual, Number, Family),
		matches_footage(Manual, Number, Footage)
	), Stops).

% Basic selectors
resolve_selector(_, numbers(Numbers), Numbers) :- !.

resolve_selector(Manual, family(Family), Stops) :-
	stops_by_family(Manual, Family, Stops), !.

resolve_selector(Manual, family(Family, Footage), Stops) :-
	stops_by_family(Manual, Family, Footage, Stops), !.

resolve_selector(Manual, family(Family, Footage, Limit, Method), LimitedStops) :-
	stops_by_family(Manual, Family, Footage, AllStops),
	apply_limit(AllStops, Limit, Method, LimitedStops), !.

resolve_selector(Manual, names(Names), Stops) :-
	findall(Number, (
		member(Name, Names),
		stop(Manual, Number, Name, stop)
	), Stops), !.

% Currently engaged stops
resolve_selector(Manual, engaged, Stops) :-
	findall(Number, engaged(Manual, Number), Stops), !.

% Compound selectors: set operations
resolve_selector(Manual, union(Selectors), Stops) :-
	findall(S, (
		member(Sel, Selectors),
		resolve_selector(Manual, Sel, SelStops),
		member(S, SelStops)
	), AllStops),
	sort(AllStops, Stops), !.

resolve_selector(Manual, intersection(Selectors), Stops) :-
	Selectors = [First|Rest],
	resolve_selector(Manual, First, FirstStops),
	foldl(intersect_selector(Manual), Rest, FirstStops, Stops), !.

intersect_selector(Manual, Selector, AccStops, Result) :-
	resolve_selector(Manual, Selector, SelStops),
	intersection(AccStops, SelStops, Result).

resolve_selector(Manual, difference(Base, Subtract), Stops) :-
	resolve_selector(Manual, Base, BaseStops),
	resolve_selector(Manual, Subtract, SubStops),
	subtract(BaseStops, SubStops, Stops), !.

% Raw Prolog expression selector
% Usage: expression(N, Goal) where Goal uses N as the stop number variable
resolve_selector(Manual, expression(Goal), Stops) :-
	findall(N, (
		stop(Manual, N, _, stop),
		call(Goal, Manual, N)
	), AllStops),
	sort(AllStops, Stops), !.

apply_limit(Stops, Limit, first, Limited) :-
	length(Stops, Len),
	Len >= Limit,
	length(Limited, Limit),
	append(Limited, _, Stops), !.
apply_limit(Stops, _, first, Stops) :- !.

apply_limit(Stops, Limit, last, Limited) :-
	length(Stops, Len),
	Len >= Limit,
	Skip is Len - Limit,
	length(Prefix, Skip),
	append(Prefix, Limited, Stops), !.
apply_limit(Stops, _, last, Stops) :- !.

apply_limit(Stops, Limit, random, Limited) :-
	random_permutation(Stops, Shuffled),
	apply_limit(Shuffled, Limit, first, Limited), !.

% ============================================================================
% Ownership reasoning
% ============================================================================

is_owned(Manual, Number) :-
	owns(_, Manual, Number).

still_owned_after_release(RuleId, Manual, Number) :-
	owns(OtherRule, Manual, Number),
	OtherRule \= RuleId.

% ============================================================================
% Rule application logic
% ============================================================================

rule_stops_cumulative(RuleId, TargetLevel, Manual, Stops) :-
	rule(RuleId, persistent),
	findall(Stop, (
		between(1, TargetLevel, Level),
		rule_level_selector(RuleId, Level, Manual, Selector),
		resolve_selector(Manual, Selector, LevelStops),
		member(Stop, LevelStops)
	), AllStops),
	sort(AllStops, Stops).

rule_stops_at_level(RuleId, Level, Manual, Stops) :-
	findall(Stop, (
		rule_level_selector(RuleId, Level, Manual, Selector),
		resolve_selector(Manual, Selector, LevelStops),
		member(Stop, LevelStops)
	), AllStops),
	sort(AllStops, Stops).

apply_rule_delta(RuleId, OldLevel, NewLevel, Manuals, Actions) :-
	findall(Manual-OldStops, (
		member(Manual, Manuals),
		(OldLevel > 0 -> rule_stops_cumulative(RuleId, OldLevel, Manual, OldStops) ; OldStops = [])
	), OldPairs),
	findall(Manual-NewStops, (
		member(Manual, Manuals),
		(NewLevel > 0 -> rule_stops_cumulative(RuleId, NewLevel, Manual, NewStops) ; NewStops = [])
	), NewPairs),
	compute_delta_actions(OldPairs, NewPairs, RuleId, Actions).

compute_delta_actions([], [], _, []).
compute_delta_actions([Manual-Old|OldRest], [Manual-New|NewRest], RuleId, Actions) :-
	subtract(New, Old, ToAdd),
	subtract(Old, New, ToRemove),
	findall(engage(Manual, S), member(S, New), EngageActions),
	findall(claim(RuleId, Manual, S), member(S, ToAdd), ClaimActions),
	findall(Action, (
		member(S, ToRemove),
		(   still_owned_after_release(RuleId, Manual, S)
		->  Action = release(RuleId, Manual, S)
		;   member(Action, [release(RuleId, Manual, S), disengage(Manual, S)])
		)
	), RemoveActions),
	compute_delta_actions(OldRest, NewRest, RuleId, RestActions),
	append([ClaimActions, EngageActions, RemoveActions, RestActions], Actions).

reassert_rule(RuleId, Manuals, Actions) :-
	(combination_level(RuleId, Level) -> true ; Level = 0),
	Level > 0,
	findall(engage(Manual, S), (
		member(Manual, Manuals),
		rule_stops_cumulative(RuleId, Level, Manual, Stops),
		member(S, Stops)
	), Actions).

% ============================================================================
% Violation checking - users define their own violation/1 rules
% ============================================================================

violations(Vs) :-
	findall(V, violation(V), Vs).

% ============================================================================
% Coupler support (placeholder)
% ============================================================================

:- dynamic coupler_spec/5.

% ============================================================================
% HTTP Server
% ============================================================================

:- http_handler(root(query), handle_query, []).
:- http_handler(root(assert), handle_assert, []).
:- http_handler(root(retract), handle_retract, []).
:- http_handler(root(reset), handle_reset, []).
:- http_handler(root(load), handle_load, []).

server(Port) :-
	http_server(http_dispatch, [port(Port)]).

handle_query(Request) :-
	http_read_json_dict(Request, Dict),
	Query = Dict.query,
	catch(
		(   term_string(Term, Query),
			findall(Result, (call(Term), term_to_dict(Term, Result)), Results),
			reply_json_dict(_{status: ok, results: Results})
		),
		Error,
		(   term_string(Error, ErrorStr),
			reply_json_dict(_{status: error, message: ErrorStr})
		)
	).

handle_assert(Request) :-
	http_read_json_dict(Request, Dict),
	Facts = Dict.facts,
	forall(member(FactStr, Facts), (
		term_string(Fact, FactStr),
		assertz(Fact)
	)),
	reply_json_dict(_{status: ok}).

handle_retract(Request) :-
	http_read_json_dict(Request, Dict),
	Facts = Dict.facts,
	forall(member(FactStr, Facts), (
		term_string(Fact, FactStr),
		retractall(Fact)
	)),
	reply_json_dict(_{status: ok}).

handle_reset(_Request) :-
	% Clear all dynamic state
	retractall(stop(_, _, _, _)),
	retractall(engaged(_, _)),
	retractall(combination_level(_, _)),
	retractall(owns(_, _, _)),
	retractall(rule_level(_, _)),
	retractall(current_preset(_)),
	reply_json_dict(_{status: ok}).

handle_load(Request) :-
	http_read_json_dict(Request, Dict),
	File = Dict.file,
	catch(
		(   consult(File),
			reply_json_dict(_{status: ok})
		),
		Error,
		(   term_string(Error, ErrorStr),
			reply_json_dict(_{status: error, message: ErrorStr})
		)
	).

% ============================================================================
% JSON serialization
% ============================================================================

term_to_json(Term, Term) :-
	(atom(Term) ; number(Term) ; string(Term)), !.

term_to_json(Term, JsonList) :-
	is_list(Term), !,
	maplist(term_to_json, Term, JsonList).

term_to_json(Term, Dict) :-
	compound(Term), !,
	Term =.. [Functor|Args],
	maplist(term_to_json, Args, JsonArgs),
	Dict = _{functor: Functor, args: JsonArgs}.

term_to_json(Term, null) :-
	var(Term), !.

term_to_dict(Term, Dict) :-
	term_to_json(Term, Dict).

% ============================================================================
% Convenience predicates
% ============================================================================

list_stops(Manual, Stops) :-
	findall(_{number: N, name: Name, type: Type}, stop(Manual, N, Name, Type), Stops).

list_engaged(Manual, Numbers) :-
	findall(N, engaged(Manual, N), Numbers).

state_summary(Summary) :-
	findall(Manual-Engaged, (
		member(Manual, [pedal, choir, great, swell]),
		list_engaged(Manual, Engaged)
	), Summary).
