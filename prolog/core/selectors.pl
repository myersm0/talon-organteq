% Selector resolution

:- module(selectors, [
	resolve_selector/3,
	dict_to_selector/2,
	% Preset matching
	preset_matches/2,
	uses_for_preset/1,
	selector_matches_preset/2,
	% Rule filtering
	rule_for_preset/2,
	rules_for_preset/2,
	computed_max_level/3,
	has_preset_specific_selectors/2,
	% Effective selectors (preset-aware)
	effective_rule_selector/3,
	effective_rule_selector/4,
	effective_rule_selector/5
]).

:- use_module(library(apply), [include/3]).

:- use_module(state, [
	element/4,
	engaged/2,
	current_preset/1,
	json_to_atom/2,
	rule/2,
	rule_predicate/1,
	rule_selector/3,
	rule_selector/4,
	rule_selector/5,
	manuals/1
]).

:- use_module(classification, [
	element_family/3,
	element_footage/3,
	base_name/2,
	alias_of/2,
	name_footage/2,
	coupler_source/2,
	coupler_destination/2,
	coupler_transposition/2
]).

:- discontiguous resolve_selector/3.
:- multifile resolve_selector/3.

% ============================================================================
% Preset pattern matching
% ============================================================================

preset_matches(Preset, Pattern) :-
	atom(Pattern),
	atom_string(Pattern, PatternStr),
	atom_string(Preset, PresetStr),
	(   sub_string(PatternStr, _, _, _, "*")
	->  glob_to_regex(PatternStr, Regex),
	    re_match(Regex, PresetStr)
	;   PresetStr = PatternStr
	).

glob_to_regex(Glob, Regex) :-
	string_chars(Glob, Chars),
	escape_and_convert(Chars, RegexChars),
	append(['^'], RegexChars, WithStart),
	append(WithStart, ['$'], Anchored),
	string_chars(RegexStr, Anchored),
	atom_string(Regex, RegexStr).

escape_and_convert([], []).
escape_and_convert(['*'|T], ['.'|['*'|T2]]) :- escape_and_convert(T, T2).
escape_and_convert(['?'|T], ['.'|T2]) :- escape_and_convert(T, T2).
escape_and_convert([H|T], ['\\'|[H|T2]]) :-
	member(H, ['.', '^', '$', '+', '(', ')', '[', ']', '{', '}', '|', '\\']),
	escape_and_convert(T, T2).
escape_and_convert([H|T], [H|T2]) :-
	H \= '*', H \= '?',
	\+ member(H, ['.', '^', '$', '+', '(', ')', '[', ']', '{', '}', '|', '\\']),
	escape_and_convert(T, T2).

% ============================================================================
% Selector preset checking
% ============================================================================

uses_for_preset(for_preset(_, _)).
uses_for_preset(Sel) :-
	compound(Sel),
	Sel \= for_preset(_, _),
	Sel =.. [_|Args],
	member(Arg, Args),
	uses_for_preset(Arg).

selector_matches_preset(for_preset(Pattern, _), Preset) :-
	!, preset_matches(Preset, Pattern).
selector_matches_preset(Sel, _) :-
	\+ uses_for_preset(Sel).

% ============================================================================
% Rule filtering by preset
% ============================================================================

rule_for_preset(RuleId, _) :-
	rule(RuleId, _),
	rule_predicate(RuleId).

rule_for_preset(RuleId, Preset) :-
	rule(RuleId, _),
	\+ rule_predicate(RuleId),
	once((
		(rule_selector(RuleId, _, Sel), selector_matches_preset(Sel, Preset))
	;   (rule_selector(RuleId, _, _, Sel), selector_matches_preset(Sel, Preset))
	;   (rule_selector(RuleId, _, _, Sel, _), selector_matches_preset(Sel, Preset))
	)).

rules_for_preset(Preset, Rules) :-
	findall(RuleId, rule_for_preset(RuleId, Preset), RulesUnsorted),
	sort(RulesUnsorted, Rules).

computed_max_level(RuleId, Preset, MaxLevel) :-
	findall(Level, (
		(   rule_selector(RuleId, Level, Sel)
		;   rule_selector(RuleId, Level, _, Sel)
		;   rule_selector(RuleId, Level, _, Sel, _)
		),
		uses_for_preset(Sel),
		selector_matches_preset(Sel, Preset)
	), PresetLevels),
	PresetLevels \= [],
	!,
	max_list(PresetLevels, MaxLevel).

computed_max_level(RuleId, _, MaxLevel) :-
	findall(Level, (
		(   rule_selector(RuleId, Level, Sel)
		;   rule_selector(RuleId, Level, _, Sel)
		;   rule_selector(RuleId, Level, _, Sel, _)
		),
		\+ uses_for_preset(Sel)
	), UniversalLevels),
	UniversalLevels \= [],
	!,
	max_list(UniversalLevels, MaxLevel).

computed_max_level(RuleId, _, MaxLevel) :-
	state:max_level(RuleId, MaxLevel).

has_preset_specific_selectors(RuleId, Preset) :-
	once((
		(   rule_selector(RuleId, _, Sel)
		;   rule_selector(RuleId, _, _, Sel)
		;   rule_selector(RuleId, _, _, Sel, _)
		),
		uses_for_preset(Sel),
		selector_matches_preset(Sel, Preset)
	)).

% ============================================================================
% Effective rule selectors (preset-aware)
% ============================================================================

effective_rule_selector(RuleId, Level, Sel) :-
	current_preset(Preset),
	has_preset_specific_selectors(RuleId, Preset),
	!,
	rule_selector(RuleId, Level, Sel),
	uses_for_preset(Sel),
	selector_matches_preset(Sel, Preset).

effective_rule_selector(RuleId, Level, Sel) :-
	rule_selector(RuleId, Level, Sel),
	\+ uses_for_preset(Sel).

effective_rule_selector(RuleId, Level, Div, Sel) :-
	current_preset(Preset),
	has_preset_specific_selectors(RuleId, Preset),
	!,
	rule_selector(RuleId, Level, Div, Sel),
	uses_for_preset(Sel),
	selector_matches_preset(Sel, Preset).

effective_rule_selector(RuleId, Level, Div, Sel) :-
	rule_selector(RuleId, Level, Div, Sel),
	\+ uses_for_preset(Sel).

effective_rule_selector(RuleId, Level, Div, Sel, Action) :-
	current_preset(Preset),
	has_preset_specific_selectors(RuleId, Preset),
	!,
	rule_selector(RuleId, Level, Div, Sel, Action),
	uses_for_preset(Sel),
	selector_matches_preset(Sel, Preset).

effective_rule_selector(RuleId, Level, Div, Sel, Action) :-
	rule_selector(RuleId, Level, Div, Sel, Action),
	\+ uses_for_preset(Sel).

% ============================================================================
% Selector resolution
% ============================================================================

resolve_selector(Division, for_preset(Pattern, InnerSelector), Elements) :-
	current_preset(Preset),
	(preset_matches(Preset, Pattern) ->
		resolve_selector(Division, InnerSelector, Elements)
	;   Elements = []
	), !.

resolve_selector(Division, numbers(Numbers), ValidElements) :-
	include(valid_element(Division), Numbers, ValidElements),
	findall(N, (member(N, Numbers), \+ element(Division, N, _, _)), Invalid),
	(Invalid \= [] ->
		format(user_error, "Warning: Invalid element numbers for ~w: ~w~n", [Division, Invalid])
	;   true
	), !.

valid_element(Division, N) :- element(Division, N, _, _).

resolve_selector(Division, all, Elements) :-
	findall(N, element(Division, N, _, _), Elements), !.

resolve_selector(Division, stops, Elements) :-
	findall(N, element(Division, N, _, stop), Elements), !.

resolve_selector(Division, family(Family), Elements) :-
	findall(N, element_family(Division, N, Family), Elements), !.

resolve_selector(Division, family(Family, any), Elements) :-
	resolve_selector(Division, family(Family), Elements), !.

resolve_selector(Division, family(Family, Footage), Elements) :-
	Footage \= any,
	findall(N, (
		element_family(Division, N, Family),
		element_footage(Division, N, Footage)
	), Elements), !.

resolve_selector(Division, family(Family, Footage, Limit, Method), Limited) :-
	(Footage = any -> resolve_selector(Division, family(Family), All)
	;   resolve_selector(Division, family(Family, Footage), All)),
	apply_limit(All, Limit, Method, Limited), !.

resolve_selector(Division, names(Names), Elements) :-
	findall(N, (
		member(Name, Names),
		element(Division, N, Name, _)
	), ExactMatches),
	(ExactMatches \= [] ->
		Elements = ExactMatches
	;
		findall(N, (
			member(Name, Names),
			base_name(Name, ReqBase),
			name_footage(Name, ReqFootage),
			element(Division, N, ElemName, _),
			base_name(ElemName, ElemBase),
			alias_of(ReqBase, ElemBase),
			name_footage(ElemName, ReqFootage)
		), Elements)
	), !.

resolve_selector(Division, engaged, Elements) :-
	findall(N, engaged(Division, N), Elements), !.

resolve_selector(Division, disengaged, Elements) :-
	findall(N, (element(Division, N, _, _), \+ engaged(Division, N)), Elements), !.

resolve_selector(Division, type(Type), Elements) :-
	findall(N, element(Division, N, _, Type), Elements), !.

resolve_selector(_, couplers, Elements) :-
	findall(N, element(coupler, N, _, coupler), Elements), !.

resolve_selector(_, mono_couplers, Elements) :-
	findall(N, element(mono_coupler, N, _, mono_coupler), Elements), !.

resolve_selector(_, tremulants, Elements) :-
	findall(N, element(tremulant, N, _, tremulant), Elements), !.

resolve_selector(_, coupler_from(Source), Elements) :-
	findall(N, coupler_source(N, Source), Elements), !.

resolve_selector(_, coupler_to(Dest), Elements) :-
	findall(N, coupler_destination(N, Dest), Elements), !.

resolve_selector(_, coupler(Source, Dest), Elements) :-
	findall(N, (coupler_source(N, Source), coupler_destination(N, Dest)), Elements), !.

resolve_selector(_, coupler(Source, Dest, Trans), Elements) :-
	findall(N, (
		coupler_source(N, Source),
		coupler_destination(N, Dest),
		coupler_transposition(N, Trans)
	), Elements), !.

resolve_selector(Division, union(Selectors), Elements) :-
	findall(E, (
		member(Sel, Selectors),
		resolve_selector(Division, Sel, SelElements),
		member(E, SelElements)
	), All),
	sort(All, Elements), !.

resolve_selector(Division, intersection([First|Rest]), Elements) :-
	resolve_selector(Division, First, FirstElements),
	foldl(intersect_selector(Division), Rest, FirstElements, Elements), !.

resolve_selector(Division, difference(Base, Subtract), Elements) :-
	resolve_selector(Division, Base, BaseElements),
	resolve_selector(Division, Subtract, SubElements),
	subtract(BaseElements, SubElements, Elements), !.

resolve_selector(Division, expression(Goal), Elements) :-
	findall(N, (
		element(Division, N, _, _),
		call(Goal, Division, N)
	), All),
	sort(All, Elements), !.

intersect_selector(Division, Selector, Acc, Result) :-
	resolve_selector(Division, Selector, SelElements),
	intersection(Acc, SelElements, Result).

apply_limit(Elements, Limit, first, Limited) :-
	length(Elements, Len),
	(Len >= Limit -> length(Limited, Limit), append(Limited, _, Elements)
	;   Limited = Elements), !.

apply_limit(Elements, Limit, last, Limited) :-
	length(Elements, Len),
	(Len >= Limit ->
		Skip is Len - Limit,
		length(Prefix, Skip),
		append(Prefix, Limited, Elements)
	;   Limited = Elements), !.

apply_limit(Elements, Limit, random, Limited) :-
	random_permutation(Elements, Shuffled),
	apply_limit(Shuffled, Limit, first, Limited), !.

% ============================================================================
% Dict to selector conversion (for JSON API)
% ============================================================================

dict_to_selector(Dict, Selector) :-
	is_dict(Dict),
	get_dict(by, Dict, ByRaw),
	json_to_atom(ByRaw, By),
	dict_to_selector_by_type(By, Dict, Selector), !.

dict_to_selector(Atom, Atom) :-
	atom(Atom), !.

dict_to_selector(String, Atom) :-
	string(String),
	atom_string(Atom, String), !.

dict_to_selector(List, numbers(List)) :-
	is_list(List), !.

get_dict_default(Key, Dict, Value, Default) :-
	(get_dict(Key, Dict, Value) -> true ; Value = Default).

dict_to_selector_by_type(numbers, Dict, numbers(Values)) :-
	get_dict(values, Dict, Values).

dict_to_selector_by_type(family, Dict, Selector) :-
	get_dict(values, Dict, FamilyRaw),
	json_to_atom(FamilyRaw, Family),
	get_dict_default(footage, Dict, Footage, any),
	get_dict_default(limit, Dict, Limit, none),
	get_dict_default(limit_method, Dict, MethodRaw, first),
	json_to_atom(MethodRaw, Method),
	(Limit = none ->
		(Footage = any -> Selector = family(Family) ; Selector = family(Family, Footage))
	;   Selector = family(Family, Footage, Limit, Method)
	).

dict_to_selector_by_type(names, Dict, names(AtomNames)) :-
	get_dict(values, Dict, Values),
	maplist(json_to_atom, Values, AtomNames).

dict_to_selector_by_type(engaged, _, engaged).
dict_to_selector_by_type(disengaged, _, disengaged).
dict_to_selector_by_type(all, _, all).
dict_to_selector_by_type(stops, _, stops).
dict_to_selector_by_type(couplers, _, couplers).
dict_to_selector_by_type(mono_couplers, _, mono_couplers).
dict_to_selector_by_type(tremulants, _, tremulants).

dict_to_selector_by_type(type, Dict, type(TypeAtom)) :-
	get_dict(values, Dict, Type),
	json_to_atom(Type, TypeAtom).

dict_to_selector_by_type(coupler_from, Dict, coupler_from(SourceAtom)) :-
	get_dict(values, Dict, Source),
	json_to_atom(Source, SourceAtom).

dict_to_selector_by_type(coupler_to, Dict, coupler_to(DestAtom)) :-
	get_dict(values, Dict, Dest),
	json_to_atom(Dest, DestAtom).

dict_to_selector_by_type(coupler, Dict, Selector) :-
	get_dict(source, Dict, Source),
	get_dict(destination, Dict, Dest),
	get_dict_default(transposition, Dict, Trans, none),
	json_to_atom(Source, SourceAtom),
	json_to_atom(Dest, DestAtom),
	(Trans = none -> Selector = coupler(SourceAtom, DestAtom)
	;   json_to_atom(Trans, TransAtom),
		Selector = coupler(SourceAtom, DestAtom, TransAtom)).

dict_to_selector_by_type(union, Dict, union(Selectors)) :-
	get_dict(values, Dict, SubDicts),
	maplist(dict_to_selector, SubDicts, Selectors).

dict_to_selector_by_type(intersection, Dict, intersection(Selectors)) :-
	get_dict(values, Dict, SubDicts),
	maplist(dict_to_selector, SubDicts, Selectors).

dict_to_selector_by_type(difference, Dict, difference(Base, Sub)) :-
	get_dict(base, Dict, BaseDict),
	get_dict(subtract, Dict, SubDict),
	dict_to_selector(BaseDict, Base),
	dict_to_selector(SubDict, Sub).
