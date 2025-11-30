% selectors.pl - Selector resolution and dict conversion

:- module(selectors, [
	resolve_selector/3,
	dict_to_selector/2
]).

:- use_module(state, [
	element/4, engaged/2, json_to_atom/2, get_dict/4
]).
:- use_module(classification, [
	element_family/3, element_footage/3,
	coupler_source/2, coupler_destination/2, coupler_transposition/2
]).

:- discontiguous resolve_selector/3.

% ============================================================================
% Selector resolution
% ============================================================================

resolve_selector(_, numbers(Numbers), Numbers) :- !.

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
	), Elements), !.

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
% Dict to selector conversion
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

dict_to_selector_by_type(numbers, Dict, numbers(Values)) :-
	get_dict(values, Dict, Values).

dict_to_selector_by_type(family, Dict, Selector) :-
	get_dict(values, Dict, FamilyRaw),
	json_to_atom(FamilyRaw, Family),
	get_dict(footage, Dict, Footage, any),
	get_dict(limit, Dict, Limit, none),
	get_dict(limit_method, Dict, MethodRaw, first),
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
	get_dict(transposition, Dict, Trans, none),
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
