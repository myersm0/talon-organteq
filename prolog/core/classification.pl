% Element classification by tonal family and footage

:- module(classification, [
	% Name parsing
	base_name/2,
	name_footage/2,
	% Element classification
	element_family/3,
	element_footage/3,
	% Alias lookup
	alias_of/2,
	% Coupler properties
	coupler_source/2,
	coupler_destination/2,
	coupler_transposition/2
]).

:- use_module(config(families), [family_of/2]).
:- use_module(config(aliases), [aliases/1]).
:- use_module(config(couplers), [coupler_mapping/6]).
:- use_module(state, [element/4, current_preset/1]).

% ============================================================================
% Name parsing
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

name_footage(Name, Footage) :-
	atom_string(Name, NameStr),
	(   sub_string(NameStr, _, _, _, "32'") -> Footage = 32
	;   sub_string(NameStr, _, _, _, "16'") -> Footage = 16
	;   sub_string(NameStr, _, _, _, "8'") -> Footage = 8
	;   sub_string(NameStr, _, _, _, "4'") -> Footage = 4
	;   sub_string(NameStr, _, _, _, "2'") -> Footage = 2
	;   sub_string(NameStr, _, _, _, "1'") -> Footage = 1
	;   Footage = unknown
	).

% ============================================================================
% Element classification
% ============================================================================

element_family(Division, Number, Family) :-
	element(Division, Number, Name, stop),
	base_name(Name, BaseName),
	family_of(BaseName, Family).

element_footage(Division, Number, Footage) :-
	element(Division, Number, Name, stop),
	name_footage(Name, Footage).

% ============================================================================
% Alias lookup
% ============================================================================

alias_of(Name, Alias) :-
	aliases(Group),
	member(Name, Group),
	member(Alias, Group).

% ============================================================================
% Coupler properties
% ============================================================================

coupler_source(Number, Source) :-
	current_preset(Preset),
	coupler_mapping(Preset, Number, Source, _, _, _).

coupler_destination(Number, Dest) :-
	current_preset(Preset),
	coupler_mapping(Preset, Number, _, Dest, _, _).

coupler_transposition(Number, Trans) :-
	current_preset(Preset),
	coupler_mapping(Preset, Number, _, _, Trans, _).
