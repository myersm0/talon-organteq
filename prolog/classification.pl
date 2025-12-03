% classification.pl - Stop classification by tonal family and footage

:- module(classification, [
	family_of/2,
	aliases/1,
	alias_of/2,
	mono_coupler_name/2,
	element_type/3,
	is_stop/2,
	is_coupler/2,
	is_mono_coupler/2,
	element_family/3,
	element_footage/3,
	name_footage/2,
	base_name/2,
	coupler_source/2,
	coupler_destination/2,
	coupler_transposition/2
]).

:- use_module(state, [element/4, current_preset/1, coupler_mapping/6, get_dict/4]).

% ============================================================================
% Element type predicates
% ============================================================================

element_type(Division, Number, Type) :-
	element(Division, Number, _, Type).

is_stop(Division, Number) :-
	element_type(Division, Number, stop).

is_coupler(Division, Number) :-
	element_type(Division, Number, coupler).

is_mono_coupler(Division, Number) :-
	element_type(Division, Number, mono_coupler).

% ============================================================================
% Stop classification
% ============================================================================

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

element_family(Division, Number, Family) :-
	element(Division, Number, Name, stop),
	base_name(Name, BaseName),
	family_of(BaseName, Family).

element_footage(Division, Number, Footage) :-
	element(Division, Number, Name, stop),
	name_footage(Name, Footage).

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

% ============================================================================
% Built-in family classifications
% ============================================================================

family_of('Barpfeife', reed).
family_of('Basson', reed).
family_of('Bombarde', reed).
family_of('Bourdon', flute).
family_of('Celinder Quinta', mutation).
family_of('Clairon', reed).
family_of('Clarinette', reed).
family_of('Contre Bombarde', reed).
family_of('Cornet', mixture).
family_of('Doppel Flute', flute).
family_of('Doublette', principal).
family_of('Fagott', reed).
family_of('Flöte', flute).
family_of('Flute', flute).
family_of('Flute Harmonique', flute).
family_of('Flute Traversiere', flute).
family_of('Gambe', string).
family_of('Gedact', flute).
family_of('Grande Tierce', mutation).
family_of('Gross Quintaden', flute).
family_of('Gross Untersatz', principal).
family_of('Hautbois', reed).
family_of('Kontra Bass', principal).
family_of('Krumhorn', reed).
family_of('Larigot', mutation).
family_of('Mixtura', mixture).
family_of('Montre', principal).
family_of('Musette', reed).
family_of('Nachthorn', flute).
family_of('Nasard', mutation).
family_of('Nassad Quinta', flute).
family_of('Octav', principal).
family_of('Piccolo', flute).
family_of('Plein Jeu', mixture).
family_of('Posaunen Bass', reed).
family_of('Prestant', principal).
family_of('Principal', principal).
family_of('Prinzipal', principal).
family_of('Quintaton', flute).
family_of('Quinte', mutation).
family_of('Regale', reed).
family_of('Salicional', string).
family_of('Schalmei', reed).
family_of('Septieme', mutation).
family_of('Sesquialtera', mixture).
family_of('Sifflöte', flute).
family_of('Super Octav', principal).
family_of('Tertia', mutation).
family_of('Tierce', mutation).
family_of('Trommet', reed).
family_of('Trompette', reed).
family_of('Unda Maris', string).
family_of('Viol Gamba', string).
family_of('Voix Celeste', string).
family_of('Voix Humaine', reed).
family_of('Vox Humana', reed).
family_of('Waldflöte', flute).
family_of('Zimbel', mixture).
family_of('Zinke', reed).

% ============================================================================
% Mono coupler names
% ============================================================================

mono_coupler_name('bass 1', 1).
mono_coupler_name('bass 2', 2).
mono_coupler_name('melody 1', 3).
mono_coupler_name('melody 2', 4).

% ============================================================================
% Stop name aliases
% ============================================================================
% Equivalent stop names across different languages/traditions.
% Used by the names() selector to find substitutes when exact match fails.

aliases(['Cromorne', 'Krummhorn', 'Cremona']).
aliases(['Trompette', 'Trumpet', 'Trommet']).
aliases(['Hautbois', 'Oboe', 'Hoboe']).
aliases(['Voix Humaine', 'Vox Humana']).
aliases(['Bourdon', 'Bordun']).
aliases(['Montre', 'Prinzipal', 'Principal', 'Prestant', 'Diapason']).
aliases(['Gambe', 'Gamba', 'Viol Gamba', 'Viola da Gamba']).
aliases(['Flöte', 'Flute', 'Flauto']).
aliases(['Gedact', 'Gedackt', 'Gedeckt']).
aliases(['Sesquialtera', 'Sesquialter']).
aliases(['Nazard', 'Nasard', 'Nasat']).
aliases(['Cornet', 'Kornett']).
aliases(['Zimbel', 'Cymbale', 'Cymbal']).
aliases(['Mixtur', 'Mixtura', 'Mixture', 'Fourniture']).
aliases(['Fagott', 'Basson', 'Bassoon']).
aliases(['Posaunen', 'Posaune', 'Trombone']).
aliases(['Clairon', 'Clarion']).
aliases(['Octav', 'Octave', 'Oktave']).
aliases(['Quinte', 'Quint', 'Quinta']).

alias_of(Name, Alias) :-
	aliases(Group),
	member(Name, Group),
	member(Alias, Group).

