
:- multifile state:rule/2.
:- multifile state:max_level/2.
:- multifile state:antonym/2.
:- multifile state:rule_selector/3.
:- multifile state:rule_selector/4.
:- multifile state:rule_selector/5.

:- dynamic current_file_preset/1.

term_expansion(
    local_selector(Rule, Level, Div, Sel),
    state:rule_selector(Rule, Level, Div, for_preset(Preset, Sel))
) :-
    current_file_preset(Preset).

% for preset: Baroque Cathedral (Freiberg - 1710-1714)
:- retractall(current_file_preset(_)), assertz(current_file_preset('Baroque Cathedral (Freiberg - 1710-1714)')).

state:rule('crescendo swell', persistent).
state:max_level('crescendo swell', 7).
local_selector('crescendo swell', 1, swell, numbers([2])).
local_selector('crescendo swell', 2, swell, numbers([3])).
local_selector('crescendo swell', 3, swell, numbers([5])).
local_selector('crescendo swell', 4, swell, numbers([1])).
local_selector('crescendo swell', 5, swell, numbers([6])).
local_selector('crescendo swell', 6, swell, numbers([4, 8])).
local_selector('crescendo swell', 7, swell, numbers([10])).

state:rule('crescendo great', persistent).
state:max_level('crescendo great', 13).
local_selector('crescendo great', 1, great, numbers([2])).
local_selector('crescendo great', 2, great, numbers([6])).
local_selector('crescendo great', 3, great, numbers([1])).
local_selector('crescendo great', 4, great, numbers([8])).
local_selector('crescendo great', 5, great, numbers([3])).
local_selector('crescendo great', 6, great, numbers([10])).
local_selector('crescendo great', 7, great, numbers([9])).
local_selector('crescendo great', 8, great, numbers([13])).
local_selector('crescendo great', 9, great, numbers([5])).
local_selector('crescendo great', 10, great, numbers([4, 7, 11])).
local_selector('crescendo great', 11, great, numbers([12])).
local_selector('crescendo great', 12, great, numbers([14])).
local_selector('crescendo great', 13, swell, numbers([1, 2, 3])).
local_selector('crescendo great', 13, coupler, coupler(swell, great)).

state:rule('crescendo choir', persistent).
state:max_level('crescendo choir', 5).
local_selector('crescendo choir', 1, choir, numbers([1])).
local_selector('crescendo choir', 2, choir, numbers([3])).
local_selector('crescendo choir', 3, choir, numbers([5])).
local_selector('crescendo choir', 4, choir, numbers([9])).
local_selector('crescendo choir', 5, choir, numbers([8])).

state:rule('crescendo pedal', persistent).
state:max_level('crescendo pedal', 8).
local_selector('crescendo pedal', 1, pedal, numbers([2])).
local_selector('crescendo pedal', 2, pedal, numbers([5])).
local_selector('crescendo pedal', 3, pedal, numbers([3])).
local_selector('crescendo pedal', 4, pedal, numbers([6])).
local_selector('crescendo pedal', 5, pedal, numbers([4, 7])).
local_selector('crescendo pedal', 6, pedal, numbers([1, 8])).
local_selector('crescendo pedal', 7, pedal, numbers([9])).
local_selector('crescendo pedal', 8, pedal, numbers([10])).


% for preset: Neo-Classical Church (L'Alpe d'Huez - 1978)
:- retractall(current_file_preset(_)), assertz(current_file_preset('Neo-Classical Church (L\'Alpe d\'Huez - 1978)')).

state:rule('crescendo swell', persistent).
state:max_level('crescendo swell', 4).
local_selector('crescendo swell', 1, swell, names(['Flute Harmonique 8\''])).
local_selector('crescendo swell', 2, swell, names(['Flöte 4\''])).
local_selector('crescendo swell', 3, swell, names(['Larigot 1\'1/3'])).
local_selector('crescendo swell', 4, swell, names(['Hautbois 8\''])).

state:rule('crescendo great', persistent).
state:max_level('crescendo great', 7).
local_selector('crescendo great', 1, great, names(['Gedact 8\''])).
local_selector('crescendo great', 2, great, names(['Prinzipal 8\''])).
local_selector('crescendo great', 3, great, names(['Flute Harmonique 8\''])).
local_selector('crescendo great', 4, great, names(['Prestant 4\''])).
local_selector('crescendo great', 5, great, names(['Doublette 2\''])).
local_selector('crescendo great', 6, great, names(['Cornet V'])).
local_selector('crescendo great', 7, great, names(['Krumhorn 8\''])).

state:rule('crescendo pedal', persistent).
state:max_level('crescendo pedal', 5).
local_selector('crescendo pedal', 1, pedal, names(['Bourdon 16\''])).
local_selector('crescendo pedal', 2, pedal, names(['Doppel Flute 8\''])).
local_selector('crescendo pedal', 3, pedal, names(['Bourdon 8\''])).
local_selector('crescendo pedal', 4, pedal, names(['Gross Quintaden 16\''])).
local_selector('crescendo pedal', 5, pedal, names(['Bombarde 16\''])).
local_selector('crescendo pedal', 5, pedal, names(['Bombardeir 16\''])).


