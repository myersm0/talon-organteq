% couplers.pl - Coupler mappings for Organteq presets
%
% coupler_mapping(Preset, Index, Source, Destination, Transposition, Direction)
% Transposition: sub (16'), unison (8'), super (4')
% Direction: normal, forward

:- module(couplers, []).

:- use_module(state, [coupler_mapping/6]).

:- discontiguous state:coupler_mapping/6.

% ============================================================================
% Baroque Cathedral I
% ============================================================================

state:coupler_mapping('Baroque Cathedral I', 1, choir, pedal, unison, normal).
state:coupler_mapping('Baroque Cathedral I', 2, great, pedal, unison, normal).
state:coupler_mapping('Baroque Cathedral I', 3, swell, pedal, unison, normal).
state:coupler_mapping('Baroque Cathedral I', 4, choir, great, unison, normal).
state:coupler_mapping('Baroque Cathedral I', 5, swell, great, unison, normal).
state:coupler_mapping('Baroque Cathedral I', 6, swell, swell, super, normal).

state:coupler_mapping('Baroque Cathedral I - Console', 1, choir, pedal, unison, normal).
state:coupler_mapping('Baroque Cathedral I - Console', 2, great, pedal, unison, normal).
state:coupler_mapping('Baroque Cathedral I - Console', 3, swell, pedal, unison, normal).
state:coupler_mapping('Baroque Cathedral I - Console', 4, choir, great, unison, normal).
state:coupler_mapping('Baroque Cathedral I - Console', 5, swell, great, unison, normal).
state:coupler_mapping('Baroque Cathedral I - Console', 6, swell, swell, super, normal).

state:coupler_mapping('Baroque Cathedral I - Solo', 1, choir, pedal, unison, normal).
state:coupler_mapping('Baroque Cathedral I - Solo', 2, great, pedal, unison, normal).
state:coupler_mapping('Baroque Cathedral I - Solo', 3, swell, pedal, unison, normal).
state:coupler_mapping('Baroque Cathedral I - Solo', 4, choir, great, unison, normal).
state:coupler_mapping('Baroque Cathedral I - Solo', 5, swell, great, unison, normal).
state:coupler_mapping('Baroque Cathedral I - Solo', 6, swell, choir, unison, normal).

% ============================================================================
% Baroque Cathedral II
% ============================================================================

state:coupler_mapping('Baroque Cathedral II', 1, choir, pedal, unison, normal).
state:coupler_mapping('Baroque Cathedral II', 2, great, pedal, unison, normal).
state:coupler_mapping('Baroque Cathedral II', 3, swell, pedal, unison, normal).
state:coupler_mapping('Baroque Cathedral II', 4, choir, great, unison, normal).
state:coupler_mapping('Baroque Cathedral II', 5, swell, great, unison, normal).
state:coupler_mapping('Baroque Cathedral II', 6, swell, choir, unison, normal).

% ============================================================================
% Romantic Cathedral
% ============================================================================

state:coupler_mapping('Romantic Cathedral', 1, choir, pedal, unison, normal).
state:coupler_mapping('Romantic Cathedral', 2, great, pedal, unison, normal).
state:coupler_mapping('Romantic Cathedral', 3, swell, pedal, unison, normal).
state:coupler_mapping('Romantic Cathedral', 4, choir, great, unison, normal).
state:coupler_mapping('Romantic Cathedral', 5, swell, great, unison, normal).
state:coupler_mapping('Romantic Cathedral', 6, swell, choir, unison, normal).

% ============================================================================
% Alsacian Organs
% ============================================================================

state:coupler_mapping('Alsacian Organ I (Strasbourg, St-Thomas - 1741)', 1, choir, pedal, unison, normal).
state:coupler_mapping('Alsacian Organ I (Strasbourg, St-Thomas - 1741)', 2, swell, pedal, unison, normal).
state:coupler_mapping('Alsacian Organ I (Strasbourg, St-Thomas - 1741)', 3, choir, great, unison, normal).
state:coupler_mapping('Alsacian Organ I (Strasbourg, St-Thomas - 1741)', 4, swell, great, unison, normal).
state:coupler_mapping('Alsacian Organ I (Strasbourg, St-Thomas - 1741)', 5, swell, choir, unison, normal).
state:coupler_mapping('Alsacian Organ I (Strasbourg, St-Thomas - 1741)', 6, swell, swell, sub, normal).

state:coupler_mapping('Alsacian Organ II (Marmoutier - 1746)', 1, great, pedal, unison, normal).
state:coupler_mapping('Alsacian Organ II (Marmoutier - 1746)', 2, choir, great, unison, normal).
state:coupler_mapping('Alsacian Organ II (Marmoutier - 1746)', 3, swell, great, unison, normal).
state:coupler_mapping('Alsacian Organ II (Marmoutier - 1746)', 4, swell, choir, unison, normal).
state:coupler_mapping('Alsacian Organ II (Marmoutier - 1746)', 5, great, great, sub, normal).
state:coupler_mapping('Alsacian Organ II (Marmoutier - 1746)', 6, swell, swell, sub, normal).

% ============================================================================
% Neo-Baroque
% ============================================================================

state:coupler_mapping('Neo-Baroque', 1, choir, pedal, unison, normal).
state:coupler_mapping('Neo-Baroque', 2, great, pedal, unison, normal).
state:coupler_mapping('Neo-Baroque', 3, swell, pedal, unison, normal).
state:coupler_mapping('Neo-Baroque', 4, choir, great, unison, normal).
state:coupler_mapping('Neo-Baroque', 5, swell, great, unison, normal).
state:coupler_mapping('Neo-Baroque', 6, swell, choir, unison, normal).

% ============================================================================
% Default fallback (common configuration)
% ============================================================================

state:coupler_mapping(_, 1, choir, pedal, unison, normal).
state:coupler_mapping(_, 2, great, pedal, unison, normal).
state:coupler_mapping(_, 3, swell, pedal, unison, normal).
state:coupler_mapping(_, 4, choir, great, unison, normal).
state:coupler_mapping(_, 5, swell, great, unison, normal).
state:coupler_mapping(_, 6, swell, choir, unison, normal).
