% couplers.pl - Coupler mappings for Organteq presets
%
% coupler_mapping(Preset, Index, Source, Destination, Transposition, Direction)
% Transposition: sub (16'), unison (8'), super (4')
% Direction: normal, forward

:- module(couplers, []).

:- use_module(state, [coupler_mapping/6]).

:- discontiguous state:coupler_mapping/6.

% coupler_mapping(Preset, Index, Source, Target, Transposition, Direction).
% Transposition: sub (16'), unison (8'), super (4')
% Direction: normal, forward
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
state:coupler_mapping('Baroque Cathedral I - Crescendo', 1, choir, pedal, unison, normal).
state:coupler_mapping('Baroque Cathedral I - Crescendo', 2, great, pedal, unison, normal).
state:coupler_mapping('Baroque Cathedral I - Crescendo', 3, swell, pedal, unison, normal).
state:coupler_mapping('Baroque Cathedral I - Crescendo', 4, choir, great, unison, normal).
state:coupler_mapping('Baroque Cathedral I - Crescendo', 5, swell, great, unison, normal).
state:coupler_mapping('Baroque Cathedral I - Crescendo', 6, swell, choir, unison, normal).
state:coupler_mapping('Baroque Cathedral II', 1, choir, pedal, unison, normal).
state:coupler_mapping('Baroque Cathedral II', 2, great, pedal, unison, normal).
state:coupler_mapping('Baroque Cathedral II', 3, swell, pedal, unison, normal).
state:coupler_mapping('Baroque Cathedral II', 4, choir, great, unison, normal).
state:coupler_mapping('Baroque Cathedral II', 5, swell, great, unison, normal).
state:coupler_mapping('Baroque Cathedral II', 6, swell, choir, unison, normal).
state:coupler_mapping('Baroque Cathedral II - Console', 1, choir, pedal, unison, normal).
state:coupler_mapping('Baroque Cathedral II - Console', 2, great, pedal, unison, normal).
state:coupler_mapping('Baroque Cathedral II - Console', 3, swell, pedal, unison, normal).
state:coupler_mapping('Baroque Cathedral II - Console', 4, choir, great, unison, normal).
state:coupler_mapping('Baroque Cathedral II - Console', 5, swell, great, unison, normal).
state:coupler_mapping('Baroque Cathedral II - Console', 6, swell, choir, unison, normal).
state:coupler_mapping('Baroque Cathedral II - Out of Tune', 1, choir, pedal, unison, normal).
state:coupler_mapping('Baroque Cathedral II - Out of Tune', 2, great, pedal, unison, normal).
state:coupler_mapping('Baroque Cathedral II - Out of Tune', 3, swell, pedal, unison, normal).
state:coupler_mapping('Baroque Cathedral II - Out of Tune', 4, choir, great, unison, normal).
state:coupler_mapping('Baroque Cathedral II - Out of Tune', 5, swell, great, unison, normal).
state:coupler_mapping('Baroque Cathedral II - Out of Tune', 6, swell, choir, unison, normal).
state:coupler_mapping('Baroque Cathedral II - Worn Out', 1, choir, pedal, unison, normal).
state:coupler_mapping('Baroque Cathedral II - Worn Out', 2, great, pedal, unison, normal).
state:coupler_mapping('Baroque Cathedral II - Worn Out', 3, swell, pedal, unison, normal).
state:coupler_mapping('Baroque Cathedral II - Worn Out', 4, choir, great, unison, normal).
state:coupler_mapping('Baroque Cathedral II - Worn Out', 5, swell, great, unison, normal).
state:coupler_mapping('Baroque Cathedral II - Worn Out', 6, swell, choir, unison, normal).
state:coupler_mapping('Baroque Church', 1, choir, pedal, unison, normal).
state:coupler_mapping('Baroque Church', 2, great, pedal, unison, normal).
state:coupler_mapping('Baroque Church', 3, swell, pedal, unison, normal).
state:coupler_mapping('Baroque Church', 4, choir, great, unison, normal).
state:coupler_mapping('Baroque Church', 5, swell, great, unison, normal).
state:coupler_mapping('Baroque Church', 6, swell, choir, unison, normal).
state:coupler_mapping('Baroque Church - Solo', 1, choir, pedal, unison, normal).
state:coupler_mapping('Baroque Church - Solo', 2, great, pedal, unison, normal).
state:coupler_mapping('Baroque Church - Solo', 3, swell, pedal, unison, normal).
state:coupler_mapping('Baroque Church - Solo', 4, choir, great, unison, normal).
state:coupler_mapping('Baroque Church - Solo', 5, swell, great, unison, normal).
state:coupler_mapping('Baroque Church - Solo', 6, swell, choir, unison, normal).
state:coupler_mapping('Baroque Church - Crescendo', 1, choir, pedal, unison, normal).
state:coupler_mapping('Baroque Church - Crescendo', 2, great, pedal, unison, normal).
state:coupler_mapping('Baroque Church - Crescendo', 3, swell, pedal, unison, normal).
state:coupler_mapping('Baroque Church - Crescendo', 4, choir, great, unison, normal).
state:coupler_mapping('Baroque Church - Crescendo', 5, swell, great, unison, normal).
state:coupler_mapping('Baroque Church - Crescendo', 6, swell, choir, unison, normal).
state:coupler_mapping('Baroque Choir - Solo', 1, choir, pedal, unison, normal).
state:coupler_mapping('Baroque Choir - Solo', 2, great, pedal, unison, normal).
state:coupler_mapping('Baroque Choir - Solo', 3, choir, great, unison, normal).
state:coupler_mapping('Baroque Choir - Solo', 4, pedal, great, unison, normal).
state:coupler_mapping('Baroque Choir - Solo', 5, none, none, unison, normal).
state:coupler_mapping('Baroque Choir - Solo', 6, none, none, unison, normal).
state:coupler_mapping('Baroque Choir - Crescendo', 1, choir, pedal, unison, normal).
state:coupler_mapping('Baroque Choir - Crescendo', 2, great, pedal, unison, normal).
state:coupler_mapping('Baroque Choir - Crescendo', 3, choir, great, unison, normal).
state:coupler_mapping('Baroque Choir - Crescendo', 4, pedal, great, unison, normal).
state:coupler_mapping('Baroque Choir - Crescendo', 5, none, none, unison, normal).
state:coupler_mapping('Baroque Choir - Crescendo', 6, none, none, unison, normal).
state:coupler_mapping('Baroque Chapel', 1, great, pedal, unison, normal).
state:coupler_mapping('Baroque Chapel', 2, swell, pedal, unison, normal).
state:coupler_mapping('Baroque Chapel', 3, swell, great, unison, normal).
state:coupler_mapping('Baroque Chapel', 4, none, none, unison, normal).
state:coupler_mapping('Baroque Chapel', 5, none, none, unison, normal).
state:coupler_mapping('Baroque Chapel', 6, none, none, unison, normal).
state:coupler_mapping('Romantic Cathedral', 1, choir, pedal, unison, normal).
state:coupler_mapping('Romantic Cathedral', 2, great, pedal, unison, normal).
state:coupler_mapping('Romantic Cathedral', 3, swell, pedal, unison, normal).
state:coupler_mapping('Romantic Cathedral', 4, choir, great, unison, normal).
state:coupler_mapping('Romantic Cathedral', 5, swell, great, unison, normal).
state:coupler_mapping('Romantic Cathedral', 6, swell, choir, unison, normal).
state:coupler_mapping('Romantic Cathedral - Solo', 1, choir, pedal, unison, normal).
state:coupler_mapping('Romantic Cathedral - Solo', 2, great, pedal, unison, normal).
state:coupler_mapping('Romantic Cathedral - Solo', 3, swell, pedal, unison, normal).
state:coupler_mapping('Romantic Cathedral - Solo', 4, choir, great, unison, normal).
state:coupler_mapping('Romantic Cathedral - Solo', 5, swell, great, unison, normal).
state:coupler_mapping('Romantic Cathedral - Solo', 6, swell, choir, unison, normal).
state:coupler_mapping('Romantic Cathedral - Crescendo', 1, choir, pedal, unison, normal).
state:coupler_mapping('Romantic Cathedral - Crescendo', 2, great, pedal, unison, normal).
state:coupler_mapping('Romantic Cathedral - Crescendo', 3, swell, pedal, unison, normal).
state:coupler_mapping('Romantic Cathedral - Crescendo', 4, choir, great, unison, normal).
state:coupler_mapping('Romantic Cathedral - Crescendo', 5, swell, great, unison, normal).
state:coupler_mapping('Romantic Cathedral - Crescendo', 6, swell, choir, unison, normal).
state:coupler_mapping('Romantic Cathedral - Expressive mutations', 1, choir, pedal, unison, normal).
state:coupler_mapping('Romantic Cathedral - Expressive mutations', 2, great, pedal, unison, normal).
state:coupler_mapping('Romantic Cathedral - Expressive mutations', 3, swell, pedal, unison, normal).
state:coupler_mapping('Romantic Cathedral - Expressive mutations', 4, choir, great, unison, normal).
state:coupler_mapping('Romantic Cathedral - Expressive mutations', 5, swell, great, unison, normal).
state:coupler_mapping('Romantic Cathedral - Expressive mutations', 6, swell, choir, unison, normal).
state:coupler_mapping('Romantic Church', 1, choir, pedal, unison, normal).
state:coupler_mapping('Romantic Church', 2, great, pedal, unison, normal).
state:coupler_mapping('Romantic Church', 3, swell, pedal, unison, normal).
state:coupler_mapping('Romantic Church', 4, choir, great, unison, normal).
state:coupler_mapping('Romantic Church', 5, swell, great, unison, normal).
state:coupler_mapping('Romantic Church', 6, swell, choir, unison, normal).
state:coupler_mapping('Romantic Church - Solo', 1, choir, pedal, unison, normal).
state:coupler_mapping('Romantic Church - Solo', 2, great, pedal, unison, normal).
state:coupler_mapping('Romantic Church - Solo', 3, swell, pedal, unison, normal).
state:coupler_mapping('Romantic Church - Solo', 4, choir, great, unison, normal).
state:coupler_mapping('Romantic Church - Solo', 5, swell, great, unison, normal).
state:coupler_mapping('Romantic Church - Solo', 6, swell, choir, unison, normal).
state:coupler_mapping('Romantic Church - Crescendo', 1, choir, pedal, unison, normal).
state:coupler_mapping('Romantic Church - Crescendo', 2, great, pedal, unison, normal).
state:coupler_mapping('Romantic Church - Crescendo', 3, swell, pedal, unison, normal).
state:coupler_mapping('Romantic Church - Crescendo', 4, choir, great, unison, normal).
state:coupler_mapping('Romantic Church - Crescendo', 5, swell, great, unison, normal).
state:coupler_mapping('Romantic Church - Crescendo', 6, swell, choir, unison, normal).
state:coupler_mapping('Romantic Abbey', 1, choir, pedal, unison, normal).
state:coupler_mapping('Romantic Abbey', 2, great, pedal, unison, normal).
state:coupler_mapping('Romantic Abbey', 3, swell, pedal, unison, normal).
state:coupler_mapping('Romantic Abbey', 4, choir, great, unison, normal).
state:coupler_mapping('Romantic Abbey', 5, swell, great, unison, normal).
state:coupler_mapping('Romantic Abbey', 6, swell, choir, unison, normal).
state:coupler_mapping('Romantic Abbey - Console', 1, choir, pedal, unison, normal).
state:coupler_mapping('Romantic Abbey - Console', 2, great, pedal, unison, normal).
state:coupler_mapping('Romantic Abbey - Console', 3, swell, pedal, unison, normal).
state:coupler_mapping('Romantic Abbey - Console', 4, choir, great, unison, normal).
state:coupler_mapping('Romantic Abbey - Console', 5, swell, great, unison, normal).
state:coupler_mapping('Romantic Abbey - Console', 6, swell, choir, unison, normal).
state:coupler_mapping('Romantic Abbey - Out of Tune', 1, choir, pedal, unison, normal).
state:coupler_mapping('Romantic Abbey - Out of Tune', 2, great, pedal, unison, normal).
state:coupler_mapping('Romantic Abbey - Out of Tune', 3, swell, pedal, unison, normal).
state:coupler_mapping('Romantic Abbey - Out of Tune', 4, choir, great, unison, normal).
state:coupler_mapping('Romantic Abbey - Out of Tune', 5, swell, great, unison, normal).
state:coupler_mapping('Romantic Abbey - Out of Tune', 6, swell, choir, unison, normal).
state:coupler_mapping('Romantic Abbey - Worn out', 1, choir, pedal, unison, normal).
state:coupler_mapping('Romantic Abbey - Worn out', 2, great, pedal, unison, normal).
state:coupler_mapping('Romantic Abbey - Worn out', 3, swell, pedal, unison, normal).
state:coupler_mapping('Romantic Abbey - Worn out', 4, choir, great, unison, normal).
state:coupler_mapping('Romantic Abbey - Worn out', 5, swell, great, unison, normal).
state:coupler_mapping('Romantic Abbey - Worn out', 6, swell, choir, unison, normal).
state:coupler_mapping('Romantic Choir - Solo', 1, swell, pedal, unison, normal).
state:coupler_mapping('Romantic Choir - Solo', 2, swell, great, unison, normal).
state:coupler_mapping('Romantic Choir - Solo', 3, great, pedal, unison, normal).
state:coupler_mapping('Romantic Choir - Solo', 4, swell, swell, sub, normal).
state:coupler_mapping('Romantic Choir - Solo', 5, pedal, great, unison, normal).
state:coupler_mapping('Romantic Choir - Solo', 6, none, none, unison, normal).
state:coupler_mapping('Romantic Choir - Crescendo', 1, swell, pedal, unison, normal).
state:coupler_mapping('Romantic Choir - Crescendo', 2, swell, great, unison, normal).
state:coupler_mapping('Romantic Choir - Crescendo', 3, great, pedal, unison, normal).
state:coupler_mapping('Romantic Choir - Crescendo', 4, swell, swell, sub, normal).
state:coupler_mapping('Romantic Choir - Crescendo', 5, pedal, great, unison, normal).
state:coupler_mapping('Romantic Choir - Crescendo', 6, none, none, unison, normal).
state:coupler_mapping('Romantic Chapel', 1, swell, pedal, unison, normal).
state:coupler_mapping('Romantic Chapel', 2, great, pedal, unison, normal).
state:coupler_mapping('Romantic Chapel', 3, swell, great, unison, normal).
state:coupler_mapping('Romantic Chapel', 4, swell, swell, sub, normal).
state:coupler_mapping('Romantic Chapel', 5, none, none, unison, normal).
state:coupler_mapping('Romantic Chapel', 6, none, none, unison, normal).
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
state:coupler_mapping('Baroque Cathedral (Freiberg - 1710-1714)', 1, choir, great, unison, normal).
state:coupler_mapping('Baroque Cathedral (Freiberg - 1710-1714)', 2, swell, great, unison, normal).
state:coupler_mapping('Baroque Cathedral (Freiberg - 1710-1714)', 3, great, pedal, unison, normal).
state:coupler_mapping('Baroque Cathedral (Freiberg - 1710-1714)', 4, great, great, super, normal).
state:coupler_mapping('Baroque Cathedral (Freiberg - 1710-1714)', 5, great, great, sub, normal).
state:coupler_mapping('Baroque Cathedral (Freiberg - 1710-1714)', 6, swell, swell, sub, normal).
state:coupler_mapping('Baroque Two-Manual I (Steinkirchen - 1687)', 1, choir, pedal, unison, normal).
state:coupler_mapping('Baroque Two-Manual I (Steinkirchen - 1687)', 2, great, pedal, unison, normal).
state:coupler_mapping('Baroque Two-Manual I (Steinkirchen - 1687)', 3, choir, great, unison, normal).
state:coupler_mapping('Baroque Two-Manual I (Steinkirchen - 1687)', 4, pedal, great, unison, normal).
state:coupler_mapping('Baroque Two-Manual I (Steinkirchen - 1687)', 5, great, choir, unison, normal).
state:coupler_mapping('Baroque Two-Manual I (Steinkirchen - 1687)', 6, great, great, sub, normal).
state:coupler_mapping('Baroque Two-Manual II (Arnstadt - 1703)', 1, great, choir, unison, normal).
state:coupler_mapping('Baroque Two-Manual II (Arnstadt - 1703)', 2, great, pedal, unison, normal).
state:coupler_mapping('Baroque Two-Manual II (Arnstadt - 1703)', 3, great, great, super, normal).
state:coupler_mapping('Baroque Two-Manual II (Arnstadt - 1703)', 4, choir, great, unison, normal).
state:coupler_mapping('Baroque Two-Manual II (Arnstadt - 1703)', 5, choir, pedal, unison, normal).
state:coupler_mapping('Baroque Two-Manual II (Arnstadt - 1703)', 6, choir, choir, sub, normal).
state:coupler_mapping('Baroque Two-Manual III (Rötha - 1721)', 1, choir, pedal, unison, normal).
state:coupler_mapping('Baroque Two-Manual III (Rötha - 1721)', 2,  choir, great, unison, normal).
state:coupler_mapping('Baroque Two-Manual III (Rötha - 1721)', 3, great, pedal, unison, normal).
state:coupler_mapping('Baroque Two-Manual III (Rötha - 1721)', 4, pedal, great, unison, normal).
state:coupler_mapping('Baroque Two-Manual III (Rötha - 1721)', 5, great, choir, unison, normal).
state:coupler_mapping('Baroque Two-Manual III (Rötha - 1721)', 6, great, great, sub, normal).
state:coupler_mapping('Classical French Organ I (Souvigny - 1783)', 1, choir, pedal, unison, normal).
state:coupler_mapping('Classical French Organ I (Souvigny - 1783)', 2, great, pedal, unison, normal).
state:coupler_mapping('Classical French Organ I (Souvigny - 1783)', 3, swell,  pedal, unison, normal).
state:coupler_mapping('Classical French Organ I (Souvigny - 1783)', 4, choir, great, unison, normal).
state:coupler_mapping('Classical French Organ I (Souvigny - 1783)', 5, swell, great, unison, normal).
state:coupler_mapping('Classical French Organ I (Souvigny - 1783)', 6, swell, choir, unison, normal).
state:coupler_mapping('Classical French Organ II (Poitiers - 1791)', 1, great, choir, unison, normal).
state:coupler_mapping('Classical French Organ II (Poitiers - 1791)', 2, pedal, choir, unison, normal).
state:coupler_mapping('Classical French Organ II (Poitiers - 1791)', 3, great, pedal, unison, normal).
state:coupler_mapping('Classical French Organ II (Poitiers - 1791)', 4, choir, pedal, unison, normal).
state:coupler_mapping('Classical French Organ II (Poitiers - 1791)', 5, choir, great, unison, normal).
state:coupler_mapping('Classical French Organ II (Poitiers - 1791)', 6, none, none, unison, normal).
state:coupler_mapping('Royal Chapel (Versailles - 1711)', 1, choir, pedal, unison, forward).
state:coupler_mapping('Royal Chapel (Versailles - 1711)', 2, great, pedal, unison, forward).
state:coupler_mapping('Royal Chapel (Versailles - 1711)', 3, swell, pedal, unison, forward).
state:coupler_mapping('Royal Chapel (Versailles - 1711)', 4, choir, great, unison, forward).
state:coupler_mapping('Royal Chapel (Versailles - 1711)', 5, swell, great, unison, forward).
state:coupler_mapping('Royal Chapel (Versailles - 1711)', 6, great, great, super, forward).
state:coupler_mapping('French Puget I - Taur (Toulouse - 1880)', 1, choir, pedal, unison, normal).
state:coupler_mapping('French Puget I - Taur (Toulouse - 1880)', 2, great, pedal, unison, normal).
state:coupler_mapping('French Puget I - Taur (Toulouse - 1880)', 3, choir, great, unison, normal).
state:coupler_mapping('French Puget I - Taur (Toulouse - 1880)', 4, swell, great, unison, normal).
state:coupler_mapping('French Puget I - Taur (Toulouse - 1880)', 5, swell, choir, unison, normal).
state:coupler_mapping('French Puget I - Taur (Toulouse - 1880)', 6, swell, swell, sub, normal).
state:coupler_mapping('French Puget II - St Exupere (Toulouse - 1842-1903)', 1, great, choir, unison, normal).
state:coupler_mapping('French Puget II - St Exupere (Toulouse - 1842-1903)', 2, swell, pedal, unison, normal).
state:coupler_mapping('French Puget II - St Exupere (Toulouse - 1842-1903)', 3, choir, great, unison, normal).
state:coupler_mapping('French Puget II - St Exupere (Toulouse - 1842-1903)', 4, swell, great, unison, normal).
state:coupler_mapping('French Puget II - St Exupere (Toulouse - 1842-1903)', 5, swell, choir, unison, normal).
state:coupler_mapping('French Puget II - St Exupere (Toulouse - 1842-1903)', 6, swell, swell, sub, normal).
state:coupler_mapping('French Baroque Cathedral (Auch - 1694)', 1, great, choir, unison, normal).
state:coupler_mapping('French Baroque Cathedral (Auch - 1694)', 2, pedal, choir, unison, normal).
state:coupler_mapping('French Baroque Cathedral (Auch - 1694)', 3, great, pedal, unison, normal).
state:coupler_mapping('French Baroque Cathedral (Auch - 1694)', 4, choir, pedal, unison, normal).
state:coupler_mapping('French Baroque Cathedral (Auch - 1694)', 5, choir, great, unison, normal).
state:coupler_mapping('French Baroque Cathedral (Auch - 1694)', 6, none, none, unison, normal).
state:coupler_mapping('French Hybrid - (Montpellier - 1754-1934)', 1, great, choir, unison, normal).
state:coupler_mapping('French Hybrid - (Montpellier - 1754-1934)', 2, pedal, choir, unison, normal).
state:coupler_mapping('French Hybrid - (Montpellier - 1754-1934)', 3, great, pedal, unison, normal).
state:coupler_mapping('French Hybrid - (Montpellier - 1754-1934)', 4, choir, pedal, unison, normal).
state:coupler_mapping('French Hybrid - (Montpellier - 1754-1934)', 5, choir, great, unison, normal).
state:coupler_mapping('French Hybrid - (Montpellier - 1754-1934)', 6, choir, choir, sub, normal).
state:coupler_mapping('French Romantic Church (Menilmontant - 1872)', 1, pedal, great, unison, normal).
state:coupler_mapping('French Romantic Church (Menilmontant - 1872)', 2, pedal, swell, unison, normal).
state:coupler_mapping('French Romantic Church (Menilmontant - 1872)', 3, swell, pedal, unison, normal).
state:coupler_mapping('French Romantic Church (Menilmontant - 1872)', 4, great, pedal, unison, normal).
state:coupler_mapping('French Romantic Church (Menilmontant - 1872)', 5, swell, great, unison, normal).
state:coupler_mapping('French Romantic Church (Menilmontant - 1872)', 6, great, great, super, normal).
state:coupler_mapping('Baroque Schnitger I (St Johannis - Hamburg - 1680)', 1, choir, great, unison, normal).
state:coupler_mapping('Baroque Schnitger I (St Johannis - Hamburg - 1680)', 2, pedal, great, unison, normal).
state:coupler_mapping('Baroque Schnitger I (St Johannis - Hamburg - 1680)', 3, choir, pedal, unison, normal).
state:coupler_mapping('Baroque Schnitger I (St Johannis - Hamburg - 1680)', 4, great, pedal, unison, normal).
state:coupler_mapping('Baroque Schnitger I (St Johannis - Hamburg - 1680)', 5, none, none, unison, normal).
state:coupler_mapping('Baroque Schnitger I (St Johannis - Hamburg - 1680)', 6, none, none, unison, normal).
state:coupler_mapping('Baroque Schnitger II (St Gertrud - Hamburg - 1699)', 1, choir, great, unison, normal).
state:coupler_mapping('Baroque Schnitger II (St Gertrud - Hamburg - 1699)', 2, pedal, great, unison, normal).
state:coupler_mapping('Baroque Schnitger II (St Gertrud - Hamburg - 1699)', 3, choir, pedal, unison, normal).
state:coupler_mapping('Baroque Schnitger II (St Gertrud - Hamburg - 1699)', 4, great, pedal, unison, normal).
state:coupler_mapping('Baroque Schnitger II (St Gertrud - Hamburg - 1699)', 5, none, none, unison, normal).
state:coupler_mapping('Baroque Schnitger II (St Gertrud - Hamburg - 1699)', 6, none, none, unison, normal).
state:coupler_mapping('German Classical Abbey (Neresheim - 1798)', 1, choir, pedal, unison, normal).
state:coupler_mapping('German Classical Abbey (Neresheim - 1798)', 2, great, pedal, unison, normal).
state:coupler_mapping('German Classical Abbey (Neresheim - 1798)', 3, swell, pedal, unison, normal).
state:coupler_mapping('German Classical Abbey (Neresheim - 1798)', 4, choir, great, unison, normal).
state:coupler_mapping('German Classical Abbey (Neresheim - 1798)', 5, swell, great, unison, normal).
state:coupler_mapping('German Classical Abbey (Neresheim - 1798)', 6, swell, swell, super, normal).
state:coupler_mapping('Baroque Peculiar (Dorstadt)', 1, great, choir, unison, normal).
state:coupler_mapping('Baroque Peculiar (Dorstadt)', 2, swell, pedal, unison, normal).
state:coupler_mapping('Baroque Peculiar (Dorstadt)', 3, choir, great, unison, normal).
state:coupler_mapping('Baroque Peculiar (Dorstadt)', 4, swell, great, unison, normal).
state:coupler_mapping('Baroque Peculiar (Dorstadt)', 5, swell, choir, unison, normal).
state:coupler_mapping('Baroque Peculiar (Dorstadt)', 6, swell, swell, sub, normal).
state:coupler_mapping('Neo-Baroque', 1, choir, pedal, unison, normal).
state:coupler_mapping('Neo-Baroque', 2, great, pedal, unison, normal).
state:coupler_mapping('Neo-Baroque', 3, swell, pedal, unison, normal).
state:coupler_mapping('Neo-Baroque', 4, choir, great, unison, normal).
state:coupler_mapping('Neo-Baroque', 5, swell, great, unison, normal).
state:coupler_mapping('Neo-Baroque', 6, swell, choir, unison, normal).
state:coupler_mapping('Transept (Lyon - 1974)', 1, choir, pedal, unison, normal).
state:coupler_mapping('Transept (Lyon - 1974)', 2, great, pedal, unison, normal).
state:coupler_mapping('Transept (Lyon - 1974)', 3, swell, pedal, unison, normal).
state:coupler_mapping('Transept (Lyon - 1974)', 4, choir, great, unison, normal).
state:coupler_mapping('Transept (Lyon - 1974)', 5, swell, great, unison, normal).
state:coupler_mapping('Transept (Lyon - 1974)', 6, swell, choir, unison, normal).
state:coupler_mapping('Neo-Baroque Choir (Castres - 1998)', 1, great, choir, unison, normal).
state:coupler_mapping('Neo-Baroque Choir (Castres - 1998)', 2, pedal, great, unison, normal).
state:coupler_mapping('Neo-Baroque Choir (Castres - 1998)', 3, great, pedal, unison, normal).
state:coupler_mapping('Neo-Baroque Choir (Castres - 1998)', 4, choir, pedal, unison, normal).
state:coupler_mapping('Neo-Baroque Choir (Castres - 1998)', 5, choir, great, unison, normal).
state:coupler_mapping('Neo-Baroque Choir (Castres - 1998)', 6, choir, choir, sub, normal).
state:coupler_mapping('Two-manual Church (Waltrop - 1984)', 1, choir, pedal, unison, normal).
state:coupler_mapping('Two-manual Church (Waltrop - 1984)', 2, choir, great, unison, normal).
state:coupler_mapping('Two-manual Church (Waltrop - 1984)', 3,  great, pedal, unison, normal).
state:coupler_mapping('Two-manual Church (Waltrop - 1984)', 4, pedal, great, unison, normal).
state:coupler_mapping('Two-manual Church (Waltrop - 1984)', 5, great, choir, unison, normal).
state:coupler_mapping('Two-manual Church (Waltrop - 1984)', 6, great, great, sub, normal).
state:coupler_mapping('Neo-Classical Cathedral (St-Dié - 2006)', 1, choir, pedal, unison, forward).
state:coupler_mapping('Neo-Classical Cathedral (St-Dié - 2006)', 2, swell, swell, super, normal).
state:coupler_mapping('Neo-Classical Cathedral (St-Dié - 2006)', 3, swell, pedal, unison, forward).
state:coupler_mapping('Neo-Classical Cathedral (St-Dié - 2006)', 4, choir, great, unison, forward).
state:coupler_mapping('Neo-Classical Cathedral (St-Dié - 2006)', 5, great, pedal, unison, forward).
state:coupler_mapping('Neo-Classical Cathedral (St-Dié - 2006)', 6, swell, great, unison, forward).
state:coupler_mapping('Neo-Classical Church (L\'Alpe d\'Huez - 1978)', 1, swell, pedal, unison, normal).
state:coupler_mapping('Neo-Classical Church (L\'Alpe d\'Huez - 1978)', 2, great, pedal, unison, normal).
state:coupler_mapping('Neo-Classical Church (L\'Alpe d\'Huez - 1978)', 3, great, swell, unison, normal).
state:coupler_mapping('Neo-Classical Church (L\'Alpe d\'Huez - 1978)', 4, swell, great, unison, normal).
state:coupler_mapping('Neo-Classical Church (L\'Alpe d\'Huez - 1978)', 5, great, great, super, normal).
state:coupler_mapping('Neo-Classical Church (L\'Alpe d\'Huez - 1978)', 6, swell, swell, sub, normal).
state:coupler_mapping('Progressive', 1, choir, pedal, unison, normal).
state:coupler_mapping('Progressive', 2,  choir, pedal, sub, normal).
state:coupler_mapping('Progressive', 3, swell, pedal, unison, normal).
state:coupler_mapping('Progressive', 4, swell, great, unison, normal).
state:coupler_mapping('Progressive', 5, great, great, super, normal).
state:coupler_mapping('Progressive', 6, swell, swell, sub, normal).
state:coupler_mapping('Tweaked', 1, choir, pedal, unison, forward).
state:coupler_mapping('Tweaked', 2, great, pedal, unison, forward).
state:coupler_mapping('Tweaked', 3, swell, pedal, unison, forward).
state:coupler_mapping('Tweaked', 4, choir, great, unison, forward).
state:coupler_mapping('Tweaked', 5, swell, great, unison, forward).
state:coupler_mapping('Tweaked', 6, swell, choir, unison, forward).
state:coupler_mapping('A Clockwork Organ', 1, swell, pedal, unison, forward).
state:coupler_mapping('A Clockwork Organ', 2, pedal, great(forward), unison, forward).
state:coupler_mapping('A Clockwork Organ', 3, choir, great, unison, forward).
state:coupler_mapping('A Clockwork Organ', 4, swell, great, unison, forward).
state:coupler_mapping('A Clockwork Organ', 5, swell, choir, unison, forward).
state:coupler_mapping('A Clockwork Organ', 6, choir, choir, sub, forward).
state:coupler_mapping('Is That A Synth ?!', 1, swell, pedal, unison, forward).
state:coupler_mapping('Is That A Synth ?!', 2, pedal, great(forward), unison, forward).
state:coupler_mapping('Is That A Synth ?!', 3, choir, great, unison, forward).
state:coupler_mapping('Is That A Synth ?!', 4, swell, great, unison, forward).
state:coupler_mapping('Is That A Synth ?!', 5, swell, choir, unison, forward).
state:coupler_mapping('Is That A Synth ?!', 6, choir, choir, sub, forward).

% Default fallback (common configuration)
state:coupler_mapping(_, 1, choir, pedal, unison, normal).
state:coupler_mapping(_, 2, great, pedal, unison, normal).
state:coupler_mapping(_, 3, swell, pedal, unison, normal).
state:coupler_mapping(_, 4, choir, great, unison, normal).
state:coupler_mapping(_, 5, swell, great, unison, normal).
state:coupler_mapping(_, 6, swell, choir, unison, normal).


