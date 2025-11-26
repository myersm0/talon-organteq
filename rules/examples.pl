% examples.pl - Example rules demonstrating the rule definition format
%
% This file provides example rules. Users can copy and modify these
% in their own rules folder (~/.config/organteq_registration/rules/).
%
% Rule types:
%   persistent - tracks ownership, cumulative levels, reasserts on level change
%   transient  - fire-and-forget, no ownership tracking

:- discontiguous rule/2.
:- discontiguous max_level/2.
:- discontiguous antonym/2.
:- discontiguous rule_level_selector/4.

% ============================================================================
% Transient rules - transformations that modify current state
% ============================================================================

% brighten/darken - antonym pair for adding/removing upper partials
rule(brighten, transient).
rule(darken, transient).
antonym(brighten, darken).
antonym(darken, brighten).
max_level(brighten, 3).
max_level(darken, 3).

% Level 1: add one mixture
rule_level_selector(brighten, 1, _, family(mixture, any, 1, first)).
% Level 2: add all mixtures
rule_level_selector(brighten, 2, _, family(mixture)).
% Level 3: add all mixtures and mutations
rule_level_selector(brighten, 3, _, family(mixture)).
rule_level_selector(brighten, 3, _, family(mutation)).

% add_reeds - add reed power
rule(add_reeds, transient).
max_level(add_reeds, 2).

rule_level_selector(add_reeds, 1, _, family(reed, any, 1, first)).
rule_level_selector(add_reeds, 2, _, family(reed)).

% ============================================================================
% Persistent rules (combinations) - layered registrations with ownership
% ============================================================================

% alpha - foundation combination building from soft to full
rule(alpha, persistent).
max_level(alpha, 3).

% Level 1: basic foundation
rule_level_selector(alpha, 1, great, numbers([1, 2, 3])).
rule_level_selector(alpha, 1, swell, numbers([1, 2])).

% Level 2: add pedal and more great
rule_level_selector(alpha, 2, pedal, numbers([1, 2])).
rule_level_selector(alpha, 2, great, numbers([4])).

% Level 3: add a reed
rule_level_selector(alpha, 3, great, family(reed, any, 1, first)).

% bravo - reed-heavy combination
rule(bravo, persistent).
max_level(bravo, 2).

rule_level_selector(bravo, 1, great, numbers([1, 2])).
rule_level_selector(bravo, 1, swell, family(reed, any, 1, first)).
rule_level_selector(bravo, 2, great, family(reed)).

% ============================================================================
% Example constraint violations - users can define their own
% ============================================================================

% Example: violation if more than N reeds are engaged
% To use: assertz(max_reeds(great, 2))
:- dynamic max_reeds/2.

violation(too_many_reeds(Manual, Count, Max)) :-
	max_reeds(Manual, Max),
	findall(N, (engaged(Manual, N), stop_family(Manual, N, reed)), Reeds),
	length(Reeds, Count),
	Count > Max.

% Example: violation if no foundation (principal) when stops engaged
% To use: assertz(requires_foundation(great))
:- dynamic requires_foundation/1.

violation(no_foundation(Manual)) :-
	requires_foundation(Manual),
	engaged(Manual, _),
	\+ (engaged(Manual, N), stop_family(Manual, N, principal)).
