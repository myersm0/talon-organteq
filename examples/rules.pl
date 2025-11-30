% rules_example.pl - Example rules for talon-organteq
%
% Copy this file to your rules directory and modify as needed.
% Load with: load_rules_from_dir('/path/to/rules', '*.pl')
%
% Rule types:
%   persistent - tracks ownership, cumulative levels
%   transient  - fire-and-forget, no ownership tracking
%
% Divisions:
%   manuals: pedal, choir, great, swell
%   auxiliaries: coupler, mono_coupler, tremulant
%
% rule_selector forms:
%   rule_selector(RuleId, Level, Selector)           - applies to all targeted divisions
%   rule_selector(RuleId, Level, Division, Selector) - applies to specific division only
%
% When apply_rule is called:
%   divisions=manuals (default) - only manuals
%   divisions=all - all divisions including auxiliaries
%   divisions=[great, swell, coupler] - explicit list

:- multifile state:rule/2.
:- multifile state:max_level/2.
:- multifile state:antonym/2.
:- multifile state:rule_selector/3.
:- multifile state:rule_selector/4.
:- multifile state:rule_selector/5.

% ============================================================================
% Transient rules
% ============================================================================

state:rule(brighten, transient).
state:rule(darken, transient).
state:antonym(brighten, darken).
state:antonym(darken, brighten).
state:max_level(brighten, 1).
state:max_level(darken, 1).

% brighten: engage mixtures and mutations
state:rule_selector(brighten, 1, all, family(mixture), engage).
state:rule_selector(brighten, 1, all, family(mutation), engage).

% darken: disengage mixtures and mutations (inverse of brighten)
state:rule_selector(darken, 1, all, family(mixture), disengage).
state:rule_selector(darken, 1, all, family(mutation), disengage).

state:rule(add_reeds, transient).
state:max_level(add_reeds, 2).

state:rule_selector(add_reeds, 1, family(reed, any, 1, first)).
state:rule_selector(add_reeds, 2, family(reed)).

% ============================================================================
% Persistent rules (combinations)
% ============================================================================

% 'my persistent rule #1' - foundation combination building from soft to full
state:rule('my persistent rule #1', persistent).
state:max_level('my persistent rule #1', 3).

state:rule_selector('my persistent rule #1', 1, great, numbers([1, 2, 3])).
state:rule_selector('my persistent rule #1', 1, swell, numbers([1, 2])).

state:rule_selector('my persistent rule #1', 2, pedal, numbers([1, 2])).
state:rule_selector('my persistent rule #1', 2, great, numbers([4])).

state:rule_selector('my persistent rule #1', 3, great, family(reed, any, 1, first)).

% 'my persistent rule #2' - reed-heavy combination
state:rule('my persistent rule #2', persistent).
state:max_level('my persistent rule #2', 2).

state:rule_selector('my persistent rule #2', 1, great, numbers([1, 2])).
state:rule_selector('my persistent rule #2', 1, swell, family(reed, any, 1, first)).
state:rule_selector('my persistent rule #2', 2, great, family(reed)).

% ============================================================================
% Persistent rules with auxiliaries (couplers, tremulants)
% ============================================================================

% full_organ - a comprehensive registration including couplers
% Use with: apply_rule("full_organ", divisions=all) or explicit list
state:rule(full_organ, persistent).
state:max_level(full_organ, 4).

% Level 1: foundation on great and pedal
state:rule_selector(full_organ, 1, great, numbers([1, 2, 3])).
state:rule_selector(full_organ, 1, pedal, numbers([1, 2])).

% Level 2: add swell, couple swell to great
state:rule_selector(full_organ, 2, swell, numbers([1, 2, 3])).
state:rule_selector(full_organ, 2, coupler, coupler(swell, great)).

% Level 3: add reeds, couple great to pedal
state:rule_selector(full_organ, 3, great, family(reed)).
state:rule_selector(full_organ, 3, swell, family(reed)).
state:rule_selector(full_organ, 3, coupler, coupler(great, pedal)).

% Level 4: full with all couplers and tremulant
state:rule_selector(full_organ, 4, coupler, coupler(swell, pedal)).
state:rule_selector(full_organ, 4, tremulant, numbers([1])).

% solo_reed - a solo reed with tremulant
state:rule(solo_reed, persistent).
state:max_level(solo_reed, 2).

state:rule_selector(solo_reed, 1, swell, family(reed, 8, 1, first)).
state:rule_selector(solo_reed, 1, swell, family(flute, 8, 1, first)).

state:rule_selector(solo_reed, 2, tremulant, numbers([1])).
