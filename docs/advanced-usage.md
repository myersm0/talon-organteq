# Advanced usage guide
This guide covers rule authoring, the selector system, preset-specific behavior, and customization.

## Table of contents
1. [Understanding rules](#understanding-rules)
2. [Writing rules](#writing-rules)
3. [Selector reference](#selector-reference)
4. [Preset-specific rules](#preset-specific-rules)
5. [Predicate-based rules](#predicate-based-rules)
6. [Ownership system](#ownership-system)
7. [Working with auxiliaries](#working-with-auxiliaries)
8. [Stop classification](#stop-classification)

---

## Understanding rules
Rules are reusable, multi-level registration presets. They come in two types:

### Transient rules
One-time, memory-less transformations. Maybe, for example, you want a rule `brighten` to add mixtures or mutations. Each application of such a rule is independent.

### Persistent rules
Transformations that persist across applications, in a cumulative manner. Useful for defining schemes that build incrementally, like crescendo effects.

---

## Writing rules
Rules are defined in Prolog files. You can keep your custom rule files in a directory and load them upon starting the Prolog server.

### Basic structure

```prolog
% Declare multifile predicates (required)
:- multifile state:rule/2.
:- multifile state:max_level/2.
:- multifile state:rule_selector/3.
:- multifile state:rule_selector/4.

% Define a persistent rule
state:rule(my_rule, persistent).
state:max_level(my_rule, 3).

% Level 1: foundation
state:rule_selector(my_rule, 1, great, numbers([1, 2, 3])).
state:rule_selector(my_rule, 1, pedal, numbers([1, 2])).

% Level 2: add color
state:rule_selector(my_rule, 2, swell, numbers([1, 2, 3])).
state:rule_selector(my_rule, 2, great, family(flute, 4)).

% Level 3: full
state:rule_selector(my_rule, 3, great, family(reed)).
state:rule_selector(my_rule, 3, swell, family(reed)).
```

### rule_selector arities

```prolog
% 3-arg: applies to all divisions
rule_selector(RuleId, Level, Selector).

% 4-arg: applies to specific division only  
rule_selector(RuleId, Level, Division, Selector).

% 5-arg: with preset filter (see Preset-Specific Rules)
rule_selector(RuleId, Level, Division, Selector, PresetPattern).
```

### Loading rules

From the Prolog server startup:
```bash
swipl -g "consult('main.pl'), load_rules_from_dir('/path/to/rules', '*.pl'), server(5000)."
```

From Python:
```python
bridge.load_rules("/path/to/my_rules.pl")
```

---

## Selector reference
Selectors are the building blocks for specifying stop sets.

### Atomic selectors

| Selector | Description |
|----------|-------------|
| `numbers([1, 2, 3])` | Specific stop numbers |
| `all` | All elements in division |
| `stops` | All stops (excludes couplers, tremulants) |
| `engaged` | Currently engaged elements |
| `disengaged` | Currently disengaged elements |
| `family(reed)` | Stops of a tonal family |
| `family(reed, 8)` | Family + footage filter |
| `family(reed, any, 2, first)` | Family + limit (first 2) |
| `family(reed, any, 1, random)` | Family + random selection |
| `names(['Trompette 8\\''])` | Stops by exact name |
| `type(stop)` | Elements by type |

### Family options
```prolog
family(Family)
family(Family, Footage)
family(Family, Footage, Limit, Method)
```

- `Family`: `principal`, `flute`, `string`, `reed`, `mutation`, `mixture`
- `Footage`: `8`, `4`, `16`, `32`, `2`, `1`, or `any`
- `Limit`: Maximum number of stops
- `Method`: `first`, `last`, or `random`

### Coupler selectors

```prolog
couplers                           % all couplers
coupler(swell, great)              % swell to great
coupler(swell, great, sub)         % swell to great, sub-octave
coupler_from(swell)                % all couplers from swell
coupler_to(pedal)                  % all couplers to pedal
```

### Compound selectors

```prolog
% Union (OR): reeds OR mixtures
union([family(reed), family(mixture)])

% Intersection (AND): engaged reeds
intersection([family(reed), engaged])

% Difference: all engaged except reeds
difference(engaged, family(reed))
```

### Expression selector
For arbitrary Prolog predicates:

```prolog
% Stops where number > 5
expression(my_filter)

% Define the filter (receives Division, Number)
my_filter(_, N) :- N > 5.
```

---

## Preset-specific rules
Rules can behave differently depending on the active Organteq preset.

### for_preset wrapper
Wrap any selector in `for_preset(Pattern, Selector)`:

```prolog
% Different stop numbers per preset family
state:rule_selector(my_rule, 1, great, for_preset('Baroque Cathedral I', numbers([1, 2, 3]))).
state:rule_selector(my_rule, 1, great, for_preset('Romantic Abbey', numbers([2, 4, 6]))).

% Universal fallback (no for_preset wrapper)
state:rule_selector(my_rule, 1, great, family(principal, 8)).
```

When applying a rule, selectors are tried in definition order. The first matching selector for the current preset wins. Always put preset-specific selectors before universal fallbacks.

### Checking available rules
```python
# List rules available for a specific preset
rules = bridge.list_rules(preset="Baroque Cathedral")
```

---

## Predicate-based rules
For rules that need dynamic behavior beyond static selectors.

### Definition
```prolog
:- multifile state:rule_predicate/1.
:- multifile state:rule_action/2.

state:rule(brighten, transient).
state:rule_predicate(brighten).

state:rule_action(brighten, Actions) :-
    findall(Action, compute_action(Action), Actions).
```

### Example: brighten/darken

```prolog
:- use_module(helpers).
:- use_module(state, [manual/1]).

state:rule(brighten, transient).
state:rule(darken, transient).
state:antonym(brighten, darken).
state:antonym(darken, brighten).
state:rule_predicate(brighten).
state:rule_predicate(darken).

% brighten: engage one disengaged mixture per manual; fallback to mutation
state:rule_action(brighten, Actions) :-
    findall(Action, (
        manual(Div),
        brighten_one(Div, Action)
    ), Actions).

brighten_one(Div, engage(Div, N)) :-
    helpers:first_disengaged_in_family(Div, mixture, N), !.
brighten_one(Div, engage(Div, N)) :-
    helpers:first_disengaged_in_family(Div, mutation, N), !.

% darken: disengage one engaged mutation per manual; fallback to mixture
state:rule_action(darken, Actions) :-
    findall(Action, (
        manual(Div),
        darken_one(Div, Action)
    ), Actions).

darken_one(Div, disengage(Div, N)) :-
    helpers:last_engaged_in_family(Div, mutation, N), !.
darken_one(Div, disengage(Div, N)) :-
    helpers:last_engaged_in_family(Div, mixture, N), !.
```

### Antonyms
Antonym pairs allow opposing rules to cancel each other out:

```prolog
state:antonym(brighten, darken).
state:antonym(darken, brighten).
```

---

## Ownership system
Persistent rules track which stops they "own" at each level.

### How it works
1. When a persistent rule engages a stop, it claims ownership
2. Multiple persistent rules can own the same stop
3. When a rule disengages (level goes down), it releases ownership
4. A stop is only actually disengaged when its last owner releases it

### Example
```python
# Both rules engage stop 1
bridge.apply_rule("my persistent rule #1", level=1)   # owns great [1, 2, 3]
bridge.apply_rule("my persistent rule #2", level=1)   # owns great [1, 2]

bridge.apply_rule("my persistent rule #1", action="mute")
# great [1, 2] still engaged

bridge.apply_rule("my persistent rule #2", action="mute")
# great [] - all off
```

### Why ownership matters
Without ownership tracking, overlapping rules would fight each other. In the example above, if both persistent rules want stop 1 engaged, muting one of the rules would incorrectly turn off stop 1 even though the other rule still needs it.

There is nothing to stop you from manually disengaging a stop, however. Similiarly, things like transient rules or engaging/disengaging stops by number or family -- operations which are not involved in the ownership-tracking system -- are not subject to such constraints. At any time, you may `reassert` a rule in order to re-engage any of its owned stops that may have been disengaged by such an operation.

---

## Working with auxiliaries
Auxiliaries are non-stop elements: couplers, mono couplers, and tremulants.

### Including auxiliaries in rules
By default, `apply_rule` only affects manuals. To include auxiliaries:

```python
# The rule itself must define auxiliary selectors
# (see full_organ example in rules_example.pl)
```

In the rule definition:

```prolog
state:rule(full_organ, persistent).
state:max_level(full_organ, 4).

% Level 2: add swell, couple swell to great
state:rule_selector(full_organ, 2, swell, numbers([1, 2, 3])).
state:rule_selector(full_organ, 2, coupler, coupler(swell, great)).

% Level 4: add tremulant
state:rule_selector(full_organ, 4, tremulant, numbers([1])).
```

### Direct control
```python
# Engage coupler 1
bridge.engage("coupler", [1])

# Engage swell to great coupler by source/dest
bridge.engage("coupler", {"by": "coupler", "source": "swell", "destination": "great"})

# Engage tremulant
bridge.engage("tremulant", [1])
```

---

## Stop classification
Stops are classified by tonal family based on their names. The classification module (`classification.pl`) uses pattern matching.

### Families
| Family | Example stops |
|--------|---------------|
| `principal` | Montre, Prestant, Prinzipal, Octav |
| `flute` | Bourdon, Gedact, Flûte, Nasard |
| `string` | Gambe, Salicional, Voix Céleste |
| `reed` | Trompette, Hautbois, Cromorne, Basson |
| `mutation` | Tierce, Quinte, Septième |
| `mixture` | Mixtura, Zimbel, Plein Jeu, Cornet |

### Footage extraction
Footage is extracted from stop names:
- `"Montre 8'"` → footage 8
- `"Bourdon 16'"` → footage 16
- `"Mixtura IV"` → footage unknown

### Adding classifications
If a stop isn't being classified correctly, add it to `classification.pl`:

```prolog
family_of('Custom Stop Name', reed).
```

---

## Tips

### Testing rules
Use the Prolog server interactively:

```bash
swipl -g "consult('main.pl'), load_rules_from_dir('.', 'rules_example.pl')."
```

Then query:

```prolog
?- state:rule(X, Y).
?- state:rule_selector(my_rule, 1, Div, Sel).
```

### Debugging
Enable debug output in your rule predicates:

```prolog
state:rule_action(my_rule, Actions) :-
    format("Computing actions...~n"),
    % ... computation ...
    format("Actions: ~w~n", [Actions]).
```

### Performance
- Keep selector resolution fast
- Avoid expensive predicates in `expression()` selectors
- Use specific selectors rather than filtering large sets

### Organizing your custom rules
Keep your rules in a separate directory from the main project, for example:

```
~/.config/talon-organteq/rules/
├── my_bach_rules.pl
├── my_mendelssohn_rules.pl
└── my_experimental_rules.pl
```

Load at startup:

```bash
swipl -g "consult('main.pl'), load_rules_from_dir('~/.config/talon-organteq/rules', '*.pl'), server(5000)."
```
