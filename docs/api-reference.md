# talon-organteq API reference

Commands and selectors for the Prolog server.

## Python bridge
The `Bridge` class provides HTTP access to the Prolog server.

```python
from client import Bridge

bridge = Bridge(endpoint="http://localhost:5000")
bridge.sync()                    # sync state from Organteq
bridge.run("engage(great, numbers([1, 2]))")
bridge.state()                   # get current state dict
bridge.query("engaged(great, N)")  # raw Prolog query
bridge.load("/path/to/rules.pl") # load rules file
```

All registration logic is in Prolog. The `run()` method sends a command string which is parsed and executed by the Prolog server.

---

## Stop control

### engage
Engage (turn on) stops matching the selector.
```prolog
engage(Division, Selector)
```

Examples:
```prolog
engage(great, numbers([1, 2, 3]))
engage(swell, family(reed))
engage(pedal, family(principal, 16))
```

### disengage
Disengage (turn off) stops matching the selector.
```prolog
disengage(Division, Selector)
```

### toggle
Toggle the state of stops matching the selector.
```prolog
toggle(Division, Selector)
```

### solo
Engage matching stops and disengage all others on the division.
```prolog
solo(Division, Selector)
```

### clear
Disengage all stops on a division.
```prolog
clear(Division)
```

### clear_all
Disengage all stops on all divisions.
```prolog
clear_all
```

---

## Selectors
Selectors specify which stops to target.

### By numbers
```prolog
numbers([1, 2, 3])
```

### All elements
```prolog
all          % all elements in division
stops        % all stops (excludes couplers, tremulants)
```

### By state
```prolog
engaged      % currently engaged elements
disengaged   % currently disengaged elements
```

### By tonal family
```prolog
family(reed)
family(principal, 8)              % family + footage
family(reed, any, 2, first)       % first 2 reeds
family(mixture, any, 1, random)   % random mixture
```

Families: `principal`, `flute`, `string`, `reed`, `mutation`, `mixture`

Footage: `32`, `16`, `8`, `4`, `2`, `1`, or `any`

Limit methods: `first`, `last`, `random`

### By name
```prolog
names(['Trompette 8''', 'Cromorne 8'''])
```

### By type
```prolog
type(stop)
type(coupler)
type(tremulant)
```

### Compound selectors

#### Union (OR)
```prolog
union([family(reed), family(mixture)])
```

#### Intersection (AND)
```prolog
intersection([family(reed), engaged])
```

#### Difference (subtract)
```prolog
difference(engaged, family(reed))
```

### Coupler selectors
```prolog
couplers                              % all couplers
coupler(swell, great)                 % swell to great
coupler(swell, great, sub)            % swell to great, sub-octave
coupler_from(swell)                   % all couplers from swell
coupler_to(pedal)                     % all couplers to pedal
```

### Preset-specific selectors
```prolog
for_preset('Baroque*', numbers([1, 2, 3]))
```

Matches only when the current preset matches the pattern. Patterns support `*` wildcards.

---

## Couplers

### couple
Engage a coupler by source and destination.
```prolog
couple(Source, Destination)
couple(Source, Destination, Transposition)
```

Transposition: `unison` (default), `sub`, `super`

Examples:
```prolog
couple(swell, great)
couple(swell, great, sub)
```

### decouple
Disengage a coupler.
```prolog
decouple(Source, Destination)
decouple(Source, Destination, Transposition)
```

### couple_index / decouple_index
Engage or disengage a coupler by its index (1-6).
```prolog
couple_index(1)
decouple_index(1)
```

### decouple_all
Disengage all couplers.
```prolog
decouple_all
```

---

## Tremulants

### tremulant_on / tremulant_off
```prolog
tremulant_on(1)
tremulant_off(1)
```

### tremulant_toggle
```prolog
tremulant_toggle(1)
```

### tremulant
Alias for `tremulant_toggle`.
```prolog
tremulant(1)
```

---

## Rules
Rules are named, multi-level registration presets. See the [advanced usage guide](advanced-usage.md) for writing custom rules.

### up / down
Increment or decrement the rule level.
```prolog
up('crescendo great')
down('crescendo great')
```

### level
Set an absolute level.
```prolog
level('crescendo great', 5)
```

### mute
Set level to 0, disengaging all owned stops.
```prolog
mute('crescendo great')
```

### maximize
Set level to the rule's maximum.
```prolog
maximize('crescendo great')
```

### minimize
Set level to 1.
```prolog
minimize('crescendo great')
```

### solo_rule
Engage only this rule's stops, disengage everything else.
```prolog
solo_rule('crescendo great')
```

### reassert
Re-engage all stops at the current level (useful after manual overrides).
```prolog
reassert('crescendo great')
```

### apply
Shorthand for `up`. Useful for predicate-based rules.
```prolog
apply(brighten)
```

---

## History

### undo
Undo the last operation.
```prolog
undo
```

### redo
Redo the last undone operation.
```prolog
redo
```

---

## Sync and state

### sync (HTTP endpoint)
Synchronize state from Organteq. Called automatically on startup and when presets change.

```python
bridge.sync()
```

### state (HTTP endpoint)
Get current state as a dict.

```python
state = bridge.state()
# {
#     "preset": "Baroque Cathedral I",
#     "engaged": {"great": [1, 2], "swell": [], ...},
#     "rule_levels": {"crescendo great": 3, ...}
# }
```

---

## Server control

### Logging
```prolog
set_log_level(0)   % silent (default)
set_log_level(1)   % actions only
set_log_level(2)   % commands + actions
```

### Polling
The server polls Organteq for preset changes every 5 seconds by default.
```prolog
stop_polling
start_polling
set_poll_interval(10)   % seconds
```

### Loading rules
```prolog
load_rules('/path/to/rules')
load_rules('/path/to/rules', '*.pl')
```

