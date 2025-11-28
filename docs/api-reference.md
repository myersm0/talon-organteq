# API reference for talon-organteq's RegistrationEngine

Complete reference for the `RegistrationEngine` API.

---

## Initialization
```python
from core import RegistrationEngine

engine = RegistrationEngine(
    prolog_host="localhost",    # Prolog server host
    prolog_port=5000,           # Prolog server port
    rules_dir=None,             # path to rules directory (optional)
    rules_glob="*.pl"           # glob pattern for rule files
)
engine.load_rules()             # load rules from rules_dir
engine.sync_state()             # sync with Organteq's current state
```

All parameters are optional. Without `rules_dir`, basic stop control works but no custom rules are available.

---

## Manuals
The `manual` argument accepts: `"pedal"`, `"choir"`, `"great"`, or `"swell"`.

Many methods accept a `manuals` parameter (list of manuals). When `None`, defaults to all four manuals.

---

## Basic stop control

### By stop number
```python
engine.engage(manual, stops)
engine.disengage(manual, stops)
engine.toggle(manual, stops)
engine.solo(manual, stops)
engine.clear(manual)
```
**Parameters:**
- `manual`: string — the manual name
- `stops`: list of strings — stop numbers (e.g., `["1", "2", "3"]`)

**Examples:**
```python
engine.engage("great", ["1", "2", "3"])      # engage stops 1, 2, 3
engine.disengage("great", ["2"])             # disengage stop 2
engine.toggle("swell", ["4", "5"])           # flip states of 4 and 5
engine.solo("great", ["1", "6"])             # only 1 and 6 engaged, all others off
engine.clear("pedal")                        # all pedal stops off
```

### By Tonal Family
```python
engine.engage_family(manual, family, footage=None, limit=None)
engine.disengage_family(manual, family, footage=None, limit=None)
engine.toggle_family(manual, family, footage=None, limit=None)
engine.solo_family(manual, family, footage=None)
```
**Parameters:**
- `manual`: string — the manual name
- `family`: string — tonal family (`"principal"`, `"flute"`, `"string"`, `"reed"`, `"mutation"`, `"mixture"`)
- `footage`: string, optional — pitch filter (e.g., `"8"`, `"16"`)
- `limit`: int, optional — maximum number of stops to affect

**Examples:**
```python
engine.engage_family("great", "reed")                    # all reeds
engine.engage_family("great", "principal", footage="8")  # only 8' principals
engine.disengage_family("swell", "mixture")              # remove all mixtures
engine.solo_family("choir", "flute", footage="4")        # only 4' flutes
```

---

## Selector operations
Selectors provide an advanced way to specify stops. The `apply_selector` method applies any selector with any action.

```python
engine.apply_selector(manual, selector, action="engage", manuals=None)
```

**Parameters:**
- `manual`: string — primary manual (used if `manuals` not specified)
- `selector`: dict — selector specification (see below)
- `action`: string — `"engage"`, `"disengage"`, `"toggle"`, or `"solo"`
- `manuals`: list, optional — apply to these manuals instead

### Selector types

#### By number
```python
{"by": "numbers", "values": [1, 2, 3]}
```

#### By family
```python
{"by": "family", "values": "reed"}
{"by": "family", "values": "reed", "footage": 8}
{"by": "family", "values": "mixture", "limit": 1, "limit_method": "first"}
```

`limit_method` can be `"first"`, `"last"`, or `"random"`.

#### By name
```python
{"by": "names", "values": ["Trompette 8'", "Cromorne 8'"]}
```

#### By current status
```python
{"by": "engaged"}
{"by": "disengaged"}
```

### Compound selectors

#### Union (OR)
```python
{
    "by": "union",
    "values": [
        {"by": "family", "values": "reed"},
        {"by": "family", "values": "mixture"}
    ]
}
```

#### Intersection (AND)
```python
{
    "by": "intersection",
    "values": [
        {"by": "family", "values": "reed"},
        {"by": "engaged"}
    ]
}
```

#### Difference (subtract)
```python
{
    "by": "difference",
    "base": {"by": "family", "values": "principal"},
    "subtract": {"by": "family", "values": "principal", "footage": 16}
}
```

### Selector examples

```python
# Engage all reeds and mixtures
engine.apply_selector("great", {
    "by": "union",
    "values": [
        {"by": "family", "values": "reed"},
        {"by": "family", "values": "mixture"}
    ]
}, action="engage")

# Disengage only the reeds that are currently on
engine.apply_selector("great", {
    "by": "intersection",
    "values": [
        {"by": "family", "values": "reed"},
        {"by": "engaged"}
    ]
}, action="disengage")

# Solo: everything currently engaged except 16' stops
engine.apply_selector("great", {
    "by": "difference",
    "base": {"by": "engaged"},
    "subtract": {"by": "family", "values": "principal", "footage": 16}
}, action="solo")
```

---

## Rules

Rules are named, multi-level registration presets defined in Prolog. The unified `apply_rule` method works for both persistent and transient rules.

### apply_rule (unified interface)

```python
engine.apply_rule(rule_id, delta=None, level=None, action=None, manuals=None)
```

**Parameters:**
- `rule_id`: string — the rule name
- `delta`: int, optional — change level by this amount (+1, -1, etc.)
- `level`: int, optional — set to this absolute level
- `action`: string, optional — special action (see below)
- `manuals`: list, optional — restrict to these manuals

**Level operations:**
```python
engine.apply_rule("foundation", delta=1)     # increment level by 1
engine.apply_rule("foundation", delta=-1)    # decrement level by 1
engine.apply_rule("foundation", delta=2)     # increment level by 2
engine.apply_rule("foundation", level=3)     # set to level 3
```

**Actions:**
```python
engine.apply_rule("foundation", action="mute")      # set to level 0
engine.apply_rule("foundation", action="minimize")  # set to level 1
engine.apply_rule("foundation", action="maximize")  # set to max level
engine.apply_rule("foundation", action="solo")      # only this rule's stops engaged
engine.apply_rule("foundation", action="reassert")  # re-engage stops at current level
```

### Convenience methods

These wrap `apply_rule` for common operations:

```python
engine.mute_rule(rule_id, manuals=None)
engine.minimize_rule(rule_id, manuals=None)
engine.maximize_rule(rule_id, manuals=None)
engine.solo_rule(rule_id, manuals=None)
engine.reassert_rule(rule_id, manuals=None)
```

### Legacy aliases

For backwards compatibility, combination-specific methods still work:

```python
engine.apply_combination(rule_id, delta=None, level=None, manuals=None)
engine.mute_combination(rule_id, manuals=None)
engine.minimize_combination(rule_id, manuals=None)
engine.maximize_combination(rule_id, manuals=None)
engine.solo_combination(rule_id, manuals=None)
engine.reassert_combination(rule_id, manuals=None)
```

### Rule types

**Persistent rules** track ownership of stops. Multiple persistent rules can engage the same stop; that stop will stay engaged until all persistent rules have released it. (Unless you disengage it yourself manually or via a application of a non-persistent (transient) rule.)

**Transient rules** are one-time, stateless transformations. They modify state but don't track ownership.

---

## History

```python
engine.undo()
engine.redo()
```

Every operation creates a snapshot. Undo/redo restores both the engine state and the actual Organteq stop states.

---

## State inspection

```python
# Current engaged stops per manual
engine.state
# {'pedal': ['1', '2'], 'choir': [], 'great': ['1', '5'], 'swell': ['3']}

# Current rule levels
engine.rule_levels
# {'brighten': 1, 'darken': 0}

# Current combination (persistent rule) levels
engine.combination_levels
# {'foundation': 2, 'reeds': 1}

# Current Organteq preset name
engine.current_preset
# 'Alsacian Organ I (Strasbourg, St-Thomas - 1741)'
```

---

## Prolog client

For advanced use, access the Prolog client directly:

```python
# Resolve a selector to stop numbers
stops = engine.prolog.resolve_selector("great", {"by": "family", "values": "reed"})
# [4, 7, 12]

# Get rule metadata
meta = engine.prolog.get_rule_metadata("foundation")
# {'type': 'persistent', 'max_level': 3, 'antonym': None}

# Query Prolog directly
results = engine.prolog.query("stop_family(great, N, reed)")
```

---

## Complete example

```python
from core import RegistrationEngine

# Initialize
engine = RegistrationEngine(rules_dir="~/.config/organteq_registration/rules")
engine.load_rules()
engine.sync_state()

# Clear everything
for m in ["pedal", "choir", "great", "swell"]:
    engine.clear(m)

# Build up a registration using rules
engine.apply_rule("foundation", level=2)    # foundations at level 2
engine.apply_rule("reeds", delta=1)         # add first level of reeds

# Add some manual tweaks
engine.engage_family("swell", "string")     # strings on swell
engine.disengage("great", ["5"])            # remove stop 5

# Change your mind about the reeds
engine.apply_rule("reeds", action="mute")   # remove reeds layer

# Or undo the whole thing
engine.undo()
engine.undo()
engine.undo()
```

