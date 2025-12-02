# talon-organteq Python API reference

Complete reference for the `Bridge` class and related components.

## Bridge
The main interface for controlling Organteq through the Prolog server.

```python
from client import Bridge

bridge = Bridge(
    organteq_endpoint="http://127.0.0.1:8081/jsonrpc",
    prolog_endpoint="http://localhost:5000"
)
```

### Initialization

#### `sync() -> dict`
Synchronize state from Organteq to the Prolog server. Call this on startup and after preset changes.

```python
result = bridge.sync()
```

Returns a dict with `status`, `state`, and `actions` keys.

---

## Stop control
### `engage(division, selector) -> dict`
Engage (turn on) stops matching the selector.

```python
bridge.engage("great", [1, 2, 3])
bridge.engage("swell", {"by": "family", "values": "reed"})
```

### `disengage(division, selector) -> dict`
Disengage (turn off) stops matching the selector.

```python
bridge.disengage("great", [4, 5])
bridge.disengage("pedal", {"by": "family", "values": "reed"})
```

### `toggle(division, selector) -> dict`

Toggle the state of stops matching the selector.

```python
bridge.toggle("great", [1, 2])
```

### `solo(division, selector) -> dict`

Engage matching stops and disengage all others on the division.

```python
bridge.solo("great", [1, 6])
bridge.solo("swell", {"by": "family", "values": "string"})
```

### `clear(division) -> dict`

Disengage all stops on a division.

```python
bridge.clear("great")
```

### `clear_all() -> dict`

Disengage all stops on all divisions.

```python
bridge.clear_all()
```

---

## Selectors
Selectors specify which stops to target. The simplest form is a list of numbers:

```python
bridge.engage("great", [1, 2, 3])
```

For more complex selections, use a selector dict:

### By numbers
```python
{"by": "numbers", "values": [1, 2, 3]}
```

### By tonal family
```python
{"by": "family", "values": "reed"}
{"by": "family", "values": "principal", "footage": 8}
{"by": "family", "values": "mixture", "limit": 1, "limit_method": "first"}
```

Options:
- `footage`: Filter by pitch (8, 4, 16, etc.)
- `limit`: Maximum number of stops to select
- `limit_method`: `"first"`, `"last"`, or `"random"`

### By name
```python
{"by": "names", "values": ["Trompette 8'", "Cromorne 8'"]}
```

### By current state
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
    "base": {"by": "engaged"},
    "subtract": {"by": "family", "values": "reed"}
}
```

### Coupler selectors
```python
{"by": "coupler", "source": "swell", "destination": "great"}
{"by": "coupler", "source": "swell", "destination": "great", "transposition": "sub"}
{"by": "coupler_from", "values": "swell"}
{"by": "coupler_to", "values": "pedal"}
```

---
## Rules
Rules are named multi-level registration presets defined in Prolog.

### `apply_rule(rule_id, action=None, delta=None, level=None) -> dict`

Apply a rule with various options.

#### By level (absolute)
```python
bridge.apply_rule("full_organ", level=2)
```

#### By delta (relative)
```python
bridge.apply_rule("full_organ", delta=1)   # increment level
bridge.apply_rule("full_organ", delta=-1)  # decrement level
```

#### By action
```python
bridge.apply_rule("full_organ", action="mute")      # set to level 0
bridge.apply_rule("full_organ", action="minimize")  # set to level 1
bridge.apply_rule("full_organ", action="maximize")  # set to max level
bridge.apply_rule("full_organ", action="solo")      # only this rule's stops
bridge.apply_rule("full_organ", action="reassert")  # re-engage at current level
```

#### Default behavior
```python
bridge.apply_rule("full_organ")  # equivalent to delta=1
```

### `list_rules(preset=None) -> list[str]`

List available rules, optionally filtered by preset.

```python
rules = bridge.list_rules()
baroque_rules = bridge.list_rules(preset="Baroque Cathedral")
```

### get_rule_info(rule_id) -> dict | None
Get metadata for a rule. Returns None if the rule doesn't exist.
```python
info = bridge.get_rule_info("crescendo great")
# {
#     "rule": "crescendo great",
#     "type": "persistent",
#     "max_level": 13,
#     "antonym": null,
#     "current_level": 0
# }
```

### get_max_level(rule_id) -> int
Convenience method to get a rule's max level. Returns 1 if rule not found.
```python
max_level = bridge.get_max_level("full_organ")  # 4
```

---

## History
### `undo() -> dict`

Undo the last operation.

```python
bridge.undo()
```

### `redo() -> dict`

Redo the last undone operation.

```python
bridge.redo()
```

---

## State inspection

### `get_state() -> dict`

Get the current state from the Prolog server.

```python
state = bridge.get_state()
# {
#     "preset": "Baroque Cathedral",
#     "engaged": [{"division": "great", "number": 1}, ...],
#     "rule_levels": [{"rule": "full_organ", "level": 2}, ...]
# }
```

---

## Low-level access
### `execute(command, args=None) -> dict`
Execute an arbitrary command on the Prolog server.

```python
result = bridge.execute("engage", {
    "division": "great",
    "selector": {"by": "numbers", "values": [1, 2, 3]}
})
```

### `load_rules(filepath) -> dict`

Load a rules file into the Prolog server.

```python
bridge.load_rules("/path/to/my_rules.pl")
```

---

## Response format
All methods return a dict with the following structure:

```python
{
    "status": "ok",  # or "error"
    "state": {
        "preset": "...",
        "engaged": [...],
        "rule_levels": [...]
    },
    "actions": [
        {"type": "set_stop", "division": "great", "number": 1, "value": 1.0},
        ...
    ],
    "message": "..."  # present on error
}
```

---

## Examples
### Build a registration incrementally

```python
bridge.sync()
bridge.clear_all()

# Foundation
bridge.engage("great", {"by": "family", "values": "principal", "footage": 8})
bridge.engage("pedal", [1, 2])

# Add color
bridge.engage("swell", {"by": "family", "values": "string"})
bridge.engage("great", {"by": "family", "values": "flute", "footage": 4})

# Add reeds
bridge.engage("great", {"by": "family", "values": "reed"})
```

### Use rules for layered registrations
```python
bridge.sync()
bridge.clear_all()

# Build up through levels
bridge.apply_rule("full_organ", level=1)  # foundation
bridge.apply_rule("full_organ", level=2)  # add swell + coupler
bridge.apply_rule("full_organ", level=3)  # add reeds

# Or incrementally (implicitly increments rule level with each application)
bridge.apply_rule("full_organ")  # +1
bridge.apply_rule("full_organ")  # +1

# Back down
bridge.apply_rule("full_organ", action="mute")
```

### Combine rules
```python
# Multiple rules can be active simultaneously
bridge.apply_rule("foundation", level=2)
bridge.apply_rule("solo_reed", level=1)

# Ownership tracking: muting foundation won't affect stops
# that solo_reed also claims
bridge.apply_rule("foundation", action="mute")
```

### Query and filter
```python
# Get currently engaged reeds
result = bridge.execute("engage", {
    "division": "great",
    "selector": {
        "by": "intersection",
        "values": [
            {"by": "family", "values": "reed"},
            {"by": "engaged"}
        ]
    }
})
```
