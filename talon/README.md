# Voice commands for talon-organteq

## Setup
1. Launch Organteq with JSON-RPC server enabled:
```bash
/Applications/Organteq\ 2/Organteq\ 2.app/Contents/MacOS/Organteq\ 2 --serve
```

2. Start the Prolog server:
```bash
cd prolog
swipl -g "consult('main.pl'), server(5000)."
```

3. Say "sync" to populate voice command lists with available rules and selectors.

---

## Stop control

The general syntax follows the pattern:
```
[action] [manual] <stop-identifier>
```

- **action** (optional): `toggle` (default), `push`/`disengage`, `pull`/`engage`, or `solo`
- **manual** (optional): `pedal`, `choir`, `great`, or `swell`. Omit to use current context.
- **stop-identifier**: Numbers, tonal family, or named selector

### By number
```
great one three twelve       # toggle stops 1, 3, 12 on Great
pull great one three         # engage stops 1, 3 on Great
push swell two               # disengage stop 2 on Swell
solo great one six           # only stops 1, 6 engaged on Great
```

### By tonal family
```
pull great reeds             # engage all reeds on Great
push swell mixtures          # disengage all mixtures on Swell
solo great principals        # only principals on Great

pull great eight foot reeds  # engage 8' reeds on Great
push swell four foot flutes  # disengage 4' flutes on Swell
```

Available families: `principals`, `flutes`, `strings`, `reeds`, `mutations`, `mixtures`

Available footages: `thirty two foot`, `sixteen foot`, `eight foot`, `four foot`, `two foot`, `one foot`

### By named selector
Named selectors are defined in Prolog and become available after saying "sync".

Built-in selectors:
```
pull great engaged           # re-engage what's already on (no-op, but useful in compounds)
push swell disengaged        # no-op example
```

User-defined selectors (these are from custom_selectors.pl):
```
pull great evens             # engage even-numbered stops
solo swell odds              # solo odd-numbered stops
push great complement        # disengage everything that's off (no-op)
solo pedal owned             # solo stops owned by active rules
```

### Manual context
Set a current manual to avoid repeating the manual name:
```
use great                    # set Great as current manual
one three twelve             # toggle stops 1, 3, 12 on Great
pull reeds                   # engage reeds on Great
evens                        # toggle evens on Great
```

---

## Clearing stops
```
cancel great                 # turn off all stops on Great
cancel                       # turn off all stops on current manual
general cancel               # turn off all stops everywhere
```

---

## Couplers
```
couple one                   # engage coupler 1
decouple one                 # disengage coupler 1

couple swell to great        # engage swell-to-great coupler
decouple swell from great    # disengage swell-to-great coupler

decouple all                 # disengage all couplers
```

---

## Utility
```
undo                         # undo last operation
redo                         # redo last undone operation

sync                         # sync state from Organteq, update voice lists

show help                    # show help overlay (selectors, rules, levels)
hide help                    # hide overlay
toggle help                  # toggle overlay
```

---

## Defining custom selectors

Create a `.pl` file with your selectors. Example (`my_selectors.pl`):

```prolog
:- multifile selectors:resolve_selector/3.
:- multifile selectors:named_selector/1.

% Register for voice commands
selectors:named_selector(favorites).

% Define resolution
selectors:resolve_selector(Division, favorites, Stops) :-
    selectors:resolve_selector(Division, numbers([1, 3, 5, 7]), Stops).
```

Load it on server start or via the `load` endpoint, then say "sync" to update voice lists.

Tips for selector names:
- Use single words or short phrases that are easy to speak
- Avoid underscores (use camelCase or single words)
- Test pronunciation with your speech engine
