# Voice commands for talon-organteq

## Setup
1. Launch Organteq with JSON-RPC server enabled. On a Mac this can be done from the command line like this:
```bash
/Applications/Organteq\ 2/Organteq\ 2.app/Contents/MacOS/Organteq\ 2 --serve
```

2. Start the Prolog server:
```bash
cd prolog
swipl -g "consult('main.pl'), server(5000)."
```

---

## Stop control
The general syntax for controlling stops follows the pattern:
```
[action] [manual] <stop-identifier(s)>
```

Where:
- **action** (optional): `toggle` (default), `push`/`disengage`, `pull`/`engage`, or `solo`
- **manual** (optional): `pedal`, `choir`, `great`, or `swell`. Can be omitted if you've set a manual context with `use [manual]`
- **stop-identifiers**: Stop numbers, tonal family, or family with footage

### By number
```
great one three twelve       # toggle stops 1, 3, and 12 on the Great
swell two                    # toggle stop 2 on the Swell
pedal four seven             # toggle Pedal stops 4 and 7

pull great one three twelve  # engage stops 1, 3, and 12 on the Great
push great two               # disengage stop 2 on the Great
solo great one six           # only stops 1 and 6 engaged on Great
```

### By tonal family
```
pull great reeds             # engage all reeds on Great
push swell mixtures          # disengage all mixtures on Swell
solo great principals        # only principals engaged on Great

pull great eight foot reeds  # engage all 8' reeds on Great
push swell four foot flutes  # disengage all 4' flutes on Swell
```

Available families: `principals`, `flutes`, `strings`, `reeds`, `mutations`, `mixtures`

Available footages: `thirty two foot`, `sixteen foot`, `eight foot`, `four foot`, `two foot`, `one foot`

### Manual context
Set a current manual to avoid repeating the manual name:
```
use great                    # set Great as current manual
one three twelve             # toggle stops 1, 3, 12 on Great
pull two four                # engage stops 2, 4 on Great
push five                    # disengage stop 5 on Great
reeds                        # toggle reeds on Great
```

---

## Clearing stops
```
cancel great                 # turn off all stops on Great
cancel swell                 # turn off all stops on Swell
cancel                       # turn off all stops on current manual
general cancel               # turn off all stops on all manuals
```

---

## Couplers
```
couple one                   # engage coupler 1
decouple one                 # disengage coupler 1

couple swell to great        # engage swell to great coupler
decouple swell from great    # disengage swell to great coupler
```

---

## History
```
undo                         # undo last operation
redo                         # redo last undone operation
```

---

## Sync
```
sync                         # sync state from Organteq
sync registration            # same as above
```

---
