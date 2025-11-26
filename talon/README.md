# Voice mappings for talon-organteq

## Usage
First you will have to launch Organteq with JSON-RPC server enabled. On a Mac this can be done from the command line like this:
```
/Applications/Organteq\ 2/Organteq\ 2.app/Contents/MacOS/Organteq\ 2 --serve
```

### Stop control
The general syntax for controlling stops follows the pattern:
```
[action] [manual] <stop-identifier(s)>
```

Where:
- **action** (optional): The operation to perform. Can be `toggle` (default if omitted), `push`/`disengage`, `pull`/`engage`, or `solo`
- **manual** (optional): The target manual (`pedal`, `choir`, `great`, or `swell`). Can be omitted if you've previously set a manual context with `use [manual]`
- **stop-identifiers** (required): Specify which stops to control. Can be either:
  - Stop numbers (e.g., `one three five`)
  - A tonal family (e.g., `reeds`)
  - A footage combined with a tonal family (e.g., `8-foot reeds`)
  - The word `memory` to reference a bank of manual-specific remembered stops

#### Stop control by number
```
great one three twelve       # toggle stops 1, 3, and 12 on the Great
swell two                    # toggle stop 2 on the Swell
pedal four seven             # toggle Pedal stops 4 and 7
```

Engage stops (pull them out):
```
pull great one three twelve       # engage stops 1, 3, and 12 on the Great
```

Disengage stops (push them in):
```
push great one three twelve       # disengage stops 1, 3, and 12 on the Great
```

Solo stops (engage selected stops, clear all others on that manual):
```
solo great one three twelve       # solo stops 1, 3, and 12 on Great
```

Set a current manual to avoid repeating the manual name:
```
use great                    # set Great as current manual
one three twelve             # toggle stops 1, 3, 12 on Great
pull two four                # engage stops 2, 4 on Great
push five                    # disengage stop 5 on Great
solo one six                 # engage only stops 1 and 6 on Great
```

Clearing all stops:
```
cancel great                  # turn off all stops on the Great
cancel swell                  # turn off all stops on the Swell
cancel                        # turn off all stops on current manual
general cancel                # turn off all stops on all keyboards
```

#### Stop control by tonal family
You can control groups of stops by their tonal family (principals, flutes, strings, reeds, mutations, mixtures) and optionally filter by footage:

```
use great                    # set context to Great manual
toggle reeds                 # toggle all reed stops on Great
pull 4-foot reeds            # engage all 4-foot reed stops
pull 8-foot principals       # engage all 8-foot principal stops
push flutes                  # disengage all flute stops
solo 8-foot reeds            # engage only 8-foot reeds, clear all other stops on Great
```

You can also specify the manual explicitly:
```
pull great 4-foot reeds      # engage all 4-foot reeds on Great
solo swell principals        # engage only principals on Swell, clear all other stops
```

Available families: `principals`, `flutes`, `strings`, `reeds`, `mutations`, `mixtures`.

