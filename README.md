# talon-organteq
[Talon](https://talonvoice.com/) voice commands for hands-free control of pipe organ registration (stops, couplers, etc) in Modartt's [Organteq 2](https://www.modartt.com/organteq_overview) software instrument, by communicating with its built-in JSON-RPC server at `http://127.0.0.1:8081/jsonrpc`.

This repo doesn't aspire to provide full control of Organteq. Rather the focus is on just a small subset of commands that are particularly useful to be able to modify hands-free during play.

## Features
- Toggle individual stops or multiple stops at once
- Clear all stops on a manual
- Set a _current manual_ context for subsequent commands
- Memorize stop configurations per manual
- Recall the last-used stops

## Manual numbering
Organteq's RPC uses 1-indexed manual numbers:
- 1: Pedal
- 2: Choir
- 3: Great
- 4: Swell

All manuals support up to 10 stops, except for the Great which can go up to 20.

## Commands
The following are _voice commands_ that you issue (speak) while the Organteq app is focused.

### Basic Stop Control
```
great one three twelve      # toggle stops 1, 3, and 12 on the Great
swell two                   # toggle stop 2 on the Swell
pedal four seven            # toggle Pedal stops 4 and 7
```

### Manual Context
Set a current manual to avoid repeating the manual name:
```
use great                   # set Great as current manual
toggle one three twelve     # toggle stops 1, 3, 12 on current manual (Great)
toggle two four             # toggle stops 2, 4 on (still on Great)
```

### Memory Features
Each manual maintains separate memory for "last stops" and "remembered stops".
```
use great
remember one seven nine     # remember stops 1, 7, 9 for current manual
toggle                      # toggle the remembered stops
toggle last                 # toggle whatever stops were just used
```

### Clearing Stops
```
clear great                 # Turn off all stops on the Great
clear swell                 # Turn off all stops on the Swell
clear choir                 # Turn off all stops on the Choir
```

## Requirements
- [Talon Voice](https://talonvoice.com/)
- [talon-community](https://github.com/talonhub/community)
- Modartt's [Organteq 2](https://www.modartt.com/organteq_overview) with JSON-RPC server enabled
  - on Mac, this can be launched from the terminal via `/Applications/Organteq\ 2/Organteq\ 2.app/Contents/MacOS/Organteq\ 2 --serve`
- `curl` available in system PATH





