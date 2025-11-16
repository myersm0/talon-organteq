# talon-organteq
[Talon](https://talonvoice.com/) voice commands for hands-free control of pipe organ registration (stops, couplers, etc) in Modartt's [Organteq 2](https://www.modartt.com/organteq_overview) software instrument, by communicating with its built-in JSON-RPC server at `http://127.0.0.1:8081/jsonrpc`.

This repo doesn't aspire to provide full control of Organteq. Rather the focus is on just a small subset of commands that are particularly useful to be able to modify hands-free during play.

## Features
- Toggle individual stops or multiple stops at once
- Clear all stops on a manual
- Set a _current manual_ context for subsequent commands
- Memorize stop configurations per manual
- Recall the last-used stops


## Getting started
### The cocktail party problem
First you should be aware of some inherent difficulties in getting good results from any setup like this in which you're trying to issue _voice commands_ while playing a potentially loud musical instrument. If you're using headphones, then you may be OK. But if you aren't, then you have two problems to solve in your setup:
1. How to avoid false positives from the speech recognition engine (i.e. sounds from the organ being spuriously picked up as voice commands)
2. How to ensure your spoken commands are clearly audible to the speech recognition, over the sound from Organteq

To get around these things, your microphone and its placement will matter a lot. I use a Shure SM7db dynamic mic with cardiod pickup pattern, directed _away_ from my speakers and placed very close to my mouth while I'm playing (within about 6 inches ideally). Alternatively, a good directional headset mic may be even better at isolating your voice from environmental noise (and music), such as the DPA d:fine 4188 or 4288, but I have not tried that myself.

Your choice of speech recognition model within Talon also matters. For example, the Conformer D2 engine (unfortunately available in Talon beta only) may do better at reducing false positives.

You may additionally have a third problem of language and dialect, if you're not a native English speaker. I may be able to later report on alternative speech engines, but for now I am not sufficiently knowledgeable on the subject.

### Prerequisites
- Install [Talon Voice](https://talonvoice.com/)
- Install [talon-community](https://github.com/talonhub/community)
- Install Modartt's [Organteq 2](https://www.modartt.com/organteq_overview), and launch it with JSON-RPC server enabled
- You must have `curl` available in your system PATH

### Installation
Once the prerequisites above are met, you can simply do this on Mac or Linux:
```
git clone https://github.com/myersm0/talon-organteq ~/.talon/user/
```

## Usage
First you will have to launch Organteq with JSON-RPC server enables. On a Mac this can be done from the command line like this:
```
  - on Mac, this can be launched from the terminal via `/Applications/Organteq\ 2/Organteq\ 2.app/Contents/MacOS/Organteq\ 2 --serve`
```

Then, the following are _voice commands_ that you issue (speak) while the Organteq app is focused.

### Basic stop control
```
great one three twelve      # toggle stops 1, 3, and 12 on the Great
swell two                   # toggle stop 2 on the Swell
pedal four seven            # toggle Pedal stops 4 and 7
```

### Manual context
Set a current manual to avoid repeating the manual name:
```
use great                   # set Great as current manual
one three twelve            # toggle stops 1, 3, 12 on current manual (Great)
two four                    # toggle stops 2, 4 (still on Great)
```

### Memory features
Each manual maintains separate memory for "last stops" and "remembered stops".
```
use great
remember one seven nine     # remember stops 1, 7, 9 for current manual
toggle                      # toggle the remembered stops
toggle last                 # toggle whatever stops were just used
```

### Clearing stops
```
clear great                 # turn off all stops on the Great
clear swell                 # turn off all stops on the Swell
clear choir                 # turn off all stops on the Choir
```






