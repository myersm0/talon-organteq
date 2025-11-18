# talon-organteq
[Talon](https://talonvoice.com/) voice commands for hands-free control of pipe organ registration (stops, couplers, etc) in Modartt's [Organteq 2](https://www.modartt.com/organteq_overview) software instrument, by communicating with its built-in JSON-RPC server at `http://127.0.0.1:8081/jsonrpc`.

This repo doesn't aspire to provide full control of Organteq. Rather the focus is on just a small subset of commands that are particularly useful to be able to modify hands-free during play.

## Features
- Toggle, engage, or disengage stops by number or by tonal family
- Filter stops by footage (e.g., "8 foot reeds")
- Clear all stops on a manual
- Set and remember a _current manual_ context for subsequent commands
- Memorize stop configurations per manual
- Recall the last-used stops
- Send custom MIDI messages

## Getting started
### The cocktail party problem
First you should be aware of some inherent difficulties in getting good results from any setup like this in which you're trying to issue _voice commands_ while playing a potentially loud musical instrument. If you're using headphones, then you may be OK. But if you aren't, then you have two problems to solve in your setup:
1. How to avoid false positives from the speech recognition engine (i.e. sounds from the organ being spuriously picked up as voice commands)
2. How to ensure your spoken commands are clearly audible over the sound from Organteq

To get around these things, your microphone and its placement will matter a lot. I get reasonably good results with my Shure SM7db dynamic mic with cardioid pickup pattern, directed _away_ from my speakers and placed very close to my mouth while I'm playing, within about 6 inches ideally. Alternatively, a good directional headset mic would presumably be much better at isolating your voice from environmental noise (and music), such as the DPA d:fine 4188 or 4288, but I have not tried that myself.

Your choice of speech recognition engine within Talon also matters. For example, the Conformer D2 engine (unfortunately available in Talon beta only) may do better at reducing false positives.

You may additionally have a third problem of language and accent, if you're not a native English speaker. I may be able to later report on alternative speech engines, but for now I am not sufficiently knowledgeable on the subject. I'm currently researching multi-lingual input so that I may eventually be able to allow reference to stops by name (which, in Organteq, are usually in French or German). But for now you can only reference them by number.

### Prerequisites
- Install [Talon Voice](https://talonvoice.com/) and set up a speech engine
- Install Modartt's [Organteq 2](https://www.modartt.com/organteq_overview)
- You must have `curl` available in your system PATH

### Installation
Once the prerequisites above are met, you can simply do this on Mac or Linux:
```
git clone https://github.com/myersm0/talon-organteq ~/.talon/user/
```

## Usage
First you will have to launch Organteq with JSON-RPC server enabled. On a Mac this can be done from the command line like this:
```
/Applications/Organteq\ 2/Organteq\ 2.app/Contents/MacOS/Organteq\ 2 --serve
```

Then, the following are _voice commands_ that you issue (speak) while the Organteq app is focused:

### Stop control by number

#### Basic operations
Toggle stops (default action if no operation is specified):
```
great one three twelve       # toggle stops 1, 3, and 12 on the Great
swell two                    # toggle stop 2 on the Swell
pedal four seven             # toggle Pedal stops 4 and 7
```

Engage stops (pull them out):
```
pull great one three twelve       # engage stops 1, 3, and 12 on the Great
engage great one three twelve     # same thing, different word
```

Disengage stops (push them in):
```
push great one three twelve       # disengage stops 1, 3, and 12 on the Great
disengage great one three twelve  # same thing, different word
```

#### Using manual context
Set a current manual to avoid repeating the manual name:
```
use great                    # set Great as current manual
one three twelve             # toggle stops 1, 3, 12 on Great
pull two four                # engage stops 2, 4 on Great
push five                    # disengage stop 5 on Great
```

#### Clearing all stops
```
cancel great                  # turn off all stops on the Great
cancel swell                  # turn off all stops on the Swell
cancel choir                  # turn off all stops on the Choir
cancel pedal                  # turn off all stops on the Pedal
general cancel                # turn off all stops on all keyboards
```

### Stop control by tonal family
You can control groups of stops by their tonal family (principals, flutes, strings, reeds, mutations, mixtures) and optionally filter by footage:

```
use great                    # set context to Great manual
toggle reeds                 # toggle all reed stops on Great
pull 4-foot reeds            # engage all 4-foot reed stops
pull 8-foot principals       # engage all 8-foot principal stops
push flutes                  # disengage all flute stops
```

Available families: `principals`, `flutes`, `strings`, `reeds`, `mutations`, `mixtures`.

### Memory features
Each manual maintains separate memory for "last stops" and "remembered stops":
```
use great
remember one seven nine      # remember stops 1, 7, 9 for Great
toggle                       # toggle the remembered stops
push                         # engage the remembered stops
toggle last                  # toggle whatever stops were most recently used
```

### Sending custom MIDI
This repo also provides a function `organteq_midi_send` that allows you to send arbitrary MIDI messages by voice to Organteq. Although no voice mappings are defined here, you can make any number of mappings yourself by putting something like the following in a file ending in `.talon` somewhere within your `~/.talon/user/` directory:

```
app: /organteq/i
-
middle see on:
    user.organteq_midi_send(144, 64, 64)
middle see off:
    user.organteq_midi_send(128, 64, 64)
```

Then, by speaking "middle C on" you send a note-on message to Organteq that will trigger the note C4. Of course, this is an inconvenient way to play notes, but you can extend this concept to send program changes and control messages to Organteq, as well. (In the Organteq GUI, see MIDI Mappings > Action mapping.)


