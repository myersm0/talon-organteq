# talon-organteq
A powerful registration control system for Modartt's [Organteq 2](https://www.modartt.com/organteq_overview), built around an extensible rule-based grammar. Commands can be invoked from a Python session, by mapping them to switches on your MIDI controller, or with [Talon](https://talonvoice.com/) voice commands for hands-free control.

Voice command documentation is available at [talon/README.md](talon/README.md).

Organteq is a trademark of Modartt. This project is not affiliated with or endorsed by Modartt.

## Motivation
Traditionally, organ registration involves manipulating stops at either a very fine or a very coarse granularity: either via individual stops, or via activation of pre-recorded combinations which must be planned in advance and which affect organ state globally (typically across all manuals).

The system prototyped in this repo introduces a different way. You can still manipulate stops individually. But you can also group them in arbitrary ways and manipulate each of those groups as units: mix them together, subtract one from another, layer them conditionally on top of each other, etc. Anything that can be expressed logically in terms of known properties of the state of your organ in Organteq, or characteristics of the stops themselves, you can express it with these tools.

This system is driven by _formal logic_, specifically by the reasoning tool called Prolog. While Prolog is not new, recent advances in voice recognition technology make it newly practical in application here.


## Prerequisites
Requires [SWI-Prolog](https://www.swi-prolog.org). On a Mac with Homebrew:
```
brew install swi-prolog
```

## Installation
```bash
git clone https://github.com/myersm0/talon-organteq
pip install requests
```
If you want to use the voice commands, you will need to download and install [Talon](https://talonvoice.com). Some advanced features require access to Talon's paid-tier beta, but most functionality here will work in the free version.

## Getting started
1. Start Organteq with JSON-RPC server. On a Mac:
```bash
/Applications/Organteq\ 2/Organteq\ 2.app/Contents/MacOS/Organteq\ 2 --serve
```

2. Start the Prolog server:
```bash
cd prolog
swipl -g "consult('main.pl'), server(5000)."
```

The server will automatically sync with Organteq and begin polling for preset changes every 5 seconds.

From here, you may control Organteq either from Python or via Talon voice. See [talon/README.md](talon/README.md) for the voice interface.

### Python usage
```python
from client import Bridge

bridge = Bridge()
bridge.sync()

# basic stop control
bridge.run("engage(great, numbers([1, 2, 3]))")
bridge.run("disengage(great, numbers([2]))")
bridge.run("toggle(swell, numbers([1, 4]))")
bridge.run("solo(great, numbers([1, 6]))")
bridge.run("clear(pedal)")

# control by tonal family
bridge.run("engage(great, family(reed))")
bridge.run("engage(great, family(principal, 8))")
bridge.run("disengage(swell, family(mixture))")

# undo/redo
bridge.run("undo")
bridge.run("redo")
```

### Interactive Prolog
You can also work directly in the Prolog REPL for testing:
```bash
cd prolog
swipl -g "consult('main.pl')."
```

Then:
```prolog
?- sync.
?- engage(great, numbers([1, 2, 3])).
?- engage(swell, family(reed)).
?- up('crescendo great').
?- undo.
```

## Documentation
- [API reference](docs/api-reference.md) - Commands and selectors
- [Advanced usage guide](docs/advanced-usage.md) - Rules, preset-specific behavior, and customization
- [Voice commands](talon/README.md) - Talon voice interface

## Tips for setting up voice recognition
If you're going to use this repo with Talon voice, which is highly recommended, you should be aware of some inherent difficulties in getting good results from any setup like this in which you're trying to issue _voice commands_ while playing a potentially loud musical instrument. If you're using headphones, then you may be OK. But if you aren't, then you have two problems to solve in your setup:
1. How to avoid false positives from the speech recognition engine (i.e. sounds from the organ being spuriously picked up as voice commands)
2. How to ensure your spoken commands are clearly audible over the sound from Organteq

To get around these things, you probably need a high-end directional headset mic that can effectively isolate your voice from environmental noise (and music), such as the DPA d:fine 4188 or 4288.

Your choice of speech recognition engine within Talon also matters. For example, the Conformer D2 engine (unfortunately available in Talon beta only) may do better at reducing false positives.

## License
MIT
