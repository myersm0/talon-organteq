# talon-organteq
A powerful registration control system for Modartt's [Organteq 2](https://www.modartt.com/organteq_overview), built around an extensible and customizable rule-based grammar. Commands can be invoked either from a Python REPL session, by mapping them to switches on your MIDI controller, or with [Talon](https://talonvoice.com/) voice commands (which allows hands-free control).

The provided voice command-mappings for optional use with Talon are documented [here](https://github.com/myersm0/talon-organteq/talon/README.md).

Organteq is a trademark of Modartt. This project is not affiliated with or endorsed by Modartt.

***Status note***: This repo has been considerably refactored and reimagined since v0.1. The forthcoming v0.2, in progress, introduces an additional dependency on the Prolog logic programming language, but with the benefit of offering a much more capable reasoning engine than I had previously in pure Python. However, there are still some bugs that I'm working out with the Talon integration. Check back soon.

## Motivation
Traditionally, organ registration involves manipulating stops at either a very fine or a very coarse granularity: either via individual stops, or via activation of pre-recorded combinations which must be planned in advance and which affect organ state globally (typically across all manuals).

The system prototyped in this repo introduces a different way. You can still manipulate stops individually. But you can also group them in arbitrary ways and manipulate each of those groups as units: mix them together, subtract one from another, layer them conditionally on top of each other, etc, or even introduce nondeterministic elements. Anything that can be expressed logically in terms of known properties of the state of your organ in Organteq, you can express it with these tools.

This system is driven by _formal logic_, specifically by the reasoning tool called Prolog. While this is not a new concept, recent advances in voice recognition technology make it newly practical in application here.


## Prerequisites
Requires [SWI-Prolog](https://www.swi-prolog.org). On a Mac with Homebrew you can install it like this:
```
brew install swi-prolog
```

## Installation
```bash
git clone https://github.com/myersm0/talon-organteq```
pip install requests
```
If you want to use the voice commands, you will need to download and install [Talon](https://talonvoice.com). Some advanced features require access to Talon's paid-tier beta, but most functionality here will work in the free version.

## Getting started
1. Start Organteq with JSON-RPC server. On a Mac that will look like this:
```bash
/Applications/Organteq\ 2/Organteq\ 2.app/Contents/MacOS/Organteq\ 2 --serve
```

2. Start a Prolog server by navigating into the directory where you cloned `talon-organteq` and then:
```bash
swipl -g "consult('./prolog/server.pl'), consult('./prolog/classification.pl'), server(5000)."
```

From here, you may now control Organteq either by Talon voice or from an interactive Python REPL session. The remainder of this document will describe usage in the Python REPL; see [here](https://github.com/myersm0/talon-organteq/talon/README.md) for documentation of the voice interface.

From Python:
```python
from core import RegistrationEngine

engine = RegistrationEngine()
engine.sync_state()

# Basic stop control
engine.engage("great", ["1", "2", "3"])
engine.engage_family("great", "reed")
engine.solo_family("great", "principal")

# Undo/redo
engine.undo()
engine.redo()
```

## API reference
The argument `manual` below should take on one of the following values: `pedal`, `choir`, `great`, or `swell`.

### Basic actions by manual and stop number

```python
engine.engage(manual, stops)
engine.disengage(manual, stops)
engine.toggle(manual, stops)
engine.solo(manual, stops)
engine.clear(manual)
```

### Basic actions by manual and tonal family

```python
engine.engage_family(manual, family, footage=None, limit=None)
engine.disengage_family(manual, family, footage=None, limit=None)
engine.toggle_family(manual, family, footage=None, limit=None)
engine.solo_family(manual, family, footage=None)
engine.engage_family_all(family, footage=None, limit=None)
engine.disengage_family_all(family, footage=None)
```

### Selector operations

```python
engine.apply_selector(manual, selector, action="engage", manuals=[manuals])
```

### Rules
#### Transient rules
```python
engine.apply_rule(rule_id, delta=1, manuals=None)
```

#### Persistent rules (combinations)
```python
engine.apply_combination(rule_id, delta=None, level=None, manuals=None)
engine.reassert_combination(rule_id, manuals=None)
engine.solo_combination(rule_id, manuals=None)
engine.minimize_combination(rule_id, manuals=None)
engine.maximize_combination(rule_id, manuals=None)
engine.mute_combination(rule_id, manuals=None)
```

### History
```python
engine.undo()
engine.redo()
```


## License
MIT



