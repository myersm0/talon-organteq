# Examples
Example files for talon-organteq.

## rules.pl
Example Prolog rules demonstrating:
- Transient rules (brighten/darken)
- Persistent rules with multiple levels
- Preset-specific selectors with `for_preset()`
- Auxiliary control (couplers, tremulants)
- Predicate-based rules

Copy to your rules directory and modify:
```bash
mkdir -p ~/.config/talon-organteq/rules
cp rules_example.pl ~/.config/talon-organteq/rules/my_rules.pl
```

Load at server startup:
```bash
cd prolog
swipl -g "consult('main.pl'), load_rules_from_dir('~/.config/talon-organteq/rules', '*.pl'), server(5000)."
```

## midi_faders.py.example
Maps MIDI faders and buttons to rule control. You'll have to adapt this for your particular controller.

Setup:
```bash
pip install mido python-rtmidi
python midi_faders.py
```


