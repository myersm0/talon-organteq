"""
midi_controller_demo.py - Map MIDI faders to rule levels

Requires: pip install mido python-rtmidi

Usage:
    python midi_faders.py

Listens for MIDI CC messages from an Arturia KeyLab mk3 keyboard. 
You would need to adapt this for your specific MIDI device.
"""

if __name__ == "__main__":
	import sys
	from pathlib import Path
	sys.path.insert(0, str(Path(__file__).parent.parent))

	import mido
	from client.bridge import Bridge

	fader_channel = 2

    # numbers 73, 75, 79, 72 are the MIDI CC message codes that correspond to 
    # the messages transmitted by the first four faders on my KeyLab, in order
	fader_rules = {
		73: "crescendo pedal",
		75: "crescendo choir",
		79: "crescendo great",
		72: "crescendo swell",
	}

	bridge = None

	def cc_to_level(cc_value: int, max_level: int) -> int:
		zone_size = 128 / (max_level + 1)
		return min(int(cc_value / zone_size), max_level)

	def handle_cc(cc_number: int, value: int):
		rule_id = fader_rules.get(cc_number)
		if not rule_id:
			return
		max_level = bridge.get_max_level(rule_id)
		level = cc_to_level(value, max_level)
		print(f"Fader CC{cc_number} = {value} → {rule_id} level {level}")
		bridge.apply_rule(rule_id, level=level)

	def list_midi_inputs():
		print("Available MIDI inputs:")
		for name in mido.get_input_names():
			print(f"  {name}")

	def main():
		global bridge

		list_midi_inputs()
		print()

		bridge = Bridge()
		bridge.sync()
		print("Bridge connected and synced")

		input_name = None
		for name in mido.get_input_names():
			if "KeyLab" in name or "MIDIIN" in name:
				input_name = name
				break

		if not input_name:
			print("Could not find KeyLab MIDI input. Available inputs listed above.")
			return

		print(f"Opening MIDI input: {input_name}")
		print(f"Fader mappings: {fader_rules}")
		print("Listening for MIDI messages... (Ctrl+C to quit)")

		with mido.open_input(input_name) as port:
			for msg in port:
				if msg.type == "control_change" and msg.channel == fader_channel:
					handle_cc(msg.control, msg.value)

	main()
