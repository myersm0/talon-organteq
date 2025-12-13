"""
Demo: Map MIDI faders to rule levels

Requires: pip install mido python-rtmidi

Usage:
    python midi_controller_demo.py

Listens for MIDI CC messages and maps fader positions to rule levels.
Adapt the fader_rules mapping for your specific MIDI controller.
"""

if __name__ == "__main__":
	import sys
	import time
	import threading
	from pathlib import Path
	sys.path.insert(0, str(Path(__file__).parent.parent))

	import mido
	from client.bridge import Bridge

	fader_channel = 2
	preset_check_interval = 5.0

	# Map MIDI CC numbers to rule IDs
	# These CC numbers are for Arturia KeyLab mk3 faders; adapt for your controller
	fader_rules = {
		73: "crescendo pedal",
		75: "crescendo choir",
		79: "crescendo great",
		72: "crescendo swell",
	}

	bridge = None
	max_levels = {}

	def get_max_level(rule_id):
		if rule_id in max_levels:
			return max_levels[rule_id]
		result = bridge.run(f"get_rule_info('{rule_id}')")
		if result.get("status") == "ok":
			max_level = result.get("state", {}).get("max_level", 1)
			max_levels[rule_id] = max_level
			return max_level
		return 1

	def cc_to_level(cc_value, max_level):
		zone_size = 128 / (max_level + 1)
		return min(int(cc_value / zone_size), max_level)

	def handle_cc(cc_number, value):
		rule_id = fader_rules.get(cc_number)
		if not rule_id:
			return
		max_level = get_max_level(rule_id)
		level = cc_to_level(value, max_level)
		print(f"Fader CC{cc_number} = {value} → '{rule_id}' level {level}")
		bridge.run(f"level('{rule_id}', {level})")

	def preset_check_loop():
		last_preset = None
		while True:
			time.sleep(preset_check_interval)
			try:
				result = bridge.state()
				if result.get("status") == "ok":
					current = result.get("state", {}).get("preset", "")
					if last_preset and current != last_preset:
						print(f"Preset changed to: {current}")
						bridge.sync()
						max_levels.clear()
					last_preset = current
			except Exception as e:
				print(f"Preset check error: {e}")

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

		result = bridge.state()
		preset = result.get("state", {}).get("preset", "unknown")
		print(f"Connected. Preset: {preset}")

		preset_thread = threading.Thread(target=preset_check_loop, daemon=True)
		preset_thread.start()
		print(f"Preset change detection active (every {preset_check_interval}s)")

		input_name = None
		for name in mido.get_input_names():
			if "KeyLab" in name or "MIDIIN" in name:
				input_name = name
				break

		if not input_name:
			print("No compatible MIDI input found. See available inputs above.")
			return

		print(f"Opening MIDI input: {input_name}")
		print(f"Fader mappings: {fader_rules}")
		print("Listening... (Ctrl+C to quit)")

		with mido.open_input(input_name) as port:
			for msg in port:
				if msg.type == "control_change" and msg.channel == fader_channel:
					handle_cc(msg.control, msg.value)

	main()
