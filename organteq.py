import subprocess
import json
from talon import Module, actions, registry, resource
import re

mod = Module()

endpoint = "http://127.0.0.1:8081/jsonrpc"
current_manual = 0

last_stops = {
	"1": [],
	"2": [],
	"3": [],
	"4": []
}

remembered_stops = {
	"1": [],
	"2": [],
	"3": [],
	"4": []
}

def organteq_call(payload):
	try:
		result = subprocess.run(
			f'curl -X POST {endpoint} -H "Content-Type: application/json" -d \'{json.dumps(payload)}\'',
			shell=True,
			check=True,
			capture_output=True,
			text=True
		)
		return result.stdout
	except subprocess.CalledProcessError as e:
		print(f"Command failed with error: {e}")
		return None

@resource.watch("family_mappings.json")
def load_family_mappings(path):
	global family_mappings
	family_mappings = json.load(path)

def get_stops_by_number(manual: str, stop_numbers: list[str]) -> list[str]:
	return stop_numbers

def get_stops_by_family(manual: str, family: str, footage: str = None) -> list[str]:
	get_stops_payload = {
		"method": "getStopNames",
		"params": [],
		"jsonrpc": "2.0",
		"id": 1
	}
	response_text = organteq_call(get_stops_payload)
	if not response_text:
		return []
	try:
		response = json.loads(response_text)
		manual_stops = response["result"][int(manual) - 1]
		matching_stop_numbers = []
		for index, stop_name in enumerate(manual_stops, start=1):
			if not stop_name:
				continue
			base_name_match = re.match(r"^(.+?)\s*\d+", stop_name)
			base_name = base_name_match.group(1).strip() if base_name_match else stop_name
			if base_name not in family_mappings:
				print(f"Warning: stop '{base_name}' is unknown; no mapping to a tonal family defined!")
				continue
			if family_mappings[base_name] == family:
				if footage:
					footage_match = re.search(rf"{footage}", stop_name)
					if footage_match:
						matching_stop_numbers.append(str(index))
				else:
					matching_stop_numbers.append(str(index))
		return matching_stop_numbers
	except (json.JSONDecodeError, KeyError, IndexError) as e:
		print(f"Failed to get stops: {e}")
		return []

def set_stop(manual: str, stop: str, value: float):
	parameter_id = f"Stop[{manual}][{stop}].Switch"
	set_payload = {
		"method": "setParameters",
		"params": [{"id": parameter_id, "normalized_value": value}],
		"jsonrpc": "2.0",
		"id": 1
	}
	organteq_call(set_payload)

def toggle_stop(manual: str, stop: str):
	parameter_id = f"Stop[{manual}][{stop}].Switch"
	get_payload = {
		"method": "getParameters",
		"params": [{"id": parameter_id}],
		"jsonrpc": "2.0",
		"id": 1
	}
	response_text = organteq_call(get_payload)
	if not response_text:
		return
	try:
		response = json.loads(response_text)
		current_value = response["result"][0]["normalized_value"]
		new_value = 0.0 if current_value == 1.0 else 1.0
		set_stop(manual, stop, new_value)
	except (json.JSONDecodeError, KeyError) as e:
		print(f"Command failed with error: {e}")

def push_stop(manual: str, stop: str):
	set_stop(manual, stop, 0.0)

def pull_stop(manual: str, stop: str):
	set_stop(manual, stop, 1.0)

def toggle_stops(manual: str, stop_numbers: list[str]):
	global last_stops
	last_stops[manual] = stop_numbers
	for stop in stop_numbers:
		toggle_stop(manual, stop)

def push_stops(manual: str, stop_numbers: list[str]):
	global last_stops
	last_stops[manual] = stop_numbers
	for stop in stop_numbers:
		push_stop(manual, stop)

def pull_stops(manual: str, stop_numbers: list[str]):
	global last_stops
	last_stops[manual] = stop_numbers
	for stop in stop_numbers:
		pull_stop(manual, stop)

def solo_stops(manual: str, stop_numbers: list[str]):
	global last_stops
	last_stops[manual] = stop_numbers
	max_stops = 20 if manual == "3" else 10
	for stop in range(1, max_stops + 1):
		stop_str = str(stop)
		if stop_str in stop_numbers:
			pull_stop(manual, stop_str)
		else:
			push_stop(manual, stop_str)

def get_remembered_stops(manual: str) -> list[str]:
	return remembered_stops.get(manual, [])


@mod.action_class
class Actions:
	def organteq_get_current_preset() -> str:
		"""get the name of the current preset"""
		payload = {
			"method": "getInfo",
			"params": [],
			"jsonrpc": "2.0",
			"id": 1
		}
		response_text = organteq_call(payload)
		if not response_text:
			return ""
		try:
			response = json.loads(response_text)
			return response["result"][0]["current_preset"]["name"]
		except (json.JSONDecodeError, KeyError, IndexError) as e:
			print(f"Failed to get current preset: {e}")
			return ""

	def organteq_toggle_stops_by_number(manual: str, stop_numbers: list[str]):
		"""toggle stops by number"""
		stops = get_stops_by_number(manual, stop_numbers)
		if stops:
			toggle_stops(manual, stops)

	def organteq_push_stops_by_number(manual: str, stop_numbers: list[str]):
		"""push stops by number"""
		stops = get_stops_by_number(manual, stop_numbers)
		if stops:
			push_stops(manual, stops)

	def organteq_pull_stops_by_number(manual: str, stop_numbers: list[str]):
		"""pull stops by number"""
		stops = get_stops_by_number(manual, stop_numbers)
		if stops:
			pull_stops(manual, stops)

	def organteq_toggle_stops_by_family(manual: str, family: str, footage: str = None):
		"""toggle stops by tonal family and optional footage"""
		stops = get_stops_by_family(manual, family, footage)
		if stops:
			toggle_stops(manual, stops)
		else:
			print(f"No {family} stops found on manual {manual}" + (f" at {footage}'" if footage else ""))

	def organteq_push_stops_by_family(manual: str, family: str, footage: str = None):
		"""push stops by tonal family and optional footage"""
		stops = get_stops_by_family(manual, family, footage)
		if stops:
			push_stops(manual, stops)
		else:
			print(f"No {family} stops found on manual {manual}" + (f" at {footage}'" if footage else ""))

	def organteq_pull_stops_by_family(manual: str, family: str, footage: str = None):
		"""pull stops by tonal family and optional footage"""
		stops = get_stops_by_family(manual, family, footage)
		if stops:
			pull_stops(manual, stops)
		else:
			print(f"No {family} stops found on manual {manual}" + (f" at {footage}'" if footage else ""))

	def organteq_solo_stops_by_number(manual: str, stop_numbers: list[str]):
		"""solo stops by number (engage these, clear all others)"""
		stops = get_stops_by_number(manual, stop_numbers)
		if stops:
			solo_stops(manual, stops)

	def organteq_solo_stops_by_family(manual: str, family: str, footage: str = None):
		"""solo stops by tonal family (engage these, clear all others)"""
		stops = get_stops_by_family(manual, family, footage)
		if stops:
			solo_stops(manual, stops)
		else:
			print(f"No {family} stops found on manual {manual}" + (f" at {footage}'" if footage else ""))

	def organteq_toggle_last():
		"""toggle the last-referenced stops for current manual"""
		manual = actions.user.organteq_get_manual()
		if manual == "0":
			print("No manual selected")
			return
		stops = last_stops.get(manual, [])
		if not stops:
			print(f"No last stops for manual {manual}")
			return
		toggle_stops(manual, stops)

	def organteq_remember_stops(manual: str, stops: list[str]):
		"""remember stops for a manual"""
		global remembered_stops
		remembered_stops[manual] = stops

	def organteq_toggle_remembered():
		"""toggle the remembered stops for current manual"""
		manual = actions.user.organteq_get_manual()
		if manual == "0":
			print("No manual selected")
			return
		stops = remembered_stops.get(manual, [])
		if not stops:
			print(f"No remembered stops for manual {manual}")
			return
		toggle_stops(manual, stops)

	def organteq_get_remembered_stops(manual: str) -> list[str]:
		"""get remembered stops for a manual"""
		return get_remembered_stops(manual)

	def organteq_clear_manual(manual: str):
		"""set all stops on a manual to 0.0"""
		max_stops = 20 if manual == "3" else 10
		for stop in range(1, max_stops + 1):
			parameter_id = f"Stop[{manual}][{stop}].Switch"
			set_payload = {
				"method": "setParameters",
				"params": [{"id": parameter_id, "normalized_value": 0.0}],
				"jsonrpc": "2.0",
				"id": 1
			}
			organteq_call(set_payload)

	def organteq_set_manual(manual: str):
		"""set the current manual for subsequent commands"""
		global current_manual
		current_manual = manual

	def organteq_get_manual() -> str:
		"""get the current manual"""
		return str(current_manual)

	def organteq_midi_send(byte1: int, byte2: int, byte3: int):
		"""send raw MIDI bytes to Organteq"""
		payload = {
			"method": "midiSend",
			"params": [[byte1, byte2, byte3]],
			"jsonrpc": "2.0",
			"id": 1
		}
		organteq_call(payload)


