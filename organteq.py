import subprocess
import json
from talon import Module, actions

mod = Module()

endpoint = "http://127.0.0.1:8081/jsonrpc"
current_manual = 0

last_stops = {
	"1": [],  # pedal
	"2": [],  # choir
	"3": [],  # great
	"4": []   # swell
}

remembered_stops = {
	"1": [],  # pedal
	"2": [],  # choir
	"3": [],  # great
	"4": []   # swell
}

def organteq_call(payload):
	"""make a JSON-RPC call to Organteq"""
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

@mod.action_class
class Actions:
	def organteq_set_stop(manual: str, stop: int, value: float):
		"""set an Organteq stop to a specific value (0.0 or 1.0)"""
		parameter_id = f"Stop[{manual}][{stop}].Switch"
		set_payload = {
			"method": "setParameters",
			"params": [{"id": parameter_id, "normalized_value": value}],
			"jsonrpc": "2.0",
			"id": 1
		}
		organteq_call(set_payload)

	def organteq_set_stops(manual: str, stops: list[str], value: float):
		"""set a list of Organteq stops to a specific value (0.0 or 1.0)"""
		global last_stops
		last_stops[manual] = stops
		for stop in stops:
			actions.user.organteq_set_stop(manual, stop, value)

	def organteq_toggle_stop(manual: str, stop: int):
		"""toggle an Organteq stop on or off"""
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
			actions.user.organteq_set_stop(manual, stop, new_value)
		except (json.JSONDecodeError, KeyError) as e:
			print(f"Command failed with error: {e}")

	def organteq_push_stops(manual: str, stops: list[str]):
		"""turn an Organteq stop on (1.0)"""
		actions.user.organteq_set_stops(manual, stops, 0.0)

	def organteq_pull_stops(manual: str, stops: list[int]):
		"""turn an Organteq stop off (0.0)"""
		actions.user.organteq_set_stops(manual, stops, 1.0)

	def organteq_toggle_stops(manual: str, stops: list[str]):
		"""toggle multiple Organteq stops on or off"""
		global last_stops
		last_stops[manual] = stops
		for stop in stops:
			actions.user.organteq_toggle_stop(manual, stop)
	
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
		for stop in stops:
			actions.user.organteq_toggle_stop(manual, stop)
	
	def organteq_remember_stops(stops: list[str]):
		"""remember stops for current manual"""
		global remembered_stops
		manual = actions.user.organteq_get_manual()
		if manual == "0":
			print("No manual selected")
			return
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
		for stop in stops:
			actions.user.organteq_toggle_stop(manual, stop)

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


