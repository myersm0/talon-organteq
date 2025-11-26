import subprocess
import json

endpoint = "http://127.0.0.1:8081/jsonrpc"

manual_names = {
	"1": "pedal",
	"2": "choir",
	"3": "great",
	"4": "swell"
}

manual_numbers = {v: k for k, v in manual_names.items()}

max_stops_per_manual = {
	"1": 10,
	"2": 10,
	"3": 20,
	"4": 10
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

def get_stop_names():
	payload = {
		"method": "getStopNames",
		"params": [],
		"jsonrpc": "2.0",
		"id": 1
	}
	response_text = organteq_call(payload)
	if not response_text:
		return {}
	try:
		response = json.loads(response_text)
		result = {}
		for manual_num, names in enumerate(response["result"], start=1):
			result[str(manual_num)] = names
		return result
	except (json.JSONDecodeError, KeyError, IndexError) as e:
		print(f"Failed to get stop names: {e}")
		return {}

def get_stops_info():
	stop_names = get_stop_names()
	if not stop_names:
		return {}
	result = {}
	for manual_num, names in stop_names.items():
		max_stops = max_stops_per_manual[manual_num]
		stops_info = []
		for stop_num in range(1, max_stops + 1):
			stop_name = names[stop_num - 1] if stop_num - 1 < len(names) else ""
			parameter_id = f"Stop[{manual_num}][{stop_num}].Switch"
			get_payload = {
				"method": "getParameters",
				"params": [{"id": parameter_id}],
				"jsonrpc": "2.0",
				"id": 1
			}
			response_text = organteq_call(get_payload)
			state = 0.0
			if response_text:
				try:
					response = json.loads(response_text)
					state = response["result"][0]["normalized_value"]
				except (json.JSONDecodeError, KeyError, IndexError):
					pass
			stops_info.append((str(stop_num), stop_name, state))
		result[manual_num] = stops_info
	return result

def set_stop(manual: str, stop: str, value: float):
	parameter_id = f"Stop[{manual}][{stop}].Switch"
	set_payload = {
		"method": "setParameters",
		"params": [{"id": parameter_id, "normalized_value": value}],
		"jsonrpc": "2.0",
		"id": 1
	}
	organteq_call(set_payload)

def get_current_stop_value(manual: str, stop: str) -> float:
	parameter_id = f"Stop[{manual}][{stop}].Switch"
	get_payload = {
		"method": "getParameters",
		"params": [{"id": parameter_id}],
		"jsonrpc": "2.0",
		"id": 1
	}
	response_text = organteq_call(get_payload)
	if response_text:
		try:
			response = json.loads(response_text)
			return response["result"][0]["normalized_value"]
		except (json.JSONDecodeError, KeyError):
			pass
	return 0.0

def get_current_preset():
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
