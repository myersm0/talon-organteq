"""
bridge.py - Minimal bridge between Prolog core and Organteq RPC

This module:
1. Communicates with the Prolog server via HTTP
2. Executes RPC actions returned by Prolog against Organteq
3. Syncs Organteq state to Prolog on startup/preset change
"""

import json
import subprocess
import requests
from typing import Any

organteq_endpoint = "http://127.0.0.1:8081/jsonrpc"
prolog_endpoint = "http://localhost:5000"

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


class OrganteqRPC:
	def __init__(self, endpoint: str = organteq_endpoint):
		self.endpoint = endpoint

	def call(self, method: str, params: list = None) -> Any:
		payload = {
			"method": method,
			"params": params or [],
			"jsonrpc": "2.0",
			"id": 1
		}
		try:
			result = subprocess.run(
				f'curl -s -X POST {self.endpoint} -H "Content-Type: application/json" -d \'{json.dumps(payload)}\'',
				shell=True,
				check=True,
				capture_output=True,
				text=True
			)
			return json.loads(result.stdout)
		except (subprocess.CalledProcessError, json.JSONDecodeError) as e:
			print(f"Organteq RPC error: {e}")
			return None

	def get_preset(self) -> str:
		response = self.call("getInfo")
		if response and "result" in response:
			return response["result"][0]["current_preset"]["name"]
		return ""

	def get_stop_names(self) -> dict[str, list[str]]:
		response = self.call("getStopNames")
		if response and "result" in response:
			return {str(i + 1): names for i, names in enumerate(response["result"])}
		return {}

	def get_stop_value(self, manual: str, stop: int) -> float:
		param_id = f"Stop[{manual}][{stop}].Switch"
		response = self.call("getParameters", [{"id": param_id}])
		if response and "result" in response:
			return response["result"][0]["normalized_value"]
		return 0.0

	def set_stop(self, manual: str, stop: int, value: float):
		param_id = f"Stop[{manual}][{stop}].Switch"
		self.call("setParameters", [{"id": param_id, "normalized_value": value}])

	def set_coupler(self, index: int, value: float):
		param_id = f"Coupler Switch[{index}]"
		self.call("setParameters", [{"id": param_id, "normalized_value": value}])

	def set_mono_coupler(self, index: int, value: float):
		param_id = f"Mono Coupler Switch[{index}]"
		self.call("setParameters", [{"id": param_id, "normalized_value": value}])

	def set_tremulant(self, index: int, value: float):
		param_id = f"Tremulant Switch[{index}]"
		self.call("setParameters", [{"id": param_id, "normalized_value": value}])


class PrologClient:
	def __init__(self, endpoint: str = prolog_endpoint):
		self.endpoint = endpoint

	def execute(self, command: str, args: dict) -> dict:
		response = requests.post(
			f"{self.endpoint}/execute",
			json={"command": command, "args": args},
			timeout=10
		)
		return response.json()

	def get_state(self) -> dict:
		response = requests.post(f"{self.endpoint}/state", timeout=5)
		return response.json()

	def load_file(self, filepath: str):
		response = requests.post(
			f"{self.endpoint}/load",
			json={"file": filepath},
			timeout=10
		)
		return response.json()


class Bridge:
	def __init__(
		self,
		organteq_endpoint: str = organteq_endpoint,
		prolog_endpoint: str = prolog_endpoint
	):
		self.organteq = OrganteqRPC(organteq_endpoint)
		self.prolog = PrologClient(prolog_endpoint)

	def sync(self) -> dict:
		preset = self.organteq.get_preset()
		stop_names = self.organteq.get_stop_names()

		elements = []
		engaged = []

		for manual_num, names in stop_names.items():
			manual_name = manual_names[manual_num]
			max_stops = max_stops_per_manual[manual_num]
			for stop_num in range(1, max_stops + 1):
				name = names[stop_num - 1] if stop_num - 1 < len(names) else ""
				elements.append({
					"division": manual_name,
					"number": stop_num,
					"name": name,
					"type": "stop"
				})
				value = self.organteq.get_stop_value(manual_num, stop_num)
				if value >= 0.5:
					engaged.append({"division": manual_name, "number": stop_num})

		for i in range(1, 7):
			elements.append({
				"division": "coupler",
				"number": i,
				"name": f"Coupler {i}",
				"type": "coupler"
			})

		for i in range(1, 5):
			elements.append({
				"division": "mono_coupler",
				"number": i,
				"name": f"Mono Coupler {i}",
				"type": "mono_coupler"
			})

		for i in range(1, 5):
			elements.append({
				"division": "tremulant",
				"number": i,
				"name": f"Tremulant {i}",
				"type": "tremulant"
			})

		return self.prolog.execute("sync", {
			"preset": preset,
			"elements": elements,
			"engaged": engaged
		})

	def execute(self, command: str, args: dict = None) -> dict:
		args = args or {}
		result = self.prolog.execute(command, args)
		if result.get("status") == "ok":
			actions = result.get("actions", [])
			self._execute_rpc_actions(actions)
		return result

	def _execute_rpc_actions(self, actions: list):
		for action in actions:
			action_type = action.get("type")
			if action_type == "set_stop":
				division = action["division"]
				manual_num = manual_numbers.get(division)
				if manual_num:
					self.organteq.set_stop(manual_num, action["number"], action["value"])
			elif action_type == "set_coupler":
				self.organteq.set_coupler(action["number"], action["value"])
			elif action_type == "set_mono_coupler":
				self.organteq.set_mono_coupler(action["number"], action["value"])
			elif action_type == "set_tremulant":
				self.organteq.set_tremulant(action["number"], action["value"])

	def load_rules(self, filepath: str) -> dict:
		return self.prolog.load_file(filepath)

	def engage(self, division: str, selector: Any) -> dict:
		return self.execute("engage", {
			"division": division,
			"selector": self._normalize_selector(selector)
		})

	def disengage(self, division: str, selector: Any) -> dict:
		return self.execute("disengage", {
			"division": division,
			"selector": self._normalize_selector(selector)
		})

	def toggle(self, division: str, selector: Any) -> dict:
		return self.execute("toggle", {
			"division": division,
			"selector": self._normalize_selector(selector)
		})

	def solo(self, division: str, selector: Any) -> dict:
		return self.execute("solo", {
			"division": division,
			"selector": self._normalize_selector(selector)
		})

	def clear(self, division: str) -> dict:
		return self.execute("clear", {"division": division})

	def clear_all(self) -> dict:
		return self.execute("clear_all", {})

	def apply_rule(
		self,
		rule_id: str,
		action: str = None,
		delta: int = None,
		level: int = None
	) -> dict:
		args = {"rule": rule_id}
		if action:
			args["action"] = action
		if delta is not None:
			args["delta"] = delta
		if level is not None:
			args["level"] = level
		return self.execute("apply_rule", args)

	def list_rules(self, preset: str = None) -> list[str]:
		args = {}
		if preset:
			args["preset"] = preset
		result = self.prolog.execute("list_rules", args)
		if result.get("status") == "ok":
			state = result.get("state", {})
			return state.get("rules", [])
		return []

	def get_rule_info(self, rule_id: str) -> dict | None:
		result = self.execute("get_rule_info", {"rule": rule_id})
		if result.get("status") == "ok":
			return result.get("state", {})
		return None

	def get_max_level(self, rule_id: str) -> int:
		info = self.get_rule_info(rule_id)
		if info:
			return info.get("max_level", 1)
		return 1

	def undo(self) -> dict:
		return self.execute("undo", {})

	def redo(self) -> dict:
		return self.execute("redo", {})

	def get_state(self) -> dict:
		return self.prolog.get_state()

	def get_preset(self) -> str:
		return self.organteq.get_preset()

	def check_and_sync(self) -> bool:
		"""Check if preset changed since last sync. If so, sync and return True."""
		result = self.get_state()
		if result.get("status") == "ok":
			state = result.get("state", {})
			last_preset = state.get("preset", "")
			current_preset = self.get_preset()
			if current_preset and current_preset != last_preset:
				self.sync()
				return True
		return False

	def _normalize_selector(self, selector: Any) -> Any:
		if isinstance(selector, list):
			if all(isinstance(x, int) for x in selector):
				return {"by": "numbers", "values": selector}
			return selector
		if isinstance(selector, int):
			return {"by": "numbers", "values": [selector]}
		if isinstance(selector, str):
			return selector
		return selector
