import requests
from typing import Any

class PrologClient:
	def __init__(self, host: str = "localhost", port: int = 5000):
		self.base_url = f"http://{host}:{port}"

	def query(self, query_string: str) -> list[dict]:
		response = requests.post(
			f"{self.base_url}/query",
			json={"query": query_string},
			timeout=5
		)
		result = response.json()
		if result["status"] == "error":
			raise RuntimeError(f"Prolog error: {result['message']}")
		return result["results"]

	def assert_facts(self, facts: list[str]):
		response = requests.post(
			f"{self.base_url}/assert",
			json={"facts": facts},
			timeout=5
		)
		result = response.json()
		if result["status"] != "ok":
			raise RuntimeError(f"Assert failed: {result}")

	def retract_facts(self, facts: list[str]):
		response = requests.post(
			f"{self.base_url}/retract",
			json={"facts": facts},
			timeout=5
		)
		result = response.json()
		if result["status"] != "ok":
			raise RuntimeError(f"Retract failed: {result}")

	def reset(self):
		response = requests.post(f"{self.base_url}/reset", timeout=5)
		result = response.json()
		if result["status"] != "ok":
			raise RuntimeError(f"Reset failed: {result}")

	def load_file(self, filepath: str):
		response = requests.post(
			f"{self.base_url}/load",
			json={"file": filepath},
			timeout=10
		)
		result = response.json()
		if result["status"] != "ok":
			raise RuntimeError(f"Load failed: {result}")

	def load_stops(self, stops_by_manual: dict[str, list[tuple[str, str]]]):
		self.retract_facts(["stop(_, _, _, _)"])
		facts = []
		for manual, stops in stops_by_manual.items():
			for number, name in stops:
				escaped_name = name.replace("'", "\\'")
				facts.append(f"stop({manual}, {number}, '{escaped_name}', stop)")
		if facts:
			self.assert_facts(facts)

	def load_couplers(self, couplers: list[dict]):
		for coupler in couplers:
			manual = coupler["manual"]
			number = coupler["number"]
			name = coupler["name"].replace("'", "\\'")
			self.assert_facts([f"stop({manual}, {number}, '{name}', coupler)"])
			if "from_manual" in coupler:
				self.assert_facts([
					f"coupler_spec({manual}, {number}, {coupler['from_manual']}, "
					f"{coupler['to_manual']}, {coupler.get('interval', 0)})"
				])

	def set_engaged(self, manual: str, number: int, engaged: bool):
		fact = f"engaged({manual}, {number})"
		if engaged:
			self.assert_facts([fact])
		else:
			self.retract_facts([fact])

	def sync_engaged_state(self, state: dict[str, list[str]]):
		self.retract_facts(["engaged(_, _)"])
		facts = []
		for manual, stops in state.items():
			for stop in stops:
				facts.append(f"engaged({manual}, {stop})")
		if facts:
			self.assert_facts(facts)

	def set_combination_level(self, combination_id: str, level: int):
		self.retract_facts([f"combination_level({combination_id}, _)"])
		if level > 0:
			self.assert_facts([f"combination_level({combination_id}, {level})"])

	def claim_stops(self, combination_id: str, manual: str, stops: list[int]):
		facts = [f"owns({combination_id}, {manual}, {s})" for s in stops]
		if facts:
			self.assert_facts(facts)

	def release_stops(self, combination_id: str, manual: str, stops: list[int]):
		facts = [f"owns({combination_id}, {manual}, {s})" for s in stops]
		if facts:
			self.retract_facts(facts)

	def get_stops_by_family(self, manual: str, family: str, footage: str = None) -> list[int]:
		if footage:
			query = f"stops_by_family({manual}, {family}, {footage}, Stops)"
		else:
			query = f"stops_by_family({manual}, {family}, Stops)"
		results = self.query(query)
		if results:
			return results[0]["args"][-1]
		return []

	def resolve_selector(self, manual: str, selector: dict) -> list[int]:
		selector_term = self._dict_to_selector_term(selector)
		query = f"resolve_selector({manual}, {selector_term}, Stops)"
		results = self.query(query)
		if results:
			return results[0]["args"][-1]
		return []

	def get_rule_stops(self, rule_id: str, level: int, manual: str) -> list[int]:
		results = self.query(f"rule_stops_at_level({rule_id}, {level}, {manual}, Stops)")
		if results:
			return results[0]["args"][-1]
		return []

	def get_combination_stops(self, combination_id: str, level: int, manual: str) -> list[int]:
		if level <= 0:
			return []
		results = self.query(f"rule_stops_cumulative({combination_id}, {level}, {manual}, Stops)")
		if results:
			return results[0]["args"][-1]
		return []

	def get_combination_delta_actions(
		self, combination_id: str, old_level: int, new_level: int, manuals: list[str]
	) -> list[dict]:
		manuals_term = "[" + ", ".join(manuals) + "]"
		query = f"apply_rule_delta({combination_id}, {old_level}, {new_level}, {manuals_term}, Actions)"
		results = self.query(query)
		if results:
			raw_actions = results[0]["args"][-1]
			return self._parse_actions(raw_actions)
		return []

	def get_reassert_actions(self, combination_id: str, manuals: list[str]) -> list[dict]:
		manuals_term = "[" + ", ".join(manuals) + "]"
		query = f"reassert_rule({combination_id}, {manuals_term}, Actions)"
		results = self.query(query)
		if results:
			raw_actions = results[0]["args"][-1]
			return self._parse_actions(raw_actions)
		return []

	def get_violations(self) -> list[dict]:
		results = self.query("violations(Vs)")
		if results:
			raw = results[0]["args"][0]
			return self._parse_violations(raw)
		return []

	def get_rule_metadata(self, rule_id: str) -> dict:
		metadata = {}
		results = self.query(f"rule({rule_id}, Type)")
		if results:
			metadata["type"] = results[0]["args"][1]
		results = self.query(f"max_level({rule_id}, Max)")
		if results:
			metadata["max_level"] = results[0]["args"][1]
		results = self.query(f"antonym({rule_id}, Antonym)")
		if results:
			metadata["antonym"] = results[0]["args"][1]
		return metadata

	def _dict_to_selector_term(self, selector: dict) -> str:
		by = selector["by"]
		if by == "numbers":
			values = selector["values"]
			return f"numbers([{', '.join(str(v) for v in values)}])"
		elif by == "family":
			family = selector["values"]
			footage = selector.get("footage", "any")
			limit = selector.get("limit")
			method = selector.get("limit_method", "first")
			if limit:
				return f"family({family}, {footage}, {limit}, {method})"
			elif footage and footage != "any":
				return f"family({family}, {footage})"
			else:
				return f"family({family})"
		elif by == "names":
			names = selector["values"]
			escaped = [f"'{n.replace(chr(39), chr(92) + chr(39))}'" for n in names]
			return f"names([{', '.join(escaped)}])"
		elif by == "engaged":
			return "engaged"
		elif by == "union":
			sub_selectors = selector["values"]
			terms = [self._dict_to_selector_term(s) for s in sub_selectors]
			return f"union([{', '.join(terms)}])"
		elif by == "intersection":
			sub_selectors = selector["values"]
			terms = [self._dict_to_selector_term(s) for s in sub_selectors]
			return f"intersection([{', '.join(terms)}])"
		elif by == "difference":
			base = self._dict_to_selector_term(selector["base"])
			subtract = self._dict_to_selector_term(selector["subtract"])
			return f"difference({base}, {subtract})"
		elif by == "expression":
			expr = selector["values"]
			return f"expression({expr})"
		raise ValueError(f"Unknown selector type: {by}")

	def _parse_actions(self, raw_actions: list) -> list[dict]:
		actions = []
		for action in raw_actions:
			if isinstance(action, dict):
				functor = action.get("functor")
				args = action.get("args", [])
				if functor == "engage":
					actions.append({"action": "engage", "manual": args[0], "stop": args[1]})
				elif functor == "disengage":
					actions.append({"action": "disengage", "manual": args[0], "stop": args[1]})
				elif functor == "claim":
					actions.append({
						"action": "claim",
						"combination": args[0],
						"manual": args[1],
						"stop": args[2]
					})
				elif functor == "release":
					actions.append({
						"action": "release",
						"combination": args[0],
						"manual": args[1],
						"stop": args[2]
					})
		return actions

	def _parse_violations(self, raw: list) -> list[dict]:
		violations = []
		for v in raw:
			if isinstance(v, dict):
				functor = v.get("functor")
				args = v.get("args", [])
				violations.append({"type": functor, "args": args})
		return violations
