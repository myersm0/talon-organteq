"""
Minimal Python bridge to Prolog server.

All logic is in Prolog. This just provides HTTP access.
"""

import requests


class Bridge:
	def __init__(self, endpoint="http://localhost:5000"):
		self.endpoint = endpoint

	def run(self, command):
		r = requests.post(f"{self.endpoint}/execute", json={"command": command}, timeout=10)
		return r.json()

	def sync(self):
		r = requests.post(f"{self.endpoint}/sync", timeout=10)
		return r.json()

	def state(self):
		r = requests.get(f"{self.endpoint}/state", timeout=5)
		return r.json()

	def query(self, query_string):
		r = requests.post(f"{self.endpoint}/query", json={"query": query_string}, timeout=10)
		return r.json()

	def load(self, filepath):
		r = requests.post(f"{self.endpoint}/load", json={"file": filepath}, timeout=10)
		return r.json()

	def get_preset(self):
		s = self.state()
		return s.get("state", {}).get("preset")

	def list_rules(self):
		r = self.query("state:rule(R, _)")
		rules = set()
		for result in r.get("results", []):
			if "R" in result:
				rules.add(result["R"])
		return sorted(rules)

	def get_rule_info(self, rule_id):
		r = self.query(f"state:rule('{rule_id}', Type)")
		if not r.get("results"):
			return None
		rule_type = r["results"][0].get("Type")
		info = {"rule": rule_id, "type": rule_type}
		r = self.query(f"selectors:computed_max_level('{rule_id}', _, Max)")
		if r.get("results"):
			info["max_level"] = r["results"][0].get("Max", 1)
		else:
			info["max_level"] = 1
		r = self.query(f"state:antonym('{rule_id}', A)")
		if r.get("results"):
			info["antonym"] = r["results"][0].get("A")
		r = self.query(f"state:get_rule_level('{rule_id}', L)")
		if r.get("results"):
			info["current_level"] = r["results"][0].get("L", 0)
		else:
			info["current_level"] = 0
		return info

	def apply_rule(self, rule_id, level=None, delta=None, action=None):
		if action:
			return self.run(f"{action}('{rule_id}')")
		elif level is not None:
			return self.run(f"level('{rule_id}', {level})")
		elif delta is not None:
			if delta > 0:
				for _ in range(delta):
					self.run(f"up('{rule_id}')")
			elif delta < 0:
				for _ in range(-delta):
					self.run(f"down('{rule_id}')")
		else:
			return self.run(f"up('{rule_id}')")

	def list_selectors(self):
		r = self.query("selectors:named_selector(S)")
		selectors = set()
		for result in r.get("results", []):
			if "S" in result:
				selectors.add(result["S"])
		return sorted(selectors)
