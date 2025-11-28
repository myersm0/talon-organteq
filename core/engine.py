from pathlib import Path
from .client import PrologClient
from .organteq_api import (
	get_stops_info, manual_names, manual_numbers,
	max_stops_per_manual, get_current_preset, set_stop
)

all_manuals = ["pedal", "choir", "great", "swell"]

class RegistrationEngine:
	def __init__(
		self,
		prolog_host: str = "localhost",
		prolog_port: int = 5000,
		rules_dir: str = None,
		rules_glob: str = "*.pl"
	):
		self.prolog = PrologClient(prolog_host, prolog_port)
		self.rules_dir = Path(rules_dir).expanduser() if rules_dir else None
		self.rules_glob = rules_glob
		self.state = {manual: [] for manual in all_manuals}
		self.combination_levels = {}
		self.rule_levels = {}
		self.history = []
		self.history_index = -1
		self.current_preset = ""

	def load_rules(self):
		if self.rules_dir and self.rules_dir.exists():
			for pl_file in self.rules_dir.glob(self.rules_glob):
				if pl_file.suffix == '.pl':
					try:
						self.prolog.load_file(str(pl_file))
					except Exception as e:
						print(f"Failed to load {pl_file}: {e}")

	def sync_state(self, save_snapshot: bool = True):
		info = get_stops_info()
		self.current_preset = get_current_preset()
		stops_by_manual = {}
		self.state = {}
		for manual_num, stops in info.items():
			manual_name = manual_names[manual_num]
			stop_list = [(num, name) for num, name, state in stops]
			stops_by_manual[manual_name] = stop_list
			engaged = [num for num, name, state in stops if state == 1.0]
			self.state[manual_name] = engaged
		self.prolog.load_stops(stops_by_manual)
		self.prolog.sync_engaged_state(self.state)
		if save_snapshot:
			self._save_snapshot("sync")

	def _save_snapshot(self, label: str):
		snapshot = {
			"state": {k: list(v) for k, v in self.state.items()},
			"rule_levels": dict(self.rule_levels),
			"combination_levels": dict(self.combination_levels),
			"label": label
		}
		if self.history_index < len(self.history) - 1:
			self.history = self.history[:self.history_index + 1]
		self.history.append(snapshot)
		self.history_index += 1

	def _execute_action(self, action: str, manual: str, stop: str):
		manual_num = manual_numbers[manual]
		if action == "engage":
			set_stop(manual_num, stop, 1.0)
			if stop not in self.state[manual]:
				self.state[manual].append(stop)
				self.state[manual] = sorted(self.state[manual], key=int)
			self.prolog.set_engaged(manual, int(stop), True)
		elif action == "disengage":
			set_stop(manual_num, stop, 0.0)
			if stop in self.state[manual]:
				self.state[manual].remove(stop)
			self.prolog.set_engaged(manual, int(stop), False)

	def _execute_actions(self, actions: list[dict]):
		for action in actions:
			act = action["action"]
			if act == "engage":
				self._execute_action("engage", action["manual"], str(action["stop"]))
			elif act == "disengage":
				self._execute_action("disengage", action["manual"], str(action["stop"]))
			elif act == "claim":
				self.prolog.claim_stops(action["combination"], action["manual"], [action["stop"]])
			elif act == "release":
				self.prolog.release_stops(action["combination"], action["manual"], [action["stop"]])

	def _normalize_stops(self, stops) -> list[str]:
		if isinstance(stops, (int, str)):
			return [str(stops)]
		return [str(s) for s in stops]

	def engage(self, manual: str, stops):
		stops = self._normalize_stops(stops)
		for stop in stops:
			self._execute_action("engage", manual, stop)
		self._save_snapshot(f"engage:{manual}:{stops}")

	def disengage(self, manual: str, stops):
		stops = self._normalize_stops(stops)
		for stop in stops:
			self._execute_action("disengage", manual, stop)
		self._save_snapshot(f"disengage:{manual}:{stops}")

	def toggle(self, manual: str, stops):
		stops = self._normalize_stops(stops)
		for stop in stops:
			if stop in self.state[manual]:
				self._execute_action("disengage", manual, stop)
			else:
				self._execute_action("engage", manual, stop)
		self._save_snapshot(f"toggle:{manual}:{stops}")

	def solo(self, manual: str, stops):
		stops = self._normalize_stops(stops)
		manual_num = manual_numbers[manual]
		max_stops = max_stops_per_manual[manual_num]
		for s in range(1, max_stops + 1):
			stop_str = str(s)
			if stop_str in stops:
				self._execute_action("engage", manual, stop_str)
			else:
				self._execute_action("disengage", manual, stop_str)
		self._save_snapshot(f"solo:{manual}:{stops}")

	def clear(self, manual: str):
		manual_num = manual_numbers[manual]
		max_stops = max_stops_per_manual[manual_num]
		for s in range(1, max_stops + 1):
			self._execute_action("disengage", manual, str(s))
		self._save_snapshot(f"clear:{manual}")

	def apply_selector(
		self,
		manual: str,
		selector: dict,
		action: str = "engage",
		manuals: list[str] = None
	):
		if manuals is None:
			target_manuals = [manual] if manual else all_manuals
		else:
			target_manuals = manuals
		for m in target_manuals:
			stops = self.prolog.resolve_selector(m, selector)
			if not stops:
				continue
			stop_strs = [str(s) for s in stops]
			if action == "engage":
				for stop in stop_strs:
					self._execute_action("engage", m, stop)
			elif action == "disengage":
				for stop in stop_strs:
					self._execute_action("disengage", m, stop)
			elif action == "toggle":
				for stop in stop_strs:
					if stop in self.state[m]:
						self._execute_action("disengage", m, stop)
					else:
						self._execute_action("engage", m, stop)
			elif action == "solo":
				manual_num = manual_numbers[m]
				max_stops = max_stops_per_manual[manual_num]
				for s in range(1, max_stops + 1):
					stop_str = str(s)
					if stop_str in stop_strs:
						self._execute_action("engage", m, stop_str)
					else:
						self._execute_action("disengage", m, stop_str)
		self._save_snapshot(f"{action}:selector:{selector}")

	def engage_family(self, manual: str, family: str, footage: str = None, limit: int = None):
		selector = {"by": "family", "values": family}
		if footage:
			selector["footage"] = int(footage)
		if limit:
			selector["limit"] = limit
			selector["limit_method"] = "first"
		self.apply_selector(manual, selector, action="engage")

	def disengage_family(self, manual: str, family: str, footage: str = None, limit: int = None):
		selector = {"by": "family", "values": family}
		if footage:
			selector["footage"] = int(footage)
		if limit:
			selector["limit"] = limit
			selector["limit_method"] = "first"
		self.apply_selector(manual, selector, action="disengage")

	def toggle_family(self, manual: str, family: str, footage: str = None, limit: int = None):
		selector = {"by": "family", "values": family}
		if footage:
			selector["footage"] = int(footage)
		if limit:
			selector["limit"] = limit
			selector["limit_method"] = "first"
		self.apply_selector(manual, selector, action="toggle")

	def solo_family(self, manual: str, family: str, footage: str = None):
		selector = {"by": "family", "values": family}
		if footage:
			selector["footage"] = int(footage)
		self.apply_selector(manual, selector, action="solo")

	def engage_family_all(self, family: str, footage: str = None, limit: int = None):
		selector = {"by": "family", "values": family}
		if footage:
			selector["footage"] = int(footage)
		if limit:
			selector["limit"] = limit
			selector["limit_method"] = "first"
		self.apply_selector(None, selector, action="engage", manuals=all_manuals)

	def disengage_family_all(self, family: str, footage: str = None):
		selector = {"by": "family", "values": family}
		if footage:
			selector["footage"] = int(footage)
		self.apply_selector(None, selector, action="disengage", manuals=all_manuals)

	def apply_rule(
		self,
		rule_id: str,
		delta: int = None,
		level: int = None,
		action: str = None,
		manuals: list[str] = None
	):
		if manuals is None:
			manuals = all_manuals
		
		metadata = self.prolog.get_rule_metadata(rule_id)
		if not metadata:
			print(f"Rule '{rule_id}' not found")
			return

		rule_type = metadata.get("type")

		if action == "mute":
			if rule_type == "persistent":
				self._apply_persistent_rule(rule_id, level=0, manuals=manuals)
			else:
				self.rule_levels[rule_id] = 0
				if metadata.get("antonym"):
					self.rule_levels[metadata["antonym"]] = 0
			self._save_snapshot(f"rule:{rule_id}:mute")
			return

		if action == "solo":
			self._solo_rule(rule_id, manuals)
			return

		if action == "reassert":
			self._reassert_rule(rule_id, manuals)
			return

		if action == "maximize":
			max_level = metadata.get("max_level", 1)
			if rule_type == "persistent":
				self._apply_persistent_rule(rule_id, level=max_level, manuals=manuals)
			else:
				self._apply_transient_rule(rule_id, target_level=max_level, manuals=manuals)
			return

		if action == "minimize":
			if rule_type == "persistent":
				self._apply_persistent_rule(rule_id, level=1, manuals=manuals)
			else:
				self._apply_transient_rule(rule_id, target_level=1, manuals=manuals)
			return

		if rule_type == "persistent":
			self._apply_persistent_rule(rule_id, delta=delta, level=level, manuals=manuals)
		else:
			self._apply_transient_rule_delta(rule_id, delta=delta if delta is not None else 1, manuals=manuals)

	def _apply_persistent_rule(
		self,
		rule_id: str,
		delta: int = None,
		level: int = None,
		manuals: list[str] = None
	):
		if manuals is None:
			manuals = all_manuals
		metadata = self.prolog.get_rule_metadata(rule_id)
		current_level = self.combination_levels.get(rule_id, 0)
		max_level = metadata.get("max_level", 1)

		if level is not None:
			target_level = max(0, min(level, max_level))
		elif delta is not None:
			target_level = max(0, min(current_level + delta, max_level))
		else:
			target_level = min(current_level + 1, max_level)

		if target_level == current_level:
			return

		actions = self.prolog.get_combination_delta_actions(
			rule_id, current_level, target_level, manuals
		)
		self.combination_levels[rule_id] = target_level
		self.prolog.set_combination_level(rule_id, target_level)
		self._execute_actions(actions)
		self._save_snapshot(f"rule:{rule_id}:level:{target_level}")

	def _apply_transient_rule_delta(self, rule_id: str, delta: int, manuals: list[str]):
		metadata = self.prolog.get_rule_metadata(rule_id)
		antonym = metadata.get("antonym")
		max_level = metadata.get("max_level", 10)
		current_level = self.rule_levels.get(rule_id, 0)
		antonym_level = self.rule_levels.get(antonym, 0) if antonym else 0
		combined = current_level - antonym_level
		new_combined = combined + delta

		if antonym:
			if new_combined > 0:
				target_level = min(new_combined, max_level)
				for level in range(current_level + 1, target_level + 1):
					self._apply_rule_level(rule_id, level, manuals, "engage")
				self.rule_levels[rule_id] = target_level
				self.rule_levels[antonym] = 0
			elif new_combined < 0:
				antonym_metadata = self.prolog.get_rule_metadata(antonym)
				antonym_max = antonym_metadata.get("max_level", 10)
				target_level = min(abs(new_combined), antonym_max)
				for level in range(antonym_level + 1, target_level + 1):
					self._apply_rule_level(antonym, level, manuals, "engage")
				self.rule_levels[antonym] = target_level
				self.rule_levels[rule_id] = 0
			else:
				self.rule_levels[rule_id] = 0
				self.rule_levels[antonym] = 0
		else:
			new_level = max(0, min(current_level + delta, max_level))
			if delta > 0:
				for level in range(current_level + 1, new_level + 1):
					self._apply_rule_level(rule_id, level, manuals, "engage")
			self.rule_levels[rule_id] = new_level
		self._save_snapshot(f"rule:{rule_id}:delta:{delta}")

	def _apply_transient_rule(self, rule_id: str, target_level: int, manuals: list[str]):
		current_level = self.rule_levels.get(rule_id, 0)
		if target_level > current_level:
			for level in range(current_level + 1, target_level + 1):
				self._apply_rule_level(rule_id, level, manuals, "engage")
		self.rule_levels[rule_id] = target_level
		self._save_snapshot(f"rule:{rule_id}:level:{target_level}")

	def _apply_rule_level(self, rule_id: str, level: int, manuals: list[str], action: str):
		for manual in manuals:
			stops = self.prolog.get_rule_stops(rule_id, level, manual)
			for stop in stops:
				self._execute_action(action, manual, str(stop))

	def _reassert_rule(self, rule_id: str, manuals: list[str]):
		metadata = self.prolog.get_rule_metadata(rule_id)
		if metadata.get("type") == "persistent":
			current_level = self.combination_levels.get(rule_id, 0)
			if current_level == 0:
				return
			actions = self.prolog.get_reassert_actions(rule_id, manuals)
			self._execute_actions(actions)
		else:
			current_level = self.rule_levels.get(rule_id, 0)
			for level in range(1, current_level + 1):
				self._apply_rule_level(rule_id, level, manuals, "engage")
		self._save_snapshot(f"reassert:{rule_id}")

	def _solo_rule(self, rule_id: str, manuals: list[str]):
		metadata = self.prolog.get_rule_metadata(rule_id)
		if metadata.get("type") == "persistent":
			current_level = self.combination_levels.get(rule_id, 0)
		else:
			current_level = self.rule_levels.get(rule_id, 0)
		if current_level == 0:
			return
		rule_stops = {}
		for manual in manuals:
			stops = self.prolog.get_combination_stops(rule_id, current_level, manual)
			rule_stops[manual] = set(str(s) for s in stops)
		for manual in manuals:
			manual_num = manual_numbers[manual]
			max_stops = max_stops_per_manual[manual_num]
			for s in range(1, max_stops + 1):
				stop_str = str(s)
				if stop_str in rule_stops[manual]:
					self._execute_action("engage", manual, stop_str)
				else:
					self._execute_action("disengage", manual, stop_str)
		self._save_snapshot(f"solo:{rule_id}")

	# Convenience methods that delegate to apply_rule
	def reassert_rule(self, rule_id: str, manuals: list[str] = None):
		self.apply_rule(rule_id, action="reassert", manuals=manuals)

	def solo_rule(self, rule_id: str, manuals: list[str] = None):
		self.apply_rule(rule_id, action="solo", manuals=manuals)

	def minimize_rule(self, rule_id: str, manuals: list[str] = None):
		self.apply_rule(rule_id, action="minimize", manuals=manuals)

	def maximize_rule(self, rule_id: str, manuals: list[str] = None):
		self.apply_rule(rule_id, action="maximize", manuals=manuals)

	def mute_rule(self, rule_id: str, manuals: list[str] = None):
		self.apply_rule(rule_id, action="mute", manuals=manuals)

	# Legacy aliases for backwards compatibility
	def apply_combination(self, combination_id: str, delta: int = None, level: int = None, manuals: list[str] = None):
		self.apply_rule(combination_id, delta=delta, level=level, manuals=manuals)

	def reassert_combination(self, combination_id: str, manuals: list[str] = None):
		self.apply_rule(combination_id, action="reassert", manuals=manuals)

	def solo_combination(self, combination_id: str, manuals: list[str] = None):
		self.apply_rule(combination_id, action="solo", manuals=manuals)

	def minimize_combination(self, combination_id: str, manuals: list[str] = None):
		self.apply_rule(combination_id, action="minimize", manuals=manuals)

	def maximize_combination(self, combination_id: str, manuals: list[str] = None):
		self.apply_rule(combination_id, action="maximize", manuals=manuals)

	def mute_combination(self, combination_id: str, manuals: list[str] = None):
		self.apply_rule(combination_id, action="mute", manuals=manuals)

	def undo(self):
		if self.history_index > 0:
			self.history_index -= 1
			self._restore_snapshot(self.history[self.history_index])
			print(f"Undo to: {self.history[self.history_index]['label']}")
		else:
			print("Nothing to undo")

	def redo(self):
		if self.history_index < len(self.history) - 1:
			self.history_index += 1
			self._restore_snapshot(self.history[self.history_index])
			print(f"Redo to: {self.history[self.history_index]['label']}")
		else:
			print("Nothing to redo")

	def _restore_snapshot(self, snapshot: dict):
		self.state = {k: list(v) for k, v in snapshot["state"].items()}
		self.rule_levels = dict(snapshot["rule_levels"])
		self.combination_levels = dict(snapshot.get("combination_levels", {}))
		self.prolog.sync_engaged_state(self.state)
		for combo_id, level in self.combination_levels.items():
			self.prolog.set_combination_level(combo_id, level)
		for manual_name, engaged_stops in self.state.items():
			manual_num = manual_numbers[manual_name]
			max_stops = max_stops_per_manual[manual_num]
			for stop in range(1, max_stops + 1):
				stop_str = str(stop)
				value = 1.0 if stop_str in engaged_stops else 0.0
				set_stop(manual_num, stop_str, value)

	def get_violations(self) -> list[dict]:
		return self.prolog.get_violations()

	# Legacy aliases
	def pull(self, manual: str, stops: list[str]):
		self.engage(manual, stops)

	def push(self, manual: str, stops: list[str]):
		self.disengage(manual, stops)

	def pull_family(self, manual: str, family: str, footage: str = None):
		self.engage_family(manual, family, footage)

	def push_family(self, manual: str, family: str, footage: str = None):
		self.disengage_family(manual, family, footage)
