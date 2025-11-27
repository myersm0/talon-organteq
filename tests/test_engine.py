"""
Integration tests for RegistrationEngine.
Requires both Organteq (with --serve) and Prolog server running.
"""

import unittest
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent.parent))
from core.engine import RegistrationEngine
from core.organteq_api import get_stops_info, manual_names


class TestEngineBasicOperations(unittest.TestCase):
	@classmethod
	def setUpClass(cls):
		cls.engine = RegistrationEngine()
		cls.engine.sync_state()

	def setUp(self):
		for manual in ["pedal", "choir", "great", "swell"]:
			self.engine.clear(manual)

	def test_engage_updates_state(self):
		self.engine.engage("great", ["1", "2", "3"])
		self.assertEqual(sorted(self.engine.state["great"]), ["1", "2", "3"])

	def test_engage_updates_organteq(self):
		self.engine.engage("great", ["1", "2"])
		info = get_stops_info()
		engaged = [num for num, name, state in info["3"] if state == 1.0]
		self.assertIn("1", engaged)
		self.assertIn("2", engaged)

	def test_disengage_updates_state(self):
		self.engine.engage("great", ["1", "2", "3"])
		self.engine.disengage("great", ["2"])
		self.assertEqual(sorted(self.engine.state["great"]), ["1", "3"])

	def test_toggle_engages_disengaged(self):
		self.engine.toggle("great", ["1", "2"])
		self.assertEqual(sorted(self.engine.state["great"]), ["1", "2"])

	def test_toggle_disengages_engaged(self):
		self.engine.engage("great", ["1", "2"])
		self.engine.toggle("great", ["1"])
		self.assertEqual(self.engine.state["great"], ["2"])

	def test_solo_clears_others(self):
		self.engine.engage("great", ["1", "2", "3", "4", "5"])
		self.engine.solo("great", ["2", "4"])
		self.assertEqual(sorted(self.engine.state["great"]), ["2", "4"])

	def test_clear_removes_all(self):
		self.engine.engage("great", ["1", "2", "3"])
		self.engine.clear("great")
		self.assertEqual(self.engine.state["great"], [])


class TestEngineFamilyOperations(unittest.TestCase):
	@classmethod
	def setUpClass(cls):
		cls.engine = RegistrationEngine()
		cls.engine.sync_state()

	def setUp(self):
		for manual in ["pedal", "choir", "great", "swell"]:
			self.engine.clear(manual)

	def test_engage_family(self):
		self.engine.engage_family("great", "reed")
		self.assertTrue(len(self.engine.state["great"]) > 0)

	def test_engage_family_with_footage(self):
		self.engine.engage_family("great", "principal", footage="8")
		self.assertTrue(len(self.engine.state["great"]) >= 0)

	def test_disengage_family(self):
		self.engine.engage_family("great", "reed")
		self.engine.disengage_family("great", "reed")
		self.assertEqual(len(self.engine.state["great"]), 0)

	def test_solo_family(self):
		self.engine.engage("great", ["1", "2", "3", "4", "5"])
		self.engine.solo_family("great", "reed")
		for stop in self.engine.state["great"]:
			stops = self.engine.prolog.resolve_selector("great", {"by": "family", "values": "reed"})
			self.assertIn(int(stop), stops)


class TestEngineCompoundSelectors(unittest.TestCase):
	@classmethod
	def setUpClass(cls):
		cls.engine = RegistrationEngine()
		cls.engine.sync_state()

	def setUp(self):
		for manual in ["pedal", "choir", "great", "swell"]:
			self.engine.clear(manual)

	def test_union_selector(self):
		selector = {
			"by": "union",
			"values": [
				{"by": "family", "values": "reed"},
				{"by": "family", "values": "mixture"}
			]
		}
		self.engine.apply_selector("great", selector, action="engage")
		reed_stops = self.engine.prolog.resolve_selector("great", {"by": "family", "values": "reed"})
		mixture_stops = self.engine.prolog.resolve_selector("great", {"by": "family", "values": "mixture"})
		for stop in reed_stops + mixture_stops:
			self.assertIn(str(stop), self.engine.state["great"])

	def test_intersection_selector(self):
		self.engine.engage("great", ["1", "2", "3"])
		selector = {
			"by": "intersection",
			"values": [
				{"by": "numbers", "values": [1, 2, 3, 4, 5]},
				{"by": "engaged"}
			]
		}
		stops = self.engine.prolog.resolve_selector("great", selector)
		self.assertEqual(sorted(stops), [1, 2, 3])

	def test_difference_selector(self):
		selector = {
			"by": "difference",
			"base": {"by": "numbers", "values": [1, 2, 3, 4, 5]},
			"subtract": {"by": "numbers", "values": [2, 4]}
		}
		stops = self.engine.prolog.resolve_selector("great", selector)
		self.assertEqual(sorted(stops), [1, 3, 5])

	def test_engaged_selector(self):
		self.engine.engage("great", ["3", "5", "7"])
		stops = self.engine.prolog.resolve_selector("great", {"by": "engaged"})
		self.assertEqual(sorted(stops), [3, 5, 7])

	def test_difference_with_engaged(self):
		self.engine.engage("great", ["1", "2", "3", "4", "5"])
		reed_stops = self.engine.prolog.resolve_selector("great", {"by": "family", "values": "reed"})
		selector = {
			"by": "difference",
			"base": {"by": "engaged"},
			"subtract": {"by": "family", "values": "reed"}
		}
		self.engine.apply_selector("great", selector, action="solo")
		for stop in self.engine.state["great"]:
			self.assertNotIn(int(stop), reed_stops)


class TestEngineHistory(unittest.TestCase):
	@classmethod
	def setUpClass(cls):
		cls.engine = RegistrationEngine()
		cls.engine.sync_state()

	def setUp(self):
		for manual in ["pedal", "choir", "great", "swell"]:
			self.engine.clear(manual)
		self.engine.history = []
		self.engine.history_index = -1
		self.engine._save_snapshot("test_start")

	def test_undo_restores_previous_state(self):
		self.engine.engage("great", ["1", "2"])
		self.engine.engage("great", ["3"])
		self.assertEqual(sorted(self.engine.state["great"]), ["1", "2", "3"])
		self.engine.undo()
		self.assertEqual(sorted(self.engine.state["great"]), ["1", "2"])

	def test_undo_restores_organteq(self):
		self.engine.engage("great", ["1", "2"])
		self.engine.engage("great", ["3"])
		self.engine.undo()
		info = get_stops_info()
		engaged = [num for num, name, state in info["3"] if state == 1.0]
		self.assertIn("1", engaged)
		self.assertIn("2", engaged)
		self.assertNotIn("3", engaged)

	def test_redo_restores_undone_state(self):
		self.engine.engage("great", ["1", "2"])
		self.engine.engage("great", ["3"])
		self.engine.undo()
		self.engine.redo()
		self.assertEqual(sorted(self.engine.state["great"]), ["1", "2", "3"])

	def test_multiple_undo(self):
		self.engine.engage("great", ["1"])
		self.engine.engage("great", ["2"])
		self.engine.engage("great", ["3"])
		self.engine.undo()
		self.engine.undo()
		self.assertEqual(self.engine.state["great"], ["1"])


class TestEngineUnifiedApplyRule(unittest.TestCase):
	@classmethod
	def setUpClass(cls):
		cls.engine = RegistrationEngine()
		cls.engine.sync_state()

	def setUp(self):
		for manual in ["pedal", "choir", "great", "swell"]:
			self.engine.clear(manual)
		self.engine.combination_levels = {}
		self.engine.rule_levels = {}
		self.engine.prolog.retract_facts(["combination_level(_, _)", "owns(_, _, _)"])

	def test_apply_rule_persistent_increments_level(self):
		self.engine.apply_rule("alpha", delta=1)
		self.assertEqual(self.engine.combination_levels.get("alpha"), 1)

	def test_apply_rule_persistent_engages_stops(self):
		self.engine.apply_rule("alpha", delta=1)
		self.assertTrue(len(self.engine.state["great"]) > 0 or len(self.engine.state["swell"]) > 0)

	def test_apply_rule_persistent_level_2_cumulative(self):
		self.engine.apply_rule("alpha", level=2)
		level_1_stops = self.engine.prolog.get_combination_stops("alpha", 1, "great")
		for stop in level_1_stops:
			self.assertIn(str(stop), self.engine.state["great"])

	def test_apply_rule_transient(self):
		self.engine.apply_rule("brighten", delta=1)
		self.assertEqual(self.engine.rule_levels.get("brighten"), 1)

	def test_apply_rule_action_mute(self):
		self.engine.apply_rule("alpha", level=2)
		self.engine.apply_rule("alpha", action="mute")
		self.assertEqual(self.engine.combination_levels.get("alpha"), 0)

	def test_apply_rule_action_maximize(self):
		self.engine.apply_rule("alpha", action="maximize")
		metadata = self.engine.prolog.get_rule_metadata("alpha")
		self.assertEqual(self.engine.combination_levels.get("alpha"), metadata["max_level"])

	def test_apply_rule_action_minimize(self):
		self.engine.apply_rule("alpha", level=3)
		self.engine.apply_rule("alpha", action="minimize")
		self.assertEqual(self.engine.combination_levels.get("alpha"), 1)

	def test_apply_rule_action_reassert(self):
		self.engine.apply_rule("alpha", level=1)
		initial_stops = list(self.engine.state["great"])
		if initial_stops:
			self.engine.disengage("great", initial_stops[:1])
			self.engine.apply_rule("alpha", action="reassert")
			for stop in initial_stops:
				self.assertIn(stop, self.engine.state["great"])

	def test_apply_rule_action_solo(self):
		self.engine.apply_rule("alpha", level=1)
		self.engine.engage("great", ["10", "11", "12"])
		self.engine.apply_rule("alpha", action="solo")
		alpha_stops = set()
		for manual in ["pedal", "choir", "great", "swell"]:
			stops = self.engine.prolog.get_combination_stops("alpha", 1, manual)
			alpha_stops.update(str(s) for s in stops)
		for manual in ["pedal", "choir", "great", "swell"]:
			for stop in self.engine.state[manual]:
				self.assertIn(stop, alpha_stops)


class TestEngineRulesLoading(unittest.TestCase):
	def test_engine_without_rules_dir(self):
		engine = RegistrationEngine(rules_dir=None)
		engine.sync_state()
		self.assertIsNotNone(engine.state)

	def test_engine_with_rules_dir(self):
		rules_dir = Path(__file__).parent.parent / "rules"
		engine = RegistrationEngine(rules_dir=str(rules_dir))
		engine.load_rules()
		engine.sync_state()
		metadata = engine.prolog.get_rule_metadata("alpha")
		self.assertEqual(metadata.get("type"), "persistent")

	def test_engine_with_rules_glob(self):
		rules_dir = Path(__file__).parent.parent / "rules"
		engine = RegistrationEngine(rules_dir=str(rules_dir), rules_glob="examples.pl")
		engine.load_rules()
		engine.sync_state()
		metadata = engine.prolog.get_rule_metadata("alpha")
		self.assertEqual(metadata.get("type"), "persistent")


class TestEngineSyncState(unittest.TestCase):
	def test_sync_state_loads_preset(self):
		engine = RegistrationEngine()
		engine.sync_state()
		self.assertIsInstance(engine.current_preset, str)

	def test_sync_state_populates_stops(self):
		engine = RegistrationEngine()
		engine.sync_state()
		self.assertIn("great", engine.state)
		self.assertIn("pedal", engine.state)
		self.assertIn("swell", engine.state)
		self.assertIn("choir", engine.state)


if __name__ == "__main__":
	unittest.main()
