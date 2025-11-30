"""
test_prolog.py - Integration tests for the Prolog server

Requires: Prolog server running on port 5000 with rules_example.pl loaded
Start server: cd prolog && swipl -g "consult('main.pl'), load_rules_from_dir('.', 'rules_example.pl'), server(5000)."
Run tests: python -m unittest tests.test_prolog -v
"""

import unittest
import requests

prolog_endpoint = "http://localhost:5000"


def execute(command: str, args: dict = None) -> dict:
	response = requests.post(
		f"{prolog_endpoint}/execute",
		json={"command": command, "args": args or {}},
		timeout=10
	)
	return response.json()


def sync_test_state():
	"""Set up a consistent test state with known elements."""
	elements = [
		{"division": "great", "number": 1, "name": "Bourdon 16'", "type": "stop"},
		{"division": "great", "number": 2, "name": "Montre 8'", "type": "stop"},
		{"division": "great", "number": 3, "name": "Prinzipal 4'", "type": "stop"},
		{"division": "great", "number": 4, "name": "Trompette 8'", "type": "stop"},
		{"division": "great", "number": 5, "name": "Zimbel III", "type": "stop"},
		{"division": "great", "number": 6, "name": "Mixtura IV", "type": "stop"},
		{"division": "swell", "number": 1, "name": "Gedact 8'", "type": "stop"},
		{"division": "swell", "number": 2, "name": "Salicional 8'", "type": "stop"},
		{"division": "swell", "number": 3, "name": "Hautbois 8'", "type": "stop"},
		{"division": "pedal", "number": 1, "name": "Subbass 16'", "type": "stop"},
		{"division": "pedal", "number": 2, "name": "Prinzipal 8'", "type": "stop"},
		{"division": "coupler", "number": 1, "name": "Swell to Great", "type": "coupler"},
		{"division": "coupler", "number": 2, "name": "Great to Pedal", "type": "coupler"},
		{"division": "coupler", "number": 3, "name": "Swell to Pedal", "type": "coupler"},
		{"division": "tremulant", "number": 1, "name": "Tremulant", "type": "tremulant"},
	]
	result = execute("sync", {
		"preset": "Test Preset",
		"elements": elements,
		"engaged": []
	})
	assert result["status"] == "ok", f"Sync failed: {result}"
	
	execute("assert_facts", {"facts": [
		"state:coupler_mapping('Test Preset', 1, swell, great, unison, normal)",
		"state:coupler_mapping('Test Preset', 2, great, pedal, unison, normal)",
		"state:coupler_mapping('Test Preset', 3, swell, pedal, unison, normal)"
	]})


def get_engaged(result, division):
	return sorted([e["number"] for e in result["state"]["engaged"] if e["division"] == division])


def get_rule_level(result, rule_id):
	for r in result["state"]["rule_levels"]:
		if r["rule"] == rule_id:
			return r["level"]
	return 0


class TestBasicCommands(unittest.TestCase):
	def setUp(self):
		sync_test_state()

	def test_engage_single(self):
		result = execute("engage", {"division": "great", "selector": {"by": "numbers", "values": [1]}})
		self.assertEqual(result["status"], "ok")
		self.assertEqual(get_engaged(result, "great"), [1])

	def test_engage_multiple(self):
		result = execute("engage", {"division": "great", "selector": {"by": "numbers", "values": [1, 2, 3]}})
		self.assertEqual(result["status"], "ok")
		self.assertEqual(get_engaged(result, "great"), [1, 2, 3])

	def test_disengage(self):
		execute("engage", {"division": "great", "selector": {"by": "numbers", "values": [1, 2, 3]}})
		result = execute("disengage", {"division": "great", "selector": {"by": "numbers", "values": [2]}})
		self.assertEqual(result["status"], "ok")
		self.assertEqual(get_engaged(result, "great"), [1, 3])

	def test_toggle_engages_disengaged(self):
		result = execute("toggle", {"division": "great", "selector": {"by": "numbers", "values": [1, 2]}})
		self.assertEqual(result["status"], "ok")
		self.assertEqual(get_engaged(result, "great"), [1, 2])

	def test_toggle_disengages_engaged(self):
		execute("engage", {"division": "great", "selector": {"by": "numbers", "values": [1, 2]}})
		result = execute("toggle", {"division": "great", "selector": {"by": "numbers", "values": [1, 3]}})
		self.assertEqual(result["status"], "ok")
		self.assertEqual(get_engaged(result, "great"), [2, 3])

	def test_solo(self):
		execute("engage", {"division": "great", "selector": {"by": "numbers", "values": [1, 2, 3, 4, 5]}})
		result = execute("solo", {"division": "great", "selector": {"by": "numbers", "values": [2, 4]}})
		self.assertEqual(result["status"], "ok")
		self.assertEqual(get_engaged(result, "great"), [2, 4])

	def test_clear(self):
		execute("engage", {"division": "great", "selector": {"by": "numbers", "values": [1, 2, 3]}})
		result = execute("clear", {"division": "great"})
		self.assertEqual(result["status"], "ok")
		self.assertEqual(get_engaged(result, "great"), [])

	def test_clear_all(self):
		execute("engage", {"division": "great", "selector": {"by": "numbers", "values": [1, 2]}})
		execute("engage", {"division": "swell", "selector": {"by": "numbers", "values": [1]}})
		result = execute("clear_all", {})
		self.assertEqual(result["status"], "ok")
		self.assertEqual(get_engaged(result, "great"), [])
		self.assertEqual(get_engaged(result, "swell"), [])


class TestSelectors(unittest.TestCase):
	def setUp(self):
		sync_test_state()

	def test_selector_family(self):
		result = execute("engage", {"division": "great", "selector": {"by": "family", "values": "reed"}})
		self.assertEqual(result["status"], "ok")
		self.assertIn(4, get_engaged(result, "great"))

	def test_selector_family_with_footage(self):
		result = execute("engage", {"division": "great", "selector": {"by": "family", "values": "principal", "footage": 8}})
		self.assertEqual(result["status"], "ok")
		self.assertIn(2, get_engaged(result, "great"))

	def test_selector_engaged(self):
		execute("engage", {"division": "great", "selector": {"by": "numbers", "values": [1, 3, 5]}})
		result = execute("disengage", {"division": "great", "selector": {"by": "engaged"}})
		self.assertEqual(result["status"], "ok")
		self.assertEqual(get_engaged(result, "great"), [])

	def test_selector_union(self):
		result = execute("engage", {"division": "great", "selector": {
			"by": "union",
			"values": [
				{"by": "family", "values": "reed"},
				{"by": "family", "values": "mixture"}
			]
		}})
		self.assertEqual(result["status"], "ok")
		engaged = get_engaged(result, "great")
		self.assertIn(4, engaged)
		self.assertIn(5, engaged)
		self.assertIn(6, engaged)

	def test_selector_intersection(self):
		execute("engage", {"division": "great", "selector": {"by": "numbers", "values": [1, 2, 4]}})
		result = execute("disengage", {"division": "great", "selector": {
			"by": "intersection",
			"values": [
				{"by": "family", "values": "reed"},
				{"by": "engaged"}
			]
		}})
		self.assertEqual(result["status"], "ok")
		self.assertEqual(get_engaged(result, "great"), [1, 2])

	def test_selector_difference(self):
		result = execute("engage", {"division": "great", "selector": {
			"by": "difference",
			"base": {"by": "numbers", "values": [1, 2, 3, 4, 5]},
			"subtract": {"by": "family", "values": "reed"}
		}})
		self.assertEqual(result["status"], "ok")
		engaged = get_engaged(result, "great")
		self.assertNotIn(4, engaged)
		self.assertIn(1, engaged)


class TestHistory(unittest.TestCase):
	def setUp(self):
		sync_test_state()

	def test_undo_single(self):
		execute("engage", {"division": "great", "selector": {"by": "numbers", "values": [1, 2]}})
		execute("engage", {"division": "great", "selector": {"by": "numbers", "values": [3]}})
		result = execute("undo", {})
		self.assertEqual(result["status"], "ok")
		self.assertEqual(get_engaged(result, "great"), [1, 2])

	def test_undo_multiple(self):
		execute("engage", {"division": "great", "selector": {"by": "numbers", "values": [1]}})
		execute("engage", {"division": "great", "selector": {"by": "numbers", "values": [2]}})
		execute("engage", {"division": "great", "selector": {"by": "numbers", "values": [3]}})
		execute("undo", {})
		result = execute("undo", {})
		self.assertEqual(result["status"], "ok")
		self.assertEqual(get_engaged(result, "great"), [1])

	def test_redo(self):
		execute("engage", {"division": "great", "selector": {"by": "numbers", "values": [1, 2]}})
		execute("engage", {"division": "great", "selector": {"by": "numbers", "values": [3]}})
		execute("undo", {})
		result = execute("redo", {})
		self.assertEqual(result["status"], "ok")
		self.assertEqual(get_engaged(result, "great"), [1, 2, 3])

	def test_undo_at_beginning(self):
		result = execute("undo", {})
		self.assertEqual(result["status"], "ok")

	def test_redo_at_end(self):
		execute("engage", {"division": "great", "selector": {"by": "numbers", "values": [1]}})
		result = execute("redo", {})
		self.assertEqual(result["status"], "ok")


class TestPersistentRules(unittest.TestCase):
	def setUp(self):
		sync_test_state()

	def test_apply_rule_level(self):
		result = execute("apply_rule", {"rule": "alpha", "level": 1})
		self.assertEqual(result["status"], "ok")
		self.assertEqual(get_engaged(result, "great"), [1, 2, 3])
		self.assertEqual(get_engaged(result, "swell"), [1, 2])

	def test_apply_rule_level_2_cumulative(self):
		result = execute("apply_rule", {"rule": "alpha", "level": 2})
		self.assertEqual(result["status"], "ok")
		self.assertEqual(get_engaged(result, "great"), [1, 2, 3, 4])
		self.assertEqual(get_engaged(result, "pedal"), [1, 2])

	def test_apply_rule_delta(self):
		execute("apply_rule", {"rule": "alpha", "level": 1})
		result = execute("apply_rule", {"rule": "alpha", "delta": 1})
		self.assertEqual(result["status"], "ok")
		self.assertEqual(get_rule_level(result, "alpha"), 2)

	def test_apply_rule_mute(self):
		execute("apply_rule", {"rule": "alpha", "level": 2})
		result = execute("apply_rule", {"rule": "alpha", "action": "mute"})
		self.assertEqual(result["status"], "ok")
		self.assertEqual(get_engaged(result, "great"), [])
		self.assertEqual(get_rule_level(result, "alpha"), 0)

	def test_apply_rule_maximize(self):
		result = execute("apply_rule", {"rule": "alpha", "action": "maximize"})
		self.assertEqual(result["status"], "ok")
		self.assertEqual(get_rule_level(result, "alpha"), 3)

	def test_apply_rule_minimize(self):
		execute("apply_rule", {"rule": "alpha", "level": 3})
		result = execute("apply_rule", {"rule": "alpha", "action": "minimize"})
		self.assertEqual(result["status"], "ok")
		self.assertEqual(get_rule_level(result, "alpha"), 1)


class TestOwnership(unittest.TestCase):
	def setUp(self):
		sync_test_state()

	def test_shared_ownership_prevents_disengage(self):
		execute("apply_rule", {"rule": "alpha", "level": 1})
		execute("apply_rule", {"rule": "bravo", "level": 1})
		result = execute("apply_rule", {"rule": "alpha", "action": "mute"})
		self.assertEqual(result["status"], "ok")
		engaged = get_engaged(result, "great")
		self.assertIn(1, engaged)
		self.assertIn(2, engaged)

	def test_last_owner_releases(self):
		execute("apply_rule", {"rule": "alpha", "level": 1})
		result = execute("apply_rule", {"rule": "alpha", "action": "mute"})
		self.assertEqual(result["status"], "ok")
		self.assertEqual(get_engaged(result, "great"), [])


class TestTransientRules(unittest.TestCase):
	def setUp(self):
		sync_test_state()

	def test_transient_rule_engages(self):
		result = execute("apply_rule", {"rule": "brighten", "level": 1})
		self.assertEqual(result["status"], "ok")
		engaged = get_engaged(result, "great")
		self.assertTrue(5 in engaged or 6 in engaged)

	def test_transient_rule_level_2(self):
		result = execute("apply_rule", {"rule": "brighten", "level": 2})
		self.assertEqual(result["status"], "ok")
		engaged = get_engaged(result, "great")
		self.assertIn(5, engaged)
		self.assertIn(6, engaged)

	def test_transient_rule_uses_wildcard_selector(self):
		# brighten uses 3-arg rule_selector form (applies to all targeted divisions)
		result = execute("apply_rule", {"rule": "brighten", "level": 1})
		self.assertEqual(result["status"], "ok")
		# Should engage mixtures on any division that has them
		great_engaged = get_engaged(result, "great")
		self.assertTrue(len(great_engaged) > 0)


class TestAuxiliaryOwnership(unittest.TestCase):
	def setUp(self):
		sync_test_state()

	def test_rule_with_coupler_division(self):
		result = execute("apply_rule", {
			"rule": "full_organ",
			"level": 2,
			"divisions": ["great", "swell", "pedal", "coupler"]
		})
		self.assertEqual(result["status"], "ok")
		couplers = get_engaged(result, "coupler")
		self.assertTrue(len(couplers) > 0)

	def test_coupler_ownership_released_on_mute(self):
		execute("apply_rule", {
			"rule": "full_organ",
			"level": 2,
			"divisions": ["great", "swell", "pedal", "coupler"]
		})
		result = execute("apply_rule", {
			"rule": "full_organ",
			"action": "mute",
			"divisions": ["great", "swell", "pedal", "coupler"]
		})
		self.assertEqual(result["status"], "ok")
		couplers = get_engaged(result, "coupler")
		self.assertEqual(couplers, [])


class TestMultiRuleOwnership(unittest.TestCase):
	def setUp(self):
		sync_test_state()

	def test_overlapping_rules_ownership(self):
		# Alpha level 1 engages great [1,2,3], swell [1,2]
		# Bravo level 1 engages great [1,2], swell reed
		execute("apply_rule", {"rule": "alpha", "level": 1})
		execute("apply_rule", {"rule": "bravo", "level": 1})
		
		# Mute alpha - stops 1,2 should stay (bravo owns them)
		result = execute("apply_rule", {"rule": "alpha", "action": "mute"})
		great_engaged = get_engaged(result, "great")
		self.assertIn(1, great_engaged)
		self.assertIn(2, great_engaged)
		self.assertNotIn(3, great_engaged)  # Only alpha owned this

	def test_last_owner_releases_all(self):
		execute("apply_rule", {"rule": "alpha", "level": 1})
		execute("apply_rule", {"rule": "bravo", "level": 1})
		execute("apply_rule", {"rule": "alpha", "action": "mute"})
		
		# Now mute bravo - everything should clear
		result = execute("apply_rule", {"rule": "bravo", "action": "mute"})
		great_engaged = get_engaged(result, "great")
		self.assertEqual(great_engaged, [])


class TestCouplers(unittest.TestCase):
	def setUp(self):
		sync_test_state()

	def test_engage_coupler_by_number(self):
		result = execute("engage", {"division": "coupler", "selector": {"by": "numbers", "values": [1]}})
		self.assertEqual(result["status"], "ok")
		self.assertEqual(get_engaged(result, "coupler"), [1])

	def test_disengage_coupler(self):
		execute("engage", {"division": "coupler", "selector": {"by": "numbers", "values": [1, 2]}})
		result = execute("disengage", {"division": "coupler", "selector": {"by": "numbers", "values": [1]}})
		self.assertEqual(result["status"], "ok")
		self.assertEqual(get_engaged(result, "coupler"), [2])

	def test_coupler_rpc_action_type(self):
		result = execute("engage", {"division": "coupler", "selector": {"by": "numbers", "values": [1]}})
		self.assertEqual(result["status"], "ok")
		actions = result["actions"]
		self.assertTrue(any(a["type"] == "set_coupler" for a in actions))


class TestTremulant(unittest.TestCase):
	def setUp(self):
		sync_test_state()

	def test_engage_tremulant(self):
		result = execute("engage", {"division": "tremulant", "selector": {"by": "numbers", "values": [1]}})
		self.assertEqual(result["status"], "ok")
		self.assertEqual(get_engaged(result, "tremulant"), [1])

	def test_tremulant_rpc_action_type(self):
		result = execute("engage", {"division": "tremulant", "selector": {"by": "numbers", "values": [1]}})
		self.assertEqual(result["status"], "ok")
		actions = result["actions"]
		self.assertTrue(any(a["type"] == "set_tremulant" for a in actions))


if __name__ == "__main__":
	unittest.main()
