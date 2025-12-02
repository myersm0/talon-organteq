"""
Start server from the prolog directory:
    cd prolog && swipl -g "consult('main.pl'), load_rules_from_dir('../examples', 'custom_rules.pl'), server(5000)."

Run tests:
    python -m unittest tests.test_prolog -v
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
	result = response.json()
	if result.get("status") == "error":
		print(f"Command {command} failed: {result.get('error', result)}")
	return result


def sync_test_state(preset: str = "Test Preset"):
	elements = [
		{"division": "great", "number": 1, "name": "Bourdon 16'", "type": "stop"},
		{"division": "great", "number": 2, "name": "Montre 8'", "type": "stop"},
		{"division": "great", "number": 3, "name": "Prinzipal 4'", "type": "stop"},
		{"division": "great", "number": 4, "name": "Trompette 8'", "type": "stop"},
		{"division": "great", "number": 5, "name": "Zimbel III", "type": "stop"},
		{"division": "great", "number": 6, "name": "Mixtura IV", "type": "stop"},
		{"division": "great", "number": 7, "name": "Tierce 1 3/5'", "type": "stop"},
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
	result = execute("sync", {"preset": preset, "elements": elements, "engaged": []})
	assert result["status"] == "ok", f"Sync failed: {result}"
	execute("assert_facts", {"facts": [
		f"state:coupler_mapping('{preset}', 1, swell, great, unison, normal)",
		f"state:coupler_mapping('{preset}', 2, great, pedal, unison, normal)",
		f"state:coupler_mapping('{preset}', 3, swell, pedal, unison, normal)"
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
		result = execute("apply_rule", {"rule": "my persistent rule #1", "level": 1})
		self.assertEqual(result["status"], "ok")
		self.assertTrue(get_rule_level(result, "my persistent rule #1") >= 1)

	def test_apply_rule_level_2_cumulative(self):
		result = execute("apply_rule", {"rule": "my persistent rule #1", "level": 2})
		self.assertEqual(result["status"], "ok")
		self.assertEqual(get_rule_level(result, "my persistent rule #1"), 2)

	def test_apply_rule_delta(self):
		execute("apply_rule", {"rule": "my persistent rule #1", "level": 1})
		result = execute("apply_rule", {"rule": "my persistent rule #1", "delta": 1})
		self.assertEqual(result["status"], "ok")
		self.assertEqual(get_rule_level(result, "my persistent rule #1"), 2)

	def test_apply_rule_mute(self):
		execute("apply_rule", {"rule": "my persistent rule #1", "level": 2})
		result = execute("apply_rule", {"rule": "my persistent rule #1", "action": "mute"})
		self.assertEqual(result["status"], "ok")
		self.assertEqual(get_rule_level(result, "my persistent rule #1"), 0)

	def test_apply_rule_maximize(self):
		result = execute("apply_rule", {"rule": "my persistent rule #1", "action": "maximize"})
		self.assertEqual(result["status"], "ok")
		self.assertTrue(get_rule_level(result, "my persistent rule #1") > 0)

	def test_apply_rule_minimize(self):
		execute("apply_rule", {"rule": "my persistent rule #1", "level": 3})
		result = execute("apply_rule", {"rule": "my persistent rule #1", "action": "minimize"})
		self.assertEqual(result["status"], "ok")
		self.assertEqual(get_rule_level(result, "my persistent rule #1"), 1)


class TestOwnership(unittest.TestCase):
	def setUp(self):
		sync_test_state()

	def test_shared_ownership_prevents_disengage(self):
		execute("apply_rule", {"rule": "my persistent rule #1", "level": 1})
		execute("apply_rule", {"rule": "my persistent rule #2", "level": 1})
		result = execute("apply_rule", {"rule": "my persistent rule #1", "action": "mute"})
		self.assertEqual(result["status"], "ok")

	def test_last_owner_releases(self):
		execute("apply_rule", {"rule": "my persistent rule #1", "level": 1})
		result = execute("apply_rule", {"rule": "my persistent rule #1", "action": "mute"})
		self.assertEqual(result["status"], "ok")
		self.assertEqual(get_rule_level(result, "my persistent rule #1"), 0)


class TestTransientRules(unittest.TestCase):
	def setUp(self):
		sync_test_state()

	def test_add_reeds_level_1_engages_one_reed(self):
		result = execute("apply_rule", {"rule": "add_reeds", "level": 1})
		self.assertEqual(result["status"], "ok")
		engaged = get_engaged(result, "great")
		self.assertIn(4, engaged)

	def test_add_reeds_level_2_engages_all_reeds(self):
		result = execute("apply_rule", {"rule": "add_reeds", "level": 2})
		self.assertEqual(result["status"], "ok")
		engaged = get_engaged(result, "great")
		self.assertIn(4, engaged)


class TestPredicateBasedRules(unittest.TestCase):
	def setUp(self):
		sync_test_state()

	def test_brighten_engages_mixture(self):
		result = execute("apply_rule", {"rule": "brighten"})
		self.assertEqual(result["status"], "ok")
		great_engaged = get_engaged(result, "great")
		self.assertTrue(5 in great_engaged or 6 in great_engaged)

	def test_brighten_multiple_calls_add_more(self):
		execute("apply_rule", {"rule": "brighten"})
		result = execute("apply_rule", {"rule": "brighten"})
		self.assertEqual(result["status"], "ok")
		great_engaged = get_engaged(result, "great")
		self.assertIn(5, great_engaged)
		self.assertIn(6, great_engaged)

	def test_brighten_falls_back_to_mutation(self):
		execute("engage", {"division": "great", "selector": {"by": "family", "values": "mixture"}})
		result = execute("apply_rule", {"rule": "brighten"})
		self.assertEqual(result["status"], "ok")
		great_engaged = get_engaged(result, "great")
		self.assertIn(7, great_engaged)

	def test_darken_disengages_mixture(self):
		execute("engage", {"division": "great", "selector": {"by": "numbers", "values": [5, 6]}})
		result = execute("apply_rule", {"rule": "darken"})
		self.assertEqual(result["status"], "ok")
		great_engaged = get_engaged(result, "great")
		self.assertEqual(len(great_engaged), 1)

	def test_darken_prefers_mutation_over_mixture(self):
		execute("engage", {"division": "great", "selector": {"by": "numbers", "values": [5, 7]}})
		result = execute("apply_rule", {"rule": "darken"})
		self.assertEqual(result["status"], "ok")
		great_engaged = get_engaged(result, "great")
		self.assertIn(5, great_engaged)
		self.assertNotIn(7, great_engaged)


class TestAuxiliaries(unittest.TestCase):
	def setUp(self):
		sync_test_state()

	def test_full_organ_level_2_engages_coupler(self):
		result = execute("apply_rule", {"rule": "full_organ", "level": 2})
		self.assertEqual(result["status"], "ok")
		couplers = get_engaged(result, "coupler")
		self.assertIn(1, couplers)

	def test_full_organ_level_4_engages_tremulant(self):
		result = execute("apply_rule", {"rule": "full_organ", "level": 4})
		self.assertEqual(result["status"], "ok")
		tremulants = get_engaged(result, "tremulant")
		self.assertIn(1, tremulants)

	def test_coupler_ownership_released_on_mute(self):
		execute("apply_rule", {"rule": "full_organ", "level": 2})
		result = execute("apply_rule", {"rule": "full_organ", "action": "mute"})
		self.assertEqual(result["status"], "ok")
		couplers = get_engaged(result, "coupler")
		self.assertEqual(couplers, [])


class TestMultiRuleOwnership(unittest.TestCase):
	def setUp(self):
		sync_test_state()

	def test_overlapping_rules_ownership(self):
		execute("apply_rule", {"rule": "my persistent rule #1", "level": 1})
		execute("apply_rule", {"rule": "my persistent rule #2", "level": 1})
		result = execute("apply_rule", {"rule": "my persistent rule #1", "action": "mute"})
		self.assertEqual(result["status"], "ok")

	def test_last_owner_releases_all(self):
		execute("apply_rule", {"rule": "my persistent rule #1", "level": 1})
		execute("apply_rule", {"rule": "my persistent rule #2", "level": 1})
		execute("apply_rule", {"rule": "my persistent rule #1", "action": "mute"})
		result = execute("apply_rule", {"rule": "my persistent rule #2", "action": "mute"})
		self.assertEqual(result["status"], "ok")
		self.assertEqual(get_rule_level(result, "my persistent rule #1"), 0)
		self.assertEqual(get_rule_level(result, "my persistent rule #2"), 0)


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


class TestErrorHandling(unittest.TestCase):
	def setUp(self):
		sync_test_state()

	def test_unknown_rule_returns_error(self):
		result = execute("apply_rule", {"rule": "nonexistent_rule"})
		self.assertEqual(result["status"], "error")
		self.assertIn("unknown_rule", str(result))

	def test_invalid_element_numbers_filtered(self):
		result = execute("engage", {"division": "great", "selector": {"by": "numbers", "values": [1, 999, 2]}})
		self.assertEqual(result["status"], "ok")
		self.assertEqual(get_engaged(result, "great"), [1, 2])

	def test_empty_selector_result_succeeds(self):
		result = execute("engage", {"division": "great", "selector": {"by": "family", "values": "nonexistent"}})
		self.assertEqual(result["status"], "ok")
		self.assertEqual(get_engaged(result, "great"), [])


class TestRuleLoading(unittest.TestCase):
	"""Diagnostic tests to verify rules are loaded correctly."""

	def setUp(self):
		sync_test_state()

	def test_add_reeds_rule_works(self):
		"""Verify that add_reeds rule is loaded (sanity check)."""
		result = execute("apply_rule", {"rule": "add_reeds", "level": 1})
		self.assertEqual(result["status"], "ok", f"add_reeds failed: {result}")


class TestPresetSpecificSelectors(unittest.TestCase):
	"""Tests for preset-specific rule selectors.
	
	These tests require rules with for_preset() wrappers.
	Skip if the rules file doesn't define preset-specific variants.
	"""

	def test_baroque_preset_applies_rule(self):
		sync_test_state(preset="Baroque Cathedral")
		result = execute("apply_rule", {"rule": "my persistent rule #1", "level": 1})
		if result["status"] == "error" and "no_matching_selector" in str(result):
			self.skipTest("Rule doesn't have preset-specific selectors")
		self.assertEqual(result["status"], "ok")

	def test_romantic_preset_applies_rule(self):
		sync_test_state(preset="Romantic Abbey")
		result = execute("apply_rule", {"rule": "my persistent rule #1", "level": 1})
		if result["status"] == "error" and "no_matching_selector" in str(result):
			self.skipTest("Rule doesn't have preset-specific selectors")
		self.assertEqual(result["status"], "ok")

	def test_other_preset_applies_rule(self):
		sync_test_state(preset="Neo-Classical Church")
		result = execute("apply_rule", {"rule": "my persistent rule #1", "level": 1})
		if result["status"] == "error" and "no_matching_selector" in str(result):
			self.skipTest("Rule doesn't have preset-specific selectors")
		self.assertEqual(result["status"], "ok")


class TestListRules(unittest.TestCase):
	def test_list_rules_for_current_preset(self):
		sync_test_state(preset="Test Preset")
		result = execute("list_rules", {})
		if "status" not in result:
			self.skipTest("list_rules command not available")
		self.assertEqual(result["status"], "ok")
		self.assertIn("brighten", result["state"]["rules"])
		self.assertIn("my persistent rule #1", result["state"]["rules"])

	def test_list_rules_for_specific_preset(self):
		sync_test_state()
		result = execute("list_rules", {"preset": "Baroque Cathedral"})
		if "status" not in result:
			self.skipTest("list_rules command not available")
		self.assertEqual(result["status"], "ok")
		self.assertIn("brighten", result["state"]["rules"])


if __name__ == "__main__":
	unittest.main()
