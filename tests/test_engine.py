import unittest
from unittest.mock import patch, MagicMock
from prolog_client import PrologClient


class TestEngineBasicOperations(unittest.TestCase):
	"""Tests for basic engage/disengage/toggle/solo operations.
	
	These tests mock the Organteq API to test engine logic in isolation.
	"""
	
	@classmethod
	def setUpClass(cls):
		cls.p = PrologClient()

	def setUp(self):
		self.p.reset()
		self.p.assert_facts([
			"stop(great, 1, 'Bourdon 16\\'', stop)",
			"stop(great, 2, 'Montre 8\\'', stop)",
			"stop(great, 3, 'Prinzipal 4\\'', stop)",
			"stop(great, 4, 'Trompette 8\\'', stop)",
			"stop(great, 5, 'Zimbel III', stop)",
			"stop(swell, 1, 'Gedact 8\\'', stop)",
			"stop(swell, 2, 'Salicional 8\\'', stop)",
			"stop(swell, 3, 'Hautbois 8\\'', stop)",
			"stop(pedal, 1, 'Subbass 16\\'', stop)",
			"stop(pedal, 2, 'Prinzipal 8\\'', stop)",
		])


class TestApplySelector(unittest.TestCase):
	"""Tests for the apply_selector interface."""
	
	@classmethod
	def setUpClass(cls):
		cls.p = PrologClient()

	def setUp(self):
		self.p.reset()
		self.p.assert_facts([
			"stop(great, 1, 'Bourdon 16\\'', stop)",
			"stop(great, 2, 'Montre 8\\'', stop)",
			"stop(great, 3, 'Prinzipal 4\\'', stop)",
			"stop(great, 4, 'Trompette 8\\'', stop)",
			"stop(great, 5, 'Clairon 4\\'', stop)",
			"stop(great, 6, 'Zimbel III', stop)",
			"stop(swell, 1, 'Gedact 8\\'', stop)",
			"stop(swell, 2, 'Hautbois 8\\'', stop)",
		])

	def test_selector_numbers_resolves(self):
		stops = self.p.resolve_selector("great", {"by": "numbers", "values": [1, 3, 5]})
		self.assertEqual(stops, [1, 3, 5])

	def test_selector_family_resolves(self):
		stops = self.p.resolve_selector("great", {"by": "family", "values": "reed"})
		self.assertEqual(sorted(stops), [4, 5])

	def test_selector_family_with_footage_resolves(self):
		stops = self.p.resolve_selector("great", {
			"by": "family",
			"values": "reed",
			"footage": "8"
		})
		self.assertEqual(stops, [4])

	def test_selector_family_with_limit_resolves(self):
		stops = self.p.resolve_selector("great", {
			"by": "family",
			"values": "reed",
			"limit": 1,
			"limit_method": "first"
		})
		self.assertEqual(len(stops), 1)

	def test_selector_cross_manual(self):
		# Test that selector works on different manuals
		great_reeds = self.p.resolve_selector("great", {"by": "family", "values": "reed"})
		swell_reeds = self.p.resolve_selector("swell", {"by": "family", "values": "reed"})
		self.assertEqual(sorted(great_reeds), [4, 5])
		self.assertEqual(swell_reeds, [2])


class TestTransientRules(unittest.TestCase):
	"""Tests for transient rule stop resolution."""
	
	@classmethod
	def setUpClass(cls):
		cls.p = PrologClient()

	def setUp(self):
		self.p.reset()
		self.p.assert_facts([
			"stop(great, 1, 'Bourdon 16\\'', stop)",
			"stop(great, 2, 'Montre 8\\'', stop)",
			"stop(great, 3, 'Mixtura IV', stop)",
			"stop(great, 4, 'Zimbel III', stop)",
			"stop(great, 5, 'Trompette 8\\'', stop)",
			"stop(great, 6, 'Nasard 2 2/3\\'', stop)",
		])

	def test_brighten_level_1_one_mixture(self):
		stops = self.p.get_rule_stops("brighten", 1, "great")
		self.assertEqual(len(stops), 1)
		self.assertIn(stops[0], [3, 4])

	def test_brighten_level_2_all_mixtures(self):
		stops = self.p.get_rule_stops("brighten", 2, "great")
		self.assertEqual(sorted(stops), [3, 4])

	def test_add_reeds_level_1_one_reed(self):
		stops = self.p.get_rule_stops("add_reeds", 1, "great")
		self.assertEqual(len(stops), 1)
		self.assertEqual(stops[0], 5)

	def test_add_reeds_level_2_all_reeds(self):
		stops = self.p.get_rule_stops("add_reeds", 2, "great")
		self.assertEqual(stops, [5])


class TestCombinationOperations(unittest.TestCase):
	"""Tests for combination minimize/maximize/mute operations via Prolog."""
	
	@classmethod
	def setUpClass(cls):
		cls.p = PrologClient()

	def setUp(self):
		self.p.reset()
		self.p.assert_facts([
			"stop(great, 1, 'Bourdon 16\\'', stop)",
			"stop(great, 2, 'Montre 8\\'', stop)",
			"stop(great, 3, 'Prinzipal 4\\'', stop)",
			"stop(great, 4, 'Trompette 8\\'', stop)",
			"stop(swell, 1, 'Gedact 8\\'', stop)",
			"stop(swell, 2, 'Salicional 8\\'', stop)",
			"stop(pedal, 1, 'Subbass 16\\'', stop)",
			"stop(pedal, 2, 'Prinzipal 8\\'', stop)",
		])

	def test_maximize_from_0_to_max(self):
		# alpha has max_level 3
		actions = self.p.get_combination_delta_actions("alpha", 0, 3, ["great", "swell", "pedal"])
		engages = [(a["manual"], a["stop"]) for a in actions if a["action"] == "engage"]
		
		# Should include all levels
		self.assertIn(("great", 1), engages)
		self.assertIn(("great", 2), engages)
		self.assertIn(("great", 3), engages)
		self.assertIn(("great", 4), engages)  # level 2
		self.assertIn(("pedal", 1), engages)  # level 2

	def test_minimize_from_3_to_1(self):
		actions = self.p.get_combination_delta_actions("alpha", 3, 1, ["great", "swell", "pedal"])
		disengages = [(a["manual"], a["stop"]) for a in actions if a["action"] == "disengage"]
		engages = [(a["manual"], a["stop"]) for a in actions if a["action"] == "engage"]
		
		# Level 1 stops should be reasserted (engaged)
		self.assertIn(("great", 1), engages)
		self.assertIn(("great", 2), engages)
		self.assertIn(("great", 3), engages)
		
		# Level 2+ stops should be disengaged
		self.assertIn(("great", 4), disengages)
		self.assertIn(("pedal", 1), disengages)
		self.assertIn(("pedal", 2), disengages)

	def test_mute_from_2_to_0(self):
		actions = self.p.get_combination_delta_actions("alpha", 2, 0, ["great", "swell", "pedal"])
		disengages = [(a["manual"], a["stop"]) for a in actions if a["action"] == "disengage"]
		
		# All should disengage
		self.assertIn(("great", 1), disengages)
		self.assertIn(("great", 2), disengages)
		self.assertIn(("great", 3), disengages)
		self.assertIn(("great", 4), disengages)
		self.assertIn(("swell", 1), disengages)
		self.assertIn(("swell", 2), disengages)
		self.assertIn(("pedal", 1), disengages)
		self.assertIn(("pedal", 2), disengages)


if __name__ == "__main__":
	unittest.main()
