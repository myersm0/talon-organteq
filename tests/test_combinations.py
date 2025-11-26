import unittest
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent.parent))
from core.client import PrologClient


class TestCombinationStops(unittest.TestCase):
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

	def test_alpha_level_1_great(self):
		stops = self.p.get_combination_stops("alpha", 1, "great")
		self.assertEqual(sorted(stops), [1, 2, 3])

	def test_alpha_level_1_swell(self):
		stops = self.p.get_combination_stops("alpha", 1, "swell")
		self.assertEqual(sorted(stops), [1, 2])

	def test_alpha_level_1_pedal_empty(self):
		stops = self.p.get_combination_stops("alpha", 1, "pedal")
		self.assertEqual(stops, [])

	def test_alpha_level_2_cumulative_great(self):
		stops = self.p.get_combination_stops("alpha", 2, "great")
		self.assertEqual(sorted(stops), [1, 2, 3, 4])

	def test_alpha_level_2_pedal(self):
		stops = self.p.get_combination_stops("alpha", 2, "pedal")
		self.assertEqual(sorted(stops), [1, 2])

	def test_alpha_level_0_empty(self):
		stops = self.p.get_combination_stops("alpha", 0, "great")
		self.assertEqual(stops, [])


class TestCombinationDelta(unittest.TestCase):
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

	def test_delta_0_to_1_claims_and_engages(self):
		actions = self.p.get_combination_delta_actions("alpha", 0, 1, ["great", "swell", "pedal"])
		claims = [(a["manual"], a["stop"]) for a in actions if a["action"] == "claim"]
		engages = [(a["manual"], a["stop"]) for a in actions if a["action"] == "engage"]
		self.assertIn(("great", 1), claims)
		self.assertIn(("great", 2), claims)
		self.assertIn(("great", 3), claims)
		self.assertIn(("swell", 1), claims)
		self.assertIn(("swell", 2), claims)
		self.assertIn(("great", 1), engages)
		self.assertIn(("great", 2), engages)
		self.assertIn(("great", 3), engages)
		self.assertNotIn(("pedal", 1), claims)

	def test_delta_1_to_2_adds_new_stops(self):
		actions = self.p.get_combination_delta_actions("alpha", 1, 2, ["great", "swell", "pedal"])
		claims = [(a["manual"], a["stop"]) for a in actions if a["action"] == "claim"]
		engages = [(a["manual"], a["stop"]) for a in actions if a["action"] == "engage"]
		self.assertIn(("pedal", 1), claims)
		self.assertIn(("pedal", 2), claims)
		self.assertIn(("great", 4), claims)
		self.assertIn(("pedal", 1), engages)
		self.assertIn(("great", 4), engages)
		self.assertNotIn(("great", 1), claims)

	def test_delta_1_to_2_reasserts_existing_stops(self):
		actions = self.p.get_combination_delta_actions("alpha", 1, 2, ["great", "swell", "pedal"])
		engages = [(a["manual"], a["stop"]) for a in actions if a["action"] == "engage"]
		self.assertIn(("great", 1), engages)
		self.assertIn(("great", 2), engages)
		self.assertIn(("great", 3), engages)

	def test_delta_2_to_1_releases_and_disengages(self):
		actions = self.p.get_combination_delta_actions("alpha", 2, 1, ["great", "swell", "pedal"])
		releases = [(a["manual"], a["stop"]) for a in actions if a["action"] == "release"]
		disengages = [(a["manual"], a["stop"]) for a in actions if a["action"] == "disengage"]
		self.assertIn(("pedal", 1), releases)
		self.assertIn(("pedal", 2), releases)
		self.assertIn(("great", 4), releases)
		self.assertIn(("pedal", 1), disengages)
		self.assertIn(("great", 4), disengages)

	def test_delta_1_to_0_releases_all(self):
		actions = self.p.get_combination_delta_actions("alpha", 1, 0, ["great", "swell", "pedal"])
		releases = [(a["manual"], a["stop"]) for a in actions if a["action"] == "release"]
		disengages = [(a["manual"], a["stop"]) for a in actions if a["action"] == "disengage"]
		self.assertIn(("great", 1), releases)
		self.assertIn(("great", 2), releases)
		self.assertIn(("great", 3), releases)
		self.assertIn(("great", 1), disengages)
		self.assertIn(("great", 2), disengages)
		self.assertIn(("great", 3), disengages)


class TestOwnership(unittest.TestCase):
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
		])

	def test_shared_ownership_prevents_disengage(self):
		self.p.claim_stops("alpha", "great", [1, 2, 3])
		self.p.claim_stops("bravo", "great", [1, 2])
		self.p.set_combination_level("alpha", 1)
		self.p.set_combination_level("bravo", 1)

		actions = self.p.get_combination_delta_actions("alpha", 1, 0, ["great", "swell", "pedal"])
		releases = [(a["manual"], a["stop"]) for a in actions if a["action"] == "release"]
		disengages = [(a["manual"], a["stop"]) for a in actions if a["action"] == "disengage"]

		self.assertIn(("great", 1), releases)
		self.assertIn(("great", 2), releases)
		self.assertIn(("great", 3), releases)
		self.assertNotIn(("great", 1), disengages)
		self.assertNotIn(("great", 2), disengages)
		self.assertIn(("great", 3), disengages)

	def test_last_owner_releases_causes_disengage(self):
		self.p.claim_stops("alpha", "great", [1, 2])
		self.p.set_combination_level("alpha", 1)

		actions = self.p.get_combination_delta_actions("alpha", 1, 0, ["great"])
		disengages = [(a["manual"], a["stop"]) for a in actions if a["action"] == "disengage"]

		self.assertIn(("great", 1), disengages)
		self.assertIn(("great", 2), disengages)


class TestReassert(unittest.TestCase):
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
		])

	def test_reassert_returns_all_stops_for_level(self):
		self.p.set_combination_level("alpha", 2)
		actions = self.p.get_reassert_actions("alpha", ["great", "swell", "pedal"])
		engages = [(a["manual"], a["stop"]) for a in actions if a["action"] == "engage"]

		self.assertIn(("great", 1), engages)
		self.assertIn(("great", 2), engages)
		self.assertIn(("great", 3), engages)
		self.assertIn(("great", 4), engages)
		self.assertIn(("swell", 1), engages)
		self.assertIn(("swell", 2), engages)

	def test_reassert_inactive_returns_empty(self):
		actions = self.p.get_reassert_actions("alpha", ["great"])
		self.assertEqual(actions, [])


if __name__ == "__main__":
	unittest.main()
