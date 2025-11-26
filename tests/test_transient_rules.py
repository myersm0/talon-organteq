import unittest
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent.parent))
from core.client import PrologClient


class TestTransientRuleStops(unittest.TestCase):
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


class TestTransientRuleMetadata(unittest.TestCase):
	@classmethod
	def setUpClass(cls):
		cls.p = PrologClient()

	def setUp(self):
		self.p.reset()

	def test_brighten_is_transient(self):
		meta = self.p.get_rule_metadata("brighten")
		self.assertEqual(meta["type"], "transient")

	def test_brighten_has_antonym(self):
		meta = self.p.get_rule_metadata("brighten")
		self.assertEqual(meta["antonym"], "darken")

	def test_darken_has_antonym(self):
		meta = self.p.get_rule_metadata("darken")
		self.assertEqual(meta["antonym"], "brighten")

	def test_add_reeds_no_antonym(self):
		meta = self.p.get_rule_metadata("add_reeds")
		self.assertNotIn("antonym", meta)


if __name__ == "__main__":
	unittest.main()
