import unittest
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent.parent))
from core.client import PrologClient


class TestStopClassification(unittest.TestCase):
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
			"stop(great, 6, 'Mixtura IV', stop)",
			"stop(swell, 1, 'Gedact 8\\'', stop)",
			"stop(swell, 2, 'Salicional 8\\'', stop)",
			"stop(swell, 3, 'Hautbois 8\\'', stop)",
			"stop(pedal, 1, 'Subbass 16\\'', stop)",
			"stop(pedal, 2, 'Prinzipal 8\\'', stop)",
		])

	def test_stops_by_family_reed(self):
		stops = self.p.get_stops_by_family("great", "reed")
		self.assertEqual(stops, [4])

	def test_stops_by_family_principal(self):
		stops = self.p.get_stops_by_family("great", "principal")
		self.assertEqual(sorted(stops), [2, 3])

	def test_stops_by_family_mixture(self):
		stops = self.p.get_stops_by_family("great", "mixture")
		self.assertEqual(sorted(stops), [5, 6])

	def test_stops_by_family_with_footage(self):
		stops = self.p.get_stops_by_family("great", "principal", "8")
		self.assertEqual(stops, [2])

	def test_stops_by_family_with_footage_4(self):
		stops = self.p.get_stops_by_family("great", "principal", "4")
		self.assertEqual(stops, [3])

	def test_stops_by_family_empty(self):
		stops = self.p.get_stops_by_family("great", "string")
		self.assertEqual(stops, [])


class TestSelectors(unittest.TestCase):
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
			"stop(great, 6, 'Mixtura IV', stop)",
			"stop(great, 7, 'Clairon 4\\'', stop)",
		])

	def test_selector_numbers(self):
		stops = self.p.resolve_selector("great", {"by": "numbers", "values": [1, 2, 3]})
		self.assertEqual(stops, [1, 2, 3])

	def test_selector_family(self):
		stops = self.p.resolve_selector("great", {"by": "family", "values": "reed"})
		self.assertEqual(sorted(stops), [4, 7])

	def test_selector_family_with_footage(self):
		stops = self.p.resolve_selector("great", {
			"by": "family",
			"values": "reed",
			"footage": "8"
		})
		self.assertEqual(stops, [4])

	def test_selector_family_with_limit_first(self):
		stops = self.p.resolve_selector("great", {
			"by": "family",
			"values": "mixture",
			"limit": 1,
			"limit_method": "first"
		})
		self.assertEqual(len(stops), 1)
		self.assertEqual(stops[0], 5)

	def test_selector_family_with_limit_last(self):
		stops = self.p.resolve_selector("great", {
			"by": "family",
			"values": "mixture",
			"limit": 1,
			"limit_method": "last"
		})
		self.assertEqual(len(stops), 1)
		self.assertEqual(stops[0], 6)

	def test_selector_names(self):
		stops = self.p.resolve_selector("great", {
			"by": "names",
			"values": ["Bourdon 16'", "Montre 8'"]
		})
		self.assertEqual(sorted(stops), [1, 2])

	def test_selector_engaged(self):
		self.p.set_engaged("great", 2, True)
		self.p.set_engaged("great", 4, True)
		stops = self.p.resolve_selector("great", {"by": "engaged"})
		self.assertEqual(sorted(stops), [2, 4])

	def test_selector_union(self):
		stops = self.p.resolve_selector("great", {
			"by": "union",
			"values": [
				{"by": "family", "values": "reed"},
				{"by": "family", "values": "mixture"}
			]
		})
		self.assertEqual(sorted(stops), [4, 5, 6, 7])

	def test_selector_intersection(self):
		self.p.set_engaged("great", 4, True)
		self.p.set_engaged("great", 5, True)
		stops = self.p.resolve_selector("great", {
			"by": "intersection",
			"values": [
				{"by": "family", "values": "reed"},
				{"by": "engaged"}
			]
		})
		self.assertEqual(stops, [4])

	def test_selector_difference(self):
		stops = self.p.resolve_selector("great", {
			"by": "difference",
			"base": {"by": "numbers", "values": [1, 2, 3, 4, 5]},
			"subtract": {"by": "family", "values": "reed"}
		})
		self.assertEqual(sorted(stops), [1, 2, 3, 5])

	def test_selector_nested_compound(self):
		stops = self.p.resolve_selector("great", {
			"by": "difference",
			"base": {
				"by": "union",
				"values": [
					{"by": "family", "values": "reed"},
					{"by": "family", "values": "mixture"}
				]
			},
			"subtract": {"by": "numbers", "values": [4]}
		})
		self.assertEqual(sorted(stops), [5, 6, 7])


class TestRuleMetadata(unittest.TestCase):
	@classmethod
	def setUpClass(cls):
		cls.p = PrologClient()

	def setUp(self):
		self.p.reset()

	def test_persistent_rule_metadata(self):
		meta = self.p.get_rule_metadata("alpha")
		self.assertEqual(meta["type"], "persistent")
		self.assertEqual(meta["max_level"], 3)
		self.assertNotIn("antonym", meta)

	def test_transient_rule_metadata(self):
		meta = self.p.get_rule_metadata("brighten")
		self.assertEqual(meta["type"], "transient")
		self.assertEqual(meta["max_level"], 3)
		self.assertEqual(meta["antonym"], "darken")

	def test_unknown_rule_metadata(self):
		meta = self.p.get_rule_metadata("nonexistent")
		self.assertEqual(meta, {})


if __name__ == "__main__":
	unittest.main()
