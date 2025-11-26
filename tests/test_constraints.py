import unittest
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent.parent))
from core.client import PrologClient


class TestConstraintViolations(unittest.TestCase):
	@classmethod
	def setUpClass(cls):
		cls.p = PrologClient()

	def setUp(self):
		self.p.reset()
		self.p.retract_facts(["max_reeds(_, _)", "requires_foundation(_)"])
		self.p.assert_facts([
			"stop(great, 1, 'Bourdon 16\\'', stop)",
			"stop(great, 2, 'Montre 8\\'', stop)",
			"stop(great, 3, 'Trompette 8\\'', stop)",
			"stop(great, 4, 'Hautbois 8\\'', stop)",
			"stop(great, 5, 'Clairon 4\\'', stop)",
			"stop(great, 6, 'Bombarde 16\\'', stop)",
		])

	def test_no_violations_when_no_constraints(self):
		self.p.set_engaged("great", 3, True)
		self.p.set_engaged("great", 4, True)
		self.p.set_engaged("great", 5, True)
		violations = self.p.get_violations()
		self.assertEqual(violations, [])

	def test_max_reeds_no_violation_under_limit(self):
		self.p.assert_facts(["max_reeds(great, 3)"])
		self.p.set_engaged("great", 3, True)
		self.p.set_engaged("great", 4, True)
		violations = self.p.get_violations()
		self.assertEqual(violations, [])

	def test_max_reeds_no_violation_at_limit(self):
		self.p.assert_facts(["max_reeds(great, 3)"])
		self.p.set_engaged("great", 3, True)
		self.p.set_engaged("great", 4, True)
		self.p.set_engaged("great", 5, True)
		violations = self.p.get_violations()
		self.assertEqual(violations, [])

	def test_max_reeds_violation_over_limit(self):
		self.p.assert_facts(["max_reeds(great, 2)"])
		self.p.set_engaged("great", 3, True)
		self.p.set_engaged("great", 4, True)
		self.p.set_engaged("great", 5, True)
		violations = self.p.get_violations()
		self.assertEqual(len(violations), 1)
		self.assertEqual(violations[0]["type"], "too_many_reeds")

	def test_no_foundation_violation(self):
		self.p.assert_facts(["requires_foundation(great)"])
		self.p.set_engaged("great", 3, True)
		violations = self.p.get_violations()
		self.assertEqual(len(violations), 1)
		self.assertEqual(violations[0]["type"], "no_foundation")

	def test_no_foundation_satisfied_with_principal(self):
		self.p.assert_facts(["requires_foundation(great)"])
		self.p.set_engaged("great", 2, True)
		self.p.set_engaged("great", 3, True)
		violations = self.p.get_violations()
		self.assertEqual(violations, [])

	def test_no_foundation_no_violation_when_empty(self):
		self.p.assert_facts(["requires_foundation(great)"])
		violations = self.p.get_violations()
		self.assertEqual(violations, [])

	def test_multiple_violations(self):
		self.p.assert_facts(["max_reeds(great, 1)"])
		self.p.assert_facts(["requires_foundation(great)"])
		self.p.set_engaged("great", 3, True)
		self.p.set_engaged("great", 4, True)
		violations = self.p.get_violations()
		types = [v["type"] for v in violations]
		self.assertIn("too_many_reeds", types)
		self.assertIn("no_foundation", types)


class TestConstraintManagement(unittest.TestCase):
	@classmethod
	def setUpClass(cls):
		cls.p = PrologClient()

	def setUp(self):
		self.p.reset()
		self.p.retract_facts(["max_reeds(_, _)", "requires_foundation(_)"])
		self.p.assert_facts([
			"stop(great, 1, 'Montre 8\\'', stop)",
			"stop(great, 2, 'Trompette 8\\'', stop)",
			"stop(great, 3, 'Hautbois 8\\'', stop)",
		])

	def test_clear_constraint_removes_violation(self):
		self.p.assert_facts(["max_reeds(great, 1)"])
		self.p.set_engaged("great", 2, True)
		self.p.set_engaged("great", 3, True)

		violations = self.p.get_violations()
		self.assertEqual(len(violations), 1)

		self.p.retract_facts(["max_reeds(great, _)"])
		violations = self.p.get_violations()
		self.assertEqual(violations, [])

	def test_update_constraint(self):
		self.p.assert_facts(["max_reeds(great, 1)"])
		self.p.set_engaged("great", 2, True)
		self.p.set_engaged("great", 3, True)

		violations = self.p.get_violations()
		self.assertEqual(len(violations), 1)

		self.p.retract_facts(["max_reeds(great, _)"])
		self.p.assert_facts(["max_reeds(great, 3)"])
		violations = self.p.get_violations()
		self.assertEqual(violations, [])


if __name__ == "__main__":
	unittest.main()
