#!/usr/bin/env python
"""Run all tests for organteq_registration.

Prerequisites:
1. Prolog server running with classification and example rules loaded:
   cd prolog/
   swipl -g "consult('server.pl'), consult('classification.pl'), consult('../rules/examples.pl'), server(5000)."
"""

import unittest
import sys
from pathlib import Path

if __name__ == "__main__":
	test_dir = Path(__file__).parent
	loader = unittest.TestLoader()
	suite = loader.discover(str(test_dir), pattern="test_*.py")

	runner = unittest.TextTestRunner(verbosity=2)
	result = runner.run(suite)

	sys.exit(0 if result.wasSuccessful() else 1)
