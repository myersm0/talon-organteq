import sys
from pathlib import Path
from talon import Module, actions, settings
from ..core.engine import RegistrationEngine

mod = Module()

mod.setting(
	"talon_organteq_rules_dir",
	type=str,
	default=None,
	desc="Directory containing Prolog rule files (.pl)"
)

mod.setting(
	"talon_organteq_rules_glob",
	type=str,
	default="*.pl",
	desc="Glob pattern for rule files to load"
)

engine = None
current_manual = "great" # todo: not acceptable

def get_engine():
	global engine
	if engine is None:
		try:
			engine = RegistrationEngine(
				prolog_host="localhost",
				prolog_port=5000,
				rules_dir=settings.get("user.talon_organteq_rules_dir"),
				rules_glob=settings.get("user.talon_organteq_rules_glob"),
			)
			engine.load_rules()
			engine.sync_state()
		except Exception as e:
			print(f"Failed to initialize engine: {e}")
			engine = None
	return engine

@mod.action_class
class Actions:
	def organteq_set_manual(manual: str):
		"""set the current manual context"""
		global current_manual
		current_manual = manual

	def organteq_get_manual() -> str:
		"""get the current manual context"""
		return current_manual

	def organteq_engage(manual: str, stops: list[str]):
		"""engage stops by number"""
		get_engine().engage(manual, stops)

	def organteq_disengage(manual: str, stops: list[str]):
		"""disengage stops by number"""
		get_engine().disengage(manual, stops)

	def organteq_toggle(manual: str, stops: list[str]):
		"""toggle stops by number"""
		get_engine().toggle(manual, stops)

	def organteq_solo(manual: str, stops: list[str]):
		"""solo stops by number"""
		get_engine().solo(manual, stops)

	def organteq_clear(manual: str):
		"""clear all stops on a manual"""
		get_engine().clear(manual)

	def organteq_engage_family(manual: str, family: str, footage: str = ""):
		"""engage stops by family and optional footage"""
		f = footage if footage else None
		get_engine().engage_family(manual, family, f)

	def organteq_disengage_family(manual: str, family: str, footage: str = ""):
		"""disengage stops by family and optional footage"""
		f = footage if footage else None
		get_engine().disengage_family(manual, family, f)

	def organteq_toggle_family(manual: str, family: str, footage: str = ""):
		"""toggle stops by family and optional footage"""
		f = footage if footage else None
		get_engine().toggle_family(manual, family, f)

	def organteq_solo_family(manual: str, family: str, footage: str = ""):
		"""solo stops by family and optional footage"""
		f = footage if footage else None
		get_engine().solo_family(manual, family, f)

	def organteq_undo():
		"""undo last operation"""
		get_engine().undo()

	def organteq_redo():
		"""redo last undone operation"""
		get_engine().redo()

	def organteq_sync():
		"""sync engine state with Organteq"""
		get_engine().sync_state()

