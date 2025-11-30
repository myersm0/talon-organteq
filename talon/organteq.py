from talon import Module, actions, settings
from ..client.bridge import Bridge

bridge = None
current_manual = "great"

def get_bridge():
	global bridge
	if bridge is None:
		try:
			bridge = Bridge()
			bridge.sync()
		except Exception as e:
			print(f"Failed to initialize bridge: {e}")
			bridge = None
	return bridge

mod = Module()

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
		get_bridge().execute("engage", {
			"division": manual,
			"selector": {"by": "numbers", "values": [int(s) for s in stops]}
		})

	def organteq_disengage(manual: str, stops: list[str]):
		"""disengage stops by number"""
		get_bridge().execute("disengage", {
			"division": manual,
			"selector": {"by": "numbers", "values": [int(s) for s in stops]}
		})

	def organteq_toggle(manual: str, stops: list[str]):
		"""toggle stops by number"""
		get_bridge().execute("toggle", {
			"division": manual,
			"selector": {"by": "numbers", "values": [int(s) for s in stops]}
		})

	def organteq_solo(manual: str, stops: list[str]):
		"""solo stops by number"""
		get_bridge().execute("solo", {
			"division": manual,
			"selector": {"by": "numbers", "values": [int(s) for s in stops]}
		})

	def organteq_clear(manual: str):
		"""clear all stops on a manual"""
		get_bridge().execute("clear", {"division": manual})

	def organteq_engage_family(manual: str, family: str, footage: str = ""):
		"""engage stops by family and optional footage"""
		selector = {"by": "family", "values": family}
		if footage:
			selector["footage"] = int(footage)
		get_bridge().execute("engage", {"division": manual, "selector": selector})

	def organteq_disengage_family(manual: str, family: str, footage: str = ""):
		"""disengage stops by family and optional footage"""
		selector = {"by": "family", "values": family}
		if footage:
			selector["footage"] = int(footage)
		get_bridge().execute("disengage", {"division": manual, "selector": selector})

	def organteq_toggle_family(manual: str, family: str, footage: str = ""):
		"""toggle stops by family and optional footage"""
		selector = {"by": "family", "values": family}
		if footage:
			selector["footage"] = int(footage)
		get_bridge().execute("toggle", {"division": manual, "selector": selector})

	def organteq_solo_family(manual: str, family: str, footage: str = ""):
		"""solo stops by family and optional footage"""
		selector = {"by": "family", "values": family}
		if footage:
			selector["footage"] = int(footage)
		get_bridge().execute("solo", {"division": manual, "selector": selector})

	def organteq_undo():
		"""undo last operation"""
		get_bridge().execute("undo", {})

	def organteq_redo():
		"""redo last undone operation"""
		get_bridge().execute("redo", {})

	def organteq_sync():
		"""sync engine state with Organteq"""
		get_bridge().sync()

