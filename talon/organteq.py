"""
organteq.py - Talon actions for Organteq control

Uses minimal Python bridge which POSTs command strings to Prolog.
All registration logic is in Prolog.
"""

from talon import Module
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
	def organteq_get_bridge():
		"""get the shared bridge instance"""
		return get_bridge()

	def organteq_set_manual(manual: str):
		"""set the current manual context"""
		global current_manual
		current_manual = manual

	def organteq_get_manual() -> str:
		"""get the current manual context"""
		return current_manual

	def organteq_engage(manual: str, stops: list[str]):
		"""engage stops by number"""
		nums = ", ".join(stops)
		get_bridge().run(f"engage({manual}, numbers([{nums}]))")

	def organteq_disengage(manual: str, stops: list[str]):
		"""disengage stops by number"""
		nums = ", ".join(stops)
		get_bridge().run(f"disengage({manual}, numbers([{nums}]))")

	def organteq_toggle(manual: str, stops: list[str]):
		"""toggle stops by number"""
		nums = ", ".join(stops)
		get_bridge().run(f"toggle({manual}, numbers([{nums}]))")

	def organteq_solo(manual: str, stops: list[str]):
		"""solo stops by number"""
		nums = ", ".join(stops)
		get_bridge().run(f"solo({manual}, numbers([{nums}]))")

	def organteq_clear(manual: str):
		"""clear all stops on a manual"""
		get_bridge().run(f"clear({manual})")

	def organteq_engage_family(manual: str, family: str, footage: str = ""):
		"""engage stops by family and optional footage"""
		if footage:
			get_bridge().run(f"engage({manual}, family({family}, {footage}))")
		else:
			get_bridge().run(f"engage({manual}, family({family}))")

	def organteq_disengage_family(manual: str, family: str, footage: str = ""):
		"""disengage stops by family and optional footage"""
		if footage:
			get_bridge().run(f"disengage({manual}, family({family}, {footage}))")
		else:
			get_bridge().run(f"disengage({manual}, family({family}))")

	def organteq_toggle_family(manual: str, family: str, footage: str = ""):
		"""toggle stops by family and optional footage"""
		if footage:
			get_bridge().run(f"toggle({manual}, family({family}, {footage}))")
		else:
			get_bridge().run(f"toggle({manual}, family({family}))")

	def organteq_solo_family(manual: str, family: str, footage: str = ""):
		"""solo stops by family and optional footage"""
		if footage:
			get_bridge().run(f"solo({manual}, family({family}, {footage}))")
		else:
			get_bridge().run(f"solo({manual}, family({family}))")

	def organteq_undo():
		"""undo last operation"""
		get_bridge().run("undo")

	def organteq_redo():
		"""redo last undone operation"""
		get_bridge().run("redo")

	def organteq_sync():
		"""sync engine state with Organteq"""
		get_bridge().sync()

	def organteq_couple_index(index: int):
		"""couple by index"""
		get_bridge().run(f"couple_index({index})")

	def organteq_decouple_index(index: int):
		"""decouple by index"""
		get_bridge().run(f"decouple_index({index})")

	def organteq_decouple_all():
		"""decouple all couplers"""
		get_bridge().run("decouple_all")

	def organteq_rule_up(rule_id: str):
		"""increment rule level"""
		get_bridge().run(f"up('{rule_id}')")

	def organteq_rule_down(rule_id: str):
		"""decrement rule level"""
		get_bridge().run(f"down('{rule_id}')")

	def organteq_rule_level(rule_id: str, level: int):
		"""set rule level"""
		get_bridge().run(f"level('{rule_id}', {level})")

	def organteq_rule_mute(rule_id: str):
		"""mute rule (level 0)"""
		get_bridge().run(f"mute('{rule_id}')")

	def organteq_rule_maximize(rule_id: str):
		"""maximize rule level"""
		get_bridge().run(f"maximize('{rule_id}')")

	def organteq_execute(command: str):
		"""execute arbitrary command string"""
		get_bridge().run(command)

	def organteq_couple_manuals(source: str, destination: str):
		"""couple two manuals"""
		get_bridge().run(f"couple({source}, {destination})")

	def organteq_decouple_manuals(source: str, destination: str):
		"""decouple two manuals"""
		get_bridge().run(f"decouple({source}, {destination})")

