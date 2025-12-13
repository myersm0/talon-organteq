"""
organteq.py - Talon actions for Organteq registration control

All registration logic is in Prolog. This module provides:
- Bridge initialization and management
- Actions for voice commands
- Regular lists (rules, selectors, transients) updated on sync
"""

from talon import Module, Context, imgui, actions
from ..client.bridge import Bridge

mod = Module()
ctx = Context()
ctx.matches = "app: /organteq/i"

bridge = None
current_manual = "great"


def get_bridge():
	global bridge
	if bridge is None:
		try:
			bridge = Bridge()
			bridge.sync()
			update_lists()
		except Exception as e:
			print(f"talon-organteq: Failed to initialize bridge: {e}")
			bridge = None
	return bridge


mod.list("organteq_rule", desc="Available rules (populated on sync)")
mod.list("organteq_selector", desc="Named selectors (populated on sync)")
mod.list("organteq_transient", desc="Transient rules (populated on sync)")


def update_lists():
	b = get_bridge()
	if not b:
		return
	try:
		rules = b.list_rules()
		ctx.lists["user.organteq_rule"] = {r: r for r in rules}
		ctx.lists["user.organteq_selector"] = {s: s for s in b.list_selectors()}
		transients = {}
		for r in rules:
			info = b.get_rule_info(r)
			if info and info.get("type") == "transient":
				transients[r] = r
		ctx.lists["user.organteq_transient"] = transients
	except Exception as e:
		print(f"talon-organteq: Failed to update lists: {e}")


def run_action(action: str, manual: str, selector: str):
	get_bridge().run(f"{action}({manual}, {selector})")


@imgui.open()
def gui_help(gui: imgui.GUI):
	gui.text("talon-organteq")
	gui.line()
	try:
		b = get_bridge()
		preset = b.get_preset()
		gui.text(f"Preset: {preset or '(none)'}")
		gui.spacer()

		selectors = b.list_selectors()
		if selectors:
			gui.text("Selectors:")
			gui.text(f"  {', '.join(selectors)}")
			gui.spacer()

		rules = b.list_rules()
		if rules:
			transients = []
			persistents = []
			for rule in sorted(rules):
				info = b.get_rule_info(rule, preset)
				if info:
					if info.get("type") == "transient":
						transients.append(rule)
					else:
						persistents.append((rule, info))

			if transients:
				gui.text("Transient Rules:")
				gui.text(f"  {', '.join(transients)}")
				gui.spacer()

			if persistents:
				gui.text("Persistent Rules:")
				for rule, info in persistents:
					level = info.get("current_level", 0)
					max_level = info.get("max_level", "?")
					gui.text(f"  {rule}: {level}/{max_level}")
		else:
			gui.text("(no rules loaded)")
	except Exception as e:
		gui.text(f"Error: {e}")
	gui.spacer()
	if gui.button("Close"):
		actions.user.organteq_hide_help()


@mod.action_class
class Actions:

	# =========================================================================
	# Bridge and context
	# =========================================================================

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

	def organteq_sync():
		"""sync state with Organteq and update voice command lists"""
		get_bridge().sync()
		update_lists()

	# =========================================================================
	# Stop control by number
	# =========================================================================

	def organteq_engage(manual: str, stops: list[str]):
		"""engage stops by number"""
		nums = ", ".join(stops)
		run_action("engage", manual, f"numbers([{nums}])")

	def organteq_disengage(manual: str, stops: list[str]):
		"""disengage stops by number"""
		nums = ", ".join(stops)
		run_action("disengage", manual, f"numbers([{nums}])")

	def organteq_toggle(manual: str, stops: list[str]):
		"""toggle stops by number"""
		nums = ", ".join(stops)
		run_action("toggle", manual, f"numbers([{nums}])")

	def organteq_solo(manual: str, stops: list[str]):
		"""solo stops by number"""
		nums = ", ".join(stops)
		run_action("solo", manual, f"numbers([{nums}])")

	def organteq_clear(manual: str):
		"""clear all stops on a manual"""
		get_bridge().run(f"clear({manual})")

	# =========================================================================
	# Stop control by family
	# =========================================================================

	def organteq_engage_family(manual: str, family: str, footage: str = ""):
		"""engage stops by family and optional footage"""
		sel = f"family({family}, {footage})" if footage else f"family({family})"
		run_action("engage", manual, sel)

	def organteq_disengage_family(manual: str, family: str, footage: str = ""):
		"""disengage stops by family and optional footage"""
		sel = f"family({family}, {footage})" if footage else f"family({family})"
		run_action("disengage", manual, sel)

	def organteq_toggle_family(manual: str, family: str, footage: str = ""):
		"""toggle stops by family and optional footage"""
		sel = f"family({family}, {footage})" if footage else f"family({family})"
		run_action("toggle", manual, sel)

	def organteq_solo_family(manual: str, family: str, footage: str = ""):
		"""solo stops by family and optional footage"""
		sel = f"family({family}, {footage})" if footage else f"family({family})"
		run_action("solo", manual, sel)

	# =========================================================================
	# Stop control by named selector
	# =========================================================================

	def organteq_engage_selector(manual: str, selector: str):
		"""engage stops by named selector"""
		run_action("engage", manual, selector)

	def organteq_disengage_selector(manual: str, selector: str):
		"""disengage stops by named selector"""
		run_action("disengage", manual, selector)

	def organteq_toggle_selector(manual: str, selector: str):
		"""toggle stops by named selector"""
		run_action("toggle", manual, selector)

	def organteq_solo_selector(manual: str, selector: str):
		"""solo stops by named selector"""
		run_action("solo", manual, selector)

	# =========================================================================
	# Auxiliaries
	# =========================================================================
	def organteq_couple_index(index: int):
		"""couple by index"""
		get_bridge().run(f"couple_index({index})")

	def organteq_decouple_index(index: int):
		"""decouple by index"""
		get_bridge().run(f"decouple_index({index})")

	def organteq_couple_manuals(source: str, destination: str):
		"""couple two manuals"""
		get_bridge().run(f"couple({source}, {destination})")

	def organteq_decouple_manuals(source: str, destination: str):
		"""decouple two manuals"""
		get_bridge().run(f"decouple({source}, {destination})")

	def organteq_decouple_all():
		"""decouple all couplers"""
		get_bridge().run("decouple_all")

	def organteq_tremulant_on(index: int):
		"""enable tremulant"""
		get_bridge().run(f"tremulant({index}, on)")

	def organteq_tremulant_off(index: int):
		"""disable tremulant"""
		get_bridge().run(f"tremulant({index}, off)")

	def organteq_mono_couple_index(index: int):
		"""engage mono coupler by index"""
		get_bridge().run(f"mono_couple({index})")

	def organteq_mono_decouple_index(index: int):
		"""disengage mono coupler by index"""
		get_bridge().run(f"mono_decouple({index})")

	def organteq_mono_couple_name(name: str):
		"""engage mono coupler by name"""
		get_bridge().run(f"mono_couple('{name}')")

	def organteq_mono_decouple_name(name: str):
		"""disengage mono coupler by name"""
		get_bridge().run(f"mono_decouple('{name}')")

	# =========================================================================
	# Rules
	# =========================================================================

	def organteq_rule_up(rules: list[str]):
		"""increment level for rules"""
		b = get_bridge()
		for rule in rules:
			b.apply_rule(rule, delta=1)

	def organteq_rule_down(rules: list[str]):
		"""decrement level for rules"""
		b = get_bridge()
		for rule in rules:
			b.apply_rule(rule, delta=-1)

	def organteq_rule_set_level(rule: str, level: int):
		"""set a rule to a specific level"""
		get_bridge().apply_rule(rule, level=level)

	def organteq_rule_maximize(rules: list[str]):
		"""set rules to maximum level"""
		b = get_bridge()
		for rule in rules:
			b.apply_rule(rule, action="maximize")

	def organteq_rule_minimize(rules: list[str]):
		"""set rules to minimum level"""
		b = get_bridge()
		for rule in rules:
			b.apply_rule(rule, action="minimize")

	def organteq_rule_mute(rules: list[str]):
		"""set rules to level 0"""
		b = get_bridge()
		for rule in rules:
			b.apply_rule(rule, action="mute")

	def organteq_rule_solo(rule: str):
		"""solo a single rule"""
		get_bridge().apply_rule(rule, action="solo")

	def organteq_rule_reassert(rule: str):
		"""reassert a single rule"""
		get_bridge().apply_rule(rule, action="reassert")

	def organteq_transient(rule: str):
		"""apply a transient rule (increment by 1)"""
		get_bridge().apply_rule(rule, delta=1)

	# =========================================================================
	# Undo/redo
	# =========================================================================

	def organteq_undo():
		"""undo last operation"""
		get_bridge().run("undo")

	def organteq_redo():
		"""redo last undone operation"""
		get_bridge().run("redo")

	# =========================================================================
	# GUI
	# =========================================================================

	def organteq_show_help():
		"""show the help overlay"""
		gui_help.show()

	def organteq_hide_help():
		"""hide the help overlay"""
		gui_help.hide()

	def organteq_toggle_help():
		"""toggle the help overlay"""
		if gui_help.showing:
			gui_help.hide()
		else:
			gui_help.show()

	# =========================================================================
	# Advanced
	# =========================================================================

	def organteq_execute(command: str):
		"""execute arbitrary command string"""
		get_bridge().run(command)
