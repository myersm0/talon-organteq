from talon import Module, Context, imgui, actions

mod = Module()
ctx = Context()
ctx.matches = "app: /organteq/i"

def get_bridge():
	return actions.user.organteq_get_bridge()

mod.list("organteq_rule")

@ctx.dynamic_list("user.organteq_rule")
def organteq_rule(_):
	try:
		rules = get_bridge().list_rules()
		return {r: r for r in rules}
	except:
		return {}

mod.list("organteq_transient")

@ctx.dynamic_list("user.organteq_transient")
def organteq_transient(_):
	try:
		b = get_bridge()
		rules = b.list_rules()
		transients = {}
		for r in rules:
			info = b.get_rule_info(r)
			if info and info.get("type") == "transient":
				transients[r] = r
		return transients
	except:
		return {}

@imgui.open()
def gui_rule_status(gui: imgui.GUI):
	gui.text("talon-organteq")
	gui.line()
	try:
		b = get_bridge()
		preset = b.get_preset()
		gui.text(f"Preset: {preset or '(none)'}")
		gui.spacer()

		rules = b.list_rules()
		if rules:
			gui.text("Rules:")
			for rule in sorted(rules):
				info = b.get_rule_info(rule)
				if info:
					level = info.get("current_level", 0)
					max_level = info.get("max_level", "?")
					rule_type = info.get("type", "")
					type_marker = "○" if rule_type == "transient" else "●"
					gui.text(f"  {type_marker} {rule}: {level}/{max_level}")
				else:
					gui.text(f"  {rule}: ?")
		else:
			gui.text("(no rules loaded)")
	except Exception as e:
		gui.text(f"Error: {e}")

	gui.spacer()
	if gui.button("Close"):
		actions.user.organteq_hide_rules()

@mod.action_class
class Actions:
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
		"""apply a transient rule"""
		get_bridge().apply_rule(rule, delta=1)

	def organteq_show_rules():
		"""show the rules status gui"""
		gui_rule_status.show()

	def organteq_hide_rules():
		"""hide the rules status gui"""
		gui_rule_status.hide()

	def organteq_toggle_rules():
		"""toggle the rules status gui"""
		if gui_rule_status.showing:
			gui_rule_status.hide()
		else:
			gui_rule_status.show()

