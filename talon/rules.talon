app: /organteq/i
-

# level control
^[up] {user.organteq_rule}+$:
	user.organteq_rule_up(organteq_rule_list)

^down {user.organteq_rule}+$:
	user.organteq_rule_down(organteq_rule_list)

^{user.organteq_rule} <number_small>:
	user.organteq_rule_set_level(organteq_rule, number_small)


# absolute operations
^maximize {user.organteq_rule}+$:
	user.organteq_rule_maximize(organteq_rule_list)

^minimize {user.organteq_rule}+$:
	user.organteq_rule_minimize(organteq_rule_list)

^mute {user.organteq_rule}+$:
	user.organteq_rule_mute(organteq_rule_list)


# solo and reassert
^solo {user.organteq_rule}$:
	user.organteq_rule_solo(organteq_rule)

^solo {user.organteq_rule} <number_small>$:
	user.organteq_rule_set_level(organteq_rule, number_small)
	user.organteq_rule_solo(organteq_rule)

^reassert {user.organteq_rule}$:
	user.organteq_rule_reassert(organteq_rule)


# transient rules (invoked by name)
^{user.organteq_transient}$:
	user.organteq_transient(organteq_transient)


# GUI
^show rules$:
	user.organteq_show_rules()

^hide rules$:
	user.organteq_hide_rules()

