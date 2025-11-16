app: /organteq/i
-

## managing stops and manuals

^clear {user.organteq_manual}$:
	user.organteq_clear_manual(organteq_manual)

^[toggle] {user.organteq_manual} {user.organteq_stop_number}+$:
	user.organteq_toggle_stops(organteq_manual, organteq_stop_number_list)

^(push|disengage) {user.organteq_manual} {user.organteq_stop_number}+$:
	user.organteq_push_stops(organteq_manual, organteq_stop_number_list)

^(pull|engage) {user.organteq_manual} {user.organteq_stop_number}+$:
	user.organteq_pull_stops(organteq_manual, organteq_stop_number_list)

^(with|use|using) {user.organteq_manual}$:
	user.organteq_set_manual(organteq_manual)

^[toggle] {user.organteq_stop_number}+$:
	which_manual = user.organteq_get_manual()
	user.organteq_toggle_stops(which_manual, organteq_stop_number_list)

^(push|disengage) {user.organteq_stop_number}+$:
	which_manual = user.organteq_get_manual()
	user.organteq_push_stops(which_manual, organteq_stop_number_list)

^(pull|engage) {user.organteq_stop_number}+$:
	which_manual = user.organteq_get_manual()
	user.organteq_pull_stops(which_manual, organteq_stop_number_list)

^toggle last$:
	user.organteq_toggle_last()

^remember {user.organteq_stop_number}+$:
	user.organteq_remember_stops(organteq_stop_number_list)

^toggle$:
	user.organteq_toggle_remembered()


## accessing settings panels
^settings [general]$:
	key(u)

^stops|stop settings$:
	key(s)

^tremulants|tremulant settings$:
	key(t)

^expression settings$:
	key(x)

^couplers|coupler settings$:
	key(r)

^combination settings$:
	key(b)

^crescendo settings$:
	key(d)

^tuning settings$:
	key(g)

^jam (view|settings)$:
	key(j)

^double jam [view|settings]$:
	key(k)






