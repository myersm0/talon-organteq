app: /organteq/i
-

## clear/cancel
^(clear|cancel) {user.organteq_manual}$:
	user.organteq_clear_manual(organteq_manual)

^(clear|cancel)$:
	which_manual = user.organteq_get_manual()
	user.organteq_clear_manual(which_manual)

^general cancel$:
	user.organteq_clear_manual("1")
	user.organteq_clear_manual("2")
	user.organteq_clear_manual("3")
	user.organteq_clear_manual("4")


## stop control by number for a specific manual
^[toggle] {user.organteq_manual} {user.organteq_stop_number}+$:
	user.organteq_toggle_stops_by_number(organteq_manual, organteq_stop_number_list)

^(push|disengage) {user.organteq_manual} {user.organteq_stop_number}+$:
	user.organteq_push_stops_by_number(organteq_manual, organteq_stop_number_list)

^(pull|engage) {user.organteq_manual} {user.organteq_stop_number}+$:
	user.organteq_pull_stops_by_number(organteq_manual, organteq_stop_number_list)

^solo {user.organteq_manual} {user.organteq_stop_number}+$:
	user.organteq_solo_stops_by_number(organteq_manual, organteq_stop_number_list)


## stop control by number using current manual context
^(with|use|using) {user.organteq_manual}$:
	user.organteq_set_manual(organteq_manual)

^[toggle] {user.organteq_stop_number}+$:
	which_manual = user.organteq_get_manual()
	user.organteq_toggle_stops_by_number(which_manual, organteq_stop_number_list)

^(push|disengage) {user.organteq_stop_number}+$:
	which_manual = user.organteq_get_manual()
	user.organteq_push_stops_by_number(which_manual, organteq_stop_number_list)

^(pull|engage) {user.organteq_stop_number}+$:
	which_manual = user.organteq_get_manual()
	user.organteq_pull_stops_by_number(which_manual, organteq_stop_number_list)

^solo {user.organteq_stop_number}+$:
	which_manual = user.organteq_get_manual()
	user.organteq_solo_stops_by_number(which_manual, organteq_stop_number_list)


## stop control by tonal family for a specific manual
^[toggle] {user.organteq_manual} [{user.organteq_footage}] {user.organteq_tonal_family}$:
	footage = organteq_footage or ""
	user.organteq_toggle_stops_by_family(organteq_manual, organteq_tonal_family, footage)

^(push|disengage) {user.organteq_manual} [{user.organteq_footage}] {user.organteq_tonal_family}$:
	footage = organteq_footage or ""
	user.organteq_push_stops_by_family(organteq_manual, organteq_tonal_family, footage)

^(pull|engage) {user.organteq_manual} [{user.organteq_footage}] {user.organteq_tonal_family}$:
	footage = organteq_footage or ""
	user.organteq_pull_stops_by_family(organteq_manual, organteq_tonal_family, footage)

^solo {user.organteq_manual} [{user.organteq_footage}] {user.organteq_tonal_family}$:
	footage = organteq_footage or ""
	user.organteq_solo_stops_by_family(organteq_manual, organteq_tonal_family, footage)




## stop control by tonal family using current manual context
^[toggle] [{user.organteq_footage}] {user.organteq_tonal_family}$:
	which_manual = user.organteq_get_manual()
	footage = organteq_footage or ""
	user.organteq_toggle_stops_by_family(which_manual, organteq_tonal_family, footage)

^(push|disengage) [{user.organteq_footage}] {user.organteq_tonal_family}$:
	which_manual = user.organteq_get_manual()
	footage = organteq_footage or ""
	user.organteq_push_stops_by_family(which_manual, organteq_tonal_family, footage)

^(pull|engage) [{user.organteq_footage}] {user.organteq_tonal_family}$:
	which_manual = user.organteq_get_manual()
	footage = organteq_footage or ""
	user.organteq_pull_stops_by_family(which_manual, organteq_tonal_family, footage)

^solo [{user.organteq_footage}] {user.organteq_tonal_family}$:
	which_manual = user.organteq_get_manual()
	footage = organteq_footage or ""
	user.organteq_solo_stops_by_family(which_manual, organteq_tonal_family, footage)


## memory features
^remember {user.organteq_stop_number}+$:
	which_manual = user.organteq_get_manual()
	user.organteq_remember_stops(which_manual, organteq_stop_number_list)

^[toggle] {user.organteq_manual} memory$:
	user.organteq_toggle_remembered()

^[toggle] memory$:
	user.organteq_toggle_remembered()

^(push|disengage) {user.organteq_manual} memory$:
	stops = user.organteq_get_remembered_stops(organteq_manual)
	user.organteq_push_stops_by_number(organteq_manual, stops)

^(push|disengage) memory$:
	which_manual = user.organteq_get_manual()
	stops = user.organteq_get_remembered_stops(which_manual)
	user.organteq_push_stops_by_number(which_manual, stops)

^(pull|engage) {user.organteq_manual} memory$:
	stops = user.organteq_get_remembered_stops(organteq_manual)
	user.organteq_pull_stops_by_number(organteq_manual, stops)

^(pull|engage) memory$:
	which_manual = user.organteq_get_manual()
	stops = user.organteq_get_remembered_stops(which_manual)
	user.organteq_pull_stops_by_number(which_manual, stops)

^solo {user.organteq_manual} memory$:
	stops = user.organteq_get_remembered_stops(organteq_manual)
	user.organteq_solo_stops_by_number(organteq_manual, stops)

^solo memory$:
	which_manual = user.organteq_get_manual()
	stops = user.organteq_get_remembered_stops(which_manual)
	user.organteq_solo_stops_by_number(which_manual, stops)

^toggle last$:
	user.organteq_toggle_last()


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


