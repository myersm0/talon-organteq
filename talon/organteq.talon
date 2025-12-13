app: /organteq/i
-

# Stop control by number - specific manual
^[toggle] {user.organteq_manual} {user.organteq_stop_number}+$:
	user.organteq_toggle(organteq_manual, organteq_stop_number_list)

^(push|disengage) {user.organteq_manual} {user.organteq_stop_number}+$:
	user.organteq_disengage(organteq_manual, organteq_stop_number_list)

^(pull|engage) {user.organteq_manual} {user.organteq_stop_number}+$:
	user.organteq_engage(organteq_manual, organteq_stop_number_list)

^solo {user.organteq_manual} {user.organteq_stop_number}+$:
	user.organteq_solo(organteq_manual, organteq_stop_number_list)


# Stop control by number - current manual context
^[toggle] {user.organteq_stop_number}+$:
	manual = user.organteq_get_manual()
	user.organteq_toggle(manual, organteq_stop_number_list)

^(push|disengage) {user.organteq_stop_number}+$:
	manual = user.organteq_get_manual()
	user.organteq_disengage(manual, organteq_stop_number_list)

^(pull|engage) {user.organteq_stop_number}+$:
	manual = user.organteq_get_manual()
	user.organteq_engage(manual, organteq_stop_number_list)

^solo {user.organteq_stop_number}+$:
	manual = user.organteq_get_manual()
	user.organteq_solo(manual, organteq_stop_number_list)


# Stop control by family - specific manual
^[toggle] {user.organteq_manual} [{user.organteq_footage}] {user.organteq_tonal_family}$:
	footage = organteq_footage or ""
	user.organteq_toggle_family(organteq_manual, organteq_tonal_family, footage)

^(push|disengage) {user.organteq_manual} [{user.organteq_footage}] {user.organteq_tonal_family}$:
	footage = organteq_footage or ""
	user.organteq_disengage_family(organteq_manual, organteq_tonal_family, footage)

^(pull|engage) {user.organteq_manual} [{user.organteq_footage}] {user.organteq_tonal_family}$:
	footage = organteq_footage or ""
	user.organteq_engage_family(organteq_manual, organteq_tonal_family, footage)

^solo {user.organteq_manual} [{user.organteq_footage}] {user.organteq_tonal_family}$:
	footage = organteq_footage or ""
	user.organteq_solo_family(organteq_manual, organteq_tonal_family, footage)


# Stop control by family - current manual context
^[toggle] [{user.organteq_footage}] {user.organteq_tonal_family}$:
	manual = user.organteq_get_manual()
	footage = organteq_footage or ""
	user.organteq_toggle_family(manual, organteq_tonal_family, footage)

^(push|disengage) [{user.organteq_footage}] {user.organteq_tonal_family}$:
	manual = user.organteq_get_manual()
	footage = organteq_footage or ""
	user.organteq_disengage_family(manual, organteq_tonal_family, footage)

^(pull|engage) [{user.organteq_footage}] {user.organteq_tonal_family}$:
	manual = user.organteq_get_manual()
	footage = organteq_footage or ""
	user.organteq_engage_family(manual, organteq_tonal_family, footage)

^solo [{user.organteq_footage}] {user.organteq_tonal_family}$:
	manual = user.organteq_get_manual()
	footage = organteq_footage or ""
	user.organteq_solo_family(manual, organteq_tonal_family, footage)


# Stop control by named selector - specific manual
^[toggle] {user.organteq_manual} {user.organteq_selector}$:
	user.organteq_toggle_selector(organteq_manual, organteq_selector)

^(push|disengage) {user.organteq_manual} {user.organteq_selector}$:
	user.organteq_disengage_selector(organteq_manual, organteq_selector)

^(pull|engage) {user.organteq_manual} {user.organteq_selector}$:
	user.organteq_engage_selector(organteq_manual, organteq_selector)

^solo {user.organteq_manual} {user.organteq_selector}$:
	user.organteq_solo_selector(organteq_manual, organteq_selector)


# Stop control by named selector - current manual context
^[toggle] {user.organteq_selector}$:
	manual = user.organteq_get_manual()
	user.organteq_toggle_selector(manual, organteq_selector)

^(push|disengage) {user.organteq_selector}$:
	manual = user.organteq_get_manual()
	user.organteq_disengage_selector(manual, organteq_selector)

^(pull|engage) {user.organteq_selector}$:
	manual = user.organteq_get_manual()
	user.organteq_engage_selector(manual, organteq_selector)

^solo {user.organteq_selector}$:
	manual = user.organteq_get_manual()
	user.organteq_solo_selector(manual, organteq_selector)


# Auxiliaries
^couple {user.organteq_stop_number}$:
	user.organteq_couple_index(organteq_stop_number)

^decouple {user.organteq_stop_number}$:
	user.organteq_decouple_index(organteq_stop_number)

^couple {user.organteq_manual} [to] {user.organteq_manual}$:
	user.organteq_couple_manuals(organteq_manual_1, organteq_manual_2)

^decouple {user.organteq_manual} [to|from] {user.organteq_manual}$:
	user.organteq_decouple_manuals(organteq_manual_1, organteq_manual_2)

^decouple all$:
	user.organteq_decouple_all()

^tremulant {user.organteq_stop_number}$:
	user.organteq_tremulant_on(organteq_stop_number)

^cancel tremulant {user.organteq_stop_number}$:
	user.organteq_tremulant_off(organteq_stop_number)

^cancel tremulants$:
	user.organteq_tremulant_off(1)
	user.organteq_tremulant_off(2)
	user.organteq_tremulant_off(3)
	user.organteq_tremulant_off(4)

^couple mono {user.organteq_stop_number}$:
	user.organteq_mono_couple_index(organteq_stop_number)

^decouple mono {user.organteq_stop_number}$:
	user.organteq_mono_decouple_index(organteq_stop_number)

^couple {user.organteq_mono_coupler}$:
	user.organteq_mono_couple_name(organteq_mono_coupler)

^decouple {user.organteq_mono_coupler}$:
	user.organteq_mono_decouple_name(organteq_mono_coupler)


# Rules - level control
^[up] {user.organteq_rule}+$:
	user.organteq_rule_up(organteq_rule_list)

^down {user.organteq_rule}+$:
	user.organteq_rule_down(organteq_rule_list)

^{user.organteq_rule} <number_small>$:
	user.organteq_rule_set_level(organteq_rule, number_small)


# Rules - absolute operations
^maximize {user.organteq_rule}+$:
	user.organteq_rule_maximize(organteq_rule_list)

^minimize {user.organteq_rule}+$:
	user.organteq_rule_minimize(organteq_rule_list)

^mute {user.organteq_rule}+$:
	user.organteq_rule_mute(organteq_rule_list)


# Rules - solo and reassert
^solo {user.organteq_rule}$:
	user.organteq_rule_solo(organteq_rule)

^solo {user.organteq_rule} <number_small>$:
	user.organteq_rule_set_level(organteq_rule, number_small)
	user.organteq_rule_solo(organteq_rule)

^reassert {user.organteq_rule}$:
	user.organteq_rule_reassert(organteq_rule)


# Transient rules (invoked by name alone)
^{user.organteq_transient}$:
	user.organteq_transient(organteq_transient)


# Manual context and clearing
^(with|use|using) {user.organteq_manual}$:
	user.organteq_set_manual(organteq_manual)

^(clear|cancel) {user.organteq_manual}$:
	user.organteq_clear(organteq_manual)

^(clear|cancel)$:
	manual = user.organteq_get_manual()
	user.organteq_clear(manual)

^general cancel$:
	user.organteq_clear("pedal")
	user.organteq_clear("choir")
	user.organteq_clear("great")
	user.organteq_clear("swell")


# Undo/redo
^undo$:
	user.organteq_undo()

^redo$:
	user.organteq_redo()


# Sync and GUI
^sync [registration|registrations]$:
	user.organteq_sync()

^show help$:
	user.organteq_show_help()

^hide help$:
	user.organteq_hide_help()

^toggle help$:
	user.organteq_toggle_help()
