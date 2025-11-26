app: /organteq/i
-

# ============================================================================
# Manual context
# ============================================================================

^(with|use|using) {user.organteq_manual}$:
	user.organteq_set_manual(organteq_manual)


# ============================================================================
# Clear/cancel
# ============================================================================

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


# ============================================================================
# Stop control by number - specific manual
# ============================================================================

^[toggle] {user.organteq_manual} {user.organteq_stop_number}+$:
	user.organteq_toggle(organteq_manual, organteq_stop_number_list)

^(push|disengage) {user.organteq_manual} {user.organteq_stop_number}+$:
	user.organteq_disengage(organteq_manual, organteq_stop_number_list)

^(pull|engage) {user.organteq_manual} {user.organteq_stop_number}+$:
	user.organteq_engage(organteq_manual, organteq_stop_number_list)

^solo {user.organteq_manual} {user.organteq_stop_number}+$:
	user.organteq_solo(organteq_manual, organteq_stop_number_list)


# ============================================================================
# Stop control by number - current manual context
# ============================================================================

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


# ============================================================================
# Stop control by family - specific manual
# ============================================================================

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


# ============================================================================
# Stop control by family - current manual context
# ============================================================================

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


# ============================================================================
# Undo/redo
# ============================================================================

^undo$:
	user.organteq_undo()

^redo$:
	user.organteq_redo()


# ============================================================================
# Utility
# ============================================================================

^sync [registration|registrations]$:
	user.organteq_sync()


# ============================================================================
# Organteq settings panels (keyboard shortcuts)
# ============================================================================

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

