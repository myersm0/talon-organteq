% HTTP server and command execution

:- module(server, [
	start/1,
	execute/3,
	load_rules/1,
	load_rules/2,
	set_log_level/1
]).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(filesex)).

:- use_module(config(couplers), [coupler_mapping/6]).

:- use_module(state, [
	element/4,
	engaged/2,
	current_preset/1,
	rule/2,
	rule_level/2,
	owns/3,
	do_engage/2,
	do_disengage/2,
	get_rule_level/2,
	set_rule_level/2,
	division/1,
	manuals/1,
	reset_history/0,
	save_snapshot/1,
	restore_snapshot/2,
	get_snapshot_state/1,
	snapshot/2,
	current_snapshot/1,
	json_to_atom/2
]).

:- use_module(classification, [element_family/3]).

:- use_module(selectors, [
	resolve_selector/3,
	rules_for_preset/2,
	computed_max_level/3
]).

:- use_module(rules, [
	apply_rule/5,
	rule_divisions/2
]).

:- discontiguous execute_term/3.

% ============================================================================
% Logging
% ============================================================================

:- dynamic log_level/1.
log_level(0).

set_log_level(Level) :-
	retractall(log_level(_)),
	assertz(log_level(Level)).

log_command(CommandStr, Actions) :-
	log_level(L), L >= 1,
	!,
	length(Actions, N),
	(L >= 2 -> format(user_error, "[cmd] ~w~n", [CommandStr]) ; true),
	(N > 0 -> log_actions(Actions) ; true).
log_command(_, _).

log_actions([]) :- !.
log_actions(Actions) :-
	format_actions(Actions, Formatted),
	format(user_error, "[rpc] ~w~n", [Formatted]).

format_actions(Actions, Formatted) :-
	maplist(format_action, Actions, Parts),
	atomic_list_concat(Parts, ', ', Formatted).

format_action(set_stop(Div, N, V), F) :-
	(V =:= 1.0 -> Op = '+' ; Op = '-'),
	format(atom(F), "~w~w:~w", [Op, Div, N]).
format_action(set_coupler(N, V), F) :-
	(V =:= 1.0 -> Op = '+' ; Op = '-'),
	format(atom(F), "~wcoupler:~w", [Op, N]).
format_action(set_mono_coupler(N, V), F) :-
	(V =:= 1.0 -> Op = '+' ; Op = '-'),
	format(atom(F), "~wmono:~w", [Op, N]).
format_action(set_tremulant(N, V), F) :-
	(V =:= 1.0 -> Op = '+' ; Op = '-'),
	format(atom(F), "~wtrem:~w", [Op, N]).

% ============================================================================
% Command execution - main interface
% ============================================================================

execute(CommandStr, Actions, State) :-
	catch(
		(
			term_string(Term, CommandStr),
			execute_term(Term, Actions, State)
		),
		Error,
		(
			term_string(Error, ErrStr),
			Actions = [],
			State = _{error: ErrStr}
		)
	).

% ============================================================================
% Stop operations
% ============================================================================

execute_term(engage(Division, Selector), Actions, State) :-
	resolve_selector(Division, Selector, Elements),
	findall(Action, (
		member(N, Elements),
		do_engage(Division, N),
		rpc_action(Division, N, 1.0, Action)
	), Actions),
	save_snapshot(engage),
	get_state_dict(State).

execute_term(disengage(Division, Selector), Actions, State) :-
	resolve_selector(Division, Selector, Elements),
	findall(Action, (
		member(N, Elements),
		do_disengage(Division, N),
		rpc_action(Division, N, 0.0, Action)
	), Actions),
	save_snapshot(disengage),
	get_state_dict(State).

execute_term(toggle(Division, Selector), Actions, State) :-
	resolve_selector(Division, Selector, Elements),
	findall(Action, (
		member(N, Elements),
		(engaged(Division, N) ->
			(do_disengage(Division, N), rpc_action(Division, N, 0.0, Action))
		;   (do_engage(Division, N), rpc_action(Division, N, 1.0, Action))
		)
	), Actions),
	save_snapshot(toggle),
	get_state_dict(State).

execute_term(solo(Division, Selector), Actions, State) :-
	resolve_selector(Division, Selector, KeepElements),
	findall(N, element(Division, N, _, _), AllElements),
	findall(Action, (
		member(N, AllElements),
		(member(N, KeepElements) ->
			(do_engage(Division, N), rpc_action(Division, N, 1.0, Action))
		;   (do_disengage(Division, N), rpc_action(Division, N, 0.0, Action))
		)
	), Actions),
	save_snapshot(solo),
	get_state_dict(State).

execute_term(clear(Division), Actions, State) :-
	findall(N, element(Division, N, _, _), AllElements),
	findall(Action, (
		member(N, AllElements),
		do_disengage(Division, N),
		rpc_action(Division, N, 0.0, Action)
	), Actions),
	save_snapshot(clear),
	get_state_dict(State).

execute_term(clear_all, Actions, State) :-
	findall(Action, (
		division(D),
		element(D, N, _, _),
		do_disengage(D, N),
		rpc_action(D, N, 0.0, Action)
	), Actions),
	save_snapshot(clear_all),
	get_state_dict(State).

% ============================================================================
% Rule operations
% ============================================================================

execute_term(apply(RuleId), Actions, State) :-
	apply_rule(RuleId, none, 1, none, Actions),
	save_snapshot(apply(RuleId)),
	get_state_dict(State).

execute_term(up(RuleId), Actions, State) :-
	apply_rule(RuleId, none, 1, none, Actions),
	save_snapshot(up(RuleId)),
	get_state_dict(State).

execute_term(down(RuleId), Actions, State) :-
	apply_rule(RuleId, none, -1, none, Actions),
	save_snapshot(down(RuleId)),
	get_state_dict(State).

execute_term(level(RuleId, Level), Actions, State) :-
	apply_rule(RuleId, none, none, Level, Actions),
	save_snapshot(level(RuleId, Level)),
	get_state_dict(State).

execute_term(mute(RuleId), Actions, State) :-
	apply_rule(RuleId, mute, none, none, Actions),
	save_snapshot(mute(RuleId)),
	get_state_dict(State).

execute_term(maximize(RuleId), Actions, State) :-
	apply_rule(RuleId, maximize, none, none, Actions),
	save_snapshot(maximize(RuleId)),
	get_state_dict(State).

execute_term(minimize(RuleId), Actions, State) :-
	apply_rule(RuleId, minimize, none, none, Actions),
	save_snapshot(minimize(RuleId)),
	get_state_dict(State).

execute_term(solo_rule(RuleId), Actions, State) :-
	apply_rule(RuleId, solo, none, none, Actions),
	save_snapshot(solo_rule(RuleId)),
	get_state_dict(State).

execute_term(reassert(RuleId), Actions, State) :-
	apply_rule(RuleId, reassert, none, none, Actions),
	save_snapshot(reassert(RuleId)),
	get_state_dict(State).

% ============================================================================
% Coupler operations
% ============================================================================

execute_term(couple(Source, Dest), Actions, State) :-
	execute_term(couple(Source, Dest, unison), Actions, State).

execute_term(couple(Source, Dest, Transposition), Actions, State) :-
	current_preset(Preset),
	(   coupler_mapping(Preset, Index, Source, Dest, Transposition, _)
	->  do_engage(coupler, Index),
	    rpc_action(coupler, Index, 1.0, Action),
	    Actions = [Action]
	;   Actions = [],
	    format(user_error, "Warning: No coupler ~w -> ~w (~w) for preset ~w~n",
	        [Source, Dest, Transposition, Preset])
	),
	save_snapshot(couple(Source, Dest)),
	get_state_dict(State).

execute_term(decouple(Source, Dest), Actions, State) :-
	execute_term(decouple(Source, Dest, unison), Actions, State).

execute_term(decouple(Source, Dest, Transposition), Actions, State) :-
	current_preset(Preset),
	(   coupler_mapping(Preset, Index, Source, Dest, Transposition, _)
	->  do_disengage(coupler, Index),
	    rpc_action(coupler, Index, 0.0, Action),
	    Actions = [Action]
	;   Actions = []
	),
	save_snapshot(decouple(Source, Dest)),
	get_state_dict(State).

execute_term(couple_index(Index), Actions, State) :-
	do_engage(coupler, Index),
	rpc_action(coupler, Index, 1.0, Action),
	Actions = [Action],
	save_snapshot(couple_index(Index)),
	get_state_dict(State).

execute_term(decouple_index(Index), Actions, State) :-
	do_disengage(coupler, Index),
	rpc_action(coupler, Index, 0.0, Action),
	Actions = [Action],
	save_snapshot(decouple_index(Index)),
	get_state_dict(State).

execute_term(decouple_all, Actions, State) :-
	findall(Action, (
		element(coupler, N, _, coupler),
		do_disengage(coupler, N),
		rpc_action(coupler, N, 0.0, Action)
	), CouplerActions),
	findall(Action, (
		element(mono_coupler, N, _, mono_coupler),
		do_disengage(mono_coupler, N),
		rpc_action(mono_coupler, N, 0.0, Action)
	), MonoActions),
	append(CouplerActions, MonoActions, Actions),
	save_snapshot(decouple_all),
	get_state_dict(State).

% ============================================================================
% Mono coupler operations
% ============================================================================

execute_term(mono_couple(Index), Actions, State) :-
	do_engage(mono_coupler, Index),
	rpc_action(mono_coupler, Index, 1.0, Action),
	Actions = [Action],
	save_snapshot(mono_couple(Index)),
	get_state_dict(State).

execute_term(mono_decouple(Index), Actions, State) :-
	do_disengage(mono_coupler, Index),
	rpc_action(mono_coupler, Index, 0.0, Action),
	Actions = [Action],
	save_snapshot(mono_decouple(Index)),
	get_state_dict(State).

% ============================================================================
% Tremulant operations
% ============================================================================

execute_term(tremulant(Index), Actions, State) :-
	execute_term(tremulant_toggle(Index), Actions, State).

execute_term(tremulant_on(Index), Actions, State) :-
	do_engage(tremulant, Index),
	rpc_action(tremulant, Index, 1.0, Action),
	Actions = [Action],
	save_snapshot(tremulant_on(Index)),
	get_state_dict(State).

execute_term(tremulant_off(Index), Actions, State) :-
	do_disengage(tremulant, Index),
	rpc_action(tremulant, Index, 0.0, Action),
	Actions = [Action],
	save_snapshot(tremulant_off(Index)),
	get_state_dict(State).

execute_term(tremulant_toggle(Index), Actions, State) :-
	(engaged(tremulant, Index) ->
		execute_term(tremulant_off(Index), Actions, State)
	;   execute_term(tremulant_on(Index), Actions, State)
	).

% ============================================================================
% History operations
% ============================================================================

execute_term(undo, Actions, State) :-
	current_snapshot(CurrentId),
	CurrentId > 0,
	!,
	PrevId is CurrentId - 1,
	restore_snapshot(PrevId, RestoredState),
	compute_restore_actions(RestoredState, Actions),
	get_state_dict(State).

execute_term(undo, [], State) :-
	get_state_dict(State).

execute_term(redo, Actions, State) :-
	current_snapshot(CurrentId),
	NextId is CurrentId + 1,
	snapshot(NextId, _),
	!,
	restore_snapshot(NextId, RestoredState),
	compute_restore_actions(RestoredState, Actions),
	get_state_dict(State).

execute_term(redo, [], State) :-
	get_state_dict(State).

compute_restore_actions(state(EngagedList, _, _), Actions) :-
	findall(Action, (
		member(engaged(D, N), EngagedList),
		rpc_action(D, N, 1.0, Action)
	), EngageActions),
	findall(Action, (
		element(D, N, _, _),
		\+ member(engaged(D, N), EngagedList),
		rpc_action(D, N, 0.0, Action)
	), DisengageActions),
	append(EngageActions, DisengageActions, Actions).

% ============================================================================
% Query operations
% ============================================================================

execute_term(get_state, [], State) :-
	get_state_dict(State).

execute_term(list_rules, [], State) :-
	current_preset(Preset),
	rules_for_preset(Preset, Rules),
	State = _{rules: Rules, preset: Preset}.

execute_term(list_rules(Preset), [], State) :-
	rules_for_preset(Preset, Rules),
	State = _{rules: Rules, preset: Preset}.

execute_term(get_rule_info(RuleId), [], State) :-
	rule(RuleId, Type),
	current_preset(Preset),
	(computed_max_level(RuleId, Preset, MaxLevel) -> true ; MaxLevel = 1),
	(state:antonym(RuleId, Antonym) -> true ; Antonym = null),
	get_rule_level(RuleId, CurrentLevel),
	rule_divisions(RuleId, Divisions),
	State = _{
		rule: RuleId,
		type: Type,
		max_level: MaxLevel,
		antonym: Antonym,
		current_level: CurrentLevel,
		divisions: Divisions
	}.

execute_term(resolve(Division, Selector), [], State) :-
	resolve_selector(Division, Selector, Elements),
	State = _{division: Division, elements: Elements}.

% ============================================================================
% Sync operation (receives state from Python/Organteq)
% ============================================================================

execute_term(sync(Preset, Elements, Engaged), [], State) :-
	retractall(current_preset(_)),
	assertz(current_preset(Preset)),
	retractall(element(_, _, _, _)),
	forall(member(element(Div, Num, Name, Type), Elements),
		assertz(element(Div, Num, Name, Type))
	),
	log_unclassified_stops,
	retractall(engaged(_, _)),
	forall(member(engaged(Div, Num), Engaged),
		assertz(engaged(Div, Num))
	),
	retractall(owns(_, _, _)),
	retractall(rule_level(_, _)),
	reset_history,
	save_snapshot(sync),
	get_state_dict(State).

log_unclassified_stops :-
	findall(Division-Number-Name, (
		element(Division, Number, Name, stop),
		Name \= '',
		\+ element_family(Division, Number, _)
	), Unclassified),
	(Unclassified \= [] ->
		format(user_error, "Warning: Unclassified stops: ~w~n", [Unclassified])
	;   true
	).

% ============================================================================
% Fallback for unknown commands
% ============================================================================

execute_term(Command, [], _{error: "Unknown command", command: CommandStr}) :-
	term_string(Command, CommandStr).

% ============================================================================
% RPC action generation
% ============================================================================

rpc_action(Division, Number, Value, Action) :-
	element(Division, Number, _, Type),
	!,
	rpc_action_for_type(Type, Division, Number, Value, Action).

rpc_action(Division, Number, _, _) :-
	format(user_error, "Warning: rpc_action for non-existent element ~w/~w~n", [Division, Number]),
	fail.

rpc_action_for_type(stop, Division, Number, Value, set_stop(Division, Number, Value)).
rpc_action_for_type(coupler, _, Number, Value, set_coupler(Number, Value)).
rpc_action_for_type(mono_coupler, _, Number, Value, set_mono_coupler(Number, Value)).
rpc_action_for_type(tremulant, _, Number, Value, set_tremulant(Number, Value)).

% ============================================================================
% State dict for JSON responses
% ============================================================================

get_state_dict(State) :-
	(current_preset(Preset) -> true ; Preset = ''),
	findall(_{division: D, number: N}, engaged(D, N), Engaged),
	findall(_{rule: R, level: L}, rule_level(R, L), RuleLevels),
	State = _{preset: Preset, engaged: Engaged, rule_levels: RuleLevels}.

% ============================================================================
% Rule loading from directory
% ============================================================================

load_rules(Dir) :-
	load_rules(Dir, "*.pl").

load_rules(Dir, Glob) :-
	(atom(Dir) -> DirAtom = Dir ; atom_string(DirAtom, Dir)),
	(exists_directory(DirAtom) ->
		(atom(Glob) -> GlobAtom = Glob ; atom_string(GlobAtom, Glob)),
		directory_files(DirAtom, Files),
		forall((
			member(File, Files),
			wildcard_match(GlobAtom, File),
			directory_file_path(DirAtom, File, Path)
		), (
			catch(consult(Path), E, (
				format(user_error, "Warning: failed to load ~w: ~w~n", [Path, E])
			))
		))
	;   format(user_error, "Warning: rules directory does not exist: ~w~n", [DirAtom])
	).

% ============================================================================
% HTTP Server
% ============================================================================

:- http_handler(root(execute), handle_execute, []).
:- http_handler(root(sync), handle_sync, []).
:- http_handler(root(state), handle_state, []).
:- http_handler(root(load), handle_load, []).
:- http_handler(root(query), handle_query, []).

start(Port) :-
	http_server(http_dispatch, [port(Port)]),
	format("% Server running on port ~w. Press Ctrl+C to stop.~n", [Port]),
	thread_get_message(_).  % Block forever

handle_execute(Request) :-
	http_read_json_dict(Request, Dict),
	CommandStr = Dict.command,
	execute(CommandStr, Actions, State),
	log_command(CommandStr, Actions),
	organteq:apply_rpc_actions(Actions),
	actions_to_json(Actions, JsonActions),
	Reply = _{status: ok, actions: JsonActions, state: State},
	reply_json_dict(Reply).

handle_sync(_Request) :-
	catch(
		(
			organteq:organteq_sync,
			get_state_dict(StateDict),
			reply_json_dict(_{status: ok, state: StateDict})
		),
		Error,
		(
			term_string(Error, ErrStr),
			reply_json_dict(_{status: error, message: ErrStr})
		)
	).

handle_state(_Request) :-
	get_state_dict(StateDict),
	reply_json_dict(_{status: ok, state: StateDict}).

handle_load(Request) :-
	http_read_json_dict(Request, Dict),
	File = Dict.file,
	catch(
		(consult(File), reply_json_dict(_{status: ok})),
		Error,
		(term_string(Error, ErrStr), reply_json_dict(_{status: error, message: ErrStr}))
	).

handle_query(Request) :-
	http_read_json_dict(Request, Dict),
	QueryStr = Dict.query,
	catch(
		(
			read_term_from_atom(QueryStr, Goal, [variable_names(VarNames)]),
			(call(Goal) ->
				maplist(binding_to_json, VarNames, Bindings),
				dict_create(BindingsDict, bindings, Bindings),
				reply_json_dict(_{status: ok, success: true, bindings: BindingsDict})
			;
				reply_json_dict(_{status: ok, success: false})
			)
		),
		Error,
		(
			term_string(Error, ErrStr),
			reply_json_dict(_{status: error, message: ErrStr})
		)
	).

binding_to_json(Name=Value, Name-JsonValue) :-
	term_to_json_value(Value, JsonValue).

term_to_json_value(Term, Term) :-
	(atom(Term) ; number(Term) ; string(Term)), !.
term_to_json_value(Term, JsonList) :-
	is_list(Term), !,
	maplist(term_to_json_value, Term, JsonList).
term_to_json_value(Term, String) :-
	compound(Term), !,
	term_string(Term, String).
term_to_json_value(Term, null) :-
	var(Term), !.

actions_to_json(Actions, JsonActions) :-
	maplist(action_to_json, Actions, JsonActions).

action_to_json(set_stop(Division, Number, Value),
	_{type: set_stop, division: Division, number: Number, value: Value}).
action_to_json(set_coupler(Number, Value),
	_{type: set_coupler, number: Number, value: Value}).
action_to_json(set_mono_coupler(Number, Value),
	_{type: set_mono_coupler, number: Number, value: Value}).
action_to_json(set_tremulant(Number, Value),
	_{type: set_tremulant, number: Number, value: Value}).
