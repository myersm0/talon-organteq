% server.pl - HTTP server and command execution

:- module(server, [
	server/1,
	execute/3,
	load_state/1,
	get_state/1,
	load_rules_from_dir/2
]).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(filesex)).

:- use_module(state, [
	element/4, engaged/2, current_preset/1,
	rule_level/2, owns/3, division/1,
	do_engage/2, do_disengage/2,
	reset_history/0, save_snapshot/1, restore_snapshot/2, get_current_state/1,
	json_to_atom/2, get_dict/4
]).
:- use_module(selectors, [resolve_selector/3, dict_to_selector/2]).
:- use_module(rules, [apply_rule_impl/6, resolve_divisions/2]).

:- discontiguous execute_command/4.

% ============================================================================
% Command execution
% ============================================================================

execute(Command, Args, Result) :-
	catch(
		execute_command(Command, Args, Actions, NewState),
		Error,
		(Result = error(Error))
	),
	(nonvar(Result) -> true ; Result = ok(Actions, NewState)), !.

execute(Command, Args, error(command_failed(Command, Args))).

% --- engage ---
execute_command(engage, Args, Actions, State) :-
	get_division(Args, Division),
	get_dict(selector, Args, SelectorDict),
	dict_to_selector(SelectorDict, Selector),
	resolve_selector(Division, Selector, Elements),
	findall(Action, (
		member(N, Elements),
		do_engage(Division, N),
		rpc_action(Division, N, 1.0, Action)
	), Actions),
	save_snapshot(engage),
	get_current_state(State).

% --- disengage ---
execute_command(disengage, Args, Actions, State) :-
	get_division(Args, Division),
	get_dict(selector, Args, SelectorDict),
	dict_to_selector(SelectorDict, Selector),
	resolve_selector(Division, Selector, Elements),
	findall(Action, (
		member(N, Elements),
		do_disengage(Division, N),
		rpc_action(Division, N, 0.0, Action)
	), Actions),
	save_snapshot(disengage),
	get_current_state(State).

% --- toggle ---
execute_command(toggle, Args, Actions, State) :-
	get_division(Args, Division),
	get_dict(selector, Args, SelectorDict),
	dict_to_selector(SelectorDict, Selector),
	resolve_selector(Division, Selector, Elements),
	findall(Action, (
		member(N, Elements),
		(engaged(Division, N) ->
			(do_disengage(Division, N), rpc_action(Division, N, 0.0, Action))
		;   (do_engage(Division, N), rpc_action(Division, N, 1.0, Action))
		)
	), Actions),
	save_snapshot(toggle),
	get_current_state(State).

% --- solo ---
execute_command(solo, Args, Actions, State) :-
	get_division(Args, Division),
	get_dict(selector, Args, SelectorDict),
	dict_to_selector(SelectorDict, Selector),
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
	get_current_state(State).

% --- clear ---
execute_command(clear, Args, Actions, State) :-
	get_division(Args, Division),
	findall(N, element(Division, N, _, _), AllElements),
	findall(Action, (
		member(N, AllElements),
		do_disengage(Division, N),
		rpc_action(Division, N, 0.0, Action)
	), Actions),
	save_snapshot(clear),
	get_current_state(State).

% --- clear_all ---
execute_command(clear_all, _, Actions, State) :-
	findall(Action, (
		division(D),
		element(D, N, _, _),
		do_disengage(D, N),
		rpc_action(D, N, 0.0, Action)
	), Actions),
	save_snapshot(clear_all),
	get_current_state(State).

% --- apply_rule ---
execute_command(apply_rule, Args, Actions, State) :-
	get_rule_id(Args, RuleId),
	get_action_type(Args, ActionType),
	get_dict(delta, Args, Delta, none),
	get_dict(level, Args, Level, none),
	get_dict(divisions, Args, DivisionsRaw, manuals),
	resolve_divisions(DivisionsRaw, TargetDivisions),
	apply_rule_impl(RuleId, ActionType, Delta, Level, TargetDivisions, Actions),
	save_snapshot(apply_rule(RuleId)),
	get_current_state(State).

% --- undo ---
execute_command(undo, _, Actions, State) :-
	current_snapshot(CurrentId),
	CurrentId > 0,
	PrevId is CurrentId - 1,
	restore_snapshot(PrevId, Actions),
	get_current_state(State).

execute_command(undo, _, [], State) :-
	get_current_state(State).

% --- redo ---
execute_command(redo, _, Actions, State) :-
	current_snapshot(CurrentId),
	NextId is CurrentId + 1,
	snapshot(NextId, _),
	restore_snapshot(NextId, Actions),
	get_current_state(State).

execute_command(redo, _, [], State) :-
	get_current_state(State).

% --- sync (receive state from Organteq) ---
execute_command(sync, Args, [], State) :-
	get_dict(preset, Args, PresetStr),
	get_dict(elements, Args, ElementsList),
	get_dict(engaged, Args, EngagedList),
	(atom(PresetStr) -> Preset = PresetStr ; atom_string(Preset, PresetStr)),
	retractall(current_preset(_)),
	assertz(current_preset(Preset)),
	retractall(element(_, _, _, _)),
	forall(member(E, ElementsList), (
		json_to_atom(E.division, Division),
		E.number = Number,
		json_to_atom(E.name, Name),
		json_to_atom(E.type, Type),
		assertz(element(Division, Number, Name, Type))
	)),
	retractall(engaged(_, _)),
	forall(member(Eng, EngagedList), (
		json_to_atom(Eng.division, D),
		Eng.number = N,
		assertz(engaged(D, N))
	)),
	retractall(owns(_, _, _)),
	retractall(rule_level(_, _)),
	reset_history,
	save_snapshot(sync),
	get_current_state(State).

% --- get_state ---
execute_command(get_state, _, [], State) :-
	get_current_state(State).

% --- assert_facts (for testing) ---
execute_command(assert_facts, Args, [], State) :-
	get_dict(facts, Args, Facts),
	forall(member(FactStr, Facts), (
		(atom(FactStr) -> atom_string(FactAtom, FactStr), term_string(Fact, FactAtom)
		;   term_string(Fact, FactStr)),
		assertz(Fact)
	)),
	get_current_state(State).

% --- retract_facts (for testing) ---
execute_command(retract_facts, Args, [], State) :-
	get_dict(facts, Args, Facts),
	forall(member(FactStr, Facts), (
		(atom(FactStr) -> atom_string(FactAtom, FactStr), term_string(Fact, FactAtom)
		;   term_string(Fact, FactStr)),
		retractall(Fact)
	)),
	get_current_state(State).

% ============================================================================
% Helper predicates
% ============================================================================

get_division(Args, Division) :-
	get_dict(division, Args, DivValue),
	json_to_atom(DivValue, Division).

get_rule_id(Args, RuleId) :-
	get_dict(rule, Args, RuleValue),
	json_to_atom(RuleValue, RuleId).

get_action_type(Args, ActionType) :-
	get_dict(action, Args, ActionValue, none),
	(ActionValue = none -> ActionType = none ; json_to_atom(ActionValue, ActionType)).

% RPC action generation
rpc_action(Division, Number, Value, Action) :-
	element(Division, Number, _, Type),
	rpc_action_for_type(Type, Division, Number, Value, Action).

rpc_action_for_type(stop, Division, Number, Value, set_stop(Division, Number, Value)).
rpc_action_for_type(coupler, _, Number, Value, set_coupler(Number, Value)).
rpc_action_for_type(mono_coupler, _, Number, Value, set_mono_coupler(Number, Value)).
rpc_action_for_type(tremulant, _, Number, Value, set_tremulant(Number, Value)).

% ============================================================================
% State loading/saving (for Python bridge)
% ============================================================================

load_state(StateDict) :-
	execute_command(sync, StateDict, _, _).

get_state(StateDict) :-
	current_preset(Preset),
	findall(_{division: D, number: N, name: Name, type: Type},
		element(D, N, Name, Type), Elements),
	findall(_{division: D, number: N}, engaged(D, N), Engaged),
	findall(_{rule: R, level: L}, rule_level(R, L), RuleLevels),
	StateDict = _{
		preset: Preset,
		elements: Elements,
		engaged: Engaged,
		rule_levels: RuleLevels
	}.

% ============================================================================
% Rule loading from directory
% ============================================================================

load_rules_from_dir(Dir, Glob) :-
	atom_string(DirAtom, Dir),
	exists_directory(DirAtom),
	atom_string(GlobAtom, Glob),
	directory_files(DirAtom, Files),
	forall((
		member(File, Files),
		wildcard_match(GlobAtom, File),
		directory_file_path(DirAtom, File, Path)
	), (
		catch(consult(Path), E, (
			format("Warning: failed to load ~w: ~w~n", [Path, E])
		))
	)).

load_rules_from_dir(_, _).  % Silently succeed if dir doesn't exist

% ============================================================================
% HTTP Server
% ============================================================================

:- http_handler(root(execute), handle_execute, []).
:- http_handler(root(state), handle_state, []).
:- http_handler(root(load), handle_load, []).

server(Port) :-
	http_server(http_dispatch, [port(Port)]).

handle_execute(Request) :-
	http_read_json_dict(Request, Dict),
	atom_string(Command, Dict.command),
	(get_dict(args, Dict, Args) -> true ; Args = _{}),
	execute(Command, Args, Result),
	(Result = ok(Actions, State) ->
		actions_to_json(Actions, JsonActions),
		state_to_json(State, JsonState),
		Reply = _{status: ok, actions: JsonActions, state: JsonState}
	; Result = error(Error) ->
		term_string(Error, ErrorStr),
		Reply = _{status: error, message: ErrorStr}
	;   Reply = _{status: error, message: "Unknown error"}
	),
	reply_json_dict(Reply).

handle_state(_Request) :-
	get_state(StateDict),
	reply_json_dict(_{status: ok, state: StateDict}).

handle_load(Request) :-
	http_read_json_dict(Request, Dict),
	File = Dict.file,
	catch(
		(consult(File), reply_json_dict(_{status: ok})),
		Error,
		(term_string(Error, ErrStr), reply_json_dict(_{status: error, message: ErrStr}))
	).

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

state_to_json(state(Engaged, RuleLevels, _), Json) :-
	findall(_{division: D, number: N}, member(engaged(D, N), Engaged), EngagedJson),
	findall(_{rule: R, level: L}, member(rule_level(R, L), RuleLevels), RulesJson),
	Json = _{engaged: EngagedJson, rule_levels: RulesJson}.
