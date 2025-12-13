% JSON-RPC client for Organteq
%
% Provides communication with Organteq's JSON-RPC server.

:- module(organteq, [
	organteq_call/3,
	organteq_available/0,
	organteq_get_preset/1,
	organteq_get_stop_names/1,
	organteq_get_stop_value/3,
	organteq_set_stop/3,
	organteq_set_coupler/2,
	organteq_set_mono_coupler/2,
	organteq_set_tremulant/2,
	organteq_sync/0,
	execute_live/2,
	execute_live/3,
	apply_rpc_actions/1,
	start_polling/0,
	stop_polling/0,
	set_poll_interval/1
]).

:- use_module(library(http/http_open)).
:- use_module(library(http/http_json)).
:- use_module(library(http/json)).

:- use_module(state, [
	element/4,
	engaged/2,
	current_preset/1,
	do_engage/2,
	do_disengage/2,
	division/1,
	manuals/1
]).

:- use_module(server, [execute/3]).

% ============================================================================
% Configuration
% ============================================================================

organteq_endpoint('http://127.0.0.1:8081/jsonrpc').

:- dynamic poll_interval/1.
poll_interval(5).

manual_number(pedal, "1").
manual_number(choir, "2").
manual_number(great, "3").
manual_number(swell, "4").

manual_name("1", pedal).
manual_name("2", choir).
manual_name("3", great).
manual_name("4", swell).

max_stops(pedal, 10).
max_stops(choir, 10).
max_stops(great, 20).
max_stops(swell, 10).

% ============================================================================
% Low-level JSON-RPC
% ============================================================================

organteq_call(Method, Params, Result) :-
	organteq_endpoint(URL),
	Payload = _{
		jsonrpc: "2.0",
		id: 1,
		method: Method,
		params: Params
	},
	catch(
		(
			http_open(URL, Stream, [
				method(post),
				post(json(Payload)),
				request_header('Content-Type'='application/json')
			]),
			json_read_dict(Stream, Response),
			close(Stream),
			(get_dict(result, Response, Result) -> true ; Result = null)
		),
		Error,
		(
			format(user_error, "Organteq RPC error: ~w~n", [Error]),
			Result = null
		)
	).

organteq_available :-
	catch(
		(
			organteq_endpoint(URL),
			http_open(URL, Stream, [
				method(post),
				post(json(_{jsonrpc: "2.0", id: 1, method: "getInfo", params: []})),
				request_header('Content-Type'='application/json'),
				timeout(2)
			]),
			close(Stream)
		),
		_,
		fail
	).

% ============================================================================
% High-level Organteq queries
% ============================================================================

organteq_get_preset(Preset) :-
	organteq_call("getInfo", [], Result),
	Result \= null,
	Result = [Info|_],
	get_dict(current_preset, Info, PresetDict),
	get_dict(name, PresetDict, Preset).

organteq_get_stop_names(StopNames) :-
	organteq_call("getStopNames", [], Result),
	Result \= null,
	findall(ManualNum-Names, (
		nth0(Idx, Result, Names),
		ManualNum is Idx + 1
	), StopNames).

organteq_get_stop_value(ManualNum, StopNum, Value) :-
	format(atom(ParamId), "Stop[~w][~w].Switch", [ManualNum, StopNum]),
	organteq_call("getParameters", [[_{id: ParamId}]], Result),
	Result \= null,
	Result = [ParamResult|_],
	get_dict(normalized_value, ParamResult, Value).

% ============================================================================
% High-level Organteq setters
% ============================================================================

organteq_set_stop(Division, Number, Value) :-
	manual_number(Division, ManualNum),
	!,
	format(atom(ParamId), "Stop[~w][~w].Switch", [ManualNum, Number]),
	organteq_call("setParameters", [[_{id: ParamId, normalized_value: Value}]], _).

organteq_set_stop(ManualNum, Number, Value) :-
	atom(ManualNum),
	format(atom(ParamId), "Stop[~w][~w].Switch", [ManualNum, Number]),
	organteq_call("setParameters", [[_{id: ParamId, normalized_value: Value}]], _).

organteq_set_coupler(Index, Value) :-
	format(atom(ParamId), "Coupler Switch[~w]", [Index]),
	organteq_call("setParameters", [[_{id: ParamId, normalized_value: Value}]], _).

organteq_set_mono_coupler(Index, Value) :-
	format(atom(ParamId), "Mono Coupler Switch[~w]", [Index]),
	organteq_call("setParameters", [[_{id: ParamId, normalized_value: Value}]], _).

organteq_set_tremulant(Index, Value) :-
	format(atom(ParamId), "Tremulant Switch[~w]", [Index]),
	organteq_call("setParameters", [[_{id: ParamId, normalized_value: Value}]], _).

% ============================================================================
% Sync: populate Prolog state from Organteq
% ============================================================================

organteq_sync :-
	organteq_get_preset(PresetStr),
	(string(PresetStr) -> atom_string(Preset, PresetStr) ; Preset = PresetStr),
	organteq_get_stop_names(StopNamesList),
	retractall(state:element(_, _, _, _)),
	retractall(state:engaged(_, _)),
	retractall(state:current_preset(_)),
	assertz(state:current_preset(Preset)),
	sync_all_manuals(StopNamesList),
	sync_couplers,
	sync_mono_couplers,
	sync_tremulants,
	format(user_error, "Synced with Organteq. Preset: ~w~n", [Preset]).

sync_all_manuals([]).
sync_all_manuals([ManualNum-Names|Rest]) :-
	number(ManualNum),
	number_string(ManualNum, ManualNumStr),
	(manual_name(ManualNumStr, Division) -> true ; fail),
	!,
	max_stops(Division, MaxStops),
	sync_manual_stops(Division, ManualNum, Names, 1, MaxStops),
	sync_all_manuals(Rest).

sync_all_manuals([_|Rest]) :-
	sync_all_manuals(Rest).

sync_manual_stops(_, _, _, N, Max) :- N > Max, !.
sync_manual_stops(Division, ManualNum, Names, N, Max) :-
	Idx is N - 1,
	(nth0(Idx, Names, Name) -> true ; Name = ""),
	atom_string(NameAtom, Name),
	assertz(state:element(Division, N, NameAtom, stop)),
	organteq_get_stop_value(ManualNum, N, Value),
	(Value >= 0.5 -> assertz(state:engaged(Division, N)) ; true),
	N1 is N + 1,
	sync_manual_stops(Division, ManualNum, Names, N1, Max).

sync_couplers :-
	forall(between(1, 6, I), (
		format(atom(Name), "Coupler ~w", [I]),
		assertz(state:element(coupler, I, Name, coupler)),
		format(atom(ParamId), "Coupler Switch[~w]", [I]),
		(organteq_get_param_value(ParamId, Value), Value >= 0.5
			-> assertz(state:engaged(coupler, I))
			; true
		)
	)).

sync_mono_couplers :-
	forall(between(1, 4, I), (
		format(atom(Name), "Mono Coupler ~w", [I]),
		assertz(state:element(mono_coupler, I, Name, mono_coupler)),
		format(atom(ParamId), "Mono Coupler Switch[~w]", [I]),
		(organteq_get_param_value(ParamId, Value), Value >= 0.5
			-> assertz(state:engaged(mono_coupler, I))
			; true
		)
	)).

sync_tremulants :-
	forall(between(1, 4, I), (
		format(atom(Name), "Tremulant ~w", [I]),
		assertz(state:element(tremulant, I, Name, tremulant)),
		format(atom(ParamId), "Tremulant Switch[~w]", [I]),
		(organteq_get_param_value(ParamId, Value), Value >= 0.5
			-> assertz(state:engaged(tremulant, I))
			; true
		)
	)).

organteq_get_param_value(ParamId, Value) :-
	organteq_call("getParameters", [[_{id: ParamId}]], Result),
	Result \= null,
	Result = [ParamResult|_],
	get_dict(normalized_value, ParamResult, Value).

% ============================================================================
% Polling for preset changes
% ============================================================================

:- dynamic polling_thread/1.

start_polling :-
	polling_thread(T), is_thread(T), thread_property(T, status(running)),
	!,
	format("Polling already running.~n").
start_polling :-
	retractall(polling_thread(_)),
	thread_create(poll_loop, T, [detached(false)]),
	assertz(polling_thread(T)),
	poll_interval(I),
	format("Started polling every ~w seconds.~n", [I]).

stop_polling :-
	polling_thread(T),
	catch(thread_signal(T, throw(stop_polling)), _, true),
	catch(thread_join(T, _), _, true),
	retractall(polling_thread(_)),
	format("Polling stopped.~n"),
	!.
stop_polling :-
	format("Polling not running.~n").

set_poll_interval(Seconds) :-
	retractall(poll_interval(_)),
	assertz(poll_interval(Seconds)),
	format("Poll interval set to ~w seconds.~n", [Seconds]).

poll_loop :-
	catch(poll_loop_, stop_polling, true).

poll_loop_ :-
	poll_interval(I),
	sleep(I),
	check_preset_change,
	poll_loop_.

check_preset_change :-
	(state:current_preset(CurrentPreset) -> true ; CurrentPreset = ''),
	(organteq_get_preset(LivePresetRaw) -> true ; LivePresetRaw = ''),
	(string(LivePresetRaw) -> atom_string(LivePreset, LivePresetRaw) ; LivePreset = LivePresetRaw),
	(CurrentPreset \= LivePreset
		-> (format("Preset changed: ~w -> ~w~n", [CurrentPreset, LivePreset]),
		    organteq_sync)
		; true
	).

% ============================================================================
% Execute with live RPC: run command and apply actions to Organteq
% ============================================================================

execute_live(CommandStr, State) :-
	execute_live(CommandStr, _, State).

execute_live(CommandStr, Actions, State) :-
	execute(CommandStr, Actions, State),
	apply_rpc_actions(Actions).

apply_rpc_actions([]).
apply_rpc_actions([Action|Rest]) :-
	apply_rpc_action(Action),
	apply_rpc_actions(Rest).

apply_rpc_action(set_stop(Division, Number, Value)) :-
	!,
	organteq_set_stop(Division, Number, Value).

apply_rpc_action(set_coupler(Number, Value)) :-
	!,
	organteq_set_coupler(Number, Value).

apply_rpc_action(set_mono_coupler(Number, Value)) :-
	!,
	organteq_set_mono_coupler(Number, Value).

apply_rpc_action(set_tremulant(Number, Value)) :-
	!,
	organteq_set_tremulant(Number, Value).

apply_rpc_action(_).
