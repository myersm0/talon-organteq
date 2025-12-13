% main.pl - Entry point for talon-organteq Prolog server
%
% Usage:
%   cd prolog && swipl -g "consult('main.pl'), server(5000)."
%
% Or with user rules:
%   cd prolog && swipl -g "consult('main.pl'), load_rules('/path/to/rules'), server(5000)."

:- prolog_load_context(directory, Dir),
   atom_concat(Dir, '/core', CoreDir),
   atom_concat(Dir, '/config', ConfigDir),
   asserta(user:file_search_path(core, CoreDir)),
   asserta(user:file_search_path(config, ConfigDir)).

:- use_module(config(divisions)).
:- use_module(config(families)).
:- use_module(config(aliases)).
:- use_module(config(couplers)).
:- use_module(core(state)).
:- use_module(core(classification)).
:- use_module(core(selectors)).
:- use_module(core(rules)).
:- use_module(core(server), [start/1, execute/3]).
:- use_module(core(helpers)).
:- use_module(core(organteq), [
	organteq_available/0,
	organteq_sync/0,
	execute_live/2,
	execute_live/3,
	apply_rpc_actions/1
]).

server(Port) :- server:start(Port).
load_rules(Dir) :- server:load_rules(Dir).
load_rules(Dir, Glob) :- server:load_rules(Dir, Glob).

% Sync from Organteq
sync :- organteq:organteq_sync.

% Polling for preset changes (delegate to organteq module)
start_polling :- organteq:start_polling.
stop_polling :- organteq:stop_polling.
set_poll_interval(S) :- organteq:set_poll_interval(S).

% Logging
set_log_level(L) :- server:set_log_level(L).

% String-based interface (matches HTTP API)
run(CommandStr) :- organteq:execute_live(CommandStr, _).
run(CommandStr, State) :- organteq:execute_live(CommandStr, State).

% Direct predicates for REPL use
engage(Division, Selector) :-
	format(string(Cmd), "engage(~w, ~w)", [Division, Selector]),
	organteq:execute_live(Cmd, _).

disengage(Division, Selector) :-
	format(string(Cmd), "disengage(~w, ~w)", [Division, Selector]),
	organteq:execute_live(Cmd, _).

toggle(Division, Selector) :-
	format(string(Cmd), "toggle(~w, ~w)", [Division, Selector]),
	organteq:execute_live(Cmd, _).

solo(Division, Selector) :-
	format(string(Cmd), "solo(~w, ~w)", [Division, Selector]),
	organteq:execute_live(Cmd, _).

clear(Division) :-
	format(string(Cmd), "clear(~w)", [Division]),
	organteq:execute_live(Cmd, _).

clear_all :- organteq:execute_live("clear_all", _).

up(RuleId) :-
	format(string(Cmd), "up(~w)", [RuleId]),
	organteq:execute_live(Cmd, _).

down(RuleId) :-
	format(string(Cmd), "down(~w)", [RuleId]),
	organteq:execute_live(Cmd, _).

level(RuleId, Level) :-
	format(string(Cmd), "level(~w, ~w)", [RuleId, Level]),
	organteq:execute_live(Cmd, _).

mute(RuleId) :-
	format(string(Cmd), "mute(~w)", [RuleId]),
	organteq:execute_live(Cmd, _).

:- initialization((
	format("talon-organteq Prolog modules loaded.~n"),
	format("~n"),
	format("Server mode:~n"),
	format("  server(5000).~n"),
	format("  load_rules('/path').~n"),
	format("  load_rules('/path', '*.pl').~n"),
	format("~n"),
	format("Standalone mode (requires Organteq --serve):~n"),
	format("  sync.                           % pull state from Organteq~n"),
	format("  engage(great, numbers([1,2])).  % engage stops~n"),
	format("  engage(great, family(reed)).    % engage by family~n"),
	format("  toggle(swell, numbers([3])).    % toggle~n"),
	format("  up(foundation).                 % rule up~n"),
	format("  clear(pedal).                   % clear division~n"),
	format("~n"),
	format("Polling (auto-started, checks preset every 5s):~n"),
	format("  stop_polling.                   % stop~n"),
	format("  set_poll_interval(10).          % change interval~n"),
	format("~n"),
	format("Logging:~n"),
	format("  set_log_level(0).               % silent (default)~n"),
	format("  set_log_level(1).               % actions only~n"),
	format("  set_log_level(2).               % commands + actions~n"),
	format("~n"),
	ignore(organteq:start_polling)
)).
