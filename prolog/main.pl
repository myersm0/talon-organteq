% main.pl - Entry point for talon-organteq Prolog server
%
% Usage:
%   swipl -g "consult('main.pl'), server(5000)."
%
% Or with user rules:
%   swipl -g "consult('main.pl'), load_rules_from_dir('/path/to/rules', '*.pl'), server(5000)."

:- use_module(state).
:- use_module(classification).
:- use_module(selectors).
:- use_module(rules).
:- use_module(server, [execute/3, load_state/1, get_state/1]).
:- use_module(couplers).

% Export key predicates at top level
server(Port) :- server:server(Port).
load_rules_from_dir(Dir, Glob) :- server:load_rules_from_dir(Dir, Glob).

:- initialization((
	format("talon-organteq Prolog modules loaded.~n"),
	format("Start server with: server(5000).~n"),
	format("Load user rules with: load_rules_from_dir('/path/to/rules', '*.pl').~n")
)).
