:- module(robo_shell, [
	sys_prim/1,
	sys_prim/2,
	sys_prim/3,
	sys_call/1,
	sys_call/2,
	sys_call/3,
	sys_call/4,
	sys_ok/1,
	sys_ok/2,
	sys_ok/3,
	sys_err/1,
	sys_err/2,
	sys_err/3,
	shell_prim/1,
	shell_prim/2,
	shell_prim/3,
	shell_call/1,
	shell_call/2,
	shell_call/3,
	shell_ok/1,
	shell_ok/2,
	shell_err/1,
	shell_err/2
]).

:- use_module(library(robo_stringify)).

child_exec(Command, Options) :-
	(
		option(cwd(Dir), Options) *->
		working_directory(_, Dir) ; true
	),
	exec(Command).

child_wait(_, _, Options) :-
	option(wait(false), Options, true), !.

child_wait(PID, Status, Options) :-
	option(ignore_stopped(true), Options, true), !,
	wait(PID, S),
	( S = stopped(_) *->
		child_wait(PID, Status, Options) ;
		Status = S
	).

child_wait(PID, Status, _) :-
	wait(PID, Status).


sys_prim(Command) :-
	sys_prim(Command, _).
sys_prim(Command, Status) :-
	sys_prim(Command, Status, []).
sys_prim(Command, _, _) :-
	robo:flag(dryrun, 1), !, writef("%d\n", [Command]).

sys_prim(Command, Status, Options) :-
    writef("%d\n", [Command]),
	fork(PID),
	(
		PID = child *->
		child_exec(Command, Options)
		;
		option(pid(PID), Options, _),
		child_wait(PID, Status, Options)
	).

sys_call(FormatName) :-
	sys_call(FormatName, _).
sys_call(FormatName, Status) :-
	sys_call(FormatName, [], Status).
sys_call(FormatName, FormatArguments, Status) :-
	sys_call(FormatName, FormatArguments, Status, []).
sys_call(FormatName, FormatArguments, Status, Options) :-
	stringify(FormatName, NameStr),
	atom_string(Name, NameStr),
	maplist(stringify, FormatArguments, Arguments),
	compound_name_arguments(Command, Name, Arguments),
	sys_prim(Command, Status, Options).

sys_ok(FormatName) :-
	sys_ok(FormatName, []).
sys_ok(FormatName, FormatArguments) :-
	sys_ok(FormatName, FormatArguments, []).
sys_ok(FormatName, FormatArguments, Options) :-
	sys_call(FormatName, FormatArguments, exited(0), Options).

sys_err(FormatName) :-
	sys_err(FormatName, []).
sys_err(FormatName, FormatArguments) :-
	sys_err(FormatName, FormatArguments, []).
sys_err(FormatName, FormatArguments, Options) :-
	sys_call(FormatName, FormatArguments, Status, Options), \+ Status = exited(0).

shell_prim(Script) :-
	shell_prim(Script, _).
shell_prim(Script, Status) :-
	shell_prim(Script, Status, []).
shell_prim(Script, _, _) :-
	robo:flag(dryrun, 1), !, writef("%d\n", [Script]).
shell_prim(Script, Status, Options) :-
    writef("%d\n", [Script]),
	getenv('SHELL', Shell),
	compound_name_arguments(Command, Shell, [ "-c", Script ]),
	sys_prim(Command, Status, Options).

shell_call(FormatScript) :-
	shell_call(FormatScript, _).
shell_call(FormatScript, Status) :-
	shell_call(FormatScript, Status, []).
shell_call(FormatScript, Status, Options) :-
	stringify(FormatScript, Script),
	shell_prim(Script, Status, Options).

shell_ok(FormatScript) :-
	 shell_ok(FormatScript, []).
shell_ok(FormatScript, Options) :-
	shell_call(FormatScript, exited(0), Options).

shell_err(FormatScript) :-
	shell_err(FormatScript, []).
shell_err(FormatScript, Options) :-
	shell_call(FormatScript, exited(N), Options), \+ N = 0.
