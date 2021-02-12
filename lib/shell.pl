:- module(robo_shell, [
	system/1,
	system/2,
	system/3,
	system_ok/1,
	system_ok/2,
	system_err/1,
	system_err/2
]).

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

system(Command) :-
	system(Command, exited(0)).
system(Command, Status) :-
	system(Command, Status, []).

system(Command, Status, Options) :-
	atomic(Command),
	getenv('SHELL', Shell),
	compound_name_arguments(ShellCommand, Shell, [ "-c", Command ]),
	system(ShellCommand, Status, Options).

system(Command, Status, Options) :-
	compound(Command),
	fork(PID),
	(
		PID = child *->
		child_exec(Command, Options)
		;
		option(pid(PID), Options, _),
		child_wait(PID, Status, Options)
	).

system_ok(Command) :- system_ok(Command, []).
system_ok(Command, Options) :-
	system(Command, exited(0), Options).

system_err(Command) :- system_err(Command, []).
system_err(Command, Options) :-
	system(Command, exited(N), Options),
	\+ N = 0.
