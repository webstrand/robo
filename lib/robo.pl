:- module(robo, []).

main :-
	current_prolog_flag(argv, Argv),
	parse_user_flags(Argv, Robofile),
	(
		catch(@(consult(Robofile), user), E, err_consult(E)),
		parse_user_commands(Argv, CommandsList),
		catch(execute(CommandsList), E, err_execute(E)),
		done(0)
		;
		true
	),
	done(1).

execute(CommandsList) :-
	forall(
		member(Command, CommandsList),
		Command
	) ; true.

done(Status) :-
	flag(halt, 1), halt(Status) ; true.

% Flags are used to store global state, such as the source file,
% and error handling routines.
:- dynamic flag/2.
flag(halt, 1).
flag(cancel, 1).
flag(dryrun, 0).

:- meta_predicate set_flag(+, ?).
set_flag(Key, Value) :-
	retract(flag(Key, _)),
	assertz(flag(Key, Value)).

% Parse the arguments list for flags to be set or unset.
parse_user_flags([], "./Robofile").

% -f "some_file" Pick a specific Robofile.
parse_user_flags([ '-f', Robofile | Args ], Robofile) :-
	!, parse_user_flags(Args, _).

% -B Prevent cancellation from succeeding.
parse_user_flags([ '-B' | Args ], Robofile) :-
	!, set_flag(cancel, 0),
	parse_user_flags(Args, Robofile).

% -i Enter SWI's interactive interpreter instead of halting.
parse_user_flags([ '-i' | Args ], Robofile) :-
	!, set_flag(halt, 0),
	parse_user_flags(Args, Robofile).

parse_user_flags([ '-d' | Args ], Robofile) :-
	!, set_flag(dryrun, 1),
	parse_user_flags(Args, Robofile).

parse_user_flags([ _ | Args ], Robofile) :-
	parse_user_flags(Args, Robofile).


% Parse the arguments list for a sequence of commands to execute.
parse_user_commands([], []).

parse_user_commands([ '-f', _ | Args], CommandsList) :- parse_user_commands(Args, CommandsList).
parse_user_commands([ '-B' | Args], CommandsList) :- parse_user_commands(Args, CommandsList).
parse_user_commands([ '-i' | Args], CommandsList) :- parse_user_commands(Args, CommandsList).
parse_user_commands([ '-d' | Args], CommandsList) :- parse_user_commands(Args, CommandsList).

% Reset all knowledge of solved targets, requiring that they be solved
% again, if cancellation fails.
parse_user_commands([ '-k' | Args], CommandsList) :-
	!, Command = reset_solved,
	parse_user_commands(Args, Next),
	CommandsList = [ Command | Next ].

% -p causes robo to print out all of the non-private targets. This
% is treated as a command instead of a flag so that the user can
% require solutions to some targets before generating the list.
parse_user_commands([ '-p' | Args], CommandsList) :-
	!, Command = writeln("We should print here!"),
	parse_user_commands(Args, Next),
	CommandsList = [ Command | Next ].

% For all other arguments, assume they're valid prolog terms
% that refer to targets.
parse_user_commands([ String | Args], CommandsList) :-
	!, parse_user_term(String, Recipe),
	Command = solve(Recipe),
	parse_user_commands(Args, Next),
	CommandsList = [ Command | Next ].

:- dynamic solved/1.
reset_solved :- retractall(solved(_)).

solve(N) :- solve(N, []).
solve(N, P) :-
	solved(N) *-> true ;
	(
		member(N, P) *-> throw(cycle) ;
		flag(cancel, 1), user:cancel(N) *-> assertz(solved(N)) ;

		append([N], P, Path),
		forall(
			find_depend(N, D),
			catch(
				solve(D, Path),
				cycle,
				err_cycle(N, D)
			)
		),
		(
			user:recipe(N) *-> assertz(solved(N)) ;
			err_fail(N)
		)
	).

find_depend(N, SingleDependency) :-
	user:depend(N, D),
	(
		is_list(D) *-> member(SingleDependency, D) ;
		D = SingleDependency
	).

err_consult(E) :- print_message(error, E), fail.
err_execute(E) :- print_message(error, E), fail.

err_cycle(N, D) :-
	with_output_to(
		user_error,
		writef("%w: ERROR Dependency cycle with %w detected\n", [ N, D ])
	),
	fail.

err_fail(N) :-
	with_output_to(
		user_error,
		writef("%w: ERROR Failed\n", [ N ])
	),
	fail.

% Utility Predicates
divide_string(String, Sep, Left, Right) :-
	string_code(I, String, Sep), !,
	LeftLen is I - 1,
	sub_string(String, 0, LeftLen, _, Left),
	sub_string(String, I, _, 0, Right).

% Parse a user provided string of a valid prolog term, and parse it
% into an actual term. Variables are immediately unified with their
% original name. So, parse_user_term("foo(Bar)", foo('Bar')).
parse_user_term(String, Term) :-
	read_term_from_atom(String, Term, [ variable_names(Pairs) ]),
	maplist(parse_user_term_name, Pairs, Names),
	maplist(parse_user_term_var, Pairs, Vars),
	Names = Vars.
parse_user_term_name('='(Name, _), Name).
parse_user_term_var('='(_, Var), Var).


