#!/usr/bin/env swipl
:- module(robocli, []).

:- initialization(robocli:main, main).

% We need to load the solver and shell libraries relative to the cli.
:- once(source_file_property(Path, module(robocli)) ; throw(missing_robocli)),
    file_directory_name(Path, Dir),
    atom_concat(Dir, '/lib', Lib),
    assertz(user:file_search_path(robo, Lib)).

:- ensure_loaded(robo(shell)).
:- use_module(robo(solver)).

main(Argv) :-
    parse_args(Argv, Conf),
    ignore(
        (Conf = help, print_help(user_output), halt(0))
    ;   (Conf = version, print_version(user_output), halt(0))
    ;   (Conf = unknown_flag(F), print_unknown_flag(user_error, F), halt(1))
    ),
    Inspect = Conf.get(inspect, silence),
    load_robofile(Conf.get(path, 'Robofile'), user),
    Goals = Conf.get(goals, []),
    forall(
        member(Goal, Goals),
        (
            empty_solved(Solved),
            catch(
                solve(Goal, user, Solved, Inspect),
                solver_error(E),
                (
                    print_solver_error(user_error, Goal, E),
                    halt(1)
                )
            )
        )
    ),
    halt(0).

silence(_).
inspect(X) :- format("~w\n", X).

print_help(Stream) :-
    write(Stream, 'Usage: robo [OPTION]... [--] [GOAL]...\n\n'),
    write(Stream, 'Options:\n'),
    forall(
        help_flag(F, M),
        format(Stream, '   ~a~a\n', [F, M])
    ).

help_flag('-h, --help           ', 'Display this message').
help_flag('-v, --version        ', 'Print version information and exit').
help_flag('-f, --file FILE      ', 'Load FILE instead of "Robofile"').
help_flag('-g, --goal GOAL      ', 'Solve and run GOAL').
help_flag('-G, --goals PATTERN  ', 'Solve and run all goals matching PATTERN').
help_flag('--inspect            ', 'Print debugging information').
help_flag('-- [GOAL]...         ', 'Solve and run the following GOALs').

print_version(Stream) :-
    write(Stream, 'robo-0.0.1\n').

print_unknown_flag(Stream, Flag) :-
    format(Stream, 'invalid option -- \'~a\'\n', [ Flag ]).

print_solver_error(Stream, Goal, unsolvable(Failed)) :-
    format(Stream, 'Failed to solve ~w: No solutions availble for ~w\n', [Goal, Failed]).

print_solver_error(Stream, Goal, cycle(Ancestors)) :-
    format(Stream, 'Failed to solve ~w: Cycle detected ~w\n', [Goal, Ancestors]).

parse_args(Args, Config) :- parse_args(Args, _{}, Config).

%! parse_args(+Args, +Cfg, -Cfgʹ) is semidet
%
% Parse arguments, mutating Cfg into Cfgʹ
parse_args([], Cfg, Cfg).
parse_args([H | T], Cfg, Cfgʹ) :-
    parse_args_flag([H | T], Cfg, Next, Tail),
    parse_args(Tail, Next, Cfgʹ).
parse_args([Flag | _], _, unknown_flag(Flag)) :-
    atom_concat('-', _, Flag),
    \+ clause(parse_args_flag([ Flag | _ ], _, _, _), _).
parse_args([Atom | Tail], Cfg, Cfgʹ) :-
    \+ atom_concat('-', _, Atom),
    parse_ground_goal(Atom, Cfg, Next),
    parse_args(Tail, Next, Cfgʹ).

%! parse_args_flag(+Args, +Cfg, -Cfgʹ, ?Tail) is semidet
%
% Parse a flag and any number of following arguments from Args. Unused args are
% unified with Tail.
parse_args_flag(['-h'            | _   ], _,   help, []).
parse_args_flag(['--help'        | _   ], _,   help, []).
parse_args_flag(['-v'            | _   ], _,   version, []).
parse_args_flag(['--version'     | _   ], _,   version, []).
parse_args_flag(['-f',      Path | Tail], Cfg, Cfg.put([ path: Path ]), Tail).
parse_args_flag(['--file',  Path | Tail], Cfg, Cfg.put([ path: Path ]), Tail).
parse_args_flag(['-g',      Atom | Tail], Cfg, Cfgʹ, Tail) :- parse_ground_goal(Atom, Cfg, Cfgʹ).
parse_args_flag(['--goal',  Atom | Tail], Cfg, Cfgʹ, Tail) :- parse_ground_goal(Atom, Cfg, Cfgʹ).
parse_args_flag(['-G',      Atom | Tail], Cfg, Cfgʹ, Tail) :- parse_nonground_goal(Atom, Cfg, Cfgʹ).
parse_args_flag(['--goals', Atom | Tail], Cfg, Cfgʹ, Tail) :- parse_nonground_goal(Atom, Cfg, Cfgʹ).
parse_args_flag(['--inspect'     | Tail], Cfg, Cfg.put([ inspect: inspect ]), Tail).
parse_args_flag(['--'            | Tail], Cfg, Cfgʹ, []) :- parse_args_goals(Tail, Cfg, Cfgʹ).

%! parse_args_goals(+Args, +Cfg, -Cfgʹ) is det
%
% Parse all remaining members of Args as ground goals. The goals are appended to
% Cfg.goals.
parse_args_goals([], Cfg, Cfg).
parse_args_goals([ Atom | Tail ], Cfg, Cfgʹ) :-
    parse_ground_goal(Atom, Cfg, Next),
    parse_args_goals(Tail, Next, Cfgʹ).

%! parse_ground_goal(+Atom, +Cfg, -Cfgʹ) is det
%
% Parse a user-provided string representing a Prolog term. Variables are
% immediately unified with their user-provided name, and the resulting goal is
% ground. The goal is appended to Cfg.goals.
parse_ground_goal(Atom, Cfg, Cfg.put([goals: Goals])) :-
    read_term_from_atom(Atom, Goal, [ variable_names(Pairs) ]),
    maplist(bind_variable_name, Pairs),
    append(Cfg.get(goals, []), [Goal], Goals).
bind_variable_name(X=X).

%! parse_nonground_goal(+Atom, +Cfg, -Cfgʹ) is det
%
% Parse a user-provided string representing a Prolog term. Variables are left
% untouched, so the resulting goal may be non-ground. The goal is appended to
% Cfg.goals.
parse_nonground_goal(Atom, Cfg, Cfg.put([goals: Goals])) :-
    read_term_from_atom(Atom, Goal, []),
    append(Cfg.get(goals, []), [Goal], Goals).

