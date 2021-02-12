:- module(robo_solver, [
    load_robofile/2,
    solve/4,
    empty_solved/1,
    valid_targets/2
]).
:- use_module(library(yall)).
:- use_module(library(nb_hashtbl)).

load_robofile(Path, Namespace) :-
    op(1, fx, Namespace:'--'),
    discontiguous(Namespace:recipe/1),
    discontiguous(Namespace:cancel/1),
    discontiguous(Namespace:depend/2),
    discontiguous(Namespace:target/1),
    discontiguous(Namespace:hidden/1),
    @(load_files(Path, [encoding(utf8)]), Namespace).

%! empty_solved(!Var)
%
% Create an empty set of solved queries.
empty_solved(Var) :- empty_nb_hashtbl(Var).

:- meta_predicate solve(-, +, ?, 1).

%! solve(-Query, +Namespace, !Solved) is semidet
%
% Find and execute all solutions for Query in the given Namespace. Solved must
% be an nb_hashtbl, but may be shared.
solve(Query, Namespace, Solved, Inspect) :-
    ignore(Inspect, solve(Query)),
    solve(Query, Namespace, Solved, Inspect, []) -> true ; throw(unreachable).

% If the Query is ground, then we begin the process of actually resolving the
% query. First, we check if the query has already been solved. Then we check if
% the user has defined any valid cancellation for the query. If the query is
% neither solved nor canceled, we resolve the dependencies that match the query
% and apply all matching recipes.
solve(Query, Namespace, Solved, Inspect, Ancestors) :-
    ground(Query),
    (   nb_hashtbl_get(Solved, Query, true)
    ->  ignore(Inspect, solved(Query)), true
    ;   forall(
            Namespace:depend(Query, Dep),
            (ignore(Inspect, solve_depend(Query, Dep)), solve(Dep, Namespace, Solved, Inspect, [Query | Ancestors]))
        ),
        (   Namespace:cancel(Query)
        ->  ignore(Inspect, cancelled(Query)), nb_hashtbl_set(Solved, Query, true)
        ;   (   memberchk(Query, Ancestors)
            ->  throw(solver_error(cycle([Query | Ancestors])))
            ;   ignore(Inspect, executing(Query)),
                (   findall(
                        Query,
                        (
                            Namespace:recipe(Query),
                            nb_hashtbl_set(Solved, Query, true)
                        ),
                        [_|_]
                    )
                *-> ignore(Inspect, solved(Query)), true
                ;   throw(solver_error(unsolvable(Query)))
                )
            )
        )
    ).

% If the Query is not ground, then we need to generate matching ground queries
% that can actually be resolved. We do this, primarily, by finding recipes,
% depends, and cancels with ground arguments and use those to generate matching
% ground queries.
solve(Query, Namespace, Solved, Inspect, Ancestors) :-
    \+ ground(Query),
    (   findall(
            Query,
            (
                valid_targets(Query, Namespace),
                ignore(Inspect, grounded(Query)),
                solve(Query, Namespace, Solved, Inspect, Ancestors)
            ),
            [_|_]
        )
    *-> true
    ;   throw(solver_error(unsolvable(Query)))
    ).

ignore(Goal, A) :-
    call(Goal, A), !.
ignore(_, _).

%! valid_targets(-Query, +Namespace)
%
% Generate valid, ground, targets for the given Query. Results are unique.
valid_targets(Query, Namespace) :-
    empty_nb_hashtbl(Seen),
    valid_targetsʹ(Query, Namespace),
    \+ nb_hashtbl_get(Seen, Query, true),
    nb_hashtbl_set(Seen, Query, true).

% The user is empowered to generate valid targets, though we only accept ground
% ones.
valid_targetsʹ(Query, Namespace) :-
    Namespace:target(Query), ground(Query).

valid_targetsʹ(Query, Namespace) :-
    Namespace:hidden(Query), ground(Query).

valid_targetsʹ(Query, Namespace) :-
    clause(Namespace:recipe(Query), _), ground(Query).

valid_targetsʹ(Query, Namespace) :-
    clause(Namespace:depend(Query, _), _), ground(Query).

valid_targetsʹ(Query, Namespace) :-
    clause(Namespace:depend(_, Query), _), ground(Query).

valid_targetsʹ(Query, Namespace) :-
    clause(Namespace:cancel(Query), _), ground(Query).
