# Robo
This is very alpha software. I'll probably change how the entire thing works, not to far in the future.

_Robo_ is an automation tool akin to _make_. Unlike _make_, it does not use filesystem paths and modification time to determine which targets need to be rebuilt. Instead users are allowed to leverage the full power of Prolog to define recipes and their dependencies as well as the conditions under which a target needs to be rebuilt.

## Building
Robo is based on SWI-Prolog and expects `swipl` to be available on the system.

You can use `robo.pl` directly, or build a pseudo-binary via:

```shell-session
$ ./robo build
```


## Robofile Predicates
### `recipe/1`
Every `recipe/1` matching the goal gets executed, unless there's a matching `cancel/1`. A target is considered successful if at least one matching `recipe/1` succeeds.

For example:
```pro
% Robofile
recipe(default) :- write("Hello,").
recipe(default) :- fail.
recipe(default) :- write(" World!\n").
```

`$ robo default` succeeds and prints `Hello, World!`.

### `cancel/1`
For any given target, if there is a matching `cancel/1`, then that target succeeds unconditionally.

For example:
```pro
% Robofile
recipe(default) :- write("Executed default\n").
cancel(default).
```

`$ robo default` succeeds, but does not print anything, because `recipe(default)` did not run.

### `depend/2`
Declares a dependency from one target to another. Dependencies of a target are solved before the target itself is solved.

For example:
```pro
% Robofile
recipe(print:String) :- write(String).
depend(default, print:"A ").
depend(default, print:"B ").
recipe(default) :- write(" C\n")
```

`$ robo default` succeeds and prints `A B C`.

### `target/1`
Defines that a target exists. When using wildcard goals (`robo -G print:_`) robo is sometimes unable to determine that a target exists. `target/1` is used to declare the existance of a target that may not have any exactly matching `recipe/1`.

For example:
```pro
% Robofile
recipe(print:String) :- write(String).
```

`$ robo -G print:_` fails, because there are no known targets that match `print:_`

```pro
% Robofile
recipe(print:String) :- write(String).
target(print:"Hello, ").
target(print:"World!\n").
```

`$ robo -G print:_` succeeds and prints `Hello, World!`

### `hidden/1`
**Unimplemented** Prevents matching targets from being requested from the command-line.

# TODO

- [ ] ./2 makes file paths rather painful.
- [ ] Should main share the Solved state between goals, when the user has requested multiple goals?
- [ ] Should every matching recipe be called, for a given goal?
- [ ] stringify from foo/bar/baz
- [x] should child cancel/1 run before their dependencies do?
- [ ] How should cancellation work? Right now, if _any_ cancel(Goal) succeeds then that goal is considered canceled. However, we _could_ require that every _clause_ succeed at least once, maybe?
- [ ] should cancel/1 be replaced with compel/1? Targets would only execute if either any of their dependencies execute or there is a matching compel/1? But what about order-only dependencies found in Makefiles?
- [ ] default targets, like in Makefiles
- [ ] Make terminology consistent. Are they targets, goals, queries, what?
