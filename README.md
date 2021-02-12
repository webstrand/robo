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
