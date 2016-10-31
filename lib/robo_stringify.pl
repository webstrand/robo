:- module(robo_stringify, [ stringify/2 ]).

stringify(Format, Str) :- stringify(Format, ' ', Str).

% env: helper
stringify(:(env, Format), Sep, Str) :-
	!, stringify(Format, Sep, Name),
	getenv(Name, Value),
	atom_string(Value, Str).

stringify(:(sep(Sep), Format), _, Str) :-
	!, stringify(Format, Sep, Str).

% | is for concatenation
stringify('|'(A, B), Sep, Str) :-
	!, stringify_tail((A, B), '', Sep, Str).

% ; Joins by semicolon
stringify(';'(A, B), Sep, Str) :-
	!, stringify_tail((A, B), ";\n", Sep, Str).

% Handle -- operator
stringify('--'(Format), Sep, Str) :-
	!, stringify(Format, Sep, Tail),
	string_concat('--', Tail, Str).

% Handle - operator
stringify('-'(Format), Sep, Str) :-
	!, stringify(Format, Sep, Tail),
	string_concat('-', Tail, Str).

% Handle , operator
stringify(','(A, B), Sep, Str) :-
	!, stringify_tail((A, B), Sep, Sep, Str).

stringify(Format, Sep, Str) :-
	compound(Format), ground(Format),
	functor(Format, Head, _),
	!, stringify_tail(Format, Sep, Sep, Tail),
	string_concat(Head, Sep, Tmp),
	string_concat(Tmp, Tail, Str).

stringify(Format, _, Str) :-
	string(Format),
	!, Str = Format.

stringify(Unknown, _, Str) :-
	!, atom_string(Unknown, Str).

stringify_tail(Format, Separator, DefaultSeparator, Str) :-
	Mem = $(nil),
	forall(
		arg(_, Format, Child),
		(
			$(Left) = Mem,
			stringify(Child, DefaultSeparator, Right),
			(
				Left = nil *-> Tmp = "" ;
				string_concat(Left, Separator, Tmp)
			),
			string_concat(Tmp, Right, Acc),
			nb_setarg(1, Mem, Acc)
		)
	),
	$(Str) = Mem.
