:- use_module(library(robo)).
:- use_module(library(robo_shell)).
:- use_module(library(robo_stringify)).

:- op(1, fx, '--').

:- discontiguous recipe/1.
:- discontiguous cancel/1.
:- discontiguous depend/2.
:- discontiguous private/1.

%:- initialization robo:main.




