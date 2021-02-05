:- use_module(library(robo)).
:- use_module(library(robo_shell)).

:- op(1, fx, '--').

:- discontiguous recipe/1.
:- discontiguous cancel/1.
:- discontiguous depend/2.
:- discontiguous private/1.

:- discontiguous solved/1.
:- dynamic solved/1.

%:- initialization robo:main.




