% Simple illustration of the use of user-defined refinement operators
%       in Aleph using Michalski's trains problem.
% To run do the following:
%       a. Load Aleph
%       b. read_all(train).
%       c. sat(1).
%       d. reduce.


:- set(i,2).
:- set(verbose,1).

:- set(refine,user).

refine(false,eastbound(_)).
refine(eastbound(X),(eastbound(X):-has_car(X,_))).
refine(eastbound(X),(eastbound(X):-has_car(X,Y),short(Y))).
refine((eastbound(X):-has_car(X,Y),short(Y)),Clause):-
	Clause = (eastbound(X):-has_car(X,Y),short(Y),closed(Y)).

