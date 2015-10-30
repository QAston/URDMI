% Simple illustration of the use of user-defined refinement operators
%       in Aleph using Michalski's trains problem.
% To run do the following:
%       a. Load Aleph
%       b. read_all(train).
%       c. sat(1).
%       d. reduce.


:- set(i,2).
:- set(verbose,1).

:- modeh(1,eastbound(+train)).
:- modeb(1,short(+car)).
:- modeb(1,closed(+car)).
:- modeb(1,long(+car)).
:- modeb(1,open_car(+car)).
:- modeb(1,double(+car)).
:- modeb(1,jagged(+car)).
:- modeb(1,shape(+car,#shape)).
:- modeb(1,load(+car,#shape,#int)).
:- modeb(1,wheels(+car,#int)).
:- modeb(*,has_car(+train,-car)).

:- determination(eastbound/1,short/1).
:- determination(eastbound/1,closed/1).
:- determination(eastbound/1,long/1).
:- determination(eastbound/1,open_car/1).
:- determination(eastbound/1,double/1).
:- determination(eastbound/1,jagged/1).
:- determination(eastbound/1,shape/2).
:- determination(eastbound/1,wheels/2).
:- determination(eastbound/1,has_car/2).
:- determination(eastbound/1,load/3).

:- set(refine,user).

refine(false,eastbound(_)).
refine(eastbound(X),(eastbound(X):-has_car(X,_))).
refine(eastbound(X),(eastbound(X):-has_car(X,Y),short(Y))).
refine((eastbound(X):-has_car(X,Y),short(Y)),Clause):-
	Clause = (eastbound(X):-has_car(X,Y),short(Y),closed(Y)).

