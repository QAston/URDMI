% Simple illustration of pretty-printing clauses in Aleph
%       using Michalski's trains problem.
% To run do the following:
%       a. Load Aleph
%       b. read_all(train).
%       c. sat(1).
%       d. reduce.

:- set(i,2).
:- set(clauselength,4).

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

:- set(portray_literals,true).

aleph_portray(eastbound(A)):-
	write('Train '), write(A),
	write(' is eastbound').

aleph_portray(has_car(A,B)):-
	write('train '), write(A),
	write(' has a car '), write(B).

aleph_portray(short(B)):-
	write('car '), write(B),
	write(' is short').

aleph_portray(closed(B)):-
	write('car '), write(B),
	write(' is closed').

aleph_portray(long(B)):-
	write('car '), write(B),
	write(' is long').

aleph_portray(open_car(B)):-
	write('car '), write(B),
	write(' is open').

aleph_portray(double(B)):-
	write('car '), write(B),
	write(' is double-walled').

aleph_portray(jagged(B)):-
	write('car '), write(B),
	write(' has a jagged roof').

aleph_portray(shape(B,C)):-
	write('car '), write(B),
	write(' is '), write(C), write('-shaped').

aleph_portray(wheels(B,C)):-
	write('car '), write(B),
	write(' has '), write(C),
	write(' wheels').

aleph_portray(load(B,C,D)):-
	write('car '), write(B),
	write(' has '), write(D),
	write(' '), write(C), write('-shaped load(s)').


