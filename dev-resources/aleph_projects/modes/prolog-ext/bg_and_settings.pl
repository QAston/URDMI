% Simple illustration of the automatic extraction of modes
%       in Aleph using Michalski's trains problem.
% To run do the following:
%       a. Load Aleph
%       b. read_all(train).
%       c. induce_modes.
% Tighter or looser chaining is achieved by changing the
% value of the mode_overlap_threshold parameter


:- set(i,2).
:- set(clauselength,4).
:- set(mode_overlap_threshold,0.95).

% commented out -- Aleph has to learn these (or some approximation to them)
% :- modeh(1,eastbound(+train)).
% :- modeb(1,short(+car)).
% :- modeb(1,closed(+car)).
% :- modeb(1,long(+car)).
% :- modeb(1,open_car(+car)).
% :- modeb(1,double(+car)).
% :- modeb(1,jagged(+car)).
% :- modeb(1,shape(+car,#shape)).
% :- modeb(1,load(+car,#shape,#int)).
% :- modeb(1,wheels(+car,#int)).
% :- modeb(*,has_car(+train,-car)).

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

% type definitions



% eastbound train 1
% infront(east1,car_11).
% infront(car_11,car_12).
% infront(car_12,car_13).
% infront(car_13,car_14).

% eastbound train 2
% infront(east2,car_21).
% infront(car_21,car_22).
% infront(car_22,car_23).

% eastbound train 3
% infront(east3,car_31).
% infront(car_31,car_32).
% infront(car_32,car_33).

% eastbound train 4
% infront(east4,car_41).
% infront(car_41,car_42).
% infront(car_42,car_43).
% infront(car_43,car_44).

% eastbound train 5
% infront(east5,car_51).
% infront(car_51,car_52).
% infront(car_52,car_53).

% westbound train 6
% infront(west6,car_61).
% infront(car_61,car_62).

% westbound train 7
% infront(west7,car_71).
% infront(car_71,car_72).
% infront(car_72,car_73).

% westbound train 8
% infront(west8,car_81).
% infront(car_81,car_82).

% westbound train 9
% infront(west9,car_91).
% infront(car_91,car_92).
% infront(car_92,car_93).
% infront(car_93,car_94).

% westbound train 10
% infront(west10,car_101).
% infront(car_101,car_102).


% cost(_,[P,N|_],Cost):-
	% Cost is 1-(P+1)/(P+N+2).
% 
% 
% :- set(refine,user).
% 
% refine(false,eastbound(X)):- !.
% refine(Clause,Clause1):-
	% add_lit(has_car(X,Y),Clause,Clause1).
% 
% add_lit(has_car(A,B),eastbound(A),(eastbound(A):-has_car(A,B))):- !.
% add_lit(has_car(A,B),(eastbound(A):-true),(eastbound(A):-has_car(A,B))):- !.
% add_lit(has_car(A,B),(eastbound(A):-Body),(eastbound(A):-Body1)):-
	% !,
	% add_lit(has_car(A,B),Body,Body1).
% add_lit(L,(L1,L2),(L1,L3)):-
	% !,
	% add_lit(L,L2,L3).
% add_lit(L,L1,(L1,L)).

% % 
% refine(false,(eastbound(X):-has_car(X,Y),short(Y),closed(Y))).
% refine(false,eastbound(X)).
% refine(eastbound(X),(eastbound(X):-has_car(X,Y))).
% refine(eastbound(X),(eastbound(X):-has_car(X,Y),short(Y))).
% refine((eastbound(X):-has_car(X,Y),short(Y)),(eastbound(X):-has_car(X,Y),short(Y),closed(Y))).

% :- set(portray_literals,true).
% :- set(portray_search,true).

% :- [portray].

portray(eastbound(A)):-
	write('Train '), write(A),
	write(' is eastbound').

portray(has_car(A,B)):-
	write('train '), write(A),
	write(' has a car '), write(B).

portray(short(B)):-
	write('car '), write(B),
	write(' is short').

portray(closed(B)):-
	write('car '), write(B),
	write(' is closed').

portray(long(B)):-
	write('car '), write(B),
	write(' is long').

portray(open_car(B)):-
	write('car '), write(B),
	write(' is open').

portray(double(B)):-
	write('car '), write(B),
	write(' is double-walled').

portray(jagged(B)):-
	write('car '), write(B),
	write(' has a jagged roof').

portray(shape(B,C)):-
	write('car '), write(B),
	write(' is '), write(C), write('-shaped').

portray(wheels(B,C)):-
	write('car '), write(B),
	write(' has '), write(C),
	write(' wheels').

portray(load(B,C,D)):-
	write('car '), write(B),
	write(' has '), write(D),
	write(' '), write(C), write('-shaped load(s)').


