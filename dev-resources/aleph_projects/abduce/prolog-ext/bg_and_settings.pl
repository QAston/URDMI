% Simple illustration of the use of Aleph for
%       abductive learning
% To run do the following:
%       a. Load Aleph
%       b. read_all(gp).
%       c. induce
% You should see Aleph first trying to learn the best rule
% it can for grandparent. It will then generate an abductive
% explanation for the examples in the form of ground facts for
% parent/2. These are then generalised to find the rules for
% parent/2.

:- dynamic grandparent/2.

:- modeh(*,grandparent(+person,-person)).
:- modeh(*,parent(+person,-person)).

:- modeb(*,mother(+person,-person)).
:- modeb(*,father(+person,-person)).
:- modeb(*,parent(+person,-person)).

:- set(abduce,true).
:- abducible(parent/2).

:- determination(grandparent/2,father/2).
:- determination(grandparent/2,parent/2).
:- determination(grandparent/2,mother/2).

father(dad(X),X):-
	person(X).
mother(mum(X),X):-
	person(X).

% The correct rule for grandparent/2. This will 
% not work because Aleph is missing the definition for
% parent/2 (see below). 
grandparent(X,Z):-
	person(X),
	person(Y),
	X \= Y,
	parent(X,Y),
	person(Z),
	Y \= Z,
	parent(Y,Z).


% The rules for parent/2 that we would like Aleph to discover
% parent(X,Y):- father(X,Y).
% parent(X,Y):- mother(X,Y).
