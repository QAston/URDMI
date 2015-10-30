% Simple illustration of positive-only learning within Aleph
% To run do the following:
%       a. Load Aleph
%       b. read_all(animals).
%       c. sat(1).
%       d. reduce.
%	or
%       a. Load Aleph
%       b. read_all(animals).
%       c. induce.


:- set(evalfn,posonly).
:- set(clauselength,2).
:- set(gsamplesize,20).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% class/2 learns the class (mammal/fish/reptile/bird) of various animals.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Mode declarations

:- modeh(1,class(+animal,#class)).
:- modeb(1,has_gills(+animal)).
:- modeb(1,has_covering(+animal,#covering)).
:- modeb(1,has_legs(+animal,#nat)).
:- modeb(1,homeothermic(+animal)).
:- modeb(1,has_eggs(+animal)).
:- modeb(1,not(has_gills(+animal))).
:- modeb(1,nhas_gills(+animal)).
:- modeb(*,habitat(+animal,#habitat)).
:- modeb(1,has_milk(+animal)).

:-determination(class/2,has_gills/1).
:-determination(class/2,has_covering/2).
:-determination(class/2,has_legs/2).
:-determination(class/2,momeotermic/1).
:-determination(class/2,has_egss/1).
:-determination(class/2,nhas_gills/1).
:-determination(class/2,habitat/2).
:-determination(class/2,has_milk/1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Types






%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Background knowledge










nhas_gills(X) :- animal(X), not(has_gills(X)).

false:-class(X,Y),class(X,Z),Y\=Z.

