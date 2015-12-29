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

:- modeb(1,nhas_gills(+animal)).

:-determination(class/2,nhas_gills/1).


nhas_gills(X) :- animal(X), not(has_gills(X)).

false:-class(X,Y),class(X,Z),Y\=Z.

