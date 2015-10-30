% Simple illustration of the use of recording good clauses found
%       during the search using the Michalski's trains problem.
% This will store a Prolog encoding of clauses above minscore 
% (optionally in the file specified by goodfile)
% To run do the following:
%       a. Load Aleph
%       b. read_all(train).
%       c. sat(1).
%       d. reduce.
%	e. show(good).

:- set(i,2).
:- set(good,true).
% :- set(goodfile,'good.pl').	% optional file to store good clauses

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