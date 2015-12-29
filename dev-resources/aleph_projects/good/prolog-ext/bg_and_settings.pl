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
