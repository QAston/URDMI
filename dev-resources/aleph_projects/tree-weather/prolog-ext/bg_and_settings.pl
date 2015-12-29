:- modeh(1,class(+day,-class)).
:- modeb(1,outlook(+day,#outlook)).
:- modeb(1,temp(+day,-temp)).
:- modeb(1,humidity(+day,-humidity)).
:- modeb(1,windy(+day,#windy)).
:- modeb(*,lteq(+temp,#temp)).
:- modeb(*,lteq(+humidity,#humidity)).

:- determination(class/2,outlook/2).
:- determination(class/2,temp/2).
:- determination(class/2,humidity/2).
:- determination(class/2,windy/2).
:- determination(class/2,lteq/2).

% :- set(tree_type,classification).
:- set(tree_type,class_probability).
:- set(classes,[play,dont_play]).
:- set(minpos,2).	% minimum examples in leaf for splitting
:- set(clauselength,5).
:- set(lookahead,2).	% to allow lookahead to lteq/2
:- set(prune_tree,true).
:- set(confidence,0.25).% pruning conf parameter used by C4.5
:- set(evalfn,entropy).
% :- set(evalfn,gini).
:- set(dependent,2).	% second arg of class/2 is to predicted


% type predicates
lteq(X,Y):-
	var(Y), !,
	X = Y.
lteq(X,Y):-
	number(X), number(Y),
	X =< Y.

outlook(Day,Outlook):-
	table(Day,Outlook,_,_,_).
temp(Day,Temp):-
	table(Day,_,Temp,_,_).
humidity(Day,Humidity):-
	table(Day,_,_,Humidity,_).
windy(Day,Windy):-
	table(Day,_,_,_,Windy).

