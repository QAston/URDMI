% Simple illustration of the use of Aleph to construct features
%       on Michalski's trains problem
% To run do the following:
%       a. Load Aleph
%       b. read_all(train).
%       c. induce_features.

:- set(i,2).
:- set(clauselength,10).
:- set(minacc,0.6).
:- set(noise,3).
:- set(minscore,3).
:- set(minpos,3).
:- set(nodes,5000).
:- set(explore,true).
:- set(max_features,10).

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

% show examples as boolean vectors

:- set(portray_examples,true).

aleph_portray(train_pos):-
        setting(train_pos,File),
        show_features(File,positive).
aleph_portray(train_neg):-
        setting(train_neg,File),
        show_features(File,negative).

show_features(File,Class):-
        open(File,read,Stream),
        repeat,
        read(Stream,Example),
        (Example = end_of_file -> close(Stream);
                write_features(Example,Class),
                fail).

write_features(Example,_):-
        features(_,(Example:- Body)),
        (Body -> write(1), write(' '); write(0), write(' ')),
        fail.
write_features(_,Class):-
	writeq(Class), nl.


% type definitions




% eastbound train 1

% eastbound train 2

% eastbound train 3

% eastbound train 4

% eastbound train 5

% westbound train 6

% westbound train 7

% westbound train 8

% westbound train 9

% westbound train 10
