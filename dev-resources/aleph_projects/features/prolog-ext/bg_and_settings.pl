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
