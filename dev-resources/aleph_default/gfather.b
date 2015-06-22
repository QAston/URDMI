%what can be used in hypotheses
:- determination(gfather/2, father/2).
:- determination(gfather/2, mother/2).
:- determination(gfather/2, parent/2).


%background knowledge
:- modeh(1, gfather(+id,+id)).

:- modeb(2,father(+id,-id)).
:- modeb(2,father(-id,+id)).
:- modeb(2,mother(+id,-id)).
:- modeb(2,mother(-id,+id)).
:- modeb(2,parent(+id,-id)).
:- modeb(2,parent(-id,+id)).


% wlacza zapisywanie do pliku
:- set(record,true).
:- set(recordfile,'out').
%remove trivial rules
:- set(interactive,true).
:- set(minpos,2).

father(1, 4).
father(2, 5).
father(6, 7).
father(7, 8).

mother(4, 6).
mother(5, 3).
mother(5, 4).

parent(A, B) :- mother(A, B) ; father(A, B).
