%background knowledge and aleph settings 
% file appended to the generated .b file
:- set(interactive, true).
%Generated settings:
:-mode(1,father(+id,-id)).
:-mode(1,father(-id,+id)).
:-mode(1,mother(+id,-id)).
:-mode(1,mother(-id,+id)).
:-mode(1,grandfather(+id,+id)).
:-determination(grandfather/2,father/2).
:-determination(grandfather/2,mother/2).
%Background relations:

% relation: father/2
father(1,4).
father(2,5).
father(6,7).
father(7,8).

% relation: mother/2
mother(4,6).
mother(5,3).
mother(5,4).
