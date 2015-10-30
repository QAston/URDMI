% Simple illustration of constructing tree-based models within Aleph
% To run do the following:
%       a. Load Aleph
%       b. read_all(animals).
%       c. induce_tree.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% specify tree type

:- set(tree_type,classification).
:- set(classes,[mammal,nmammal]).
:- set(minpos,2).       % minimum examples in leaf for splitting
:- set(prune_tree,true).
:- set(confidence,0.25).% pruning conf parameter used by C4.5
:- set(evalfn,entropy).
:- set(dependent,2).	% second argument of class/2 is to predicted


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Mode declarations

:- modeh(1,class(+animal,-class)).
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


