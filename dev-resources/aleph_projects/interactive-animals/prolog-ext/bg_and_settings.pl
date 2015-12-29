% Simple illustration of interactive construction of tree-based models
% within Aleph
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
:- set(dependent,2).	% second argument of class/2 is the one to predict


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Mode declarations

:- modeb(1,nhas_gills(+animal)).

:-determination(class/2,nhas_gills/1).

nhas_gills(X) :- animal(X), not(has_gills(X)).


