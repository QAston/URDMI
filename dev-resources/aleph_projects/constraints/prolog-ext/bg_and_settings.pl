% Simple illustration of the extraction of integrity constraints
%       by Aleph
% To run do the following:
%       a. Load Aleph
%       b. read_all(fam).
%       c. induce_constraints.
% This will learn a set of (possibly redundant) constraints that hold
% in the background knowledge. The procedure is similar to that used
% by DeRaedt et al in Claudien. Constraints that are ``nearly true''
% can be obtained by changing the noise parameter.

:- set(noise,0).


