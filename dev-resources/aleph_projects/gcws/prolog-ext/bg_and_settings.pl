% Simple illustration of the technique of generalised
%       closed-world specialisation within Aleph
% To run do the following:
%       a. Load Aleph
%       b. read_all(cal).
%       c. rdhyp.
%	d. normal(A).
%       e. sphyp.
%       f. show(gcws).
%       h. addgcws.

:- mode(*,normal(+year)).
:- mode(*,div4(+year)).
:- mode(*,div100(+year)).
:- mode(*,div400(+year)).

:- determination(normal/1,div4/1).
:- determination(normal/1,div100/1).
:- determination(normal/1,div400/1).
:- determination(normal/1,year/1).
