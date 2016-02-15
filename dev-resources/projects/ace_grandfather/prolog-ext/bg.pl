% background knowledge 
% file appended to the generated .bg file

%predicate to use inside model(). declaration of .kb file.
%speficies classes for icl
grandfather_yes :- grandfather(_, _, 1).
grandfather_no :- grandfather(_, _, 0).