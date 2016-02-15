% background knowledge 
% file appended to the generated .bg file
polygon(X) :- rectangle(X).
polygon(X) :- triangle(X).

object(X) :- polygon(X).
object(X) :- circle(X).

right(X,Y) :- left(Y,X).
above(X,Y) :- under(Y,X).

good :- bongard(pos).
wrong :- bongard(neg).