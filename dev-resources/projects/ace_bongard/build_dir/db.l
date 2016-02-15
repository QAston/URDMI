dlab_template('len-len:[0-len:[sort(X), sort(Y), size(X), size(Y), direction(X), direction(Y), relation1(X,Y), relation1(Y,X), relation2(X,Y), relation2(Y,X), relation3(X,Y), relation3(Y,X)] ] <-- len-len:[ 1-len:[sort(X), sort(Y)], 0-len:[size(X), size(Y), direction(X), direction(Y),relation1(X,Y), relation1(Y,X), relation2(X,Y), relation2(Y,X), relation3(X,Y), relation3(Y,X) ]]') .

dlab_variable(sort,1 - 1,[circle,triangle,rectangle, polygon, object]) .

dlab_variable(size,1 - 1,[large,small]) .

dlab_variable(direction,1 - 1,[down,up]) .

dlab_variable(relation1, 1 - 1, [left, right]).

dlab_variable(relation2, 1 - 1, [under, above]).

dlab_variable(relation3, 1 - 1, [in]).
 
