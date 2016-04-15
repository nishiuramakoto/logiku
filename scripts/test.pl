
a :- f(_).
f(0) :- b(0).
f(1) :- b(1).

b(X) :- write_term(X,[]),
    assertz(d(X)).

g(L) :- f(0),f(1), bagof(X,d(X),L).
