conc([],L,L).
conc([X|L1],L2,[X|L3]) :-
    conc(L1,L2,L3).

link(a,b).
link(a,c).
link(b,d).
link(c,d).
link(c,f).
link(d,e).
link(d,f).
link(f,a).


path(Node,Node).
path(StartNode,EndNode) :-
    link(StartNode, NextNode),
    path(NextNode,EndNode).

% path/3
path(Node,Node,[Node]).
path(Start,End,[Start|Rest]) :-
    link(Start,Next),
    path(Next,End,Rest).
