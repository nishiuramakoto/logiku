
member(X,[X|_]).
member(X,[_|Tail]) :-
    member(X,Tail).

conc([],L,L).
conc([X|L1],L2,[X|L3]) :-
    conc(L1,L2,L3).


deleteLastThree(L,L1) :-
    conc(L1,[_,_,_],L).

last(Item,List) :-
    conc(_,[Item],List).

last2(Item,[Item]).
last2(Item,[_|Tail]) :-
    last2(Item,Tail).

add(X,L,[X|L]).

del(X,[X|Tail],Tail).
del(X,[Y|Tail],[Y|Tail1]) :-
    del(X,Tail,Tail1).

insert(X,List,BiggerList) :-
    del(X,BiggerList, List).

member2(X,List):-
    del(X,List,_).

sublist(S,L):-
    conc(_,L2,L),
    conc(S,_,L2).

permutation([],[]).
permutation([X|L],P):-
    permutation(L,L1),
    insert(X,L1,P).

permutation2([],[]).
permutation2(L,[X|P]) :-
    del(X,L,L1),
    permutation2(L1,P).

evenlength([]).
evenlength(L) :-
    conc([_,_],L1,L),
    evenlength(L1).

oddlength([_]).
oddlength(L) :-
    conc([_,_],L1,L),
    oddlength(L1).

reverse([],[]).
reverse([X|List],ReversedList):-
    reverse(List,List1),
    conc(List1,[X],ReversedList).

palindrome([]).
palindrome([_]).
palindrome([Head|Tail]) :-
    conc(L1,[Head],Tail),
    palindrome(L1).

shift([],[]).
shift([X],[X]).
shift([Head|Tail],List) :-
    shift(Tail,[Head1|Tail1]),
    conc([Head1,Head],Tail1,List).


translate([],[]).
translate([H|Tail] , [H1|Tail1]) :-
    means(H,H1),
    translate(Tail,Tail1).

means(0,zero).
means(1,one).

subset(_,[]).
subset(L,[Head|Tail]):-
    conc(L1,[Head|L2],L),
    conc(L1,L2,L3),
    subset(L3,Tail).

dividelist([],[],[]).
dividelist([X],[X],[]).
dividelist([H1,H2|Tail],L2,L3) :-
    L1 = [H1,H2|Tail] ,
    conc([H1|M1],[X1],L1),
    dividelist(M1,M2,M3),
    conc([H1],M2,L2),
    conc(M3,[X1],L3).

equal_length([],[]).
equal_length([X|Xs],[Y|Ys]) :-
    equal_length(Xs,Ys).

% flatten(List,FlatList)
flatten([],[]).
flatten([Head|Tail],FlatList) :-
    flatten_one(Head,FlatHead),
    flatten(Tail,FlatList1),
    conc(FlatHead, FlatList1, FlatList).

flatten_one([],[]).
flatten_one([A|AS],FlatList) :-
    flatten(AS,FlatList1),
    flatten_one(A,FlatA) ,
    conc(FlatA,FlatList1, FlatList).

flatten_one(X,[X]).
