
f(X,normal) :- X < 3,!.
f(X,alert1) :- 3 =< X, X < 6,!.
f(X,alert2) :- 6 =< X.

f2(X,normal) :- X < 3,!.
f2(X,alert1) :- X < 6,!.
f2(X,alert2).

f3(X,normal) :- X < 3.
f3(X,alert1) :- X < 6.
f3(X,alert2).


max(X,Y,X) :- X >= Y.
max(X,Y,Y) :- X <  Y.

% Wrong: max2(3,1,1) => true
max2(X,Y,X) :- X >= Y,!.
max2(X,Y,Y).

% BUG: max3(3,1,1) => true (should be false)
max3(X,Y,Max) :-
    X >= Y, !, Max = X
    ;
    Max = Y.


% Usual member
member(X,[X|L]).
member(X,[Y|L]) :- member(X,L).

% BUG: member(X,[a,b,c]) should be a
member2(X,[X|L]):- !.
member2(X,[Y|L]) :- member2(X,L).

add(X,L,L) :- member(X,L),!.
add(X,L,[X|L]).

add2(X,L,L) :- member(X,L).
add2(X,L,[X|L]).


% 5.2.4

beat(tom,jim).
beat(ann,tom).
beat(pat,jim).

class(X,fighter):-
    beat(X,_),
    beat(_,X),!.

class(X,winner) :-
    beat(X,_),!.
class(X,sportsman):-
    beat(_,X).


p(1).
p(2):-!.
p(3).

class(Number,zero)     :- Number is 0, !.
class(Number,positive) :- Number > 0, !.
class(Number,negative).

class2(Number,Class) :-
    (Number is 0, !, Class = zero)
    ;
    (Number > 0 , ! , Class = positive)
    ;
    Class = negative.


split([],[],[]).

split([X|XS],[X|YS],ZS) :-
    X >= 0,
    split(XS,YS,ZS).

split([X|XS],YS,[X|ZS]) :-
    X < 0,
    split(XS,YS,ZS).

split2([],[],[]).

split2([X|XS],[X|YS],ZS) :-
    X >= 0, !,
    split2(XS,YS,ZS).

split2([X|XS],YS,[X|ZS]) :-
    X < 0,
    split2(XS,YS,ZS).

likes(mary,X) :-
    snake(X),!, fail.
likes(mary,X) :-
    animal(X).

different(X,X) :- !, fail.
different(X,Y).

not(P) :-
    P,!,fail
    ;
    true.

likes2(mary,X) :-
    animal(X),
    not(snake(X)).

different2(X,Y) :-
    not(X=Y).


class2(X,fighter) :-
    beat(X,_),
    beat(_,X).

class(X,winner) :-
    beat(X,_),
    not(beat(_,X)).

class(X,sportsman):-
    beat(_,X),
    not(beat(X,_)).



solution([]).
solution([coord(X,Y) | Others]):-
    solution(Others),
    member(Y,[1,2,3,4,5,6,7,8]),
    not(attacks(coord(X,Y), Others)).

attacks(coord(X,Y), Others):-
     member(coord(X1,Y1), Others),
     ( Y1 = Y;
       Y1 is Y + X1 - X;
       Y1 is Y - X1 + X).

%solution([coord(1,Y1),coord(2,Y2),coord(3,Y3),coord(4,Y4),coord(5,Y5),coord(6,Y6),coord(7,Y7),coord(8,Y8)]).

%  member(X,Candidates) , not(member(X,RuledOut))

set_difference([],_,[]).
set_difference([X|XS],YS,ZS) :-
    member(X,YS),
    set_difference(XS,YS,ZS).

set_difference([X|XS],YS,[X|ZS]) :-
    not(member(X,YS)),
    set_difference(XS,YS,ZS).

unifiable([],_,[]).
unifiable([X|XS], T , YS) :-
    not(X=T) ,
    !,
    unifiable(XS,T,YS).

unifiable([X|XS], T , [X|YS]) :-
    unifiable(XS,T,YS).
