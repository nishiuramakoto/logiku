member(X,[X|_]).
member(X,[_|Tail]) :-
    member(X,Tail).

% simplify(PX,E) -- simplify linear expressions

simplify([], [{1,0}]).
simplify([X|XS] , E) :-
    number(X),!,
    simplify(XS,E1),
    E1 = [{1,C}|ES],
    C1 is C + X,
    E  = [{1,C1}|ES].

simplify([X|XS] ,E) :-
    simplify(XS,E1),
    add_term(X,E1,E).

add_term(X,[],[{X,1}]).
add_term(X,[{X,C}|XS], [{X,C1}|XS]) :-
    !,
    C1 is C + 1.
add_term(X,[Y|XS],XS1) :-
    add_term(X,XS,XS2),
    XS1 = [Y|XS2].


add_to_tail(X,[X|Tail]) :-
    var(Tail),!.

add_to_tail(X,[_|List]) :-
    add_to_tail(X,List).

member2(X,[X|Tail]) :-
    var(Tail),!.

member2(X,[_|List]) :-
    member2(X,List).

% ground(Term) true if Term does not contain any uninstantiated variable
ground_term(X) :-
    var(X) ,!,fail.
ground_term(X) :-
    atomic(X).
ground_term(X) :-
    compound(X),
    X =.. [F|AS],
    atomic(F),
    ground_list(AS).

ground_list([]).
ground_list([X|XS]) :-
    ground_term(X),
    ground_list(XS).


% substitute( Subterm, Term, Subterm1, Term1):
%    if all occurrences of Subterm in Term are substituted
%    with Subterm1 then we get Term1.



% Case 1: Substitute whole term

substitute( Term, Term, Term1, Term1).


% Case 2: Nothing to substitute

substitute( _, Term, _, Term)  :-
   atomic(Term).


% Case 3: Do substitution on arguments

substitute( Sub, Term, Sub1, Term1)  :-
    compound(Term),
    Term  =..  [F|Args],                        % Get arguments
    substlist( Sub, Args, Sub1, Args1),         % Perform substitution on them
    Term1  =..  [F|Args1].		       % Construct Term1


substlist( _, [], _, []).

substlist( Sub, [Term|Terms], Sub1, [Term1|Terms1])  :-
   substitute( Sub, Term, Sub1, Term1),
   substlist( Sub, Terms, Sub1, Terms1).


% subsume(Term1, Term2)
% True if Term1 is more general than Term2

% Case 1: Any term subsumes itself.
subsumes( Term, Term ).

% Case 2: a variable subsumes anything.
subsumes( Term1, _Term2 ) :-
    var(Term1).

% Case 3: a structure subsumes a term if they match
%         and all the arguments subsumes the ones in the other.
subsumes( Term1, Term2 ) :-
    compound(Term1),
    Term1 =.. [_F1|Args1],
    compound(Term2),
    Term2 =.. [_F2|Args2],
    Term1 = Term2,
    subsume_list(Args1,Args2).

subsume_list([],[]).
subsume_list([X|XS],[Y|YS]) :-
    subsumes(X,Y),
    subsume_list(XS,YS).

dosquares :-
    repeat,
    read(X),
    ( X = stop,!
     ;
     Y is X*X , write(Y),
     fail
    ).

age(peter,7).
age(ann,5).
age(pat,8).
age(tom,5).

% powerset/2

powerset(Set,Subsets) :-
    bagof(Subset, subset(Set,Subset), Subsets).

subset([], []).
subset([E|Tail], [E|NTail]):-
  subset(Tail, NTail).
subset([_|Tail], NTail):-
  subset(Tail, NTail).
