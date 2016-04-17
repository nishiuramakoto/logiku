:- module(test,[]).
:- use_module(rouki_common).
:- use_module(date_time_test).

違法(Case) :- 事件(Case,case1),
	      要件(Case,cond1).


a :- f(_).
f(0) :- b(0).
f(1) :- b(1).


要件(X) :- catch(X, Err, 確認要件(X,Err)).

確認要件(X,Err) :- write(Err),nl,
		   write(X),nl,
		   read(Answer),
		   確認要件(X,Err,Answer).

確認要件(_X,_Err,yes).
確認要件(X,_Err,dontKnow) :- 要件(X).

b :- fail.
inquire :- b, 要件(b).
