%% Implementation
:- module(rouki_common,[事件/2,
			要件/2,
			要件リスト/4,
			事件リスト/2,
			事件検索/1
	 ]).
:- meta_predicate 事件リスト(1,?).
:- meta_predicate 事件検索(1).


:- dynamic 事件書き出し/0.
:- dynamic 事件記述/1.

事件書き出し :- fail.

事件(_Case, Desc) :- 事件書き出し ,! ,
                    assertz(事件記述(Desc)),
                    write(事件), put_char(':'), write(Desc), nl.
事件(Case, Case).

要件(Case, Cond) :-  事件書き出し, !
		    ;
		    write(要件), print(Cond),
		    catch(Cond, Err, 確認要件(Err, Case, Cond)).

確認要件(Err, Case, Cond) :- write(yes_no_or_dontKnow), nl,
			   write(Err), nl,
			   write(Cond), nl,
			   read(Answer),
			   確認要件(Err, Case, Cond, Answer).

確認要件(_Err,_Case,_Cond, yes).
確認要件(_Err, Case, Cond, dontKnow) :- write(確認要件), 要件(Case, Cond).

要件リスト(Case,Desc,List,Elem) :- write(choose_number),nl,
				   write(Case), write(Desc),nl,
				   write(List),
				   read(N),
				   nth0(N,List,Elem).



事件リスト(F, Ds) :- setup_call_cleanup(事件リスト準備, 事件リストを計算(F, Ds), 事件リスト後片付け).

事件リスト準備 :- retractall(事件記述(_)),
		  retractall(事件書き出し),
                  asserta(事件書き出し).

事件リストを計算(F,Ds) :- bagof(X,call(F,X),_),
			  bagof(Desc, 事件記述(Desc),Ds).

事件リスト後片付け :- retractall(事件記述(_)),
		      retractall(事件書き出し),
		      asserta(事件書き出し :- fail).

事件検索(F) :- 事件リスト(F, List),
	       write(choose_number), nl,
	       write(List),
	       read(N),
	       nth0(N, List, Desc),
	       call(F, Desc).

%% Tests

%% 違法(Case) :- 事件(Case,case1),
%% 	      要件(Case,cond1).
