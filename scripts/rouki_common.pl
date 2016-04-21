% -*- mode: prolog; -*-

:- module(rouki_common,[事件/2,
			要件/2,
			選択要件/4,
                        自然数要件/3,
                        自然数要件/4,
                        暦日要件/3,
			期間要件/3,
			事件リスト/2,
			事件検索/1,
                        日前/3,
                        月前/3,
                        早い/2,
                        期間日数/2,
			日数/3,
			期間内日/2,
			期間内週/2
	 ]).
:- use_module(date_time).

:- meta_predicate 事件リスト(1,?).
:- meta_predicate 事件検索(1).


:- dynamic 事件書き出し/0.
:- dynamic 事件記述/1.
:- dynamic fact/2.
:- dynamic already_asked/2.

事件書き出し :- fail.

事件(_Case, Desc) :- 事件書き出し ,! ,
                    assertz(事件記述(Desc)),
                    write(事件), put_char(':'), write(Desc), nl.
事件(Case, Case).

要件(Case, Cond) :-  事件書き出し, !
		    ;
		    fact(Case,Cond)
		    ;
		    \+ fact(Case,Cond),
		    \+ already_asked(Case,Cond),
		    write(要件), write(':'), write(Cond), nl,
		    要件確認(Case, Cond).

要件確認(Case, Cond) :- functor(Cond,_Name,0), !,
                        要件確認0(Case, Cond) .
要件確認(Case, Cond) :- Cond =.. [ \+ , NCond ],
                        functor(NCond, _Name , 0) ,!,
                        要件確認0(Case, Cond) .

要件確認(Case, Cond) :- write('Answer yes, no, or dk.'), nl,
			write(Cond), nl,
			read(Answer),
			要件確認1(Case, Cond, Answer).


要件確認0(Case, Cond) :- write('Answer yes, no, or dk.'), nl,
			 write(Cond), nl,
			 read(Answer),
			 要件確認0(Case, Cond, Answer).

要件確認0(Case, Cond, dk) :- write(要件確認), catch(Cond, Err, 要件確認処理(Err, Case, Cond)).
要件確認0(Case,Cond, yes) :- asserta((fact(Case,Cond) :- write('known fact:'), write(Cond))),
			    asserta(already_asked(Case,Cond)).

要件確認0(Case,Cond, no) :- asserta(already_asked(Case,Cond)),
			   fail.


要件確認1(_Case, Cond, dk) :- write(要件確認), call(Cond).
要件確認1(Case,Cond, yes) :- asserta((fact(Case,Cond) :- write('known fact:'), write(Cond))),
			    asserta(already_asked(Case,Cond)).

要件確認1(Case,Cond, no) :- asserta(already_asked(Case,Cond)),
			   fail.



要件確認処理(Err, Case, Cond) :- write(Err), nl,
				 write(Case), nl,
				 write(Cond), nl,
                                 write('yes か no で答えて下さい。'), nl,
				 read(Answer),
				 Answer = yes.


選択要件(Case,Desc,List,Elem) :- write(Case), write(Desc),nl,
                                 write(次のリストから選択してください),nl,
				 write(List),
				 read(N),
				 nth1(N,List,Elem).

自然数要件(Case,Desc,N) :- write(Case),nl,
                           write(Desc),nl,
                           write('Input a natural number or say dk:'),
                           read(N),
			   (N = dk -> call(Desc) ; integer(N)).


自然数要件(Case,Desc, Args, N) :- write(Case),nl,
				  format(Desc,Args),
				  write('Input a natural number or say dk: '),
				  read(N),
				  (N = dk -> call(Desc) ; integer(N)).

暦日要件(Case,Desc,date(Y,M,D)) :- write(Case),nl,
                               write(Desc),nl,
                               write('Input a date in the form (Year, Month, Day)'),
                               read((Y,M,D)).

期間要件(Case,Desc,期間(date(Y0,M0,D0),date(Y1,M1,D1))) :-
    write(Case),nl,
    write(Desc),nl,
    write('Input a starting date in the form (Year, Month, Day): '),
    read((Y0,M0,D0)),
    write('Input a terminating date in the form (Year, Month, Day): '),
    read((Y1,M1,D1)).


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


%%%%%%%%%%%%%%%%%%%%%%%%% DateTime functions %%%%%%%%%%%%%%%%%%%%%%%%%

日前(D0, N , D ) :- date_add(D0, [ days(- N) ], D).
月前(D0, N , D ) :- date_add(D0, [ months(- N) ], D).
早い(D1,D2) :- date_compare(D1, < , D2).

期間日数(期間(D0,D1),N) :- date_interval(D1,D0, days(N)).
日数(D0,D1,N) :- date_interval(D1,D0, days(N)).


期間内日(期間(D0,D1) , D0) :-
    date_compare(D0, < , D1) .

期間内日(期間(D0,D1) , D) :-
    date_add(D0, [days(1)] , D2),
    date_compare(D2, < , D1),
    期間内日(期間(D2,D1), D).


期間内日(期間(D0,D1) , D) :-
    date_compare(D0, =< , D),
    date_compare(D , <  , D1).

期間内週(期間(D0,D1) , W ) :-
    date_add(D0, [ days(7) ], D2) ,
    週分割(期間(D0,D1) , D2 , W ).

週分割(期間(D0,D1) , D2 , 期間(D0,D1) ) :-
    date_compare(D1 , =< , D2).

週分割(期間(D0,D1) , D2 , 期間(D0,D2) ) :-
    date_compare(D1 , > , D2).

週分割(期間(_D0,D1) , D2 ,  W ) :-
    date_compare(D1 , > , D2),
    期間内週(期間(D2,D1), W).
