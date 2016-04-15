不法 :- 不法(_).

無効労働契約部分(X) :- 確認要件(労働契約(Y)),
                       確認要件(契約の一部(X,Y)),
                       確認要件(労基法の基準に逹しない(Y)).


不法(_) :- 事件(Case,労働契約期間の違反),
           確認要件(Case,契約期間年数 > 3),
           不該当要件(Case,労働契約期間の例外(Case)).

労働契約期間の例外(Case) :- 確認要件(Case,一定の事業の完了に必要な期間を定める労働契約) ,
                            確認要件(Case, 契約期間終了日 =< 終期)
                            ;
                            確認要件(Case, 認定職業訓練を受ける労働者に係る労働契約) ,
                            確認要件(Case, 契約期間終了日 =< 終期)
                            ;
                            確認要件(Case, 専門知識労働者等又は60歳以上労働者の労働契約),
                            確認要件(Case, 契約期間年数 =< 5).
%% Implementation

事件(Case, Desc) :- 該当(Case, 事件(Desc)),
                    assertz(caseDescription(Desc)),
                    write(事件),put_char(':'), write(Desc), nl.

確認要件(Case,Cond) :- 該当(Case,要件(Cond)).

不該当要件(Case,Cond) :- 該当(Case,不該当要件(Cond)).


descList(Ds) :- retractall(該当(Case,Desc)),
                retractall(不該当(Case,Desc)),
                retractall(caseDescription(Desc)),
                retractall(condition(Case,Desc)),
                asserta(該当(Case,Desc)),
                asserta(不該当(Case,Desc)),
                bagof(X,不法(X),_),
                bagof(Desc, caseDescription(Desc),Ds),
                retractall(該当(Case,Desc)),
                retractall(不該当(Case,Desc)),
                retractall(caseDescription(Desc)),
                retractall(condition(Case,Desc)).

事件該当(_Case,Desc, Desc).

要件該当(Case,Cond) :-  call(Cond).

確認要件該当(Case,Cond) :- write(Case),
                       write(Cond),
                       read(Answer),
                       Answer = yes.

要件不該当(Case,Cond) :- \+ call(Cond).

確認要件不該当(Case,Cond) :- write(Case),
                             write(Cond),
                             read(Answer),
                             Answer = no.

:- dynamic 該当/2.

inquire :- descList(DescList),
           write(choose_number),nl,
           write(DescList) ,
           read(N),
           nth0(N,DescList,MyDesc),

           asserta(該当(Case,事件(Desc)) :- 事件該当(Case,Desc,MyDesc) ),
           asserta(該当(Case,要件(Desc)) :- 確認要件該当(Case,Desc) ),
           asserta(該当(Case,不該当要件(Desc)) :- 要件不該当(Case,Desc)),
           不法,
           retractall(該当(Case,Desc)).
