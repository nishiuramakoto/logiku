:- use_module(library(lists)).

法令違反 :- 法令違反(_).
法令違反(Case) :- 不法(Case) ; 有期労働契約基準違反(Case).
不法 :- 不法(_).


不法(Case) :- 事件(Case, 劣悪労働条件) ,
	      要件(Case,労働条件が労基法の基準に逹しない(Case)).


不法(Case) :- 事件(Case,労働契約期間の違反),
              要件(Case,契約期間年数(Case) > 3),
              要件(Case, \+ 労働契約期間の例外(Case)).

% 法附則137条
不法(Case) :- 事件(Case,早期任意退職),
	      ( 要件(Case, 労働契約期間の例外(Case)),
		要件(Case, 契約期間内に任意退職)
		;
		要件(Case, \+ 労働契約期間の例外(Case)),
		( 要件(Case, (労働契約期間年数(Case,D) , D =< 1)) ;
		  要件(Case, (退職日が1年経過せず(Case) )))).


労働契約期間の例外(Case) :- 要件(Case,一定の事業の完了に必要な期間を定める労働契約) ,
                            要件(Case, 契約期間終了日 =< 終期)
                            ;
                            要件(Case, 認定職業訓練を受ける労働者に係る労働契約) ,
                            要件(Case, 契約期間終了日 =< 終期)
                            ;
                            ( 要件(Case, 専門知識労働者等の労働契約(Case)) ;
                              要件(Case, 満60歳以上労働者の労働契約 )),
                            要件(Case, 契約期間年数 =< 5).

専門知識労働者等の労働契約(Case) :- 要件(Case,労働契約(Case,_Employer,Employee)),
                                    要件(Case,専門知識労働者(Case,Employee,K)),
				    要件(Case,従事(Case,Employee,K)).


専門知識労働者(Case,E,K) :- 専門知識リスト(L), 要件リスト(Case,有する専門知識等,L,K)
			    ;
			    要件リスト(Case,有する専門知識等,[ 農林水産業, システムエンジニア ],K),
			    要件(Case, (年収万円(E,Salary), Salary >= 1075)) ,
			    要件(Case, 保持(E,K)).


専門知識リスト([ 博士の学位,
		 公認会計士, 医師, 歯科医師, 獣医師, 弁護士,
		 一級建築士,税理士,薬剤師,社会保険労務士, 不動産鑑定士, 技術士,弁理士,
		 itストラテジスト,システムアナリスト,アクチュアリー,
		 特許発明者,登録意匠創作者,登録品種育成者 ]).


有期労働契約基準違反 :- 有期労働契約基準違反(_Case).

有期労働契約基準違反(Case) :- 事件(Case, 雇止めの予告),
			      有期労働契約基準対象契約1(Case),
			      (要件(Case,(雇止めの予告日(Case,Day1), 契約期間満了日(Case,Day2), Day2 - Day1 < 30))
			       ;
			       要件(Case,(使用者が雇止め前又は後の不更新理由請求に対し遅滞なく証明書を交付しない))).

有期労働契約基準違反(Case) :- 事件(Case, 契約期間の配慮),
			      有期労働契約基準対象契約2(Case),
			      (要件(Case, 使用者が有期契約を更新しようとした),
			       要件(Case, \+ 使用者が契約の実態及び労働者の希望に応じできるだけ契約期間を長くするよう努めた)).

有期労働契約基準対象契約1(Case) :- 要件(Case, 有期労働契約(Case)) ,
				   ( 要件(Case, (更新回数(Case,N) , N >= 3))
				     ; 要件(Case, 継続勤務1年超)),
				   要件(Case, \+ あらかじめ不更新を明示).

有期労働契約基準対象契約2(Case) :- 要件(Case, 有期労働契約(Case)) ,
				   ( 要件(Case, (更新回数(Case,N) , N >= 1))
				     , 要件(Case, 継続勤務1年超)).


%% Implementation

:- dynamic 事件書き出し/0.
:- dynamic 事件記述/1.

事件書き出し :- fail.

事件(Case, Desc) :- 事件書き出し ,
                    assertz(事件記述(Desc)),
                    write(事件),put_char(':'), write(Desc), nl , !
		    ;
		    Case = Desc.

要件(Case,Cond) :-  事件書き出し , !
		    ;
		    write(要件), print(Cond),
		    catch(Cond, Err, 確認要件(Err,Case,Cond)).

確認要件(Err,Case,Cond) :- write(yes_no_or_dontKnow),nl,
			   write(Err),nl,
			   write(Cond),nl,
			   read(Answer),
			   確認要件(Err,Case,Cond,Answer).

確認要件(_Err,_Case,_Cond, yes).
確認要件(_Err, Case, Cond, dontKnow) :- write(確認要件), 要件(Case, Cond).

要件リスト(Case,Desc,List,Elem) :- write(choose_number),nl,
				   write(Case), write(Desc),nl,
				   write(List),
				   read(N),
				   nth0(N,List,Elem).



事件リスト(Ds) :- retractall(事件記述(Desc)),
		  retractall(事件書き出し),
                  asserta(事件書き出し),
                  bagof(X,法令違反(X),_),
                  bagof(Desc, 事件記述(Desc),Ds),
                  retractall(事件記述(Desc)),
		  retractall(事件書き出し),
		  asserta(事件書き出し :- fail).


chooseCase :- 事件リスト(List),
              write(choose_number),nl,
              write(List) ,
              read(N),
              nth0(N,List,Desc),
              法令違反(Desc).
