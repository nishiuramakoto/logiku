% -*- mode: prolog; -*-
:- module(rouki_7, [ 月単位の変形労働時間制/1,
		     月変形期間の週平均が法定労働時間を超えない/1,
		     月変形労働時間_法定労働時間の総枠/2,
		     月変形期間の所定労働時間の合計/2,
		     月単位の変形労働時間制の定め/1,
		     月変形労働時間_時間外労働時間/2,
		     月変形労働時間_期間/2,
		     月変形労働時間_時間外労働_日合計/3,
		     月変形労働時間_時間外労働_日/4,
		     月変形労働時間_時間内労働時間_日/4,
		     月変形労働時間_日労働時間/3,
		     月変形労働時間_日所定労働時間/3,
		     月変形労働時間_時間外労働_週合計/3,
		     月変形労働時間_時間外労働_週/4,
		     月変形労働時間_時間外労働_期間合計/3,
		     月変形労働時間_時間外労働_期間/3,
		     月変形労働時間_時間内労働時間_週/4
         ]).
:- use_module(rouki_common).
:- use_module(rouki_5).


% １ヶ月単位の変形労働時間制

月単位の変形労働時間制(Case) :- 要件(Case, 月単位の変形労働時間制の定め(Case)),
				要件(Case, '1月以内の期間'),
				要件(Case, 変形期間の週平均が法定労働時間を超えない(Case) ) ,
				要件(Case, 各日の労働時間を全て特定 ),
				要件(Case, 各週の労働時間を全て特定 ),
				要件(Case, \+ 変形労働時間制の例外(Case)).


月変形期間の週平均が法定労働時間を超えない(Case) :- 月変形労働時間_法定労働時間の総枠(Case,H1),
						  変形期間の所定労働時間の合計(Case,H2),
						  H2 =< H1.

月変形労働時間_法定労働時間の総枠(Case,H) :- 週法定労働時間(Case,HW),
					     月変形労働時間_期間(Case,P),
					     期間日数(P,D),
					     H is HW * D / 7.

月変形期間の所定労働時間の合計(Case,H2) :- 自然数要件(Case, 変形期間の所定労働時間の合計 , H2).

月単位の変形労働時間制の定め(Case) :- (要件(Case, 労使協定で定める) -> 要件(Case, 届出)
				       ; 要件(Case, 就業規則等で定める)),
				      全選択要件(Case, 定める事項,
						 [ '変形期間の長さ(1月以内)とその起算日',
						   '対象労働者の範囲',
						   '変形期間における各日・各週の労働時間'
						]),
				      ( 要件(Case, 労使協定) -> 要件(Case, 労使協定の有効期間を定める) ).


月変形労働時間_時間外労働時間(Case,HO) :- 月変形労働時間_期間(Case, P) ,
					  月変形労働時間_時間外労働_日合計(Case,P,HD) ,
					  月変形労働時間_時間外労働_週合計(Case,P,HW) ,
					  月変形労働時間_時間外労働_期間合計(Case,P,HT) ,
					  HO is HD + HW + HT.

月変形労働時間_期間(Case,P) :- 期間要件(Case, 月変形労働時間の期間 , P).

月変形労働時間_時間外労働_日合計(Case, P, HD) :-
    bagof(OverWorkHourDay, 月変形労働時間_時間外労働_日(Case,P, _D, OverWorkHourDay) , HS) ,
    sumlist(HS, HD).

月変形労働時間_時間外労働_日(Case,P,D,H) :-
    write(月変形労働時間_時間外労働_日(P,D)),
    期間内日(P,D),
    日法定労働時間(Case,HL),
    月変形労働時間_日所定労働時間(Case,D,HD),
    月変形労働時間_日労働時間(Case,D,H1),
    H is max(0, H1 - max(HL,HD)).

月変形労働時間_時間内労働時間_日(Case,P, D, H) :-
    期間内日(P,D),
    日法定労働時間(Case,HL),
    月変形労働時間_日所定労働時間(Case,D,HD),
    月変形労働時間_日労働時間(Case,D,H1),
    H is min(H1, max(HD,HL)).

月変形労働時間_日労働時間(Case,D,H) :-
    自然数要件(Case, '月変形労働時間 ~w日の労働時間', [D] , H).

月変形労働時間_日所定労働時間(Case,D,H) :-
    自然数要件(Case, '月変形労働時間 ~w日の所定労働時間', [D] , H).

月変形労働時間_週所定労働時間(Case,W,H) :-
    自然数要件(Case, '月変形労働時間 ~w週の所定労働時間', [W] , H).


月変形労働時間_時間外労働_週合計(Case, P, HW) :-
    bagof(OverWorkHourWeek, 月変形労働時間_時間外労働_週(Case,P, _D, OverWorkHourWeek) , HS) ,
    sumlist(HS, HW).

月変形労働時間_時間外労働_週(Case,P,D,H) :-
    期間内週(P,W),
    週法定労働時間(Case,HL),
    月変形労働時間_週所定労働時間(Case,W,HD),
    bagof(WorkHourDay, 月変形労働時間_時間内労働時間_日(Case,W, D, WorkHourDay) , HS) ,
    sumlist(HS, HO),
    H is max(0, HO - max(HD,HL)).


月変形労働時間_時間外労働_期間合計(Case, P, HW) :-  月変形労働時間_時間外労働_期間(Case, P, HW).



月変形労働時間_時間外労働_期間(Case, P, H) :-
    月変形労働時間_法定労働時間の総枠(Case, HL),
    bagof(WorkHourWeek, 月変形労働時間_時間内労働時間_週(Case, P, _W, WorkHourWeek) , HS),
    sumlist(HS,HO),
    H is max(0, HO - HL).



月変形労働時間_時間内労働時間_週(Case, P, W, WorkHourWeek) :-
    期間内週(P,W),
    週法定労働時間(Case,HL),
    月変形労働時間_週所定労働時間(Case, W, HD),
    bagof(WorkHourDay, 月変形労働時間_時間内労働時間_日(Case, W, _D , WorkHourDay), HS),
    sumlist(HS,HO),
    WorkHourWeek is min( HO ,  max(HD,HL) ).
