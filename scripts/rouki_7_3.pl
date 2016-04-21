% -*- mode: prolog; -*-


年変形労働時間制(Case) :- 要件(Case, 労使協定(Case)),
			  要件(Case, 対象期間(Case, P)),
			  要件(Case, 対象期間の労働日数の限度(Case, P)),
			  要件(Case, 連続労働日数の限度(Case, P)),
			  要件(Case, 労働時間の限度(Case,P)),
			  要件(Case, 労働時間の特定(Case)),
			  要件(Case, \+ 変形労働時間制の例外(Case)).

対象期間(Case, P) :- 期間要件(Case, 年変形労働時間制の期間 , P),
		     期間月数(P,MN) , MN >  1,
		     期間年数(P,YN) , YN =< 1.


労働時間の特定(Case) :- 要件(Case, 各日・各週の所定労働時間を特定),
			要件(Case, 週平均の労働時間が40時間を超えない)

労使協定(Case) :- 要件(Case, 対象労働者の範囲),
		  要件(Case, 対象期間と起算日),
		  要件(Case, 特定期間),
		  要件(Case, 対象期間における労働日と労働時間の特定(Case)),
		  要件(Case, 労使協定の有効期間),
		  要件(Case, 届出).

対象期間における労働日と労働時間の特定(Case) :-
    要件(Case, 対象期間における労働日と労働日ごとの労働時間),
    ;
    要件(Case, 対象期間を1月以上の期間ごとに区分),
    要件(Case, 最初の期間の労働日と労働時間),
    要件(Case, 当初2番目以後の期間の労働日数と総労働時間),
    要件(Case, 当初2番目以後の期間の30日前に過半数組合等の同意と書面で労働日と各日労働時間を特定).


対象期間の労働日数の限度(Case, P)    :- 対象期間の労働日数の限度(Case, P, N) ,
					日数(P,PN) ,
					PN =< N.

対象期間の労働日数の限度(Case, P, N) :-
    月数(P,PM) ,  PM > 3 ,
    日数(P,PD) ,
    N is floor( 280 * PD / 365 ).

対象期間の労働日数の限度(Case, P, N) :-
    月数(P,PM) ,  PM =< 3 ,
    日数(P,PD) ,
    N is PD.


連続労働日数の限度(Case, P, N) :- 要件(Case, 特定期間(Case,P) ), !,
				   一週間に１日の休日が確保できる日数.

連続労働日数の限度(Case, P, N) :- N is 6.



労働時間の限度(Case,P) :- 要件(Case, 積雪地域の建設業の屋外労働者等) , !,
			  要件(Case, １日について10時間、1週間について52時間).


労働時間の限度(Case,P) :- 要件(Case, 隔日勤務のタクシーの運転手), !,
			  要件(Case, １日について16時間、1週間について52時間).

労働時間の限度(Case,P) :- 月数(P,M) , M > 3 , ! ,
			  要件(Case, １日について10時間、1週間について52時間),
			  要件(Case, 対象期間全体を通時労働時間48時間超の週が連続3以下),
			  要件(Case, 対象期間を3月ごとに区分した各期間、週労働時間48時間超の週の初日の数が3以下).

労働時間の限度(Case,P) :- 要件(Case, １日について10時間、1週間について52時間).

途中退職者への割増賃金の支払(Case) :- 要件(Case,労働期間が対象期間より短い),
				      要件(Case, 週平均の労働時間が40時間超 ),
				      要件(Case, 超えた時間につき割増賃金を払う ) .
