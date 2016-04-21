
% 割増賃金


最低割増賃金率(Case, .50) :- 深夜労働(Case) , 時間外労働 ,! .
最低割増賃金率(Case, .60) :- 深夜労働(Case) , 休日労働 ,! .
最低割増賃金率(Case, .35) :- 時間外労働(Case) , 休日労働 ,! .

最低割増賃金率(Case, .25) :- 時間外労働.
最低割増賃金率(Case, .35) :- 休日労働.
最低割増賃金率(Case, .25) :- 深夜労働(Case).

割増賃金率(Case, R) :- 最低割増賃金率(Case, R0) , R >= R0.

時間内深夜労働時間_割増賃金率(Case,R) :- R > 0.25.
時間外昼間労働時間_割増賃金率(Case,R) :- R > 0.25.
時間外深夜労働時間_割増賃金率(Case,R) :- R > 0.50.

休日昼間労働労働時間_割増賃金率(Case,R) :- R > 0.35.
休日深夜労働労働時間_割増賃金率(Case,R) :- R > 0.60.


労働日賃金(Case, Period, Salary) :-
    時間内昼間労働時間(Case,Period,Period0), 時間数(Period0,H0),

    時間内深夜労働時間(Case,Period,Period1), 時間数(Period0,H1),
    時間内深夜労働時間_割増賃金率(Case,R1),

    時間外昼間労働時間(Case,Period,Period2), 時間数(Period0,H2),
    時間外昼間労働時間_割増賃金率(Case,R2),

    時間外深夜労働時間(Case,Period,Period3), 時間数(Period0,H3),
    時間外深夜労働時間_割増賃金率(Case,R3),

    計算単価(Case, Unit),
    Salary is Unit * ( H0 + H1 * R1 + H2 * R2 + H3 * R3 ).


休日賃金(Case, Period, Salary) :-
    昼間労働時間(Case,Period,Period0), 時間数(Period0,H0),
    休日昼間労働時間_割増賃金率(Case,R0),
    深夜労働時間(Case,Period,Period1), 時間数(Period0,H1),
    休日深夜労働時間_割増賃金率(Case,R1),

    計算単価(Case, Unit),
    Salary is Unit * ( H0 * R0 + H1 * R1 ).


計算単価(Case, S) :- 時間給(Case,S1) ,
		     日給の時間単価(Case,S2) ,
		     週給の時間単価(Case,S3),
		     月給の時間単価(Case,S4),
		     その他一定期間の時間単価(Case,S5),
		     出来高払い等の時間単価(Case,S6),
		     除外賃金(Case,E),
		     総労働時間(Case,H),
		     S is S1 + S2 + S3 + S4 + S5 + S6 - E / H.

日給の時間単価(Case,S) :- 週の日平均所定労働時間(Case,H),
			  日給(Case,D),
			  S is D / H .

週給の時間単価(Case,S) :- 4週の週平均所定労働時間(Case,H),
			  週給(Case,D),
			  S is D / H.

月給の時間単価(Case,S) :- 1年の月平均所定労働時間(Case,H),
			  月給(Case,D),
			  S is D / H.

その他一定期間の時間単価(Case,S) :- 日給等に準じる単価(Case,S).

出来高払い等の時間単価(Case,S) :- 賃金総額(Case,T),
				  総労働時間数(Case,H),
				  S is T / H.

除外賃金(Case,E) :- 家族手当,
		    通勤手当,
		    別居手当,
		    子女教育手当,
		    住宅手当,
		    臨時賃金,
		    月超ごとの賃金 .

月賃金(Case, Salary) :-
    中小企業, ! ,
    月起算日(Case,D0),
    次月起算日(Case,Dend),
    bagof(S, (日賃金(Case,D, S) , 期間内(期間(D0,Dend), D)) , Ss ),
    sum(Ss, ST),
    Salary is ST.

月賃金(Case, Salary) :-
    月起算日(Case,D0),
    時間外労働60時間到達日(Case, D60),
    時間外労働60時間到達時(Case, T60),
    bagof(S, (日賃金(Case,D, S) , 期間内(期間(D0,D60), D)) , Ss ),
    sum(Ss, ST),
    特例到達日賃金(Case, T60, SD60),
    bagof(S, (特例日賃金(Case,D, S) , 期間内(期間(D60next , Dend) , D)) , S60 ),
    代替休暇相当賃金(Case,H).
    Salary is ST + SD60 + S60 - H.

特例日賃金(Case,D,S) :- 特例労働日賃金(Case,D,SW),
			休日賃金(Case,D,SH),
			S is SW + SH.

特例労働日賃金(Case,D,SW) :-
    時間内昼間労働時間(Case,Period,Period0), 時間数(Period0,H0),

    時間内深夜労働時間(Case,Period,Period1), 時間数(Period0,H1),
    時間内深夜労働時間_割増賃金率(Case,R1),

    時間外昼間労働時間(Case,Period,Period2), 時間数(Period0,H2),
    時間外昼間労働時間_特例割増賃金率(Case,R2),

    時間外深夜労働時間(Case,Period,Period3), 時間数(Period0,H3),
    時間外深夜労働時間_特例割増賃金率(Case,R3),

    計算単価(Case, Unit),
    Salary is Unit * ( H0 + H1 * R1 + H2 * R2 + H3 * R3 ).

時間外昼間労働時間_特例割増賃金率(Case,R) :- R > 0.5.
時間外深夜労働時間_特例割増賃金率(Case,R) :- R > 0.75.

代替休暇相当賃金(Case, S) :-
    代替休暇時間数(Case, H),
    代替休暇換算率(Case, R),
    計算単価(Case,U),
    S is H * R * U.

代替休暇換算率(Case, R) :-
    時間外昼間労働時間_特例割増賃金率(Case,R1),
    時間外昼間労働時間_割増賃金率(Case,R2),
    R is R1 - R2.
