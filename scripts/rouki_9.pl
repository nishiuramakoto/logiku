

年休権 :- 6か月継続勤務,
	  全労働日の8割出勤率.

年休日数(Case,N) :- 継続勤務期間(Case,P),
		    年休日数(Case,P,N).


年休日数(Case,P,D) :- 比例付与割合(Case,P,R), !,
		      原則年休日数(Case,P,N),
		      D is floor( R * N) .

年休日数(Case,P,D) :- 原則年休日数(Case,P,D).


原則年休日数(Case,P,20) :- 月数(P,M) , M >= 78 , !.
原則年休日数(Case,P,18) :- 月数(P,M) , M >= 66 , !.
原則年休日数(Case,P,16) :- 月数(P,M) , M >= 54 , !.
原則年休日数(Case,P,14) :- 月数(P,M) , M >= 42 , !.
原則年休日数(Case,P,12) :- 月数(P,M) , M >= 30 , !.
原則年休日数(Case,P,11) :- 月数(P,M) , M >= 18 , !.
原則年休日数(Case,P,10) :- 月数(P,M) , M >= 6 , !.

比例付与割合(Case,P,R) :- 週所定労働日数(Case,P,WD),
			WD =< 4,
			週所定労働時間(Case,P,WH),
			WH < 30,
			R is WD / 5.2

比例付与割合(Case,P,R) :- 年間所定労働日数(Case,P,WD),
			WD =< 216,
			週所定労働時間(Case,P,WH),
			WH < 30,
			R is WD /(365/7) / 5.2.





継続勤務 :- 長期休職
	    ;
	    臨時工を正規に切替
	    ;
	    在籍出向
	    ;
	    定年後に引き続き再雇用
	    ;
	    解散会社が新会社に承継される
	    ;
	    その他労働契約の存続期間.


出勤率(Case,P,R) :- 計算対象期間(Case,P) ,
		    出勤日数(Case,P,WN),
		    出勤みなし日数(Case,P,WA),
		    全労働日数(Case,P,DN),
		    全労働日除外日数(Case,P,DE),
		    R is (WN + WA) / (DN - DE).

出勤みなし日数 :- 業務上傷病による休業(Case,D0)
		  産前産後休業(Case,D1),
		  育児・介護休業(Case,D2),
		  年休取得日(Case,D3),
		  労働者責によらない不就労日(Case,D4),
		  使用者の不可抗力による休業日(Case,E0),
		  使用者側に起因する経営管理上の障害による休業日(Case,E1),
		  正当な争議行為による労務不提供日(Case,E2),
		  D is D0 + D1 + D2 + D3 + D4 - E0 - E1 - E2.

全労働日除外日数(Case,P,DE) :-  使用者の不可抗力による休業日(Case,E0),
				使用者側に起因する経営管理上の障害による休業日(Case,E1),
				正当な争議行為による労務不提供日(Case,E2),
				所定休日労働,
				終日代替休暇.

計算対象期間(Case,P) :- 雇入れ直後, !,
			P = 6月
			;
			P = 1年ずつ.

年休の自由利用 :- 年休権.
年休で使用者利益に反する街頭行動や他社でのアルバイト :- 年休権.
年休をその事業場での争議行為に利用することを使用者が拒否 :- 年休権.

法令違反 :- 年休権,
	    利用方法の干渉.

%% 付与日数

違法 :- 基準日(Case,P),
	年休権(Case,P),
	P 以降に年休日数を減らす.

時間単位年休 :- 労使協定,
		1年に5労働日まで,
		時間単位で年休を付与.

労使協定 :- 労働者の範囲,
	    '時間単位年休の日数(繰越分含む)',
	    時間単位年休の１日相当時間,
	    付与単位.

年休の時効成立 :- 基準日から2年.

斉一的取扱い :- すべての労働者の基準日を統一,
		短縮された期間は全期間出勤とみなす,
		翌年以降も最低同じ日数繰上げ.

時季指定権 :- 年休権,
	      \+ 計画的付与.

指定時期の年休取得 :- 時季指定権,
		      労働日,
		      \+ 休職期間,
		      \+ (育児休業 , 休業申出後),
		      \+ 使用者の時期変更権

時季変更権 :- 請求時期が事業の正常な運営を妨げる,
	      他の時期に与える,
	      解雇時期を超えない,
	      \+ 計画的付与.

計画的付与 :- 労使協定,
	      '5日を超える部分(繰越含む)について計画的付与',
	      \+ 時間単位年休,
	      選択要件(Case, [ 事業場での一斉付与 ,
			       班別の交替制付与,
			       個人別付与,
			       その他
			     ] , X).
