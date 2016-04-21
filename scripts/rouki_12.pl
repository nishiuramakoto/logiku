% -*- mode:prolog -*-
% 就業規則

就業規則の作成・届出の義務 :- 常時10人以上使用する使用者

就業規則の作成・届出の義務違反 :- \+ 就業規則の作成・届出の義務遵守.

就業規則の作成・届出の義務遵守 :- 就業規則を作成,
				  就業規則の作成手続,
				  全労働者に適用,
				  所轄署に届出.

就業規則の届出の義務     :- 必要記載事項を変更した使用者
就業規則の届出の義務違反 :- \+ 就業規則の届出の義務遵守.
就業規則の届出の義務遵守 :- 所轄署に届出.

% 必要記載事項

絶対的必要記載事項(始業及び就業の時刻).
絶対的必要記載事項(休憩時間).
絶対的必要記載事項(休日).
絶対的必要記載事項(休暇).
絶対的必要記載事項(就業時転換) :- 交替操業.
絶対的必要記載事項(賃金の決定).
絶対的必要記載事項(賃金の計算).
絶対的必要記載事項(賃金の支払の方法).
絶対的必要記載事項(賃金の締切).
絶対的必要記載事項(賃金の支払時期).
絶対的必要記載事項(昇給).
絶対的必要記載事項(退職).
絶対的必要記載事項(解雇).

相対的必要記載事項(退職手当).
相対的必要記載事項(臨時の手当).
相対的必要記載事項(最低賃金額).

相対的必要記載事項(食費その他の負担).
相対的必要記載事項(安全衛生).
相対的必要記載事項(職業訓練).
相対的必要記載事項(災害補償及び業務外傷病扶助).
相対的必要記載事項(表彰).
相対的必要記載事項(制裁).
相対的必要記載事項(労働者のすべてに適用される定め).

% 就業規則の作成手続

就業規則の作成手続 :- 就業規則の作成又は変更,
		      過半数組合等の意見を聴く,
		      意見を書面に記し申出に添付,
		      記名押印.

% 制裁

懲戒の判例違反 :- 労働者を懲戒,
		  あらかじめ懲戒の種別及び事由を就業規則で定めない.

% 減給

減給の制限 :- 減給の制裁を就業規則で定める,
減給の制限遵守 :- 減給の制限,
		  1回の額が平均賃金の半額以下,
		  総額が一賃金支払期の総額の1割以下.

減給の制限違反 :- 減給の制限,
		  \+ 減給の制限遵守.

% 就業規則の効力

就業規則の法令等遵守 :- 就業規則,
			法令を遵守,
			労働協約を遵守.

所轄署の就業規則変更命令 :- 就業規則,
			    \+ 就業規則の法令等遵守

所轄署の就業規則変更命令違反 :- 所轄署の就業規則変更命令,
				就業規則を変更しない.

% 優先関係

採用する定め(Case, Matter, L ) :- 法令の定め(Case, Matter, L),
				  労働協約の定め(Case, Matter, A),
				  水準比較(Case,Matter, L , > , A).


採用する定め(Case, Matter, A ) :- 労働協約の定め(Case, Matter, A),
				  就業規則の定め(Case, Matter, R),
				  水準比較(Case,Matter, A , > , R).

採用する定め(Case, Matter, R ) :- 就業規則の定め(Case, Matter, R),
				  労働契約の定め(Case, Matter, C),
				  水準比較(Case,Matter, R , > , C).

% 判例
就業規則の法的規範性 :- 就業規則,
			合理的な労働条件を定める.


就業規則の不利益変更 :- 合理的,

就業規則の不利益変更の遵守義務 :- 就業規則の不利益変更.

就業規則の拘束力 :- 労働者に周知する手続.
