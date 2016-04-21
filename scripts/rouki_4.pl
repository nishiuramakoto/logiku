% -*- mode:prolog; -*-
:- module(rouki_4,[賃金/1,
                   労働の対償/1,
                   賃金の例外/1,
                   '任意・恩恵的性格のもの'/1,
                   福利厚生的性格のもの/1,
                   実費弁償的性格のもの/1,
                   平均賃金/2,
                   平均賃金の原則額/2,
                   平均賃金の起算日/2,
                   平均賃金の起算日/2,
                   平均賃金の算定期間/2,
                   直前の賃金締切日/3,
                   平均賃金の例外額/2,
                   平均賃金の例外額1/2,
                   平均賃金の例外額2/2,
                   平均賃金控除日数/3,
                   平均賃金控除額/3,
                   算定事由発生日/2,
                   雇入れ日/2
         ]).
:- use_module(rouki_common).

% 4-1

賃金(Case) :- 要件(Case,労働の対償(Case)),
              要件(Case, 使用者が労働者に支払うもの),
              要件(Case, \+ 賃金の例外(Case)).


労働の対償(Case) :- 要件(Case, 労働に対する見返り).

賃金の例外(Case) :- 要件(Case, '任意・恩恵的性格のもの'(Case)),
                    要件(Case, \+ 労働規範によって支給条件が明確)
                    ;
                    要件(Case, 福利厚生的性格のもの(Case)),
                    要件(Case, \+ 非貸与者に均衡手当を支給する住宅手当)
                    ;
                    要件(Case, 実費弁償的性格のもの(Case)),
                    要件(Case, \+ 通勤手当や労働協約による通勤定期券)
                    ;
                    要件(Case, 解雇予告手当)
                    ;
                    要件(Case, 休業補償費) ,
                    要件(Case, \+ 休業手当)
                    ;
                    要件(Case, 使用者が支払うものでない) ,
                    要件(Case, \+ 使用者が再配分するチップ).


'任意・恩恵的性格のもの'(Case) :- 選択要件(Case, 使用者からの支給,
                                         [結婚祝金,死亡弔慰金, 災害見舞金, 退職手当, その他 ], _).
福利厚生的性格のもの(Case)   :- 選択要件(Case, 使用者からの支給,
                                         [住宅の貸与, 生命保険料の補助, その他 ], _ ).
実費弁償的性格のもの(Case)   :- 選択要件(Case, 使用者からの支給,
                                         [制服の貸与,出張旅費, 交際費, その他 ], _ ).


平均賃金(Case,W) :- 要件(Case,日雇) , ! ,  W = 大臣の定める額
                    ;
                    平均賃金の原則額(Case,W1),
                    平均賃金の例外額(Case,W2),
                    Wpre  is max(W1,W2),
                    (Wpre is 0 -> W = 大臣の定めるところによる額; W = Wpre).


平均賃金の原則額(Case, W) :- 平均賃金の算定期間(Case, Period),
                             賃金総額(Case, Period, Total),
                             期間日数(Period, DN),
                             平均賃金控除日数(Case, Period, DS),
                             平均賃金控除額(Case, Period, TS),
                             ( DN > DS
                               ->  W is floor(100 * (Total - TS) / (DN - DS)) / 100
                               ;   W is 0 ).

賃金総額(Case,Period,Total) :- 自然数要件(Case, 問(次の期間内の賃金総額,Period), Total) .

平均賃金の起算日(Case,D) :- 算定事由発生日(Case, D0),
                            直前の賃金締切日(Case,D0,D) , !.

平均賃金の起算日(Case,D) :- 算定事由発生日(Case, D0) ,
                            日前(D0, 1, D).

平均賃金の算定期間(Case, P) :- 平均賃金の起算日(Case, DW),
                               日前(DW ,-1, DW1),
                               月前(DW1, 3, D0),

                               雇入れ日(Case, DE),


                               (早い(DE,D0) ->  P  = 期間(D0, DW1) ; P = 期間(DE,DW1)).


直前の賃金締切日(Case,DN, D) :- 要件(Case,賃金締切日がある),
                                暦日要件(Case, 直前の賃金締切日(DN), D),
                                早い(D,DN).


平均賃金の例外額(Case, W) :- 平均賃金の例外額1(Case,W1),
                             平均賃金の例外額2(Case,W2) ,
                             W is floor( 100 * (W1 + W2) ) /100.


平均賃金の例外額1(Case, X) :-
    要件(Case, '日給・時間給・出来高払制その他請負制の場合')  , ! ,
    自然数要件(Case, '日給・時間給・出来高払制その他請負制の期間の賃金総額',T) ,
    自然数要件(Case, '日給・時間給・出来高払制その他請負制の期間の労働日数(休日含まず)', DN),
    平均賃金控除日数(Case, '日給・時間給・出来高払制その他請負制の労働日数(休日含まず)', DS),
    平均賃金控除額(Case, '日給・時間給・出来高払制その他請負制の労働日数(休日含まず)' , TS) ,
    (DN > DS
     -> X is ((T-TS)/(DN-DS)) * (60/100)
     ;  X is 0).

平均賃金の例外額1(_Case, X) :- X is 0.

平均賃金の例外額2(Case, X) :-
    要件(Case, '賃金の一部が月・週・その他一定の期間によって定められた場合') ,! ,
    自然数要件(Case, 'その部分の総額', T) ,
    自然数要件(Case, 'その期間の総日数', DN) ,
    平均賃金控除日数(Case, 'その期間', DS) ,
    平均賃金控除額(Case, 'その期間', TS) ,
    (DN > DS
     -> X is (T-TS)/(DN-DS)
     ;  X is 0).
平均賃金の例外額2(_Case, X) :- X is 0.


平均賃金控除日数(Case,Period, DS) :-
    write(Period),
    write( [ 業務上の疾病で休業 ,
             産前産後休業 ,
             使用者責任の休業,
             育児休業,
             介護休業,
             試用期間 ]) ,
    自然数要件(Case,期間内の上記日数を入力して下さい,DS).


平均賃金控除額(Case,Period, TS) :-
    write(Period),
    write( [ 業務上の疾病で休業 ,
             産前産後休業 ,
             使用者責任の休業,
             育児休業,
             介護休業,
             試用期間 ,
             臨時賃金 ,
             '3月超ごとの賃金' ,
             通貨以外で一定範囲外のもの ]) ,
    自然数要件(Case,期間内の上記の賃金額を入力して下さい,TS).


算定事由発生日(Case,D) :- 暦日要件(Case, 算定事由日を入力してください(
                                                 [ 解雇通告日 ,
                                                   最初の休業日,
                                                   年次有給休暇取得初日,
                                                   '負傷・死亡の原因事故発生日',
                                                   疾病の診断による発生日,
                                                   減給制裁の意思到達日
                                             ]) , D).


雇入れ日(Case,D) :- 暦日要件(Case, 雇入れの日 , D).