:- module(rouki_4_2,[法令違反/1,
         ]).
:- use_module(rouki_common).

%%%%%%%%%%%%%%%%%%%%%%%% Top Level Predicate %%%%%%%%%%%%%%%%%%%%%%%%%
%1
法令違反(Case) :- 事件(Case, 法定労働時間違反) ,
                  要件(Case, 原則の法定労働時間違反(Case)),
                  要件(Case, \+ 法定労働時間の特例(Case)),
                  要件(Case, \+ みなし労働時間制(Case)).
%2
法令違反(Case) :- 事件(Case, 休憩付与義務違反) ,
                  労働時間(Case,H),
                  ((H < 6 && H =< 8) -> 要件(Case, 休憩時間45分未満)),
                  ((H > 8)           -> 要件(Case, 休憩時間一時間未満)),
                  要件(Case,\+ 休憩時間の特例1(Case)),
%3
法令違反(Case) :- 事件(Case, 休憩一斉付与義務違反) ,
                  要件(Case, 休憩時間を一斉に与えない),
                  要件(Case,\+ 休憩時間の特例2(Case)).

%4
法令違反(Case) :- 事件(Case, 休憩自由利用保障義務違反) ,
                  要件(Case, 休憩時間を自由に利用させない),
                  要件(Case,\+ 休憩時間の特例3(Case)).


%%%%%%%%%%%%%%%%%%%%%%% Individual Predicates %%%%%%%%%%%%%%%%%%%%%%%%

%1
原則の法定労働時間違反(Case) :- 要件(Case, 週40時間を超え労働させる)
                                ;
                                要件(Case, １日8時間を超え労働させる) .

法定労働時間の特例(Case) :- 要件(Case, 特例対象事業(Case)),
                            要件(Case, 週44時間以内労働させる) ,
                            要件(Case, １日8時間以内労働させる) .

特例対象事業(Case) :- 選択要件(Case, 特例対象事業, [ 商業,
                                                     '映画・演劇(映画の制作除く)',
                                                     保健衛生,
                                                     接客娯楽 ] , _ ).

労働時間(Case) :- 選択要件(Case, 労働時間 , [ 業務命令で来客当番,
                                              2人乗車のトラックで一方が仮眠
                                            ]
                  ;
                  (要件(Case, 坑内労働)
                   -> 要件(Case, 坑内にいる時間)
                   ;  要件(Case,労働者の行為が使用者の指揮命令下に置かれたものと評価できる) ).

労働時間(Case,H) :- 事業場(Case, WPs) ,
                    add_work_hour(Case, WPs, H).

労働時間(Case,WP, H) :- 自然数要件(Case, 問(この事業場における労働時間(WP)) , H).

事業場(Case,Wp) :- リスト要件(Case, 労働する事業場をリストしてください).

add_work_hour(Case [] , 0).
add_work_hour(Case, [WP|WPs] , H ) :- 労働時間(Case,WP,H0) ,
                                      add_work_hour(Case,WPs,H1) ,
                                      H is H0 + H1.

%2
休憩時間の特例1(Case) :- 選択要件(Case, [ 長距離乗務員 , 小規模郵便局の郵便業務従事者 , その他一定の者 ],_).

%3
休憩時間の特例2(Case) :- 要件(Case,労使協定)
                         ;
                         選択要件(Case, [ 運輸交通 ,
                                          商業 ,
                                          その他 ] )
                         ;
                         要件(Case, 坑内労働).


%4
休憩時間の特例3(Case) :- 選択要件(Case, [ 警察官,
                                          その他 ] )
                         ;
                         要件(Case,児童養護施設等で児童と起居をともに),
                         要件(Case, あらかじめ労基署長の許可)
                         ;
                         要件(Case, 坑内労働).
