

事業場外労働のみなし労働時間(Case) :-
    要件(Case, 労働者が労働時間の全部又は一部について事業場外で業務に従事),
    要件(Case, 労働時間を算定し難い),
    要件(Case, 通常所定労働時間を超える必要あり), !,
    (要件(Case, 法定労働時間を超える)   -> 要件(Case, 労使協定を届け出る)),
    要件(Case, 事業場内の労働時間と場外で通常必要とされる時間内).


事業場外労働のみなし労働時間(Case) :-
    要件(Case, 労働者が労働時間の全部又は一部について事業場外で業務に従事),
    要件(Case, 労働時間を算定し難い),
    要件(Case, 所定労働時間内).


事業場外労働のみなし労働時間(Case,Wp,H) ,!.

%

専門業務型裁量労働制の対象業務(Case) :- 選択要件(Case, [ 研究開発,
                                                         その他 ] ,X ) ,
                                        要件(Case, 専門業務型裁量労働制の労使協定(Case,X)).

専門業務型裁量労働制の労使協定(Case,X) :- D = 有効期間中及び3年間,
                                          要件(Case, 対象業務とすること(X)),
                                          要件(Case, みなし労働時間を定める),
                                          要件(Case, 使用者が具体的な指示をしないこと),
                                          要件(Case, 健康及び福祉の確保措置を講じ記録保存(D)),
                                          要件(Case, 苦情処理措置を講じ記録保存(D)),
                                          要件(Case, 有効期間の定め),
                                          要件(Case, 労働時間の状況を記録保存(D)).


%%%
企画業務型裁量労働制の対象業務(Case,X)  :- 該当(Case, X, '企画・立案調査及び分析の業務'),
                                           該当(Case, X, 労働者の裁量に委ねる必要),
                                           該当(Case, X, 使用者が具体的な指示をしない),
                                           該当(Case, X, 企画業務型裁量労働制の決議(Case)).


企画業務型裁量労働制の対象労働者(Case,X)  :- 該当(Case, X, 対象業務遂行のため知識経験を有する),
                                             該当(Case, X, 同意).


企画業務型裁量労働制(Case) :- 要件(Case, 労使委員会を設置),
                              要件(Case, 労使委員会の要件に適合(Case)),
                              要件(Case, 労使委員会の5分の4の議決),
                              要件(Case, 労使委員会の議決を届出),
                              要件(Case, 定期報告).

労使委員会の要件に適合(Case) :-  要件(Case, '目的：労働条件について調査審議、意見を述べる'),
                                 要件(Case, その他).
