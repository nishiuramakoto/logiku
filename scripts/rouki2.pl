% 適用の範囲、労働者と使用者の定義


適用事業所(X) :- 要件(労働者を一人以上使用(X)),
                 \+ 適用除外(X), ! ,
                 (要件(事業(X)) ; 要件(事務所(X))).


適用除外(X) :- 全部適用除外(X)
               ;
               一部適用除外(X).

全部適用除外(X) :- 要件(同居の親族のみを使用(X))
                   ;
                   要件(家事使用人(X))
                   ;
                   要件(一般職の国家公務員(X)).

一部適用除外(X) :- 要件(一般職の地方公務員(X))
                   ;
                   要件(船員法に規定する船員(X)).

労働者(X) :- 要件(事業に使用される(X)),
             要件(賃金を支払われる(X)).

使用者(X) :- 要件(事業主(X))
             ;
             要件(事業の労働者に関する事項について事業主のために行為をする(X)).

%% Implementation


要件(X) :- write(X),
               read(Answer),
               Answer = yes.
