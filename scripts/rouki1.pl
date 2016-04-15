% 共通
不法 :-  不法(_X).
適法 :-  /+(不法).

労働規範(労働協約).
労働規範(就業規則).
労働規範(労働契約).

労働関係の当事者(使用者).
労働関係の当事者(労働者).

使用者が3条差別(Reason) :- 均等待遇差別理由(Reason).

均等待遇差別理由(国籍).
均等待遇差別理由(信条).
均等待遇差別理由(社会的身分).

法6条例外(職業安定法の規定による職業紹介事業).
法6条例外(職業安定法の規定による委託募集).
法6条例外(その他).

% 法1条
不法(罰則なし) :-  事件(Case, 労働条件が劣悪),
                要件(Case, 労働条件が人たるに値する生活を営むための必要を満たさない).

不法(罰則なし) :-  事件(Case, 労働条件が低下または向上せず),
                (要件(Case,労基法の基準を理由として労働条件を低下させる);
                 要件(Case,労働条件を向上を図るように努めない)).

% 法2条
不法(罰則なし) :-  事件(Case,労働条件の決定),
                要件(Case, 労働関係の当事者が対等に決定しない).
不法(罰則なし) :-  事件(Case, 労働規範の不遵守),
                   労働規範(Rule),
                   (不該当要件(Case, 当事者が遵守(Rule)) ;
                    不該当要件(Case, 当事者が誠実に履行(Rule))).

% 法3条

不法(_) :-  事件(Case, 労働条件につき差別),
            要件(Case, 使用者が3条差別(Reason)),
            均等待遇差別理由(Reason).


% 法4条
不法(_) :- 事件(Case,賃金につき事業者が女性を差別),
           要件(Case, 賃金で差別),
           要件(Case, 女性であることを理由として).

%法5条
不法(_) :-  事件(Case, 労働の強制),
            要件(Case, 労働を強制),
            要件(Case, 労働者の精神又は身体の自由を不当に拘束),
            要件(Case, 労働者の意思に反する).

%法6条
不法(_) :-  事件(Case, 中間搾取),
            要件(Case, 他人の就業に介入して利益を得る),
            要件(Case, 業として),
            要件(Case, not(法6条例外(Case))).


% 法7条
不法(_) :- 事件(Case,公民権行使の拒否),
           (要件(Case,労働者が公民権行使又は公職執行のため時間を請求),
            (要件(Case, 使用者が時間請求を拒否);
             要件(Case, 使用者が公民権行使に妨げのある時刻に変更))).

%% Implementation
事件(Case, Desc) :- 該当(Case, 事件(Desc)),
                    assertz(desc(Desc)),
                    write(事件),put_char(':'),  write(Desc), nl.

要件(Case,Cond) :- 該当(Case,要件(Cond)),
                   assertz(cond(Case,Cond)).

不該当要件(Case,Cond) :- 該当(Case,不該当要件(Cond)),
                         assertz(notCond(Case,Cond)).

descList(Ds) :- retractall(該当(Case,Desc)),
                retractall(不該当(Case,Desc)),
                retractall(desc(Desc)),
                retractall(cond(Case,Desc)),
                asserta(該当(Case,Desc)),
                asserta(不該当(Case,Desc)),
                bagof(X,不法(X),_),
                bagof(Desc, desc(Desc),Ds),
                retractall(該当(Case,Desc)),
                retractall(不該当(Case,Desc)),
                retractall(desc(Desc)),
                retractall(cond(Case,Desc)).



事件該当(_Case,Desc, Desc).

要件該当(Case,Cond) :- write(Case),
                       write(Cond),
                       read(Answer),
                       Answer = yes.

要件不該当(Case,Cond) :- write(Case),
                         write(Cond),
                         read(Answer),
                         Answer = no.

:- dynamic 該当/2.

inquire :- descList(DescList),
           write(choose_number),nl,
           write(DescList) ,
           read(N),
           nth0(N,DescList,MyDesc),

           asserta(該当(Case,事件(Desc)) :- 事件該当(Case,Desc,MyDesc) ),
           asserta(該当(Case,要件(Desc)) :- 要件該当(Case,Desc) ),
           asserta(該当(Case,不該当要件(Desc)) :- 要件不該当(Case,Desc)),
           不法,
           retractall(該当(Case,Desc)).
