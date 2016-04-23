:- op(1000, yfx , [ bin ] ).
:- op(1000, fx  , [ pre ] ).
:- op(1000, xf  , [ post ] ).


a :- asserta(:- op(1000, fx , [pre2] )).
