
lgg_term(X,Y, X, List,List) :- X == Y, !.
lgg_term(X,Y, G, List,[s(X,Y,G)|List]) :- var(X); var(Y).

lgg_term(X,Y, G, List,List) :-
  member(s(A,B,G), List),
  A == X, B == Y, !.

lgg_term(X,Y, G, In,Out) :-
  X =.. [P|Xa], Y =.. [P|Ya],
  lgg_list(Xa,Ya, Ga, In,Out),
  G =.. [P|Ga], !.

lgg_term(X,Y, G, List,[s(X,Y,G)|List]).

% LGG on lists of equal length
lgg_list([],     [],      [],       Lst,Lst).
lgg_list([H1|T1],[H2|T2], [G|Rest], Beg,End):-
  lgg_term(H1,    H2,      G,       Beg,Mid),
  lgg_list(  T1,     T2,     Rest,  Mid,End).
