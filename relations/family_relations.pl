%% main relations between the monarch family

monarch_wife(W,H):-female(W),married(H,W),monarch(H).

monarch_husband(H,W):-male(H),married(H,W),monarch(W).

mother(M,C):-female(M),parent(M,C).

father(F,C):-male(F),parent(F,C).

son(S,P):-male(S),parent(P,S).

daughter(D,P):-female(D),parent(P,D).

sibling(X,Y):- X\=Y,brother(X,Y);sister(X,Y).

child(X,Y):-son(X,Y);daughter(X,Y).

brother(Y,X):-male(Y),son(Y,P),child(X,P),Y\=P,X\=Y.

sister(X,Y):-female(X),daughter(X,P),child(Y,P),X\=P,X\=Y.

grandparent(G,X):- parent(P,X), parent(G,P).

grandchild(G,X):- parent(G,P),parent(P,X).

aunt(A,X):- parent(P,X), sister(A,P).

uncle(U,X):- parent(P,X), brother(U,P).

ancestor(A,A).
ancestor(A,D):-
  parent(A,C),
  ancestor(C,D).

cousin(Child1,Child2) :-
    grandparent(Y1,Child1),
    grandparent(Y2,Child2),
    not(sibling(Child1,Child2)),
    Y1=Y2.

samefather(X,Y) :-
   father(F,X),
   father(F,Y),
   X\=Y.

samemother(X,Y) :-
   mother(M,X),
   mother(M,Y),
   X\=Y.

sameparent(X,Y) :-
   samefather(X,Y).
sameparent(X,Y) :-
   samemother(X,Y),
   not(samefather(X,Y)).

niece(X,Y) :-
    female(X),
    parent(Z,X),
    sameparent(Z,Y),
    Z \= Y.

nephew(X,Y) :-
    male(X),
    parent(Z,X),
    sameparent(Z,Y),
    Z \= Y.
