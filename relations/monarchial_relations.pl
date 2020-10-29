
%%women are direct heirs since they changed the law in 2011
heir_to_throne(H):-
  oldest(H),not(monarch_husband(philip,W)),monarch(W),!.


generation(1;2;3;4).

queen(X):-monarch(X),female(X).

prince_1(Y):-monarch(Y),male(Y),generation(1).

prince_2(Y):-monarch(Y),male(Y),generation(2).

prince_3(Y):-monarch(Y),male(Y),generation(3).

prince_4(Y):-monarch(Y),male(Y),generation(4).

princess_1(X):-monarch(X),female(X),generation(1).

princess_2(X):-monarch(X),female(X),generation(2).

princess_3(X):-monarch(X),female(X),generation(3).

princess_4(X):-monarch(X),female(X),generation(4).
