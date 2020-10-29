%%%     Questions

q(S => answer(X))   --> whpron, vp(finite, X^S, nogap).
q(S => answer(X))   --> whpron, sinv(S, gap(np, X)).
q(S => answer(yes)) --> sinv(S, nogap).
q(S => answer(yes)) --> [is],  np((X^S0)^S, nogap),
                  np((X^true)^exists(X,S0&true), nogap).
