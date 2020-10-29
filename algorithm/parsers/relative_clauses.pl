
%%% Relative Clauses

optrel((X^S1)^(X^(S1&S2))) --> relpron, vp(finite, X^S2, nogap).
optrel((X^S1)^(X^(S1&S2))) --> relpron, s(S2, gap(np, X)).
optrel(N^N) --> [].
