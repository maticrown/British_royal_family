
% Determiner

det(LF)   --> [D], {det(D, LF)}.

det(every,  (X^S1) ^ (X^S2) ^ all(X, S1=>S2)).
det(a,      (X^S1) ^ (X^S2) ^ exists(X, S1&S2)).
det(some,   (X^S1) ^ (X^S2) ^ exists(X, S1&S2)).
