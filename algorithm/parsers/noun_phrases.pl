
%%% Noun Phrases

%%  These are the parsing of the the nouns
np(NP, nogap) --> det(N2^NP), n(N1), optrel(N1^N2).
np(NP, nogap) --> pn(NP).
np((X^S)^S, gap(np, X)) --> [].
