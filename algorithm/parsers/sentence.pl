
%%% Declarative Sentences

s(S, GapInfo)   --> np(VP^S, nogap), vp(finite, VP, GapInfo).

%%% Inverted Sentences

sinv(S, GapInfo) --> aux(finite/Form, VP1^VP2),
                     np(VP2^S, nogap), vp(Form, VP1, GapInfo).
