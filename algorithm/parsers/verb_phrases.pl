
%%% Verb Phrases

%%  These are for the parsing of the verbs
vp(Form, X^S, GapInfo)  --> tv(Form, X^VP), np(VP^S, GapInfo).
vp(Form, VP, nogap)     --> iv(Form, VP).
vp(Form1, VP2, GapInfo) --> aux(Form1/Form2, VP1^VP2),
                            vp(Form2, VP1, GapInfo).
vp(Form1, VP2, GapInfo) --> rov(Form1/Form2, NP^VP1^VP2),
                            np(NP, GapInfo), vp(Form2, VP1, nogap).
vp(Form2, VP2, GapInfo) --> rov(Form1/Form2, NP^VP1^VP2),
                            np(NP, nogap), vp(Form1, VP1, GapInfo).
vp(finite, X^S, GapInfo) --> [is], np((X^P)^exists(X,S&P), GapInfo).
