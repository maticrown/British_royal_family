

% Auxiliary verb

aux(Form, LF) --> [Aux], {aux(Aux, Form, LF)}.
aux(to,    infinitival/nonfinite,    VP^ VP).
aux(does,  finite/nonfinite,         VP^ VP).
aux(did,   finite/nonfinite,         VP^ VP).
aux(could, finite/nonfinite,         VP^ VP).
aux(have,  nonfinite/past_participle, VP^ VP).
aux(has,   finite/past_participle,   VP^ VP).
aux(been,  past_participle/present_participle, VP^ VP).
aux(be,    nonfinite/present_participle,       VP^ VP).
