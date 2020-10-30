
% loading the facts & relations
load :-
    consult('/proj/facts/parentings.pl'),
    consult('/proj/facts/person_details.pl'),
    consult('/proj/facts/marriages.pl'),

    consult('/proj/relations/family_relations.pl'),
    consult('/proj/relations/monarchial_relations.pl'),

    consult('/proj/shared/operators.pl'),

    consult('/proj/algorithm/talk.pl'),
    consult('/proj/algorithm/clausifier.pl'),

    consult('/proj/io/read_sent.pl'),
    consult('/proj/io/reply.pl'),
    consult('/proj/io/print_reply.pl'),
    consult('/proj/io/print_answers.pl'),

    consult('/proj/algorithm/parsers/sentence.pl'),
    consult('/proj/algorithm/parsers/question.pl'),
    consult('/proj/algorithm/parsers/verb_phrases.pl'),
    consult('/proj/algorithm/parsers/noun_phrases.pl'),
    consult('/proj/algorithm/parsers/relative_clauses.pl'),

    consult('/proj/algorithm/dictionary/verb.pl'),
    consult('/proj/algorithm/dictionary/auxiliary.pl'),
    consult('/proj/algorithm/dictionary/pronoun.pl'),
    consult('/proj/algorithm/dictionary/determiner.pl'),
    consult('/proj/algorithm/dictionary/noun.pl').

%% This is the main run function that starts the questions and answers
%% of the "Marechet Mumche"
run :-
    load,
    write('>> '),           % prompt the user
    read_sent(Words),       % read a sentence
    nth0(0, Words, FirstWord),
    (FirstWord = 'l', !, load, write_ln('Reloaded successfully.') ;
    talk(Words, Reply),     % process it with TALK
    print_reply(Reply)),     % generate a reply
    run.                    % pocess more sentences
