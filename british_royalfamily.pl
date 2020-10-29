
% loading the facts & relations
:- consult('facts/parentings.pl').
:- consult('facts/person_detailes.pl').
:- consult('facts/marriages.pl').

:- consult('relations/family_relations.pl').
:- consult('relations/monarchial_relations.pl').

:- consult('shared/operators.pl').

:- consult('algorithm/talk.pl').
:- consult('algorithm/clausifier.pl').

:- consult('io/read_sent.pl').
:- consult('io/reply.pl').
:- consult('io/print_reply.pl').
:- consult('io/print_answers.pl').

:- consult('algorithm/parsers/sentence.pl').
:- consult('algorithm/parsers/question.pl').
:- consult('algorithm/parsers/verb_phrases.pl').
:- consult('algorithm/parsers/noun_phrases.pl').
:- consult('algorithm/parsers/relative_clauses.pl').

:- consult('algorithm/dictionary/verb.pl').
:- consult('algorithm/dictionary/auxiliary.pl').
:- consult('algorithm/dictionary/pronoun.pl').
:- consult('algorithm/dictionary/determiner.pl').
:- consult('algorithm/dictionary/noun.pl').


%% This is the main run function that starts the questions and answers
%% of the "Marechet Mumche"
run :-
        write('>> '),           % prompt the user
        read_sent(Words),       % read a sentence
        talk(Words, Reply),     % process it with TALK
        print_reply(Reply),     % generate a reply
        run.                    % pocess more sentences
