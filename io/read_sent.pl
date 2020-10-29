
%%% read_sent(Words)
%%% input ==> series of words terminated by a new line character
%%% Words <== list of the words
read_sent(Words) :- get0(Char), read_sent(Char, Words).

read_sent(C, []) :- newline(C), !.
read_sent(C, Words) :- space(C), !, get0(Char),
                        read_sent(Char, Words).
read_sent(C, [Word|Words]) :- read_word(C, Chars, Next),
                        name(Word, Chars),
                        read_sent(Next, Words).

read_word(C, [], C) :- space(C), !.
read_word(C, [], C) :- newline(C), !.
read_word(C, [C|Chars], Last) :- get0(Next),
                        read_word(Next, Chars, Last).

newline(10).
space(32).
