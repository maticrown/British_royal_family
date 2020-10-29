

/* Nonterminal names:
   q       Question
   sinv    INVerted Sentence
   s       noninverted Sentence
   np      Noun Phrase
   vp      Verb Phrase
   iv      Intransitive Verb
   tv      Transitive Verb
   aux     AUXiliary verb
   rov     subject_Object Raising Verb
   optrel  OPTional RELative clause
   relpron RELative PRONoun
   whpron  WH PRONoun
   det     DETerminer
   n       Noun
   pn      Proper Noun

  Typical order of and values for arguments:

  1.    verb form:
     (main verbs) finite, nonfinite, etc.
     (auxiliaries and raising verbs) Form1-Form2
         where Forml is form of embedded VP
               Form2 is form of verb itself
  2.    FOL   logical form
  3.    gap   information:
              nogap or gap(Nonterm, Var)
                 where Nonterm is nonterminal for gap
                 Var is the LF variable that
                     the filler will bind
*/

% Dictionary

