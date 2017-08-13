/**
 * Prolog code for the msic text test cases.
 *
 * Warranty & Liability
 * To the extent permitted by applicable law and unless explicitly
 * otherwise agreed upon, XLOG Technologies GmbH makes no warranties
 * regarding the provided information. XLOG Technologies GmbH assumes
 * no liability that any problems might be solved with the information
 * provided by XLOG Technologies GmbH.
 *
 * Rights & License
 * All industrial property rights regarding the information - copyright
 * and patent rights in particular - are the sole property of XLOG
 * Technologies GmbH. If the company was not the originator of some
 * excerpts, XLOG Technologies GmbH has at least obtained the right to
 * reproduce, change and translate the information.
 *
 * Reproduction is restricted to the whole unaltered document. Reproduction
 * of the information is only allowed for non-commercial uses. Selling,
 * giving away or letting of the execution of the library is prohibited.
 * The library can be distributed as part of your applications and libraries
 * for execution provided this comment remains unchanged.
 *
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */

:- use_package(library(jekdev/reference/testing)).

:- multifile runner:ref/4.
:- discontiguous runner:ref/4.

:- multifile runner:case/4.
:- discontiguous runner:case/4.

:- use_module(library(misc/text)).

/*******************************************************************/
/* Char Type etc..                                                 */
/*******************************************************************/

/* char_type(C, N) */
runner:ref(char_type, 2, extra_regex, 'XLOG 2.1.1').
runner:case(char_type, 2, extra_regex, 'XLOG 2.1.1, XLOG 1') :-
   char_type(' ', N),
   N == white.
runner:case(char_type, 2, extra_regex, 'XLOG 2.1.1, XLOG 2') :-
   char_type('\n', N),
   N == control.
runner:case(char_type, 2, extra_regex, 'XLOG 2.1.1, XLOG 3') :-
   char_type('\xFFFE\', N),
   N == inval.
runner:case(char_type, 2, extra_regex, 'XLOG 2.1.1, XLOG 4') :-
   char_type(!, N),
   N == solo.
runner:case(char_type, 2, extra_regex, 'XLOG 2.1.1, XLOG 5') :-
   char_type('A', N),
   N == upper.
runner:case(char_type, 2, extra_regex, 'XLOG 2.1.1, XLOG 6') :-
   catch(char_type(_, _), error(E,_), true),
   nonvar(E),
   E = instantiation_error.
runner:case(char_type, 2, extra_regex, 'XLOG 2.1.1, XLOG 7') :-
   catch(char_type(77, _), error(E,_), true),
   nonvar(E),
   E = type_error(atom,_).
/* unicode >0xFFFF, deseret alphabet */
runner:case(char_type, 2, extra_regex, 'XLOG 2.1.1, XLOG 8') :-
   char_type(𐐬, N),
   N == lower.

/* code_type(C, N) */
runner:ref(code_type, 2, extra_regex, 'XLOG 2.1.2').
runner:case(code_type, 2, extra_regex, 'XLOG 2.1.2, XLOG 1') :-
   code_type(0'_, N),
   N == score.
runner:case(code_type, 2, extra_regex, 'XLOG 2.1.2, XLOG 2') :-
   code_type(0'a, N),
   N == lower.
runner:case(code_type, 2, extra_regex, 'XLOG 2.1.2, XLOG 3') :-
   code_type(0'¼, N),
   N == other.
runner:case(code_type, 2, extra_regex, 'XLOG 2.1.2, XLOG 4') :-
   code_type(0'0, N),
   N == digit.
runner:case(code_type, 2, extra_regex, 'XLOG 2.1.2, XLOG 5') :-
   code_type(0'=, N),
   N == graph.
runner:case(code_type, 2, extra_regex, 'XLOG 2.1.2, XLOG 6') :-
   catch(code_type(_, _), error(E,_), true),
   nonvar(E),
   E = instantiation_error.
runner:case(code_type, 2, extra_regex, 'XLOG 2.1.2, XLOG 7') :-
   catch(code_type(foo, _), error(E,_), true),
   nonvar(E),
   E = type_error(integer,_).
/* unicode >0xFFFF, deseret alphabet */
runner:case(code_type, 2, extra_regex, 'XLOG 2.1.2, XLOG 8') :-
   code_type(0'𐐄, N),
   N == upper.

/*******************************************************************/
/* Code Upper etc..                                                */
/*******************************************************************/

/* code_upper(C, D) */
runner:ref(code_upper, 2, extra_regex, 'XLOG 2.2.1').
runner:case(code_upper, 2, extra_regex, 'XLOG 2.2.1, XLOG 1') :-
   code_upper(0'_, D),
   D == 0'_.
runner:case(code_upper, 2, extra_regex, 'XLOG 2.2.1, XLOG 2') :-
   code_upper(0xFFFE, D),
   D == 0xFFFE.
runner:case(code_upper, 2, extra_regex, 'XLOG 2.2.1, XLOG 3') :-
   code_upper(0'a, D),
   D == 0'A.
runner:case(code_upper, 2, extra_regex, 'XLOG 2.2.1, XLOG 4') :-
   code_upper(0'A, D),
   D == 0'A.
runner:case(code_upper, 2, extra_regex, 'XLOG 2.2.1, XLOG 5') :-
   catch(code_upper(_, _), error(E,_), true),
   nonvar(E),
   E = instantiation_error.
/* unicode >0xFFFF, deseret alphabet */
runner:case(code_upper, 2, extra_regex, 'XLOG 2.2.1, XLOG 6') :-
   code_upper(0'𐐬, D),
   D == 0'𐐄.

/* code_lower(C, D) */
runner:ref(code_lower, 2, extra_regex, 'XLOG 2.2.2').
runner:case(code_lower, 2, extra_regex, 'XLOG 2.2.2, XLOG 1') :-
   code_lower(0'=, D),
   D == 0'= .
runner:case(code_lower, 2, extra_regex, 'XLOG 2.2.2, XLOG 2') :-
   code_lower(0'0, D),
   D == 0'0.
runner:case(code_lower, 2, extra_regex, 'XLOG 2.2.2, XLOG 3') :-
   code_lower(0'a, D),
   D == 0'a.
runner:case(code_lower, 2, extra_regex, 'XLOG 2.2.2, XLOG 4') :-
   code_lower(0'A, D),
   D == 0'a.
runner:case(code_lower, 2, extra_regex, 'XLOG 2.2.2, XLOG 5') :-
   catch(code_lower(foo, _), error(E,_), true),
   nonvar(E),
   E = type_error(integer,_).
/* unicode >0xFFFF, deseret alphabet */
runner:case(code_lower, 2, extra_regex, 'XLOG 2.2.2, XLOG 6') :-
   code_lower(0'𐐄, D),
   D == 0'𐐬.

/* atom_upper(A, B) */
runner:ref(atom_upper, 2, extra_regex, 'XLOG 2.2.3').
runner:case(atom_upper, 2, extra_regex, 'XLOG 2.2.3, XLOG 1') :-
   atom_upper('_A7', D),
   D == '_A7'.
runner:case(atom_upper, 2, extra_regex, 'XLOG 2.2.3, XLOG 2') :-
   atom_upper('\xFFFE\', D),
   D == '\xFFFE\'.
runner:case(atom_upper, 2, extra_regex, 'XLOG 2.2.3, XLOG 3') :-
   atom_upper(abc, D),
   D == 'ABC'.
runner:case(atom_upper, 2, extra_regex, 'XLOG 2.2.3, XLOG 4') :-
   atom_upper('ABC', D),
   D == 'ABC'.
runner:case(atom_upper, 2, extra_regex, 'XLOG 2.2.3, XLOG 5') :-
   catch(atom_upper(_, _), error(E,_), true),
   nonvar(E),
   E = instantiation_error.
/* unicode >0xFFFF, deseret alphabet */
runner:case(atom_upper, 2, extra_regex, 'XLOG 2.2.3, XLOG 6') :-
   atom_upper(𐐫𐐬𐐭, D),
   D == '𐐃𐐄𐐅'.

/* atom_lower(A, B) */
runner:ref(atom_lower, 2, extra_regex, 'XLOG 2.2.4').
runner:case(atom_lower, 2, extra_regex, 'XLOG 2.2.4, XLOG 1') :-
   atom_lower(=<, D),
   D == =< .
runner:case(atom_lower, 2, extra_regex, 'XLOG 2.2.4, XLOG 2') :-
   atom_lower('123', D),
   D == '123'.
runner:case(atom_lower, 2, extra_regex, 'XLOG 2.2.4, XLOG 3') :-
   atom_lower(abc, D),
   D == abc.
runner:case(atom_lower, 2, extra_regex, 'XLOG 2.2.4, XLOG 4') :-
   atom_lower('ABC', D),
   D == abc.
runner:case(atom_lower, 2, extra_regex, 'XLOG 2.2.4, XLOG 5') :-
   catch(atom_lower(77, _), error(E,_), true),
   nonvar(E),
   E = type_error(atom,_).
/* unicode >0xFFFF, deseret alphabet */
runner:case(atom_lower, 2, extra_regex, 'XLOG 2.2.4, XLOG 6') :-
   atom_lower('𐐃𐐄𐐅', D),
   D == 𐐫𐐬𐐭.

/*******************************************************************/
/* Create Patern Match etc..                                       */
/*******************************************************************/

/* pattern_match(S, P) */
/* default settings */
runner:ref(pattern_match, 2, extra_regex, 'XLOG 2.3.1').
runner:case(pattern_match, 2, extra_regex, 'XLOG 2.3.1, XLOG 1') :-
   \+ pattern_match(foobarbaz, bar).
runner:case(pattern_match, 2, extra_regex, 'XLOG 2.3.1, XLOG 2') :-
   \+ pattern_match(foobarbaz, 'bar*').
runner:case(pattern_match, 2, extra_regex, 'XLOG 2.3.1, XLOG 3') :-
   pattern_match(foobarbaz, '*bar*').
runner:case(pattern_match, 2, extra_regex, 'XLOG 2.3.1, XLOG 4') :-
   \+ pattern_match(foobarbaz, baz).
runner:case(pattern_match, 2, extra_regex, 'XLOG 2.3.1, XLOG 5') :-
   \+ pattern_match(foobarbaz, 'baz*').
runner:case(pattern_match, 2, extra_regex, 'XLOG 2.3.1, XLOG 6') :-
   pattern_match(foobarbaz, '*baz*').
runner:case(pattern_match, 2, extra_regex, 'XLOG 2.3.1, XLOG 7') :-
   \+ pattern_match(foobarbaz, foo).
runner:case(pattern_match, 2, extra_regex, 'XLOG 2.3.1, XLOG 8') :-
   pattern_match(foobarbaz, 'foo*').
/* unicode >0xFFFF, deseret alphabet */
runner:case(pattern_match, 2, extra_regex, 'XLOG 2.3.1, XLOG 9') :-
   pattern_match('𐐀𐐁𐐂𐐃𐐄𐐅𐐆𐐇𐐈', '*𐐃𐐄𐐅*').
runner:case(pattern_match, 2, extra_regex, 'XLOG 2.3.1, XLOG 10') :-
   \+ pattern_match('𐐀𐐁𐐂𐐃𐐄𐐅𐐆𐐇𐐈', '𐐆𐐇𐐈*').

/* setting ignore_case=true */
runner:case(pattern_match, 2, extra_regex, 'XLOG 2.3.1, XLOG 11') :-
   \+ pattern_match('FOOBARBAZ', '*bar', [ignore_case(true)]).
runner:case(pattern_match, 2, extra_regex, 'XLOG 2.3.1, XLOG 12') :-
   pattern_match(foobarbaz, '*BAZ', [ignore_case(true)]).
runner:case(pattern_match, 2, extra_regex, 'XLOG 2.3.1, XLOG 13') :-
   \+ pattern_match('FOOBARBAZ', '*foo', [ignore_case(true)]).
runner:case(pattern_match, 2, extra_regex, 'XLOG 2.3.1, XLOG 14') :-
   pattern_match(foobarbaz, '*FOO*', [ignore_case(true)]).
/* unicode >0xFFFF, deseret alphabet */
runner:case(pattern_match, 2, extra_regex, 'XLOG 2.3.1, XLOG 15') :-
   pattern_match('𐐀𐐁𐐂𐐃𐐄𐐅𐐆𐐇𐐈', '*𐐫𐐬𐐭*', [ignore_case(true)]).
runner:case(pattern_match, 2, extra_regex, 'XLOG 2.3.1, XLOG 16') :-
   \+ pattern_match('𐐀𐐁𐐂𐐃𐐄𐐅𐐆𐐇𐐈', '𐐮𐐯𐐰*', [ignore_case(true)]).

/* setting boundary=part */
runner:case(pattern_match, 2, extra_regex, 'XLOG 2.3.1, XLOG 17') :-
   \+ pattern_match(foobarbaz, tak, [boundary(part)]).
runner:case(pattern_match, 2, extra_regex, 'XLOG 2.3.1, XLOG 18') :-
   pattern_match(foobarbaz, bar, [boundary(part)]).
runner:case(pattern_match, 2, extra_regex, 'XLOG 2.3.1, XLOG 19') :-
   pattern_match(foobarbaz, foo, [boundary(part)]).
runner:case(pattern_match, 2, extra_regex, 'XLOG 2.3.1, XLOG 20') :-
   pattern_match(foobarbaz, baz, [boundary(part)]).
/* unicode >0xFFFF, deseret alphabet */
runner:case(pattern_match, 2, extra_regex, 'XLOG 2.3.1, XLOG 21') :-
   pattern_match('𐐀𐐁𐐂𐐃𐐄𐐅𐐆𐐇𐐈', '𐐃𐐄𐐅', [boundary(part)]).

/* setting boundary=word */
runner:case(pattern_match, 2, extra_regex, 'XLOG 2.3.1, XLOG 22') :-
   \+ pattern_match('foo bar baz', tak, [boundary(word)]).
runner:case(pattern_match, 2, extra_regex, 'XLOG 2.3.1, XLOG 23') :-
   pattern_match('foo bar baz', bar, [boundary(word)]).
runner:case(pattern_match, 2, extra_regex, 'XLOG 2.3.1, XLOG 24') :-
   \+ pattern_match('foo barbaz', bar, [boundary(word)]).
runner:case(pattern_match, 2, extra_regex, 'XLOG 2.3.1, XLOG 25') :-
   pattern_match('foo bar baz', foo, [boundary(word)]).
runner:case(pattern_match, 2, extra_regex, 'XLOG 2.3.1, XLOG 26') :-
   \+ pattern_match('foobar baz', foo, [boundary(word)]).
runner:case(pattern_match, 2, extra_regex, 'XLOG 2.3.1, XLOG 27') :-
   pattern_match('foo bar baz', baz, [boundary(word)]).
runner:case(pattern_match, 2, extra_regex, 'XLOG 2.3.1, XLOG 28') :-
   \+ pattern_match('foo barbaz', baz, [boundary(word)]).
/* unicode >0xFFFF, deseret alphabet */
runner:case(pattern_match, 2, extra_regex, 'XLOG 2.3.1, XLOG 29') :-
   pattern_match('𐐀𐐁𐐂 𐐃𐐄𐐅 𐐆𐐇𐐈', '𐐃𐐄𐐅', [boundary(word)]).
runner:case(pattern_match, 2, extra_regex, 'XLOG 2.3.1, XLOG 30') :-
   \+ pattern_match('𐐀𐐁𐐂𐐃𐐄𐐅𐐆𐐇𐐈', '𐐃𐐄𐐅', [boundary(word)]).

/* setting boundary=part, ignore_case=true */
runner:case(pattern_match, 2, extra_regex, 'XLOG 2.3.1, XLOG 31') :-
   pattern_match(foobarbaz, 'BAR', [boundary(part),ignore_case(true)]).
runner:case(pattern_match, 2, extra_regex, 'XLOG 2.3.1, XLOG 32') :-
   \+ pattern_match(foobarbaz, 'FOOBAZ', [boundary(part),ignore_case(true)]).
/* unicode >0xFFFF, deseret alphabet */
runner:case(pattern_match, 2, extra_regex, 'XLOG 2.3.1, XLOG 33') :-
   pattern_match('𐐀𐐁𐐂𐐃𐐄𐐅𐐆𐐇𐐈', 𐐫𐐬𐐭, [boundary(part),ignore_case(true)]).

/* setting boundary=word, ignore_case=true */
runner:case(pattern_match, 2, extra_regex, 'XLOG 2.3.1, XLOG 34') :-
   pattern_match('foo bar baz', bar, [boundary(word),ignore_case(true)]).
runner:case(pattern_match, 2, extra_regex, 'XLOG 2.3.1, XLOG 35') :-
   \+ pattern_match('foo bar baz', 'FOO BAZ', [boundary(word),ignore_case(true)]).
/* unicode >0xFFFF, deseret alphabet */
runner:case(pattern_match, 2, extra_regex, 'XLOG 2.3.1, XLOG 36') :-
   pattern_match('𐐀𐐁𐐂 𐐃𐐄𐐅 𐐆𐐇𐐈', 𐐫𐐬𐐭, [boundary(word),ignore_case(true)]).

/* pattern_replace(S, P, R, T) */
/* default settings */
runner:ref(pattern_replace, 4, extra_regex, 'XLOG 2.3.2').
runner:case(pattern_replace, 4, extra_regex, 'XLOG 2.3.2, XLOG 1') :-
   \+ pattern_replace(foobarbaz, '*bar', '*tok', _).
runner:case(pattern_replace, 4, extra_regex, 'XLOG 2.3.2, XLOG 2') :-
   \+ pattern_replace(foobarbaz, 'bar*', 'tok*', _).
runner:case(pattern_replace, 4, extra_regex, 'XLOG 2.3.2, XLOG 3') :-
   pattern_replace(foobarbaz, '*bar*', '*tok*', X),
   X == footokbaz.
runner:case(pattern_replace, 4, extra_regex, 'XLOG 2.3.2, XLOG 4') :-
   \+ pattern_replace(foobarbaz, baz, tok, _).
runner:case(pattern_replace, 4, extra_regex, 'XLOG 2.3.2, XLOG 5') :-
   pattern_replace(foobarbaz, '*baz', '*tok', X),
   X == foobartok.
runner:case(pattern_replace, 4, extra_regex, 'XLOG 2.3.2, XLOG 6') :-
   pattern_replace(foobarbaz, '*baz*', '*tok*', X),
   X == foobartok.
runner:case(pattern_replace, 4, extra_regex, 'XLOG 2.3.2, XLOG 7') :-
   \+ pattern_replace(foobarbaz, '*foo', '*tok', _).
runner:case(pattern_replace, 4, extra_regex, 'XLOG 2.3.2, XLOG 8') :-
   pattern_replace(foobarbaz, '*foo*', '*tok*', X),
   X == tokbarbaz.
runner:case(pattern_replace, 4, extra_regex, 'XLOG 2.3.2, XLOG 9') :-
   pattern_replace(foobarfoobar, '*foo*', '*tok*', X),
   X == tokbarfoobar.
/* unicode >0xFFFF, deseret alphabet */
runner:case(pattern_replace, 4, extra_regex, 'XLOG 2.3.2, XLOG 10') :-
   pattern_replace('𐐀𐐁𐐂𐐃𐐄𐐅𐐆𐐇𐐈', '*𐐃𐐄𐐅*', '*𐐎𐐏𐐐*', X),
   X == '𐐀𐐁𐐂𐐎𐐏𐐐𐐆𐐇𐐈'.
runner:case(pattern_replace, 4, extra_regex, 'XLOG 2.3.2, XLOG 11') :-
   \+ pattern_replace('𐐀𐐁𐐂𐐃𐐄𐐅𐐆𐐇𐐈', '𐐆𐐇𐐈*', '𐐎𐐏𐐐*', _).

/* setting ignore_case=true */
runner:case(pattern_replace, 4, extra_regex, 'XLOG 2.3.2, XLOG 12') :-
   \+ pattern_replace(foobarbaz, 'BAR', tok, _, [ignore_case(true)]).
runner:case(pattern_replace, 4, extra_regex, 'XLOG 2.3.2, XLOG 13') :-
   \+ pattern_replace('FOOBARBAZ', 'baz*', 'tok*', _, [ignore_case(true)]).
runner:case(pattern_replace, 4, extra_regex, 'XLOG 2.3.2, XLOG 14') :-
   \+ pattern_replace(foobarbaz, 'FOO', tok, _, [ignore_case(true)]).
runner:case(pattern_replace, 4, extra_regex, 'XLOG 2.3.2, XLOG 15') :-
   pattern_replace('FOOBARBAZ', 'foo*', 'tok*', X, [ignore_case(true)]),
   X == 'TOKBARBAZ'.
runner:case(pattern_replace, 4, extra_regex, 'XLOG 2.3.2, XLOG 16') :-
   pattern_replace(foobarfoobar, '*BAR*', '*TOK*', X, [ignore_case(true)]),
   X == footokfoobar.
/* unicode >0xFFFF, deseret alphabet */
runner:case(pattern_replace, 4, extra_regex, 'XLOG 2.3.2, XLOG 17') :-
   pattern_replace('𐐀𐐁𐐂𐐃𐐄𐐅𐐆𐐇𐐈', '*𐐫𐐬𐐭*', '*𐐎𐐏𐐐*', X, [ignore_case(true)]),
   X == '𐐀𐐁𐐂𐐶𐐷𐐸𐐆𐐇𐐈'.
runner:case(pattern_replace, 4, extra_regex, 'XLOG 2.3.2, XLOG 18') :-
   \+ pattern_replace('𐐀𐐁𐐂𐐃𐐄𐐅𐐆𐐇𐐈', '𐐮𐐯𐐰*', '𐐎𐐏𐐐*', _, [ignore_case(true)]).

/* setting boundary=part */
runner:case(pattern_replace, 4, extra_regex, 'XLOG 2.3.2, XLOG 19') :-
   \+ pattern_replace(foobarbaz, tak, tok, _, [boundary(part)]).
runner:case(pattern_replace, 4, extra_regex, 'XLOG 2.3.2, XLOG 20') :-
   pattern_replace(foobarbaz, bar, tok, X, [boundary(part)]),
   X == footokbaz.
runner:case(pattern_replace, 4, extra_regex, 'XLOG 2.3.2, XLOG 21') :-
   pattern_replace(foobarbaz, foo, tok, X, [boundary(part)]),
   X == tokbarbaz.
runner:case(pattern_replace, 4, extra_regex, 'XLOG 2.3.2, XLOG 22') :-
   pattern_replace(foobarbaz, baz, tok, X, [boundary(part)]),
   X == foobartok.
/* unicode >0xFFFF, deseret alphabet */
runner:case(pattern_replace, 4, extra_regex, 'XLOG 2.3.2, XLOG 23') :-
   pattern_replace('𐐀𐐁𐐂𐐃𐐄𐐅𐐆𐐇𐐈', '𐐃𐐄𐐅', '𐐎𐐏𐐐', X, [boundary(part)]),
   X == '𐐀𐐁𐐂𐐎𐐏𐐐𐐆𐐇𐐈'.

/* setting boundary=word */
runner:case(pattern_replace, 4, extra_regex, 'XLOG 2.3.2, XLOG 24') :-
   \+ pattern_replace('foo bar baz', tak, tok, _, [boundary(word)]).
runner:case(pattern_replace, 4, extra_regex, 'XLOG 2.3.2, XLOG 25') :-
   pattern_replace('foo bar baz', bar, tok, X, [boundary(word)]),
   X == 'foo tok baz'.
runner:case(pattern_replace, 4, extra_regex, 'XLOG 2.3.2, XLOG 26') :-
   \+ pattern_replace('foo barbaz', bar, tok, _, [boundary(word)]).
runner:case(pattern_replace, 4, extra_regex, 'XLOG 2.3.2, XLOG 27') :-
   pattern_replace('foo bar baz', foo, tok, X, [boundary(word)]),
   X == 'tok bar baz'.
runner:case(pattern_replace, 4, extra_regex, 'XLOG 2.3.2, XLOG 28') :-
   \+ pattern_replace('foobar baz', foo, tok, _, [boundary(word)]).
runner:case(pattern_replace, 4, extra_regex, 'XLOG 2.3.2, XLOG 29') :-
   pattern_replace('foo bar baz', baz, tok, X, [boundary(word)]),
   X == 'foo bar tok'.
runner:case(pattern_replace, 4, extra_regex, 'XLOG 2.3.2, XLOG 30') :-
   \+ pattern_replace('foo barbaz', baz, tok, _, [boundary(word)]).
/* unicode >0xFFFF, deseret alphabet */
runner:case(pattern_replace, 4, extra_regex, 'XLOG 2.3.2, XLOG 31') :-
   pattern_replace('𐐀𐐁𐐂 𐐃𐐄𐐅 𐐆𐐇𐐈', '𐐃𐐄𐐅', '𐐎𐐏𐐐', X, [boundary(word)]),
   X == '𐐀𐐁𐐂 𐐎𐐏𐐐 𐐆𐐇𐐈'.
runner:case(pattern_replace, 4, extra_regex, 'XLOG 2.3.2, XLOG 32') :-
   \+ pattern_replace('𐐀𐐁𐐂𐐃𐐄𐐅𐐆𐐇𐐈', '𐐃𐐄𐐅', '𐐎𐐏𐐐', _, [boundary(word)]).

/* setting boundary=part, ignore_case=true */
runner:case(pattern_replace, 4, extra_regex, 'XLOG 2.3.2, XLOG 33') :-
   pattern_replace(foobarbaz, 'BAR', tok, X, [boundary(part),ignore_case(true)]),
   X == fooTOKbaz.
runner:case(pattern_replace, 4, extra_regex, 'XLOG 2.3.2, XLOG 34') :-
   \+ pattern_replace(foobarbaz, 'FOOBAZ', tok, _, [boundary(part),ignore_case(true)]).
/* unicode >0xFFFF, deseret alphabet */
runner:case(pattern_replace, 4, extra_regex, 'XLOG 2.3.2, XLOG 35') :-
   pattern_replace('𐐀𐐁𐐂𐐃𐐄𐐅𐐆𐐇𐐈', 𐐫𐐬𐐭, '𐐎𐐏𐐐', X, [boundary(part),ignore_case(true)]),
   X == '𐐀𐐁𐐂𐐶𐐷𐐸𐐆𐐇𐐈'.

/* setting boundary=word, ignore_case=true */
runner:case(pattern_replace, 4, extra_regex, 'XLOG 2.3.2, XLOG 36') :-
   pattern_replace('foo bar baz', 'BAR', tok, X, [boundary(word),ignore_case(true)]),
   X == 'foo TOK baz'.
runner:case(pattern_replace, 4, extra_regex, 'XLOG 2.3.2, XLOG 37') :-
   \+ pattern_replace('foo foo bar', baz, tok, _, [boundary(word),ignore_case(true)]).
/* unicode >0xFFFF, deseret alphabet */
runner:case(pattern_replace, 4, extra_regex, 'XLOG 2.3.2, XLOG 38') :-
   pattern_replace('𐐀𐐁𐐂 𐐃𐐄𐐅 𐐆𐐇𐐈', 𐐫𐐬𐐭, '𐐎𐐏𐐐', X, [boundary(word),ignore_case(true)]),
   X == '𐐀𐐁𐐂 𐐶𐐷𐐸 𐐆𐐇𐐈'.

/* last_pattern_replace(S, P, R, T) */
/* default settings */
runner:ref(last_pattern_replace, 4, extra_regex, 'XLOG 2.3.3').
runner:case(last_pattern_replace, 4, extra_regex, 'XLOG 2.3.3, XLOG 1') :-
   \+ last_pattern_replace(foobarbaz, bar, tok, _).
runner:case(last_pattern_replace, 4, extra_regex, 'XLOG 2.3.3, XLOG 2') :-
   \+ last_pattern_replace(foobarbaz, '*bar', '*tok', _).
runner:case(last_pattern_replace, 4, extra_regex, 'XLOG 2.3.3, XLOG 3') :-
   last_pattern_replace(foobarbaz, '*bar*', '*tok*', X),
   X == footokbaz.
runner:case(last_pattern_replace, 4, extra_regex, 'XLOG 2.3.3, XLOG 4') :-
   last_pattern_replace(foobarbaz, '*baz', '*tok', X),
   X == foobartok.
runner:case(last_pattern_replace, 4, extra_regex, 'XLOG 2.3.3, XLOG 5') :-
   \+ last_pattern_replace(foobarbaz, foo, tok, _).
runner:case(last_pattern_replace, 4, extra_regex, 'XLOG 2.3.3, XLOG 6') :-
   \+ last_pattern_replace(foobarbaz, '*foo', '*tok', _).
runner:case(last_pattern_replace, 4, extra_regex, 'XLOG 2.3.3, XLOG 7') :-
   last_pattern_replace(foobarbaz, 'foo*', 'tok*', X),
   X == tokbarbaz.
runner:case(last_pattern_replace, 4, extra_regex, 'XLOG 2.3.3, XLOG 8') :-
   last_pattern_replace(foobarbaz, '*foo*', '*tok*', X),
   X == tokbarbaz.
runner:case(last_pattern_replace, 4, extra_regex, 'XLOG 2.3.3, XLOG 9') :-
   last_pattern_replace(foobarfoobar, '*bar*', '*tok*', X),
   X == foobarfootok.
/* unicode >0xFFFF, deseret alphabet */
runner:case(last_pattern_replace, 4, extra_regex, 'XLOG 2.3.3, XLOG 10') :-
   last_pattern_replace('𐐀𐐁𐐂𐐃𐐄𐐅𐐆𐐇𐐈', '*𐐃𐐄𐐅*', '*𐐎𐐏𐐐*', X),
   X == '𐐀𐐁𐐂𐐎𐐏𐐐𐐆𐐇𐐈'.
runner:case(last_pattern_replace, 4, extra_regex, 'XLOG 2.3.3, XLOG 11') :-
   \+ last_pattern_replace('𐐀𐐁𐐂𐐃𐐄𐐅𐐆𐐇𐐈', '𐐆𐐇𐐈*', '𐐎𐐏𐐐*', _).

/* setting ignore_case=true */
runner:case(last_pattern_replace, 4, extra_regex, 'XLOG 2.3.3, XLOG 12') :-
   \+ last_pattern_replace(foobarbaz, 'BAR*', 'tok*', _, [ignore_case(true)]).
runner:case(last_pattern_replace, 4, extra_regex, 'XLOG 2.3.3, XLOG 13') :-
   \+ last_pattern_replace('FOOBARBAZ', baz, tok, _, [ignore_case(true)]).
runner:case(last_pattern_replace, 4, extra_regex, 'XLOG 2.3.3, XLOG 14') :-
   \+ last_pattern_replace(foobarbaz, 'BAZ*', 'tok*', _, [ignore_case(true)]).
runner:case(last_pattern_replace, 4, extra_regex, 'XLOG 2.3.3, XLOG 15') :-
   last_pattern_replace(foobarbaz, '*BAZ*', '*TOK*', X, [ignore_case(true)]),
   X == foobartok.
runner:case(last_pattern_replace, 4, extra_regex, 'XLOG 2.3.3, XLOG 16') :-
   last_pattern_replace('FOOBARFOOBAR', '*foo*', '*tok*', X, [ignore_case(true)]),
   X == 'FOOBARTOKBAR'.
/* unicode >0xFFFF, deseret alphabet */
runner:case(last_pattern_replace, 4, extra_regex, 'XLOG 2.3.3, XLOG 17') :-
   last_pattern_replace('𐐀𐐁𐐂𐐃𐐄𐐅𐐆𐐇𐐈', '*𐐫𐐬𐐭*', '*𐐎𐐏𐐐*', X, [ignore_case(true)]),
   X == '𐐀𐐁𐐂𐐶𐐷𐐸𐐆𐐇𐐈'.
runner:case(last_pattern_replace, 4, extra_regex, 'XLOG 2.3.3, XLOG 18') :-
   \+ last_pattern_replace('𐐀𐐁𐐂𐐃𐐄𐐅𐐆𐐇𐐈', '𐐮𐐯𐐰*', '𐐎𐐏𐐐*', _, [ignore_case(true)]).

/* setting boundary=part */
runner:case(last_pattern_replace, 4, extra_regex, 'XLOG 2.3.3, XLOG 19') :-
   \+ last_pattern_replace(foobarbaz, tak, tok, _, [boundary(part)]).
runner:case(last_pattern_replace, 4, extra_regex, 'XLOG 2.3.3, XLOG 20') :-
   last_pattern_replace(foobarbaz, bar, tok, X, [boundary(part)]),
   X == footokbaz.
runner:case(last_pattern_replace, 4, extra_regex, 'XLOG 2.3.3, XLOG 21') :-
   last_pattern_replace(foobarbaz, foo, tok, X, [boundary(part)]),
   X == tokbarbaz.
runner:case(last_pattern_replace, 4, extra_regex, 'XLOG 2.3.3, XLOG 22') :-
   last_pattern_replace(foobarbaz, baz, tok, X, [boundary(part)]),
   X == foobartok.
/* unicode >0xFFFF, deseret alphabet */
runner:case(last_pattern_replace, 4, extra_regex, 'XLOG 2.3.3, XLOG 23') :-
   last_pattern_replace('𐐀𐐁𐐂𐐃𐐄𐐅𐐆𐐇𐐈', '𐐃𐐄𐐅', '𐐎𐐏𐐐', X, [boundary(part)]),
   X == '𐐀𐐁𐐂𐐎𐐏𐐐𐐆𐐇𐐈'.

/* setting boundary=word */
runner:case(last_pattern_replace, 4, extra_regex, 'XLOG 2.3.3, XLOG 24') :-
   \+ last_pattern_replace('foo bar baz', tak, tok, _, [boundary(word)]).
runner:case(last_pattern_replace, 4, extra_regex, 'XLOG 2.3.3, XLOG 25') :-
   last_pattern_replace('foo bar baz', bar, tok, X, [boundary(word)]),
   X == 'foo tok baz'.
runner:case(last_pattern_replace, 4, extra_regex, 'XLOG 2.3.3, XLOG 26') :-
   \+ last_pattern_replace('foo barbaz', bar, tok, _, [boundary(word)]).
runner:case(last_pattern_replace, 4, extra_regex, 'XLOG 2.3.3, XLOG 27') :-
   last_pattern_replace('foo bar baz', foo, tok, X, [boundary(word)]),
   X == 'tok bar baz'.
runner:case(last_pattern_replace, 4, extra_regex, 'XLOG 2.3.3, XLOG 28') :-
   \+ last_pattern_replace('foobar baz', foo, tok, _, [boundary(word)]).
runner:case(last_pattern_replace, 4, extra_regex, 'XLOG 2.3.3, XLOG 29') :-
   last_pattern_replace('foo bar baz', baz, tok, X, [boundary(word)]),
   X == 'foo bar tok'.
runner:case(last_pattern_replace, 4, extra_regex, 'XLOG 2.3.3, XLOG 30') :-
   \+ last_pattern_replace('foo barbaz', baz, tok, _, [boundary(word)]).
/* unicode >0xFFFF, deseret alphabet */
runner:case(last_pattern_replace, 4, extra_regex, 'XLOG 2.3.3, XLOG 31') :-
   last_pattern_replace('𐐀𐐁𐐂 𐐃𐐄𐐅 𐐆𐐇𐐈', '𐐃𐐄𐐅', '𐐎𐐏𐐐', X, [boundary(word)]),
   X == '𐐀𐐁𐐂 𐐎𐐏𐐐 𐐆𐐇𐐈'.
runner:case(last_pattern_replace, 4, extra_regex, 'XLOG 2.3.3, XLOG 32') :-
   \+ last_pattern_replace('𐐀𐐁𐐂𐐃𐐄𐐅𐐆𐐇𐐈', '𐐃𐐄𐐅', '𐐎𐐏𐐐', _, [boundary(word)]).

/* setting boundary=part, ignore_case=true */
runner:case(last_pattern_replace, 4, extra_regex, 'XLOG 2.3.3, XLOG 33') :-
   last_pattern_replace(foobarbaz, 'BAR', tok, X, [boundary(part),ignore_case(true)]),
   X == fooTOKbaz.
runner:case(last_pattern_replace, 4, extra_regex, 'XLOG 2.3.3, XLOG 34') :-
   \+ last_pattern_replace(foobarbaz, 'FOOBAZ', tok, _, [boundary(part),ignore_case(true)]).
/* unicode >0xFFFF, deseret alphabet */
runner:case(last_pattern_replace, 4, extra_regex, 'XLOG 2.3.3, XLOG 35') :-
   last_pattern_replace('𐐀𐐁𐐂𐐃𐐄𐐅𐐆𐐇𐐈', 𐐫𐐬𐐭, '𐐎𐐏𐐐', X, [boundary(part),ignore_case(true)]),
   X == '𐐀𐐁𐐂𐐶𐐷𐐸𐐆𐐇𐐈'.

/* setting boundary=word, ignore_case=true */
runner:case(last_pattern_replace, 4, extra_regex, 'XLOG 2.3.3, XLOG 36') :-
   last_pattern_replace('foo bar baz', 'BAR', tok, X, [boundary(word),ignore_case(true)]),
   X == 'foo TOK baz'.
runner:case(last_pattern_replace, 4, extra_regex, 'XLOG 2.3.3, XLOG 37') :-
   \+ last_pattern_replace('foo foo bar', baz, tok, _, [boundary(word),ignore_case(true)]).
/* unicode >0xFFFF, deseret alphabet */
runner:case(last_pattern_replace, 4, extra_regex, 'XLOG 2.3.3, XLOG 38') :-
   last_pattern_replace('𐐀𐐁𐐂 𐐃𐐄𐐅 𐐆𐐇𐐈', 𐐫𐐬𐐭, '𐐎𐐏𐐐', X, [boundary(word),ignore_case(true)]),
   X == '𐐀𐐁𐐂 𐐶𐐷𐐸 𐐆𐐇𐐈'.

/*******************************************************************/
/* Parse Patern Match etc.. Ignore Case                            */
/*******************************************************************/

/* pattern_match(S, P, O) */
runner:ref(pattern_match, 3, extra_regex, 'XLOG 2.4.1').
runner:case(pattern_match, 3, extra_regex, 'XLOG 2.4.1, XLOG 1') :-
   pattern_match(foobarbaz, '*bar*', [style(parse)]).
/* ignore_case=true downgrade */
runner:case(pattern_match, 3, extra_regex, 'XLOG 2.4.1, XLOG 2') :-
   pattern_match(foobarbaz, '*BAR*', [ignore_case(true),style(parse)]).
runner:case(pattern_match, 3, extra_regex, 'XLOG 2.4.1, XLOG 3') :-
   \+ pattern_match(foobarbaz, '=*BAR*', [ignore_case(true),style(parse)]).
/* bondary=part downgrade */
runner:case(pattern_match, 3, extra_regex, 'XLOG 2.4.1, XLOG 3') :-
   pattern_match(foobarbaz, bar, [boundary(part),style(parse)]).
runner:case(pattern_match, 3, extra_regex, 'XLOG 2.4.1, XLOG 4') :-
   \+ pattern_match(foobarbaz, '`bar`', [boundary(part),style(parse)]).

/* pattern_replace(S, P, R, T, O) */
runner:ref(pattern_replace, 5, extra_regex, 'XLOG 2.4.2').
runner:case(pattern_replace, 5, extra_regex, 'XLOG 2.4.2, XLOG 1') :-
   pattern_replace(foobarfoobar, '*bar*', '*tok*', X, [style(parse)]),
   X == footokfoobar.
/* ignore_case=true downgrade */
runner:case(pattern_replace, 5, extra_regex, 'XLOG 2.4.2, XLOG 2') :-
   pattern_replace(foobarfoobar, '*BAR*', '*tok*', X, [ignore_case(true),style(parse)]),
   X == fooTOKfoobar.
runner:case(pattern_replace, 5, extra_regex, 'XLOG 2.4.2, XLOG 3') :-
   \+ pattern_replace(foobarfoobar, '=*BAR*', '*tok*', _, [ignore_case(true),style(parse)]).
/* bondary=part downgrade */
runner:case(pattern_replace, 5, extra_regex, 'XLOG 2.4.2, XLOG 4') :-
   pattern_replace(foobarfoobar, bar, tok, X, [boundary(part),style(parse)]),
   X == footokfoobar.
runner:case(pattern_replace, 5, extra_regex, 'XLOG 2.4.2, XLOG 5') :-
   \+ pattern_replace(foobarfoobar, '`bar`', '`tok`', _, [boundary(part),style(parse)]).

/* last_pattern_replace(S, P, R, T, O) */
runner:ref(last_pattern_replace, 5, extra_regex, 'XLOG 2.4.3').
runner:case(last_pattern_replace, 5, extra_regex, 'XLOG 2.4.3, XLOG 1') :-
   last_pattern_replace(foobarfoobar, '*bar*', '*tok*', X, [style(parse)]),
   X == foobarfootok.
/* ignore_case=true downgrade */
runner:case(last_pattern_replace, 5, extra_regex, 'XLOG 2.4.3, XLOG 2') :-
   last_pattern_replace(foobarfoobar, '*BAR*', '*tok*', X, [ignore_case(true),style(parse)]),
   X == foobarfooTOK.
runner:case(last_pattern_replace, 5, extra_regex, 'XLOG 2.4.3, XLOG 3') :-
   \+ last_pattern_replace(foobarfoobar, '=*BAR*', '*tok*', _, [ignore_case(true),style(parse)]).
/* bondary=part downgrade */
runner:case(last_pattern_replace, 5, extra_regex, 'XLOG 2.4.3, XLOG 4') :-
   last_pattern_replace(foobarfoobar, bar, tok, X, [boundary(part),style(parse)]),
   X == foobarfootok.
runner:case(last_pattern_replace, 5, extra_regex, 'XLOG 2.4.3, XLOG 5') :-
   \+ last_pattern_replace(foobarfoobar, '`bar`', '`tok`', _, [boundary(part),style(parse)]).
