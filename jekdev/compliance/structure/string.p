/**
 * Prolog code for the structure string theory test cases.
 *
 * Source of test cases are the following standards:
 *   - Prolog General Core ISO/IUEC 13211-1
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
 * Restrictions
 * Only to be distributed with programs that add significant and primary
 * functionality to the library. Not to be distributed with additional
 * software intended to replace any components of the library.
 *
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */

:- use_package(library(jekdev/reference/testing)).

:- multifile runner:ref/4.
:- discontiguous runner:ref/4.

:- multifile runner:case/4.
:- discontiguous runner:case/4.

/****************************************************************/
/* String Predicate                                             */
/****************************************************************/

/* atom_length(X, Y) */

runner:ref(atom_length, 2, structure_string, 'ISO 8.16.1.4').
runner:case(atom_length, 2, structure_string, 'ISO 8.16.1.4, ISO 1') :-
   atom_length('enchanted evening', N), N == 17.
runner:case(atom_length, 2, structure_string, 'ISO 8.16.1.4, ISO 3') :-
   atom_length('', N), N == 0.
runner:case(atom_length, 2, structure_string, 'ISO 8.16.1.4, ISO 4') :-
   \+ atom_length(scarlet, 5).
runner:case(atom_length, 2, structure_string, 'ISO 8.16.1.4, ISO 5') :-
   catch(atom_length(_, 4), error(E, _), true), E == instantiation_error.
runner:case(atom_length, 2, structure_string, 'ISO 8.16.1.4, ISO 6') :-
   catch(atom_length(1.23, 4), error(E, _), true), E == type_error(atom, 1.23).
runner:case(atom_length, 2, structure_string, 'ISO 8.16.1.4, ISO 7') :-
   catch(atom_length(atom, '4'), error(E, _), true), E == type_error(integer, '4').

/* atom_concat(X, Y, Z) */

runner:ref(atom_concat, 3, structure_string, 'ISO 8.16.2.4').
runner:case(atom_concat, 3, structure_string, 'ISO 8.16.2.4, ISO 1') :-
   atom_concat(hello, ' world', S), S == 'hello world'.
runner:case(atom_concat, 3, structure_string, 'ISO 8.16.2.4, ISO 2') :-
   atom_concat(T, ' world', 'small world'), T == small.
runner:case(atom_concat, 3, structure_string, 'ISO 8.16.2.4, ISO 3') :-
   \+ atom_concat(hello, ' world', 'small world').
runner:case(atom_concat, 3, structure_string, 'ISO 8.16.2.4, ISO 4a') :-
   atom_concat(T1, T2, hello), !, T1 == '', T2 == hello.
runner:case(atom_concat, 3, structure_string, 'ISO 8.16.2.4, ISO 4b') :-
   findall(T1-T2, atom_concat(T1, T2, hello), [_, T1-T2|_]), T1 == h, T2 == ello.
runner:case(atom_concat, 3, structure_string, 'ISO 8.16.2.4, ISO 5') :-
   catch(atom_concat(small, _, _), error(E, _), true), E == instantiation_error.

/* sub_atom(X, Y, Z, T, U) */

runner:ref(sub_atom, 5, structure_string, 'ISO 8.16.3.4').
runner:case(sub_atom, 5, structure_string, 'ISO 8.16.3.4, ISO 1') :-
   sub_atom(abracadabra, 0, 5, _, S), S == abrac.
runner:case(sub_atom, 5, structure_string, 'ISO 8.16.3.4, ISO 2') :-
   sub_atom(abracadabra, _, 5, 0, S), S == dabra.
runner:case(sub_atom, 5, structure_string, 'ISO 8.16.3.4, ISO 3') :-
   sub_atom(abracadabra, 3, L, 3, S), L == 5, S == acada.
runner:case(sub_atom, 5, structure_string, 'ISO 8.16.3.4, ISO 4a') :-
   sub_atom(abracadabra, B, 2, A, ab), !, B == 0, A == 9.
runner:case(sub_atom, 5, structure_string, 'ISO 8.16.3.4, ISO 4b') :-
   findall(A-B, sub_atom(abracadabra, B, 2, A, ab), [_, A-B|_]), B == 7, A == 2.
runner:case(sub_atom, 5, structure_string, 'ISO 8.16.3.4, ISO 5') :-
   sub_atom('Banana', 3, 2, _, S), S == an.
runner:case(sub_atom, 5, structure_string, 'ISO 8.16.3.4, ISO 6a') :-
   sub_atom(charity, _, 3, _, S), !, S == cha.
runner:case(sub_atom, 5, structure_string, 'ISO 8.16.3.4, ISO 6b') :-
   findall(S, sub_atom(charity, _, 3, _, S), [_, S|_]), S == har.
runner:case(sub_atom, 5, structure_string, 'ISO 8.16.3.4, ISO 7a') :-
   sub_atom(ab, Start, Length, _, Sub_atom), !, Start == 0, Length == 0, Sub_atom = ''.
runner:case(sub_atom, 5, structure_string, 'ISO 8.16.3.4, ISO 7b') :-
   findall(Start-Length-Sub_atom, sub_atom(ab, Start, Length, _, Sub_atom),
      [_, Start-Length-Sub_atom|_]), Start == 0, Length == 1, Sub_atom = a.

/* atom_chars(X, Y) */

runner:ref(atom_chars, 2, structure_string, 'ISO 8.16.4.4').
runner:case(atom_chars, 2, structure_string, 'ISO 8.16.4.4, ISO 1') :-
   atom_chars('', L), L == [].
runner:case(atom_chars, 2, structure_string, 'ISO 8.16.4.4, ISO 2') :-
   atom_chars([], L), L == ['[', ']'].
runner:case(atom_chars, 2, structure_string, 'ISO 8.16.4.4, ISO 3') :-
   atom_chars('''', L), L == [''''].
runner:case(atom_chars, 2, structure_string, 'ISO 8.16.4.4, ISO 4') :-
   atom_chars(ant, L), L == [a, n, t].
runner:case(atom_chars, 2, structure_string, 'ISO 8.16.4.4, ISO 5') :-
   atom_chars(Str, [s, o, p]), Str == sop.
runner:case(atom_chars, 2, structure_string, 'ISO 8.16.4.4, ISO 6') :-
   atom_chars('North', ['N'|X]), X = [o, r, t, h].
runner:case(atom_chars, 2, structure_string, 'ISO 8.16.4.4, ISO 7') :-
   \+ atom_chars(soap, [s, o, p]).
runner:case(atom_chars, 2, structure_string, 'ISO 8.16.4.4, ISO 8') :-
   catch(atom_chars(_, _), error(E, _), true), E == instantiation_error.
runner:case(atom_chars, 2, structure_string, 'ISO 8.16.4.4, XLOG 1') :-
   atom_chars('a\20\b', X), X == [a, '\20\', b].
runner:case(atom_chars, 2, structure_string, 'ISO 8.16.4.4, XLOG 2') :-
   atom_chars(X, ['\xD83D\', '\xDE02\']), X == 😂 .
runner:case(atom_chars, 2, structure_string, 'ISO 8.16.4.4, XLOG 3') :-
   atom_chars(😂, X), X == [😂].

/* atom_codes(X, Y) */

runner:ref(atom_codes, 2, structure_string, 'ISO 8.16.5.4').
runner:case(atom_codes, 2, structure_string, 'ISO 8.16.5.4, ISO 1') :-
   atom_codes('', L), L == "".
runner:case(atom_codes, 2, structure_string, 'ISO 8.16.5.4, ISO 2') :-
   atom_codes([], L), L == "[]".
runner:case(atom_codes, 2, structure_string, 'ISO 8.16.5.4, ISO 3') :-
   atom_codes('''', L), L == "'".
runner:case(atom_codes, 2, structure_string, 'ISO 8.16.5.4, ISO 4') :-
   atom_codes(ant, L), L == "ant".
runner:case(atom_codes, 2, structure_string, 'ISO 8.16.5.4, ISO 5') :-
   atom_codes(Str, "sop"), Str == sop.
runner:case(atom_codes, 2, structure_string, 'ISO 8.16.5.4, ISO 6') :-
   [H] = "N", atom_codes('North', [H|T]), T == "orth".
runner:case(atom_codes, 2, structure_string, 'ISO 8.16.5.4, ISO 7') :-
   \+ atom_codes(soap, "sop").
runner:case(atom_codes, 2, structure_string, 'ISO 8.16.5.4, ISO 8') :-
   catch(atom_codes(_, _), error(E, _), true), E == instantiation_error.
runner:case(atom_codes, 2, structure_string, 'ISO 8.16.5.4, XLOG 1') :-
   atom_codes(X, [97, 16, 98]), X == 'a\20\b'.
runner:case(atom_codes, 2, structure_string, 'ISO 8.16.5.4, XLOG 2') :-
   atom_codes(😂, X), X == [0x1F602].
runner:case(atom_codes, 2, structure_string, 'ISO 8.16.5.4, XLOG 3') :-
   atom_codes(X, [0x1F602]), X == 😂 .

/* char_code(X, Y) */

runner:ref(char_code, 2, structure_string, 'ISO 8.16.6.4').
runner:case(char_code, 2, structure_string, 'ISO 8.16.6.4, ISO 1') :-
   char_code(a, Code), Code == 97.
runner:case(char_code, 2, structure_string, 'ISO 8.16.6.4, ISO 2') :-
   char_code(Str, 99), Str == c.
runner:case(char_code, 2, structure_string, 'ISO 8.16.6.4, ISO 3') :-
   char_code(Str, 0'c), Str == c.
runner:case(char_code, 2, structure_string, 'ISO 8.16.6.4, ISO 4') :-
   char_code(Str, 163), Str == £ .
runner:case(char_code, 2, structure_string, 'ISO 8.16.6.4, ISO 5') :-
   \+ char_code(b, 84).
runner:case(char_code, 2, structure_string, 'ISO 8.16.6.4, ISO 6') :-
   catch(char_code(ab, _), error(E, _), true),
   E == representation_error(character).
runner:case(char_code, 2, structure_string, 'ISO 8.16.6.4, ISO 7') :-
   catch(char_code(_, _), error(E, _), true),
   E == instantiation_error.

/* number_chars(X, Y) */

runner:ref(number_chars, 2, structure_string, 'ISO 8.16.7.4, ISO 6.3.1.2').
runner:case(number_chars, 2, structure_string, 'ISO 8.16.7.4, ISO 1') :-
   number_chars(33, L), L == ['3', '3'].
runner:case(number_chars, 2, structure_string, 'ISO 8.16.7.4, ISO 2') :-
   number_chars(33, ['3', '3']).
runner:case(number_chars, 2, structure_string, 'ISO 8.16.7.4, ISO 3') :-
   number_chars(33.0, L), L == ['3', '3', '.', '0'].
runner:case(number_chars, 2, structure_string, 'ISO 8.16.7.4, ISO 5') :-
   number_chars(A, [-, '2', '5']), A == -25.
runner:case(number_chars, 2, structure_string, 'ISO 8.16.7.4, ISO 6') :-
   number_chars(A, ['\n', ' ', '3']), A == 3.
runner:case(number_chars, 2, structure_string, 'ISO 8.16.7.4, ISO 8') :-
   number_chars(A, ['0', x, f]), A == 15.
runner:case(number_chars, 2, structure_string, 'ISO 8.16.7.4, ISO 9') :-
   number_chars(A, ['0', '''', a]), A == 97.
runner:case(number_chars, 2, structure_string, 'ISO 8.16.7.4, ISO 10') :-
   number_chars(A, ['4', '.', '2']), A == 4.2.
runner:case(number_chars, 2, structure_string, 'ISO 8.16.7.4, ISO 11') :-
   number_chars(A, ['4', '2', '.', '0', e, -, '1']), A == 4.2.
runner:case(number_chars, 2, structure_string, 'ISO 8.16.7.4, XLOG 1') :-
   catch(number_chars(_, ['1', 'E', '9']), error(E, _), true),
   E == syntax_error(illegal_number).
runner:case(number_chars, 2, structure_string, 'ISO 8.16.7.4, XLOG 2') :-
   number_chars(A, [-, '0', x, '1']), A == -1.

/* number_codes(X, Y) */

runner:ref(number_codes, 2, structure_string, 'ISO 8.16.8.4, ISO 6.4.4').
runner:case(number_codes, 2, structure_string, 'ISO 8.16.8.4, ISO 1') :-
   number_codes(33, L), L == "33".
runner:case(number_codes, 2, structure_string, 'ISO 8.16.8.4, ISO 2') :-
   number_codes(33, "33").
runner:case(number_codes, 2, structure_string, 'ISO 8.16.8.4, ISO 3') :-
   number_codes(33.0, L), L == "33.0".
runner:case(number_codes, 2, structure_string, 'ISO 8.16.8.4, ISO 4') :-
   number_codes(33.0, "3.3e+01").
runner:case(number_codes, 2, structure_string, 'ISO 8.16.8.4, ISO 5') :-
   number_codes(A, "-25"), A == -25.
runner:case(number_codes, 2, structure_string, 'ISO 8.16.8.4, ISO 6') :-
   number_codes(A, " 3"), A == 3.
runner:case(number_codes, 2, structure_string, 'ISO 8.16.8.4, ISO 7') :-
   number_codes(A, "0xf"), A == 15.
runner:case(number_codes, 2, structure_string, 'ISO 8.16.8.4, ISO 8') :-
   number_codes(A, "0'a"), A == 97.
runner:case(number_codes, 2, structure_string, 'ISO 8.16.8.4, ISO 9') :-
   number_codes(A, "4.2"), A == 4.2.
runner:case(number_codes, 2, structure_string, 'ISO 8.16.8.4, ISO 10') :-
   number_codes(A, "42.0e-1"), A == 4.2.
runner:case(number_codes, 2, structure_string, 'ISO 8.16.8.4, XLOG 1') :-
   catch(number_codes(_, "0X1"), error(E, _), true),
   E == syntax_error(illegal_number).

