/**
 * Prolog code for the extra block test cases.
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

:- use_module(library(structure/bytes)).

/****************************************************************/
/* bytes.p extras                                               */
/****************************************************************/

/* atom_block(X, Y) */

runner:ref(atom_block, 2, extra_block, 'XLOG 4.1.1').
runner:case(atom_block, 2, extra_block, 'XLOG 4.1.1, XLOG 1') :-
   atom_block(aÿbc, X), atom_block(Y, X),
   Y == aÿbc.
runner:case(atom_block, 2, extra_block, 'XLOG 4.1.1, XLOG 2') :-
   catch(atom_block(aǿbc, _), error(E, _), true),
   E == representation_error(octet).
runner:case(atom_block, 2, extra_block, 'XLOG 4.1.1, XLOG 3') :-
   catch(atom_block(_, foo), error(E, _), true),
   E == type_error(ref, foo).
runner:case(atom_block, 2, extra_block, 'XLOG 4.1.1, XLOG 4') :-
   catch(atom_block(_, _), error(E, _), true),
   E == instantiation_error.

/* atom_block(X, Y, O) */

runner:ref(atom_block, 3, extra_block, 'XLOG 4.1.2').
runner:case(atom_block, 3, extra_block, 'XLOG 4.1.2, XLOG 1') :-
   atom_block(aÿbc, X, []), atom_block(Y, X, []),
   Y == aÿbc.
runner:case(atom_block, 3, extra_block, 'XLOG 4.1.2, XLOG 2') :-
   atom_block(aǿbc, X, []), atom_block(Y, X, []),
   Y == aǿbc.
runner:case(atom_block, 3, extra_block, 'XLOG 4.1.2, XLOG 3') :-
   atom_block('a𝄞b€c', X, []), atom_block(Y, X, []),
   Y == 'a𝄞b€c'.
runner:case(atom_block, 3, extra_block, 'XLOG 4.1.2, XLOG 4') :-
   catch(atom_block(_, foo, []), error(E, _), true),
   E == type_error(ref, foo).
runner:case(atom_block, 3, extra_block, 'XLOG 4.1.2, XLOG 5') :-
   catch(atom_block(_, _, []), error(E, _), true),
   E == instantiation_error.

/* term_block(T, B) */

runner:ref(term_block, 2, extra_block, 'XLOG 4.1.3').
runner:case(term_block, 2, extra_block, 'XLOG 4.1.3, XLOG 1') :-
   atom_block('[1,2,3]', B, []),
   term_block(T, B),
   T == [1, 2, 3].
runner:case(term_block, 2, extra_block, 'XLOG 4.1.3, XLOG 2') :-
   term_block(1 < 2, B),
   atom_block(A, B, []),
   A == '1 < 2'.
runner:case(term_block, 2, extra_block, 'XLOG 4.1.3, XLOG 3') :-
   term_block('$VAR'(1), B),
   atom_block(A, B, []),
   A == '''$VAR''(1)'.
runner:case(term_block, 2, extra_block, 'XLOG 4.1.3, XLOG 4') :-
   atom_block('foo(A+Roger,A+_)', B, []),
   term_block(T, B),
   T = foo(Y+_, Z+_), Y == Z.
runner:case(term_block, 2, extra_block, 'XLOG 4.1.3, XLOG 5') :-
   term_block('Foo', B),
   atom_block(A, B, []),
   A == '''Foo'''.
runner:case(term_block, 2, extra_block, 'XLOG 4.1.3, XLOG 6') :-
   catch(term_block(_, 123), error(E, _), true),
   E == type_error(ref, 123).
runner:case(term_block, 2, extra_block, 'XLOG 4.1.3, XLOG 7') :-
   T is pi, term_block(T, B),
   atom_block(A, B, []),
   A == '3.141592653589793'.
runner:case(term_block, 2, extra_block, 'XLOG 4.1.3, XLOG 8') :-
   atom_block('''\\z''', B, []),
   catch(term_block(_, B), error(E, _), true),
   E == syntax_error(illegal_escape).

/* term_block(T, B, O) */

runner:ref(term_block, 3, extra_block, 'XLOG 4.1.4').
runner:case(term_block, 3, extra_block, 'XLOG 4.1.4, XLOG 1') :-
   atom_block('[1,2,3]', B, []),
   term_block(T, B, [double_quotes(string)]),
   T == [1, 2, 3].
runner:case(term_block, 3, extra_block, 'XLOG 4.1.4, XLOG 2') :-
   term_block('1<2', B, [double_quotes(string)]),
   atom_block(A, B, []),
   A == '''1<2'''.
runner:case(term_block, 3, extra_block, 'XLOG 4.1.4, XLOG 3') :-
   atom_block('"foo"', B, []),
   term_block(T, B, [double_quotes(string)]),
   T == '$STR'(foo).
runner:case(term_block, 3, extra_block, 'XLOG 4.1.4, XLOG 4') :-
   term_block('$STR'(bar), B, [double_quotes(string)]),
   atom_block(A, B, []),
   A == '"bar"'.
runner:case(term_block, 3, extra_block, 'XLOG 4.1.4, XLOG 5') :-
   term_block('$STR'('\xFFFD\'), B, [double_quotes(string)]),
   atom_block(A, B, []),
   A == '"\\uFFFD"'.
runner:case(term_block, 3, extra_block, 'XLOG 4.1.4, XLOG 6') :-
   atom_block('"\\""', B, []),
   term_block(T, B, [double_quotes(string)]),
   T == '$STR'('"').
runner:case(term_block, 3, extra_block, 'XLOG 4.1.4, XLOG 7') :-
   catch(term_block(_, "abc", [double_quotes(string)]), error(E, _), true),
   E == type_error(ref, [97, 98, 99]).
runner:case(term_block, 3, extra_block, 'XLOG 4.1.4, XLOG 8') :-
   atom_block('"\\u00"', B, []),
   catch(term_block(_, B, [double_quotes(string)]), error(E, _), true),
   E == syntax_error(illegal_escape).

/* term_atom(X, Y) */

runner:ref(term_atom, 2, extra_block, 'XLOG 4.1.5').
runner:case(term_atom, 2, extra_block, 'XLOG 4.1.5, XLOG 1') :-
   term_atom(T, '[1,2,3]'),
   T == [1, 2, 3].
runner:case(term_atom, 2, extra_block, 'XLOG 4.1.5, XLOG 2') :-
   term_atom(1 < 2, A),
   A == '1 < 2'.
runner:case(term_atom, 2, extra_block, 'XLOG 4.1.5, XLOG 3') :-
   term_atom('$VAR'(1), A),
   A == '''$VAR''(1)'.
runner:case(term_atom, 2, extra_block, 'XLOG 4.1.5, XLOG 4') :-
   term_atom(T, 'foo(A+Roger,A+_)'),
   T = foo(Y+_, Z+_), Y == Z.
runner:case(term_atom, 2, extra_block, 'XLOG 4.1.5, XLOG 5') :-
   term_atom('Foo', A),
   A == '''Foo'''.
runner:case(term_atom, 2, extra_block, 'XLOG 4.1.5, XLOG 6') :-
   catch(term_atom(_, 123), error(E, _), true),
   E == type_error(atom, 123).
runner:case(term_atom, 2, extra_block, 'XLOG 4.1.5, XLOG 7') :-
   T is pi, term_atom(T, A),
   A == '3.141592653589793'.
runner:case(term_atom, 2, extra_block, 'XLOG 4.1.5, XLOG 8') :-
   catch(term_atom(_, '''\\z'''), error(E, _), true),
   E == syntax_error(illegal_escape).

/* term_atom(T, A, O) */

runner:ref(term_atom, 3, extra_block, 'XLOG 4.1.6').
runner:case(term_atom, 3, extra_block, 'XLOG 4.1.6, XLOG 1') :-
   term_atom(T, '[1,2,3]', [double_quotes(string)]),
   T == [1, 2, 3].
runner:case(term_atom, 3, extra_block, 'XLOG 4.1.6, XLOG 2') :-
   term_atom('1<2', A, [double_quotes(string)]),
   A == '''1<2'''.
runner:case(term_atom, 3, extra_block, 'XLOG 4.1.6, XLOG 3') :-
   term_atom(T, '"foo"', [double_quotes(string)]),
   T == '$STR'(foo).
runner:case(term_atom, 3, extra_block, 'XLOG 4.1.6, XLOG 4') :-
   term_atom('$STR'(bar), A, [double_quotes(string)]),
   A == '"bar"'.
runner:case(term_atom, 3, extra_block, 'XLOG 4.1.6, XLOG 5') :-
   term_atom('$STR'('\xFFFD\'), A, [double_quotes(string)]),
   A == '"\\uFFFD"'.
runner:case(term_atom, 3, extra_block, 'XLOG 4.1.6, XLOG 6') :-
   term_atom(T, '"\\""', [double_quotes(string)]),
   T == '$STR'('"').
runner:case(term_atom, 3, extra_block, 'XLOG 4.1.6, XLOG 7') :-
   catch(term_atom(_, "abc", [double_quotes(string)]), error(E, _), true),
   E == type_error(atom, [97, 98, 99]).
runner:case(term_atom, 3, extra_block, 'XLOG 4.1.6, XLOG 8') :-
   catch(term_atom(_, '"\\u00"', [double_quotes(string)]), error(E, _), true),
   E == syntax_error(illegal_escape).
