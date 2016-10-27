/**
 * Prolog code for the extra structure theory test cases.
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


/****************************************************************/
/* compare.p extras                                             */
/****************************************************************/

/****************************************************************/
/* intatom.p extras                                             */
/****************************************************************/

/* reference */

/* decimal */

/****************************************************************/
/* set.p extras                                                 */
/****************************************************************/

/* sys_distinct(L, R) */

/* sys_keygroup(L, R) */

/* term_hash(T, H) */

/****************************************************************/
/* string.p extras                                              */
/****************************************************************/

/* sub_atom(X, Y, Z, U) */

/* last_atom_concat(X, Y, Z) */

runner:ref(last_atom_concat, 3, extra_last, 'XLOG 1.1.1').
runner:case(last_atom_concat, 3, extra_last, 'XLOG 1.1.1, XLOG 1') :-
   last_atom_concat(hello, ' world', S),
   S == 'hello world'.
runner:case(last_atom_concat, 3, extra_last, 'XLOG 1.1.1, XLOG 2') :-
   last_atom_concat(T, ' world', 'small world'),
   T == small.
runner:case(last_atom_concat, 3, extra_last, 'XLOG 1.1.1, XLOG 3') :-
   \+ last_atom_concat(hello, ' world', 'small world').
runner:case(last_atom_concat, 3, extra_last, 'XLOG 1.1.1, XLOG 4a') :-
   last_atom_concat(T1, T2, hello), !,
   T1 == hello,
   T2 == ''.
runner:case(last_atom_concat, 3, extra_last, 'XLOG 1.1.1, XLOG 4b') :-
   findall(T1-T2, last_atom_concat(T1, T2, hello), [_,T1-T2|_]),
   T1 == hell,
   T2 == o.
runner:case(last_atom_concat, 3, extra_last, 'XLOG 1.1.1, XLOG 5') :-
   catch(last_atom_concat(small, _, _), error(E,_), true),
   E == instantiation_error.

/* last_sub_atom(X, Y, Z, U) */

/* last_sub_atom(X, Y, Z, T, U) */

runner:ref(last_sub_atom, 5, extra_last, 'XLOG 1.1.2').
runner:case(last_sub_atom, 5, extra_last, 'XLOG 1.1.2, XLOG 1') :-
   last_sub_atom(abracadabra, 0, 5, _, S),
   S == abrac.
runner:case(last_sub_atom, 5, extra_last, 'XLOG 1.1.2, XLOG 2') :-
   last_sub_atom(abracadabra, _, 5, 0, S),
   S == dabra.
runner:case(last_sub_atom, 5, extra_last, 'XLOG 1.1.2, XLOG 3') :-
   last_sub_atom(abracadabra, 3, L, 3, S),
   L == 5,
   S == acada.
runner:case(last_sub_atom, 5, extra_last, 'XLOG 1.1.2, XLOG 4a') :-
   last_sub_atom(abracadabra, B, 2, A, ab), !,
   B == 7,
   A == 2.
runner:case(last_sub_atom, 5, extra_last, 'XLOG 1.1.2, XLOG 4b') :-
   findall(A-B, last_sub_atom(abracadabra, B, 2, A, ab), [_,A-B|_]),
   B == 0,
   A == 9.
runner:case(last_sub_atom, 5, extra_last, 'XLOG 1.1.2, XLOG 5') :-
   last_sub_atom('Banana', 3, 2, _, S),
   S == an.
runner:case(last_sub_atom, 5, extra_last, 'XLOG 1.1.2, XLOG 6a') :-
   last_sub_atom(charity, _, 3, _, S), !,
   S == ity.
runner:case(last_sub_atom, 5, extra_last, 'XLOG 1.1.2, XLOG 6b') :-
   findall(S, last_sub_atom(charity, _, 3, _, S), [_,S|_]),
   S == rit.
runner:case(last_sub_atom, 5, extra_last, 'XLOG 1.1.2, XLOG 7a') :-
   last_sub_atom(ab, Start, Length, _, Sub_atom), !,
   Start == 2,
   Length == 0,
   Sub_atom = ''.
runner:case(last_sub_atom, 5, extra_last, 'XLOG 1.1.2, XLOG 7b') :-
   findall(Start-Length-Sub_atom, last_sub_atom(ab, Start, Length, _, Sub_atom),
      [_,Start-Length-Sub_atom|_]),
   Start == 1,
   Length == 1,
   Sub_atom = b.

/****************************************************************/
/* term.p extras                                                */
/****************************************************************/

/* set_arg(K, X, Y, Z) */
