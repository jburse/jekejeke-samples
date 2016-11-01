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

:- ensure_loaded('../harness/data').

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
/* derived from sort/2 test cases, sys_distinct/2 preservres input order */
runner:ref(sys_distinct, 2, extra_structure, 'XLOG 1.1.1').
runner:case(sys_distinct, 2, extra_structure, 'XLOG 1.1.1, XLOG 1') :-
   sys_distinct([1,1.0], L),
   L == [1,1.0].
runner:case(sys_distinct, 2, extra_structure, 'XLOG 1.1.1, XLOG 2') :-
   sys_distinct([1.0,X,a,a,X], L),
   L == [1.0,X,a].
runner:case(sys_distinct, 2, extra_structure, 'XLOG 1.1.1, XLOG 3') :-
   sys_distinct([north(a),shorter,short,foo(a,b)], L),
   L == [north(a),shorter,short,foo(a,b)].
runner:case(sys_distinct, 2, extra_structure, 'XLOG 1.1.1, ISO 6') :-
   sys_distinct([f(U),V,f(V),U], L),
   L == [f(U),V,f(V),U].
runner:case(sys_distinct, 2, extra_structure, 'XLOG 1.1.1, XLOG 4') :-
   findall(Y, a(_, Y), L),
   sys_distinct(L, R),
   L = [A,B],
   R == [A,B].
runner:case(sys_distinct, 2, extra_structure, 'XLOG 1.1.1, XLOG 5') :-
   catch(sys_distinct(_, _), error(E,_), true),
   nonvar(E),
   E = instantiation_error.
runner:case(sys_distinct, 2, extra_structure, 'XLOG 1.1.1, XLOG 6') :-
   catch(sys_distinct([77|35], _), error(E,_), true),
   nonvar(E),
   E = type_error(list,_).

/* sys_keygroup(L, R) */
/* derived from keysort/2 test cases, sys_keygroup/2 preservres input order */
runner:ref(sys_keygroup, 2, extra_structure, 'XLOG 1.1.2').
runner:case(sys_keygroup, 2, extra_structure, 'XLOG 1.1.2, XLOG 1') :-
   sys_keygroup([1-x,1.0-y], L),
   L == [1-x,1.0-y].
runner:case(sys_keygroup, 2, extra_structure, 'XLOG 1.1.2, XLOG 2') :-
   sys_keygroup([1.0-z,X-x,a-y,a-x,X-y], L),
   L == [1.0-z,X-x,X-y,a-y,a-x].
runner:case(sys_keygroup, 2, extra_structure, 'XLOG 1.1.2, XLOG 3') :-
   sys_keygroup([north(a)-x,shorter-y,short-z,foo(a,b)-t], L),
   L == [north(a)-x,shorter-y,short-z,foo(a,b)-t].
runner:case(sys_keygroup, 2, extra_structure, 'XLOG 1.1.2, XLOG 4') :-
   sys_keygroup([f(U)-x,V-y,f(V)-z,U-t], L),
   L == [f(U)-x,V-y,f(V)-z,U-t].
runner:case(sys_keygroup, 2, extra_structure, 'XLOG 1.1.2, XLOG 5') :-
   findall(X-Y, a(X, Y), L),
   sys_keygroup(L, R),
   L == R.
runner:case(sys_keygroup, 2, extra_structure, 'XLOG 1.1.2, XLOG 6') :-
   catch(sys_keygroup(_, _), error(E,_), true),
   nonvar(E),
   E = instantiation_error.
runner:case(sys_keygroup, 2, extra_structure, 'XLOG 1.1.2, XLOG 7') :-
   catch(sys_keygroup([77-x|35], _), error(E,_), true),
   nonvar(E),
   E = type_error(list,_).
runner:case(sys_keygroup, 2, extra_structure, 'XLOG 1.1.2, XLOG 8') :-
   catch(sys_keygroup([77], _), error(E,_), true),
   nonvar(E),
   E = type_error(pair,77).

/* hash_code(T, H) */
runner:ref(hash_code, 2, extra_structure, 'XLOG 1.1.3').
runner:case(hash_code, 2, extra_structure, 'XLOG 1.1.3, XLOG 1') :-
   hash_code(1, H),
   H == 1.
runner:case(hash_code, 2, extra_structure, 'XLOG 1.1.3, XLOG 2') :-
   hash_code(1.0, H),
   H == 1072693248.
runner:case(hash_code, 2, extra_structure, 'XLOG 1.1.3, XLOG 3') :-
   hash_code(a, H),
   H == 97.
runner:case(hash_code, 2, extra_structure, 'XLOG 1.1.3, XLOG 4') :-
   hash_code(f(a,b), H),
   H == 101127.
runner:case(hash_code, 2, extra_structure, 'XLOG 1.1.3, XLOG 5') :-
   hash_code(_, H),
   integer(H).
runner:case(hash_code, 2, extra_structure, 'XLOG 1.1.3, XLOG 6') :-
   hash_code(f(_,_), H),
   integer(H).

/* term_hash(T, H) */
runner:ref(term_hash, 2, extra_structure, 'XLOG 1.1.4').
runner:case(term_hash, 2, extra_structure, 'XLOG 1.1.4, XLOG 1') :-
   term_hash(1, H),
   H == 1.
runner:case(term_hash, 2, extra_structure, 'XLOG 1.1.4, XLOG 2') :-
   term_hash(1.0, H),
   H == 1072693248.
runner:case(term_hash, 2, extra_structure, 'XLOG 1.1.4, XLOG 3') :-
   term_hash(a, H),
   H == 97.
runner:case(term_hash, 2, extra_structure, 'XLOG 1.1.4, XLOG 4') :-
   term_hash(f(a,b), H),
   H == 101127.
runner:case(term_hash, 2, extra_structure, 'XLOG 1.1.4, XLOG 5') :-
   term_hash(_, H),
   var(H).
runner:case(term_hash, 2, extra_structure, 'XLOG 1.1.4, XLOG 6') :-
   term_hash(f(_,_), H),
   var(H).

/* term_hash(T, D, R, H) */
runner:ref(term_hash, 4, extra_structure, 'XLOG 1.1.5').
runner:case(term_hash, 4, extra_structure, 'XLOG 1.1.5, XLOG 1') :-
   term_hash(1, -1, 0, H),
   H == 1.
runner:case(term_hash, 4, extra_structure, 'XLOG 1.1.5, XLOG 2') :-
   term_hash(1.0, -1, 0, H),
   H == 1072693248.
runner:case(term_hash, 4, extra_structure, 'XLOG 1.1.5, XLOG 3') :-
   term_hash(a, -1, 0, H),
   H == 97.
runner:case(term_hash, 4, extra_structure, 'XLOG 1.1.5, XLOG 4') :-
   term_hash(f(a,b), -1, 100, H),
   H == 27.
runner:case(term_hash, 4, extra_structure, 'XLOG 1.1.5, XLOG 5') :-
   term_hash(_, 1, 0, H),
   var(H).
runner:case(term_hash, 4, extra_structure, 'XLOG 1.1.5, XLOG 6') :-
   term_hash(f(_,_), 1, 0, H),
   H == 102.

/****************************************************************/
/* string.p extras                                              */
/****************************************************************/

/* sub_atom(X, Y, Z, U) */

/* last_atom_concat(X, Y, Z) */

runner:ref(last_atom_concat, 3, extra_structure, 'XLOG 1.2.1').
runner:case(last_atom_concat, 3, extra_structure, 'XLOG 1.2.1, XLOG 1') :-
   last_atom_concat(hello, ' world', S),
   S == 'hello world'.
runner:case(last_atom_concat, 3, extra_structure, 'XLOG 1.2.1, XLOG 2') :-
   last_atom_concat(T, ' world', 'small world'),
   T == small.
runner:case(last_atom_concat, 3, extra_structure, 'XLOG 1.2.1, XLOG 3') :-
   \+ last_atom_concat(hello, ' world', 'small world').
runner:case(last_atom_concat, 3, extra_structure, 'XLOG 1.2.1, XLOG 4a') :-
   last_atom_concat(T1, T2, hello), !,
   T1 == hello,
   T2 == ''.
runner:case(last_atom_concat, 3, extra_structure, 'XLOG 1.2.1, XLOG 4b') :-
   findall(T1-T2, last_atom_concat(T1, T2, hello), [_,T1-T2|_]),
   T1 == hell,
   T2 == o.
runner:case(last_atom_concat, 3, extra_structure, 'XLOG 1.2.1, XLOG 5') :-
   catch(last_atom_concat(small, _, _), error(E,_), true),
   E == instantiation_error.

/* last_sub_atom(X, Y, Z, U) */

/* last_sub_atom(X, Y, Z, T, U) */

runner:ref(last_sub_atom, 5, extra_structure, 'XLOG 1.2.2').
runner:case(last_sub_atom, 5, extra_structure, 'XLOG 1.2.2, XLOG 1') :-
   last_sub_atom(abracadabra, 0, 5, _, S),
   S == abrac.
runner:case(last_sub_atom, 5, extra_structure, 'XLOG 1.2.2, XLOG 2') :-
   last_sub_atom(abracadabra, _, 5, 0, S),
   S == dabra.
runner:case(last_sub_atom, 5, extra_structure, 'XLOG 1.2.2, XLOG 3') :-
   last_sub_atom(abracadabra, 3, L, 3, S),
   L == 5,
   S == acada.
runner:case(last_sub_atom, 5, extra_structure, 'XLOG 1.2.2, XLOG 4a') :-
   last_sub_atom(abracadabra, B, 2, A, ab), !,
   B == 7,
   A == 2.
runner:case(last_sub_atom, 5, extra_structure, 'XLOG 1.2.2, XLOG 4b') :-
   findall(A-B, last_sub_atom(abracadabra, B, 2, A, ab), [_,A-B|_]),
   B == 0,
   A == 9.
runner:case(last_sub_atom, 5, extra_structure, 'XLOG 1.2.2, XLOG 5') :-
   last_sub_atom('Banana', 3, 2, _, S),
   S == an.
runner:case(last_sub_atom, 5, extra_structure, 'XLOG 1.2.2, XLOG 6a') :-
   last_sub_atom(charity, _, 3, _, S), !,
   S == ity.
runner:case(last_sub_atom, 5, extra_structure, 'XLOG 1.2.2, XLOG 6b') :-
   findall(S, last_sub_atom(charity, _, 3, _, S), [_,S|_]),
   S == rit.
runner:case(last_sub_atom, 5, extra_structure, 'XLOG 1.2.2, XLOG 7a') :-
   last_sub_atom(ab, Start, Length, _, Sub_atom), !,
   Start == 2,
   Length == 0,
   Sub_atom = ''.
runner:case(last_sub_atom, 5, extra_structure, 'XLOG 1.2.2, XLOG 7b') :-
   findall(Start-Length-Sub_atom, last_sub_atom(ab, Start, Length, _, Sub_atom),
      [_,Start-Length-Sub_atom|_]),
   Start == 1,
   Length == 1,
   Sub_atom = b.

/****************************************************************/
/* term.p extras                                                */
/****************************************************************/

/* set_arg(K, X, Y, Z) */
