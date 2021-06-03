/**
 * Prolog code for the extra structure test cases.
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
:- dynamic runner:case/4.
:- discontiguous runner:case/4.

:- ensure_loaded('../harness/data').
:- use_module(library(misc/residue)).
:- use_module(library(structure/bytes)).

/****************************************************************/
/* compare.p extras                                             */
/****************************************************************/

/* compare(C, X, Y, O) */
/* derived from compare/3 test cases, but we fix locale at en_GB */
runner:ref(compare, 4, extra_structure, 'XLOG 1.1.1').
runner:case(compare, 4, extra_structure, 'XLOG 1.1.1, XLOG 1') :-
   compare(<, 1.0, 1, [type(collator), locale(en_GB)]).
runner:case(compare, 4, extra_structure, 'XLOG 1.1.1, XLOG 2') :-
   compare(<, aardvark, zebra, [type(collator), locale(en_GB)]).
runner:case(compare, 4, extra_structure, 'XLOG 1.1.1, XLOG 3') :-
   compare(=, short, short, [type(collator), locale(en_GB)]).
runner:case(compare, 4, extra_structure, 'XLOG 1.1.1, XLOG 4') :-
   compare(=, X, X, [type(collator), locale(en_GB)]).
runner:case(compare, 4, extra_structure, 'XLOG 1.1.1, XLOG 5') :-
   compare(>, foo(a, b), north(a), [type(collator), locale(en_GB)]).
runner:case(compare, 4, extra_structure, 'XLOG 1.1.1, XLOG 6') :-
   compare(<, foo(a, _), foo(b, _), [type(collator), locale(en_GB)]).
runner:case(compare, 4, extra_structure, 'XLOG 1.1.1, XLOG 7') :-
   compare(<, a, 'A', [type(collator), locale(en_GB)]).
runner:case(compare, 4, extra_structure, 'XLOG 1.1.1, XLOG 8') :-
   compare(<, œ, ü, [type(collator), locale(en_GB)]).

/* we use ignore case and reverse */
runner:case(compare, 4, extra_structure, 'XLOG 1.1.1, XLOG 9') :-
   compare(=, a, 'A', [ignore_case(true)]).
runner:case(compare, 4, extra_structure, 'XLOG 1.1.1, XLOG 10') :-
   compare(=, a, 'A', [type(collator), locale(en_GB), ignore_case(true)]).
runner:case(compare, 4, extra_structure, 'XLOG 1.1.1, XLOG 11') :-
   compare(<, a, 'A', [reverse(true)]).
runner:case(compare, 4, extra_structure, 'XLOG 1.1.1, XLOG 12') :-
   compare(>, a, 'A', [type(collator), locale(en_GB), reverse(true)]).

/****************************************************************/
/* intatom.p extras                                             */
/****************************************************************/

/* reference */
/* derived from callable/1 test cases. */
runner:ref(reference, 1, extra_structure, 'XLOG 1.2.1').
runner:case(reference, 1, extra_structure, 'XLOG 1.2.1, XLOG 1') :-
   current_input(R), reference(R).
runner:case(reference, 1, extra_structure, 'XLOG 1.2.1, XLOG 2') :-
   \+ reference(3).
runner:case(reference, 1, extra_structure, 'XLOG 1.2.1, XLOG 3') :-
   \+ reference(_).
runner:case(reference, 1, extra_structure, 'XLOG 1.2.1, XLOG 4') :-
   \+ reference((1, 2)).
runner:case(reference, 1, extra_structure, 'XLOG 1.2.1, XLOG 5') :-
   \+ reference(string).

/* decimal */
/* derived from number/1 test cases. */
runner:ref(decimal, 1, extra_structure, 'XLOG 1.2.2').
runner:case(decimal, 1, extra_structure, 'XLOG 1.2.2, XLOG 1') :-
   \+ decimal(3).
runner:case(decimal, 1, extra_structure, 'XLOG 1.2.2, XLOG 2') :-
   decimal(0d3.3).
runner:case(decimal, 1, extra_structure, 'XLOG 1.2.2, XLOG 3') :-
   decimal(-0d3).
runner:case(decimal, 1, extra_structure, 'XLOG 1.2.2, XLOG 4') :-
   \+ decimal(a).
runner:case(decimal, 1, extra_structure, 'XLOG 1.2.2, XLOG 5') :-
   \+ decimal(_).

/****************************************************************/
/* sort.p extras                                                */
/****************************************************************/

/* type(hash) */

/* sort(L, R, O) */
/* derived from sort/2 test cases, type(hash) preserves input order */
runner:ref(sort_hash, 3, extra_structure, 'XLOG 1.3.1').
runner:case(sort_hash, 3, extra_structure, 'XLOG 1.3.1, XLOG 1') :-
   sort([1, 1.0], L, [type(hash)]), L == [1, 1.0].
runner:case(sort_hash, 3, extra_structure, 'XLOG 1.3.1, XLOG 2') :-
   sort([1.0, X, a, a, X], L, [type(hash)]), L == [1.0, X, a].
runner:case(sort_hash, 3, extra_structure, 'XLOG 1.3.1, XLOG 3') :-
   sort([north(a), shorter, short, foo(a, b)], L, [type(hash)]),
   L == [north(a), shorter, short, foo(a, b)].
runner:case(sort_hash, 3, extra_structure, 'XLOG 1.3.1, XLOG 4') :-
   sort([f(U), V, f(V), U], L, [type(hash)]),
   L == [f(U), V, f(V), U].
runner:case(sort_hash, 3, extra_structure, 'XLOG 1.3.1, XLOG 5') :-
   findall(Y, a(_, Y), L), sort(L, R, [type(hash)]),
   L = [A, B], R == [A, B].
runner:case(sort_hash, 3, extra_structure, 'XLOG 1.3.1, XLOG 6') :-
   catch(sort(_, _, [type(hash)]), error(E, _), true),
   E == instantiation_error.
runner:case(sort_hash, 3, extra_structure, 'XLOG 1.3.1, XLOG 7') :-
   catch(sort([77|35], _, [type(hash)]), error(E, _), true),
   nonvar(E), E = type_error(list, _).

/* keysort(L, R, O) */
/* derived from keysort/2 test cases, type(hash) preserves input order */
runner:ref(keysort_hash, 3, extra_structure, 'XLOG 1.3.2').
runner:case(keysort_hash, 3, extra_structure, 'XLOG 1.3.2, XLOG 1') :-
   keysort([1-x, 1.0-y], L, [type(hash)]), L == [1-x, 1.0-y].
runner:case(keysort_hash, 3, extra_structure, 'XLOG 1.3.2, XLOG 2') :-
   keysort([1.0-z, X-x, a-y, a-x, X-y], L, [type(hash)]),
   L == [1.0-z, X-x, X-y, a-y, a-x].
runner:case(keysort_hash, 3, extra_structure, 'XLOG 1.3.2, XLOG 3') :-
   keysort([north(a)-x, shorter-y, short-z, foo(a, b)-t], L, [type(hash)]),
   L == [north(a)-x, shorter-y, short-z, foo(a, b)-t].
runner:case(keysort_hash, 3, extra_structure, 'XLOG 1.3.2, XLOG 4') :-
   keysort([f(U)-x, V-y, f(V)-z, U-t], L, [type(hash)]),
   L == [f(U)-x, V-y, f(V)-z, U-t].
runner:case(keysort_hash, 3, extra_structure, 'XLOG 1.3.2, XLOG 5') :-
   findall(X-Y, a(X, Y), L), keysort(L, R, [type(hash)]), L == R.
runner:case(keysort_hash, 3, extra_structure, 'XLOG 1.3.2, XLOG 6') :-
   catch(keysort(_, _, [type(hash)]), error(E, _), true),
   E == instantiation_error.
runner:case(keysort_hash, 3, extra_structure, 'XLOG 1.3.2, XLOG 7') :-
   catch(keysort([77-x|35], _, [type(hash)]), error(E, _), true),
   nonvar(E), E = type_error(list, _).
runner:case(keysort_hash, 3, extra_structure, 'XLOG 1.3.2, XLOG 8') :-
   catch(keysort([77], _, [type(hash)]), error(E, _), true),
   nonvar(E), E = type_error(pair, 77).

/* type(tree) and type(collator) */

/* sort(L, R, O), with tree or collator */
/* we use locale, ignore case and reverse */
runner:ref(sort_tree, 3, extra_structure, 'XLOG 1.3.3').
runner:case(sort_tree, 3, extra_structure, 'XLOG 1.3.3, XLOG 1') :-
   sort([a, 'A', £], X, [type(collator), locale(en_GB)]),
   X == [£, a, 'A'].
runner:case(sort_tree, 3, extra_structure, 'XLOG 1.3.3, XLOG 2') :-
   sort([a, 'A', £], X, [ignore_case(true)]),
   X == [a, £].
runner:case(sort_tree, 3, extra_structure, 'XLOG 1.3.3, XLOG 3') :-
   sort([a, 'A', £], X, [type(collator), locale(en_GB), ignore_case(true)]),
   X == [£, a].
runner:case(sort_tree, 3, extra_structure, 'XLOG 1.3.3, XLOG 4') :-
   sort([a, 'A', £], X, [reverse(true)]),
   X == [£, a, 'A'].
runner:case(sort_tree, 3, extra_structure, 'XLOG 1.3.3, XLOG 5') :-
   sort([a, 'A', £], X, [type(collator), locale(en_GB), reverse(true)]),
   X == ['A', a, £].

/* keysort(L, R, O), with tree or collator */
/* we use locale, ignore case and reverse */
runner:ref(keysort_tree, 3, extra_structure, 'XLOG 1.3.4').
runner:case(keysort_tree, 3, extra_structure, 'XLOG 1.3.4, XLOG 1') :-
   keysort([a-1, 'A'-2, £ -3], X, [type(collator), locale(en_GB)]),
   X == [£ -3, a-1, 'A'-2].
runner:case(keysort_tree, 3, extra_structure, 'XLOG 1.3.4, XLOG 2') :-
   keysort([a-1, 'A'-2, £ -3], X, [ignore_case(true)]),
   X == [a-1, a-2, £ -3].
runner:case(keysort_tree, 3, extra_structure, 'XLOG 1.3.4, XLOG 3') :-
   keysort([a-1, 'A'-2, £ -3], X, [type(collator), locale(en_GB), ignore_case(true)]),
   X == [£ -3, a-1, a-2].
runner:case(keysort_tree, 3, extra_structure, 'XLOG 1.3.4, XLOG 4') :-
   keysort([a-1, 'A'-2, £ -3], X, [reverse(true)]),
   X == [£ -3, a-1, 'A'-2].
runner:case(keysort_tree, 3, extra_structure, 'XLOG 1.3.4, XLOG 5') :-
   keysort([a-1, 'A'-2, £ -3], X, [type(collator), locale(en_GB), reverse(true)]),
   X == ['A'-2, a-1, £ -3].

/* type(callback) */

/* sort(L, R, O), with callback */
runner:ref(sort_callback, 3, extra_structure, 'XLOG 1.3.5').
runner:case(sort_callback, 3, extra_structure, 'XLOG 1.3.5, XLOG 1') :-
   sort([4, 2.0, 1], L),
   L == [2.0, 1, 4].
runner:case(sort_callback, 3, extra_structure, 'XLOG 1.3.5, XLOG 2') :-
   sort([4, 2.0, 1], L, [type(callback), comparator(number_compare)]),
   L == [1, 2.0, 4].
runner:case(sort_callback, 3, extra_structure, 'XLOG 1.3.5, XLOG 3') :-
   sort([t(4), t(2.0), t(1)], L, [type(callback), comparator(number_compare)]),
   L == [t(1), t(2.0), t(4)].
runner:case(sort_callback, 3, extra_structure, 'XLOG 1.3.5, XLOG 4') :-
   catch(sort([a, b], _, [type(callback), comparator(_)]), error(E, _), true),
   E == instantiation_error.

/* keysort(L, R, O), with callback */
runner:ref(keysort_callback, 3, extra_structure, 'XLOG 1.3.6').
runner:case(keysort_callback, 3, extra_structure, 'XLOG 1.3.6, XLOG 1') :-
   keysort([4-a, 2.0-b, 1-c], L),
   L == [2.0-b, 1-c, 4-a].
runner:case(keysort_callback, 3, extra_structure, 'XLOG 1.3.6, XLOG 2') :-
   keysort([4-a, 2.0-b, 1-c], L, [type(callback), comparator(number_compare)]),
   L == [1-c, 2.0-b, 4-a].
runner:case(keysort_callback, 3, extra_structure, 'XLOG 1.3.6, XLOG 3') :-
   keysort([t(4)-a, t(2.0)-b, t(1)-c], L, [type(callback), comparator(number_compare)]),
   L == [t(1)-c, t(2.0)-b, t(4)-a].
runner:case(keysort_callback, 3, extra_structure, 'XLOG 1.3.6, XLOG 4') :-
   catch(keysort([a-1, b-2], _, [type(callback), comparator(1)]), error(E, _), true),
   E == type_error(callable, 1).

/* hash_code(T, H) */
runner:ref(hash_code, 2, extra_structure, 'XLOG 1.3.7').
runner:case(hash_code, 2, extra_structure, 'XLOG 1.3.7, XLOG 1') :-
   hash_code(1, H), H == 1.
runner:case(hash_code, 2, extra_structure, 'XLOG 1.3.7, XLOG 2') :-
   hash_code(1.0, H), H == 1072693248.
runner:case(hash_code, 2, extra_structure, 'XLOG 1.3.7, XLOG 3') :-
   hash_code(a, H), H == 97.
runner:case(hash_code, 2, extra_structure, 'XLOG 1.3.7, XLOG 4') :-
   hash_code(f(a, b), H), H == 101127.
runner:case(hash_code, 2, extra_structure, 'XLOG 1.3.7, XLOG 5') :-
   hash_code(_, H), integer(H).
runner:case(hash_code, 2, extra_structure, 'XLOG 1.3.7, XLOG 6') :-
   hash_code(f(_, _), H), integer(H).

/* number_compare(C, X, Y) */

runner:ref(number_compare, 3, extra_structure, 'XLOG 1.3.8').
runner:case(number_compare, 3, extra_structure, 'XLOG 1.3.8, XLOG 1') :-
   number_compare(C, 0, 1), C == < .
runner:case(number_compare, 3, extra_structure, 'XLOG 1.3.8, XLOG 2') :-
   number_compare(C, t(1.0), t(1)), C == = .
runner:case(number_compare, 3, extra_structure, 'XLOG 1.3.8, XLOG 3') :-
   number_compare(C, 3, 2.0), C == > .
runner:case(number_compare, 3, extra_structure, 'XLOG 1.3.8, XLOG 4') :-
   number_compare(C, -1048576, -1024), C == < .
runner:case(number_compare, 3, extra_structure, 'XLOG 1.3.8, XLOG 5') :-
   number_compare(C, foo(a, b), north(a)), C == > .
runner:case(number_compare, 3, extra_structure, 'XLOG 1.3.8, XLOG 6') :-
   number_compare(C, foo(a, _), foo(b, _)), C == < .

/****************************************************************/
/* string.p extras                                              */
/****************************************************************/

/* sub_atom(X, Y, Z, U) */

/* last_atom_concat(X, Y, Z) */
/* derived from atom_concat/3 test cases. */
runner:ref(last_atom_concat, 3, extra_structure, 'XLOG 1.4.1').
runner:case(last_atom_concat, 3, extra_structure, 'XLOG 1.4.1, XLOG 1') :-
   last_atom_concat(hello, ' world', S), S == 'hello world'.
runner:case(last_atom_concat, 3, extra_structure, 'XLOG 1.4.1, XLOG 2') :-
   last_atom_concat(T, ' world', 'small world'), T == small.
runner:case(last_atom_concat, 3, extra_structure, 'XLOG 1.4.1, XLOG 3') :-
   \+ last_atom_concat(hello, ' world', 'small world').
runner:case(last_atom_concat, 3, extra_structure, 'XLOG 1.4.1, XLOG 4a') :-
   last_atom_concat(T1, T2, hello), !, T1 == hello, T2 == ''.
runner:case(last_atom_concat, 3, extra_structure, 'XLOG 1.4.1, XLOG 4b') :-
   findall(T1-T2, last_atom_concat(T1, T2, hello), [_, T1-T2|_]), T1 == hell, T2 == o.
runner:case(last_atom_concat, 3, extra_structure, 'XLOG 1.4.1, XLOG 5') :-
   catch(last_atom_concat(small, _, _), error(E, _), true),
   E == instantiation_error.

/* last_sub_atom(X, Y, Z, U) */

/* last_sub_atom(X, Y, Z, T, U) */
/* derived from last_sub_atom/5 test cases. */
runner:ref(last_sub_atom, 5, extra_structure, 'XLOG 1.4.2').
runner:case(last_sub_atom, 5, extra_structure, 'XLOG 1.4.2, XLOG 1') :-
   last_sub_atom(abracadabra, 0, 5, _, S),
   S == abrac.
runner:case(last_sub_atom, 5, extra_structure, 'XLOG 1.4.2, XLOG 2') :-
   last_sub_atom(abracadabra, _, 5, 0, S),
   S == dabra.
runner:case(last_sub_atom, 5, extra_structure, 'XLOG 1.4.2, XLOG 3') :-
   last_sub_atom(abracadabra, 3, L, 3, S),
   L == 5, S == acada.
runner:case(last_sub_atom, 5, extra_structure, 'XLOG 1.4.2, XLOG 4a') :-
   last_sub_atom(abracadabra, B, 2, A, ab), !,
   B == 7, A == 2.
runner:case(last_sub_atom, 5, extra_structure, 'XLOG 1.4.2, XLOG 4b') :-
   findall(A-B, last_sub_atom(abracadabra, B, 2, A, ab), [_, A-B|_]),
   B == 0, A == 9.
runner:case(last_sub_atom, 5, extra_structure, 'XLOG 1.4.2, XLOG 5') :-
   last_sub_atom('Banana', 3, 2, _, S),
   S == an.
runner:case(last_sub_atom, 5, extra_structure, 'XLOG 1.4.2, XLOG 6a') :-
   last_sub_atom(charity, _, 3, _, S), !,
   S == ity.
runner:case(last_sub_atom, 5, extra_structure, 'XLOG 1.4.2, XLOG 6b') :-
   findall(S, last_sub_atom(charity, _, 3, _, S), [_, S|_]),
   S == rit.
runner:case(last_sub_atom, 5, extra_structure, 'XLOG 1.4.2, XLOG 7a') :-
   last_sub_atom(ab, Start, Length, _, Sub_atom), !,
   Start == 2, Length == 0, Sub_atom = ''.
runner:case(last_sub_atom, 5, extra_structure, 'XLOG 1.4.2, XLOG 7b') :-
   findall(Start-Length-Sub_atom, last_sub_atom(ab, Start, Length, _, Sub_atom),
      [_, Start-Length-Sub_atom|_]),
   Start == 1, Length == 1, Sub_atom = b.

/* atom_split(X, Y, Z) */
runner:ref(atom_split, 3, extra_structure, 'XLOG 1.4.3').
runner:case(atom_split, 3, extra_structure, 'XLOG 1.4.3, XLOG 1') :-
   catch(atom_split(_, ',', _), error(E, _), true),
   E == instantiation_error.
runner:case(atom_split, 3, extra_structure, 'XLOG 1.4.3, XLOG 2') :-
   atom_split('a,b,c', ',', X),
   X == [a, b, c].
runner:case(atom_split, 3, extra_structure, 'XLOG 1.4.3, XLOG 3') :-
   atom_split(X, ',', [a, b, c]),
   X == 'a,b,c'.
runner:case(atom_split, 3, extra_structure, 'XLOG 1.4.3, XLOG 4') :-
   \+ atom_split('a,d,c', ',', [a, b, c]).
runner:case(atom_split, 3, extra_structure, 'XLOG 1.4.3, XLOG 5') :-
   atom_split('a,b,c', ',', [a, b, c]).
runner:case(atom_split, 3, extra_structure, 'XLOG 1.4.3, XLOG 6') :-
   catch(atom_split(_, ',', foo), error(E, _), true),
   E == type_error(list, foo).

/* atom_numnber(X, Y) */
runner:ref(atom_number, 2, extra_structure, 'XLOG 1.4.4').
runner:case(atom_number, 2, extra_structure, 'XLOG 1.4.4, XLOG 1') :-
   atom_number(X, 33), X == '33'.
runner:case(atom_number, 2, extra_structure, 'XLOG 1.4.4, XLOG 2') :-
   atom_number('33.0', X), X == 33.0.
runner:case(atom_number, 2, extra_structure, 'XLOG 1.4.4, XLOG 3') :-
   atom_number('3.3e+01', 33.0).
runner:case(atom_number, 2, extra_structure, 'XLOG 1.4.4, XLOG 4') :-
   atom_number('0xf', X), X == 15.
runner:case(atom_number, 2, extra_structure, 'XLOG 1.4.4, XLOG 5') :-
   catch(atom_number(_, foo), error(E, _), true),
   E == type_error(number, foo).
runner:case(atom_number, 2, extra_structure, 'XLOG 1.4.4, XLOG 6') :-
   catch(atom_number(_, _), error(E, _), true),
   E == instantiation_error.

/****************************************************************/
/* term.p extras                                                */
/****************************************************************/

/* set_arg(K, X, Y, Z) */
/* derived from arg/3 test cases. */
runner:ref(set_arg, 4, extra_structure, 'XLOG 1.5.1').
runner:case(set_arg, 4, extra_structure, 'XLOG 1.5.1, XLOG 1') :-
   set_arg(1, foo(a, b), c, foo(c, b)).
runner:case(set_arg, 4, extra_structure, 'XLOG 1.5.1, XLOG 2') :-
   set_arg(1, foo(a, b), c, X), X == foo(c, b).
runner:case(set_arg, 4, extra_structure, 'XLOG 1.5.1, XLOG 3') :-
   set_arg(1, foo(a, b), c, foo(X, b)), X == c.
runner:case(set_arg, 4, extra_structure, 'XLOG 1.5.1, XLOG 4') :-
   set_arg(1, foo(_, b), Y, Z), Z == foo(Y, b).
runner:case(set_arg, 4, extra_structure, 'XLOG 1.5.1, XLOG 5') :-
   \+ set_arg(1, foo(a, b), c, f(b, _)).
runner:case(set_arg, 4, extra_structure, 'XLOG 1.5.1, XLOG 6') :-
   \+ set_arg(0, foo(a, b), baz, _).
runner:case(set_arg, 4, extra_structure, 'XLOG 1.5.1, XLOG 7') :-
   \+ set_arg(3, foo(3, 4), _, _).
runner:case(set_arg, 4, extra_structure, 'XLOG 1.5.1, XLOG 8') :-
   catch(set_arg(_, foo(a, b), c, _), error(E, _), true),
   nonvar(E), E = instantiation_error.
runner:case(set_arg, 4, extra_structure, 'XLOG 1.5.1, XLOG 9') :-
   catch(set_arg(1, _, c, _), error(E, _), true),
   nonvar(E), E = instantiation_error.
runner:case(set_arg, 4, extra_structure, 'XLOG 1.5.1, XLOG 10') :-
   catch(set_arg(0, atom, c, _), error(E, _), true),
   nonvar(E), E = type_error(compound, atom).
runner:case(set_arg, 4, extra_structure, 'XLOG 1.5.1, XLOG 11') :-
   catch(set_arg(0, 3, c, _), error(E, _), true),
   nonvar(E), E = type_error(_, 3).

/* sys_extend_term(F, L, T) */

runner:ref(sys_extend_term, 3, extra_structure, 'XLOG 1.5.2').
runner:case(sys_extend_term, 3, extra_structure, 'XLOG 1.5.2, XLOG 1') :-
   sys_extend_term(integer, [3], X),
   X == integer(3).
runner:case(sys_extend_term, 3, extra_structure, 'XLOG 1.5.2, XLOG 2') :-
   sys_extend_term(functor(F, c), [0], X),
   X == functor(F, c, 0).
runner:case(sys_extend_term, 3, extra_structure, 'XLOG 1.5.2, XLOG 3') :-
   sys_extend_term(;, [A = 1, B = 2], R),
   R == (A = 1; B = 2).
runner:case(sys_extend_term, 3, extra_structure, 'XLOG 1.5.2, XLOG 4') :-
   catch(sys_extend_term(3.1415, [foo], _), error(E, _), true),
   E == type_error(callable, 3.1415).

/* sys_shrink_term(T, N, F, L) */

runner:ref(sys_shrink_term, 3, extra_structure, 'XLOG 1.5.3').
runner:case(sys_shrink_term, 3, extra_structure, 'XLOG 1.5.3, XLOG 1') :-
   sys_shrink_term(integer(3), 1, X, Y),
   X == integer, Y == [3].
runner:case(sys_shrink_term, 3, extra_structure, 'XLOG 1.5.3, XLOG 2') :-
   sys_shrink_term(functor(F, c, 0), 1, X, Y),
   X == functor(F, c), Y == [0].
runner:case(sys_shrink_term, 3, extra_structure, 'XLOG 1.5.3, XLOG 3') :-
   sys_shrink_term((A = 1; B = 2), 2, X, Y),
   X == ;, Y == [A = 1, B = 2].
runner:case(sys_shrink_term, 3, extra_structure, 'XLOG 1.5.3, XLOG 4') :-
   \+ sys_shrink_term(foo(bar), 2, _, _).
runner:case(sys_shrink_term, 3, extra_structure, 'XLOG 1.5.3, XLOG 5') :-
   \+ sys_shrink_term(3.1415, 1, _, _).
