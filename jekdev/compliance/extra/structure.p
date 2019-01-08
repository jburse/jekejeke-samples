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

:- ensure_loaded('../harness/data').
:- use_module(library(experiment/surrogate)).

/****************************************************************/
/* compare.p extras                                             */
/****************************************************************/

/* locale_compare(O, X, Y) */
/* derived from compare/3 test cases, but We fix locale at en_UK */
runner:ref(locale_compare, 3, extra_structure, 'XLOG 1.1.1').
runner:case(locale_compare, 3, extra_structure, 'XLOG 1.1.1, XLOG 1') :-
   locale_compare(en_UK, <, 1.0, 1).
runner:case(locale_compare, 3, extra_structure, 'XLOG 1.1.1, XLOG 2') :-
   locale_compare(en_UK, <, aardvark, zebra).
runner:case(locale_compare, 3, extra_structure, 'XLOG 1.1.1, XLOG 3') :-
   locale_compare(en_UK, =, short, short).
runner:case(locale_compare, 3, extra_structure, 'XLOG 1.1.1, XLOG 4') :-
   locale_compare(en_UK, =, X, X).
runner:case(locale_compare, 3, extra_structure, 'XLOG 1.1.1, XLOG 5') :-
   locale_compare(en_UK, >, foo(a,b), north(a)).
runner:case(locale_compare, 3, extra_structure, 'XLOG 1.1.1, XLOG 6') :-
   locale_compare(en_UK, <, foo(a,_), foo(b,_)).
runner:case(locale_compare, 3, extra_structure, 'XLOG 1.1.1, XLOG 7') :-
   locale_compare(en_UK, <, a, 'A').
runner:case(locale_compare, 3, extra_structure, 'XLOG 1.1.1, XLOG 8') :-
   locale_compare(en_UK, <, œ, ü).

/****************************************************************/
/* intatom.p extras                                             */
/****************************************************************/

/* reference */
/* derived from callable/1 test cases. */
runner:ref(reference, 1, extra_structure, 'XLOG 1.2.1').
runner:case(reference, 1, extra_structure, 'XLOG 1.2.1, XLOG 1') :-
   sys_new_surrogate(R),
   reference(R).
runner:case(reference, 1, extra_structure, 'XLOG 1.2.1, XLOG 2') :-
   \+ reference(3).
runner:case(reference, 1, extra_structure, 'XLOG 1.2.1, XLOG 3') :-
   \+ reference(_).
runner:case(reference, 1, extra_structure, 'XLOG 1.2.1, XLOG 4') :-
   \+ reference((1,2)).
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
/* set.p extras                                                 */
/****************************************************************/

/* sys_distinct(L, R) */
/* derived from sort/2 test cases, sys_distinct/2 preserves input order */
runner:ref(sys_distinct, 2, extra_structure, 'XLOG 1.3.1').
runner:case(sys_distinct, 2, extra_structure, 'XLOG 1.3.1, XLOG 1') :-
   sys_distinct([1,1.0], L),
   L == [1,1.0].
runner:case(sys_distinct, 2, extra_structure, 'XLOG 1.3.1, XLOG 2') :-
   sys_distinct([1.0,X,a,a,X], L),
   L == [1.0,X,a].
runner:case(sys_distinct, 2, extra_structure, 'XLOG 1.3.1, XLOG 3') :-
   sys_distinct([north(a),shorter,short,foo(a,b)], L),
   L == [north(a),shorter,short,foo(a,b)].
runner:case(sys_distinct, 2, extra_structure, 'XLOG 1.3.1, XLOG 4') :-
   sys_distinct([f(U),V,f(V),U], L),
   L == [f(U),V,f(V),U].
runner:case(sys_distinct, 2, extra_structure, 'XLOG 1.3.1, XLOG 5') :-
   findall(Y, a(_, Y), L),
   sys_distinct(L, R),
   L = [A,B],
   R == [A,B].
runner:case(sys_distinct, 2, extra_structure, 'XLOG 1.3.1, XLOG 6') :-
   catch(sys_distinct(_, _), error(E,_), true),
   nonvar(E),
   E = instantiation_error.
runner:case(sys_distinct, 2, extra_structure, 'XLOG 1.3.1, XLOG 7') :-
   catch(sys_distinct([77|35], _), error(E,_), true),
   nonvar(E),
   E = type_error(list,_).

/* sys_keygroup(L, R) */
/* derived from keysort/2 test cases, sys_keygroup/2 preserves input order */
runner:ref(sys_keygroup, 2, extra_structure, 'XLOG 1.3.2').
runner:case(sys_keygroup, 2, extra_structure, 'XLOG 1.3.2, XLOG 1') :-
   sys_keygroup([1-x,1.0-y], L),
   L == [1-x,1.0-y].
runner:case(sys_keygroup, 2, extra_structure, 'XLOG 1.3.2, XLOG 2') :-
   sys_keygroup([1.0-z,X-x,a-y,a-x,X-y], L),
   L == [1.0-z,X-x,X-y,a-y,a-x].
runner:case(sys_keygroup, 2, extra_structure, 'XLOG 1.3.2, XLOG 3') :-
   sys_keygroup([north(a)-x,shorter-y,short-z,foo(a,b)-t], L),
   L == [north(a)-x,shorter-y,short-z,foo(a,b)-t].
runner:case(sys_keygroup, 2, extra_structure, 'XLOG 1.3.2, XLOG 4') :-
   sys_keygroup([f(U)-x,V-y,f(V)-z,U-t], L),
   L == [f(U)-x,V-y,f(V)-z,U-t].
runner:case(sys_keygroup, 2, extra_structure, 'XLOG 1.3.2, XLOG 5') :-
   findall(X-Y, a(X, Y), L),
   sys_keygroup(L, R),
   L == R.
runner:case(sys_keygroup, 2, extra_structure, 'XLOG 1.3.2, XLOG 6') :-
   catch(sys_keygroup(_, _), error(E,_), true),
   nonvar(E),
   E = instantiation_error.
runner:case(sys_keygroup, 2, extra_structure, 'XLOG 1.3.2, XLOG 7') :-
   catch(sys_keygroup([77-x|35], _), error(E,_), true),
   nonvar(E),
   E = type_error(list,_).
runner:case(sys_keygroup, 2, extra_structure, 'XLOG 1.3.2, XLOG 8') :-
   catch(sys_keygroup([77], _), error(E,_), true),
   nonvar(E),
   E = type_error(pair,77).

/* hash_code(T, H) */
runner:ref(hash_code, 2, extra_structure, 'XLOG 1.3.3').
runner:case(hash_code, 2, extra_structure, 'XLOG 1.3.3, XLOG 1') :-
   hash_code(1, H),
   H == 1.
runner:case(hash_code, 2, extra_structure, 'XLOG 1.3.3, XLOG 2') :-
   hash_code(1.0, H),
   H == 1072693248.
runner:case(hash_code, 2, extra_structure, 'XLOG 1.3.3, XLOG 3') :-
   hash_code(a, H),
   H == 97.
runner:case(hash_code, 2, extra_structure, 'XLOG 1.3.3, XLOG 4') :-
   hash_code(f(a,b), H),
   H == 101127.
runner:case(hash_code, 2, extra_structure, 'XLOG 1.3.3, XLOG 5') :-
   hash_code(_, H),
   integer(H).
runner:case(hash_code, 2, extra_structure, 'XLOG 1.3.3, XLOG 6') :-
   hash_code(f(_,_), H),
   integer(H).

/* locale_sort(L, R) */
/* derived from sort/2 test cases, but We fix locale at en_UK */
runner:ref(locale_sort, 2, extra_structure, 'XLOG 1.3.6').
runner:case(locale_sort, 2, extra_structure, 'XLOG 1.3.6, XLOG 1') :-
   locale_sort(en_UK, [1,1.0], L),
   L == [1.0,1].
runner:case(locale_sort, 2, extra_structure, 'XLOG 1.3.6, XLOG 2') :-
   locale_sort(en_UK, [1.0,X,a,a,X], L),
   L == [X,1.0,a].
runner:case(locale_sort, 2, extra_structure, 'XLOG 1.3.6, XLOG 3') :-
   locale_sort(en_UK, [north(a),shorter,short,foo(a,b)], L),
   L == [short,shorter,north(a),foo(a,b)].
runner:case(locale_sort, 2, extra_structure, 'XLOG 1.3.6, XLOG 4') :-
   locale_sort(en_UK, [f(U),V,f(V),U], L),
   (  L == [U,V,f(U),f(V)]
   ;  L == [V,U,f(V),f(U)]).
runner:case(locale_sort, 2, extra_structure, 'XLOG 1.3.6, XLOG 5') :-
   findall(Y, a(_, Y), L),
   locale_sort(en_UK, L, R),
   L = [A,B],
   (  R == [A,B]
   ;  R == [B,A]).
runner:case(locale_sort, 2, extra_structure, 'XLOG 1.3.6, XLOG 6') :-
   catch(locale_sort(en_UK, _, _), error(E,_), true),
   nonvar(E),
   E = instantiation_error.
runner:case(locale_sort, 2, extra_structure, 'XLOG 1.3.6, XLOG 7') :-
   catch(locale_sort(en_UK, [77|35], _), error(E,_), true),
   nonvar(E),
   E = type_error(list,_).
runner:case(locale_sort, 2, extra_structure, 'XLOG 1.3.6, XLOG 8') :-
   locale_sort(en_UK, [a,'A',b,'B'], L),
   L == [a,'A',b,'B'].
runner:case(locale_sort, 2, extra_structure, 'XLOG 1.3.6, XLOG 9') :-
   locale_sort(en_UK, [ü,u,œ,o], L),
   L == [o,œ,u,ü].

/* locale_keysort(L, R) */
/* derived from keysort/2 test cases, but We fix locale at en_UK */
runner:ref(locale_keysort, 2, extra_structure, 'XLOG 1.3.7').
runner:case(locale_keysort, 2, extra_structure, 'XLOG 1.3.7, XLOG 1') :-
   locale_keysort(en_UK, [1-x,1.0-y], L),
   L == [1.0-y,1-x].
runner:case(locale_keysort, 2, extra_structure, 'XLOG 1.3.7, XLOG 2') :-
   locale_keysort(en_UK, [1.0-z,X-x,a-y,a-x,X-y], L),
   L == [X-x,X-y,1.0-z,a-y,a-x].
runner:case(locale_keysort, 2, extra_structure, 'XLOG 1.3.7, XLOG 3') :-
   locale_keysort(en_UK, [north(a)-x,shorter-y,short-z,foo(a,b)-t], L),
   L == [short-z,shorter-y,north(a)-x,foo(a,b)-t].
runner:case(locale_keysort, 2, extra_structure, 'XLOG 1.3.7, XLOG 4') :-
   locale_keysort(en_UK, [f(U)-x,V-y,f(V)-z,U-t], L),
   (  L == [U-t,V-y,f(U)-x,f(V)-z]
   ;  L == [V-y,U-t,f(V)-z,f(U)-x]).
runner:case(locale_keysort, 2, extra_structure, 'XLOG 1.3.7, XLOG 5') :-
   findall(X-Y, a(X, Y), L),
   locale_keysort(en_UK, L, R),
   L == R.
runner:case(locale_keysort, 2, extra_structure, 'XLOG 1.3.7, XLOG 6') :-
   catch(locale_keysort(en_UK, _, _), error(E,_), true),
   nonvar(E),
   E = instantiation_error.
runner:case(locale_keysort, 2, extra_structure, 'XLOG 1.3.7, XLOG 7') :-
   catch(locale_keysort(en_UK, [77-x|35], _), error(E,_), true),
   nonvar(E),
   E = type_error(list,_).
runner:case(locale_keysort, 2, extra_structure, 'XLOG 1.3.7, XLOG 8') :-
   catch(locale_keysort(en_UK, [77], _), error(E,_), true),
   nonvar(E),
   E = type_error(pair,77).
runner:case(locale_keysort, 2, extra_structure, 'XLOG 1.3.7, XLOG 9') :-
   locale_keysort(en_UK, [a-1,'A'-2,a-3,'B'-4,b-5,'B'-6], L),
   L == [a-1,a-3,'A'-2,b-5,'B'-4,'B'-6].
runner:case(locale_keysort, 2, extra_structure, 'XLOG 1.3.7, XLOG 10') :-
   locale_keysort(en_UK, [ü-1,u-2,ü-3,œ-4,o-5,o-6], L),
   L == [o-5,o-6,œ-4,u-2,ü-1,ü-3].

/****************************************************************/
/* string.p extras                                              */
/****************************************************************/

/* sub_atom(X, Y, Z, U) */

/* last_atom_concat(X, Y, Z) */
/* derived from atom_concat/3 test cases. */
runner:ref(last_atom_concat, 3, extra_structure, 'XLOG 1.4.1').
runner:case(last_atom_concat, 3, extra_structure, 'XLOG 1.4.1, XLOG 1') :-
   last_atom_concat(hello, ' world', S),
   S == 'hello world'.
runner:case(last_atom_concat, 3, extra_structure, 'XLOG 1.4.1, XLOG 2') :-
   last_atom_concat(T, ' world', 'small world'),
   T == small.
runner:case(last_atom_concat, 3, extra_structure, 'XLOG 1.4.1, XLOG 3') :-
   \+ last_atom_concat(hello, ' world', 'small world').
runner:case(last_atom_concat, 3, extra_structure, 'XLOG 1.4.1, XLOG 4a') :-
   last_atom_concat(T1, T2, hello), !,
   T1 == hello,
   T2 == ''.
runner:case(last_atom_concat, 3, extra_structure, 'XLOG 1.4.1, XLOG 4b') :-
   findall(T1-T2, last_atom_concat(T1, T2, hello), [_,T1-T2|_]),
   T1 == hell,
   T2 == o.
runner:case(last_atom_concat, 3, extra_structure, 'XLOG 1.4.1, XLOG 5') :-
   catch(last_atom_concat(small, _, _), error(E,_), true),
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
   L == 5,
   S == acada.
runner:case(last_sub_atom, 5, extra_structure, 'XLOG 1.4.2, XLOG 4a') :-
   last_sub_atom(abracadabra, B, 2, A, ab), !,
   B == 7,
   A == 2.
runner:case(last_sub_atom, 5, extra_structure, 'XLOG 1.4.2, XLOG 4b') :-
   findall(A-B, last_sub_atom(abracadabra, B, 2, A, ab), [_,A-B|_]),
   B == 0,
   A == 9.
runner:case(last_sub_atom, 5, extra_structure, 'XLOG 1.4.2, XLOG 5') :-
   last_sub_atom('Banana', 3, 2, _, S),
   S == an.
runner:case(last_sub_atom, 5, extra_structure, 'XLOG 1.4.2, XLOG 6a') :-
   last_sub_atom(charity, _, 3, _, S), !,
   S == ity.
runner:case(last_sub_atom, 5, extra_structure, 'XLOG 1.4.2, XLOG 6b') :-
   findall(S, last_sub_atom(charity, _, 3, _, S), [_,S|_]),
   S == rit.
runner:case(last_sub_atom, 5, extra_structure, 'XLOG 1.4.2, XLOG 7a') :-
   last_sub_atom(ab, Start, Length, _, Sub_atom), !,
   Start == 2,
   Length == 0,
   Sub_atom = ''.
runner:case(last_sub_atom, 5, extra_structure, 'XLOG 1.4.2, XLOG 7b') :-
   findall(Start-Length-Sub_atom, last_sub_atom(ab, Start, Length, _, Sub_atom),
      [_,Start-Length-Sub_atom|_]),
   Start == 1,
   Length == 1,
   Sub_atom = b.

/* atom_split(X, Y, Z) */
runner:ref(atom_split, 3, extra_structure, 'XLOG 1.4.3').
runner:case(atom_split, 3, extra_structure, 'XLOG 1.4.3, XLOG 1') :-
   catch(atom_split(_, ',', _), error(E,_), true),
   E == instantiation_error.
runner:case(atom_split, 3, extra_structure, 'XLOG 1.4.3, XLOG 2') :-
   atom_split('a,b,c', ',', X),
   X == [a,b,c].
runner:case(atom_split, 3, extra_structure, 'XLOG 1.4.3, XLOG 3') :-
   atom_split(X, ',', [a,b,c]),
   X == 'a,b,c'.
runner:case(atom_split, 3, extra_structure, 'XLOG 1.4.3, XLOG 4') :-
   \+ atom_split('a,d,c', ',', [a,b,c]).
runner:case(atom_split, 3, extra_structure, 'XLOG 1.4.3, XLOG 5') :-
   atom_split('a,b,c', ',', [a,b,c]).
runner:case(atom_split, 3, extra_structure, 'XLOG 1.4.3, XLOG 6') :-
   catch(atom_split(_, ',', foo), error(E,_), true),
   E == type_error(list,foo).

/* atom_numnber(X, Y) */
runner:ref(atom_number, 2, extra_structure, 'XLOG 1.4.4').
runner:case(atom_number, 2, extra_structure, 'XLOG 1.4.4, XLOG 1') :-
   atom_number(X, 33),
   X == '33'.
runner:case(atom_number, 2, extra_structure, 'XLOG 1.4.4, XLOG 2') :-
   atom_number('33.0', X),
   X == 33.0.
runner:case(atom_number, 2, extra_structure, 'XLOG 1.4.4, XLOG 3') :-
   atom_number('3.3e+01', 33.0).
runner:case(atom_number, 2, extra_structure, 'XLOG 1.4.4, XLOG 4') :-
   atom_number('0xf', X),
   X == 15.
runner:case(atom_number, 2, extra_structure, 'XLOG 1.4.4, XLOG 5') :-
   catch(atom_number(_, foo), error(E,_), true),
   E == type_error(number,foo).
runner:case(atom_number, 2, extra_structure, 'XLOG 1.4.4, XLOG 6') :-
   catch(atom_number(_, _), error(E,_), true),
   E == instantiation_error.

/****************************************************************/
/* term.p extras                                                */
/****************************************************************/

/* set_arg(K, X, Y, Z) */
/* derived from arg/3 test cases. */
runner:ref(set_arg, 4, extra_structure, 'XLOG 1.5.1').
runner:case(set_arg, 4, extra_structure, 'XLOG 1.5.1, XLOG 1') :-
   set_arg(1, foo(a,b), c, foo(c,b)).
runner:case(set_arg, 4, extra_structure, 'XLOG 1.5.1, XLOG 2') :-
   set_arg(1, foo(a,b), c, X),
   X == foo(c,b).
runner:case(set_arg, 4, extra_structure, 'XLOG 1.5.1, XLOG 3') :-
   set_arg(1, foo(a,b), c, foo(X,b)),
   X == c.
runner:case(set_arg, 4, extra_structure, 'XLOG 1.5.1, XLOG 4') :-
   set_arg(1, foo(_,b), Y, Z),
   Z == foo(Y,b).
runner:case(set_arg, 4, extra_structure, 'XLOG 1.5.1, XLOG 5') :-
   \+ set_arg(1, foo(a,b), c, f(b,_)).
runner:case(set_arg, 4, extra_structure, 'XLOG 1.5.1, XLOG 6') :-
   \+ set_arg(0, foo(a,b), baz, _).
runner:case(set_arg, 4, extra_structure, 'XLOG 1.5.1, XLOG 7') :-
   \+ set_arg(3, foo(3,4), _, _).
runner:case(set_arg, 4, extra_structure, 'XLOG 1.5.1, XLOG 8') :-
   catch(set_arg(_, foo(a,b), c, _), error(E,_), true),
   nonvar(E),
   E = instantiation_error.
runner:case(set_arg, 4, extra_structure, 'XLOG 1.5.1, XLOG 9') :-
   catch(set_arg(1, _, c, _), error(E,_), true),
   nonvar(E),
   E = instantiation_error.
runner:case(set_arg, 4, extra_structure, 'XLOG 1.5.1, XLOG 10') :-
   catch(set_arg(0, atom, c, _), error(E,_), true),
   nonvar(E),
   E = type_error(compound,atom).
runner:case(set_arg, 4, extra_structure, 'XLOG 1.5.1, XLOG 11') :-
   catch(set_arg(0, 3, c, _), error(E,_), true),
   nonvar(E),
   E = type_error(_,3).

/****************************************************************/
/* moddiv.p extras                                              */
/****************************************************************/

/* integer(X)  */

runner:ref(integer, -2, extra_structure, 'XLOG 1.6.1').
runner:case(integer, -2, extra_structure, 'XLOG 1.6.1, XLOG 1') :-
   0 is integer(-0.5).
runner:case(integer, -2, extra_structure, 'XLOG 1.6.1, XLOG 2') :-
   7 is integer(7.6).
runner:case(integer, -2, extra_structure, 'XLOG 1.6.1, XLOG 3') :-
   catch(_ is integer(foobar), error(E,_), true),
   E == type_error(evaluable,foobar/0).
