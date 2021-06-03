/**
 * Prolog code for the aggregate test cases.
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

:- use_module(library(advanced/aggregate)).
:- use_module(library(standard/arith)).
:- use_module(library(basic/lists)).
:- use_module(library(arithmetic/ratio)).
:- use_module(library(standard/approx)).

/****************************************************************/
/* bags.p extras                                                */
/****************************************************************/

/* type(hash) */

/* bagof(T, A1^...^An^G, L, O) */
/* derived from bagof/3 test cases, type(hash) preserves input order */
runner:ref(bagof_hash, 4, extend_struct, 'XLOG 2.4.1').
runner:case(bagof_hash, 4, extend_struct, 'XLOG 2.4.1, XLOG 1a') :-
   findall(Y-S, bagof(X, ((Y = 2; Y = 1; Y = 2), (X = 3; X = 1; X = 2)), S, [type(hash)]), [R|_]),
   R == 2-[3, 1, 2, 3, 1, 2].
runner:case(bagof_hash, 4, extend_struct, 'XLOG 2.4.1, XLOG 1b') :-
   findall(Y-S, bagof(X, ((Y = 2; Y = 1; Y = 2), (X = 3; X = 1; X = 2)), S, [type(hash)]), [_, R|_]),
   R == 1-[3, 1, 2].
runner:case(bagof_hash, 4, extend_struct, 'XLOG 2.4.1, XLOG 1c') :-
   findall(Y-S, bagof(X, ((Y = 2; Y = 1; Y = 2), (X = 3; X = 1; X = 2)), S, [type(hash)]), [_, _]).
runner:case(bagof_hash, 4, extend_struct, 'XLOG 2.4.1, XLOG 2a') :-
   findall((Y-S, A-B), bagof(X, ((Y = f(A); Y = B; Y = f(A)),
      (X = 3; X = 1; X = 2)), S, [type(hash)]), [(R, A-B)|_]),
   R = f(A)-[3, 1, 2, 3, 1, 2].
runner:case(bagof_hash, 4, extend_struct, 'XLOG 2.4.1, XLOG 2b') :-
   findall((Y-S, A-B), bagof(X, ((Y = f(A); Y = B; Y = f(A)),
      (X = 3; X = 1; X = 2)), S, [type(hash)]), [_, (R, A-B)|_]),
   R = B-[3, 1, 2].
runner:case(bagof_hash, 4, extend_struct, 'XLOG 2.4.1, XLOG 2c') :-
   findall((Y-S, A-B), bagof(X, ((Y = f(A); Y = B; Y = f(A)),
      (X = 3; X = 1; X = 2)), S, [type(hash)]), [_, _]).

/* setof(T, A1^...^An^G, L, O) */
/* derived from bagof/3 test cases, type(hash) preserves input order */
runner:ref(setof_hash, 4, extend_struct, 'XLOG 2.4.2').
runner:case(setof_hash, 4, extend_struct, 'XLOG 2.4.2, XLOG 1a') :-
   findall(Y-S, setof(X, ((Y = 2; Y = 1; Y = 2), (X = 3; X = 1; X = 2)), S, [type(hash)]), [R|_]),
   R == 2-[1, 2, 3].
runner:case(setof_hash, 4, extend_struct, 'XLOG 2.4.2, XLOG 1b') :-
   findall(Y-S, setof(X, ((Y = 2; Y = 1; Y = 2), (X = 3; X = 1; X = 2)), S, [type(hash)]), [_, R|_]),
   R == 1-[1, 2, 3].
runner:case(setof_hash, 4, extend_struct, 'XLOG 2.4.2, XLOG 1c') :-
   findall(Y-S, setof(X, ((Y = 2; Y = 1; Y = 2), (X = 3; X = 1; X = 2)), S, [type(hash)]), [_, _]).
runner:case(setof_hash, 4, extend_struct, 'XLOG 2.4.2, XLOG 2a') :-
   findall((Y-S, A-B), setof(X, ((Y = f(A); Y = B; Y = f(A)),
      (X = 3; X = 1; X = 2)), S, [type(hash)]), [(R, A-B)|_]),
   R = f(A)-[1, 2, 3].
runner:case(setof_hash, 4, extend_struct, 'XLOG 2.4.2, XLOG 2b') :-
   findall((Y-S, A-B), setof(X, ((Y = f(A); Y = B; Y = f(A)),
      (X = 3; X = 1; X = 2)), S, [type(hash)]), [_, (R, A-B)|_]),
   R = B-[1, 2, 3].
runner:case(setof_hash, 4, extend_struct, 'XLOG 2.4.2, XLOG 2c') :-
   findall((Y-S, A-B), setof(X, ((Y = f(A); Y = B; Y = f(A)),
      (X = 3; X = 1; X = 2)), S, [type(hash)]), [_, _]).

/* type(tree) and type(collator) */

/* bagof(T, A1^...^An^G, L, O), with tree or collator */
/* we use locale, ignore case and reverse */
/* cannot yet test ignore case, collator key not yet available */
runner:ref(bagof_tree, 4, extend_struct, 'XLOG 2.4.3').
runner:case(bagof_tree, 4, extend_struct, 'XLOG 2.4.3, XLOG 1') :-
   findall(Y-S, bagof(X, (member(Y, [a, 'A', £, '0']),
      (X = 3; X = 1)), S), R),
   R == ['0'-[3, 1], 'A'-[3, 1], a-[3, 1], £ -[3, 1]].
runner:case(bagof_tree, 4, extend_struct, 'XLOG 2.4.3, XLOG 2') :-
   findall(Y-S, bagof(X, (member(Y, [a, 'A', £, '0']),
      (X = 3; X = 1)), S, [type(collator), locale(en_GB)]), R),
   R == [£ -[3, 1], '0'-[3, 1], a-[3, 1], 'A'-[3, 1]].
runner:case(bagof_tree, 4, extend_struct, 'XLOG 2.4.3, XLOG 3') :-
   findall(Y-S, bagof(X, (member(Y, [a, 'A', £, '0']),
      (X = 3; X = 1)), S, [reverse(true)]), R),
   R == [£ -[3, 1], a-[3, 1], 'A'-[3, 1], '0'-[3, 1]].
runner:case(bagof_tree, 4, extend_struct, 'XLOG 2.4.3, XLOG 4') :-
   findall(Y-S, bagof(X, (member(Y, [a, 'A', £, '0']),
      (X = 3; X = 1)), S, [type(collator), locale(en_GB), reverse(true)]), R),
   R == ['A'-[3, 1], a-[3, 1], '0'-[3, 1], £ -[3, 1]].

/* setof(T, A1^...^An^G, L, O), with tree or collator */
/* we use locale, ignore case and reverse */
/* cannot yet test ignore case, collator key not yet available */
runner:ref(setof_tree, 4, extend_struct, 'XLOG 2.4.4').
runner:case(setof_tree, 4, extend_struct, 'XLOG 2.4.4, XLOG 1') :-
   findall(Y-S, setof(X, (member(Y, [a, 'A', £, '0']),
      (X = 3; X = 1)), S), R),
   R == ['0'-[1, 3], 'A'-[1, 3], a-[1, 3], £ -[1, 3]].
runner:case(setof_tree, 4, extend_struct, 'XLOG 2.4.4, XLOG 2') :-
   findall(Y-S, setof(X, (member(Y, [a, 'A', £, '0']),
      (X = 3; X = 1)), S, [type(collator), locale(en_GB)]), R),
   R == [£ -[1, 3], '0'-[1, 3], a-[1, 3], 'A'-[1, 3]].
runner:case(setof_tree, 4, extend_struct, 'XLOG 2.4.4, XLOG 3') :-
   findall(Y-S, setof(X, (member(Y, [a, 'A', £, '0']),
      (X = 3; X = 1)), S, [reverse(true)]), R),
   R == [£ -[1, 3], a-[1, 3], 'A'-[1, 3], '0'-[1, 3]].
runner:case(setof_tree, 4, extend_struct, 'XLOG 2.4.4, XLOG 4') :-
   findall(Y-S, setof(X, (member(Y, [a, 'A', £, '0']),
      (X = 3; X = 1)), S, [type(collator), locale(en_GB), reverse(true)]), R),
   R == ['A'-[1, 3], a-[1, 3], '0'-[1, 3], £ -[1, 3]].

/* type(callback) */

/* bagof(T, A1^...^An^G, L, O), with callback */
/* cannot yet test comparator, wont traverse terms */
runner:ref(bagof_callback, 4, extend_struct, 'XLOG 2.4.5').
runner:case(bagof_callback, 4, extend_struct, 'XLOG 2.4.5, XLOG 1') :-
   findall(Y-S, bagof(X, (member(Y, [2#3, 1#2, 4#11]),
      (X = 3; X = 1; X = 2)), S), R),
   R == [1#2-[3, 1, 2], 2#3-[3, 1, 2], 4#11-[3, 1, 2]].
runner:case(bagof_callback, 4, extend_struct, 'XLOG 2.4.5, XLOG 2') :-
   findall(Y-S, bagof(X, (member(Y, [2#3, 1#2, 4#11]),
      (X = 3; X = 1; X = 2)), S, [type(callback), comparator(number_compare)]), R),
   R == [4#11-[3, 1, 2], 1#2-[3, 1, 2], 2#3-[3, 1, 2]].
runner:case(bagof_callback, 4, extend_struct, 'XLOG 2.4.5, XLOG 3') :-
   findall(Y-S, bagof(X, (member(Y, [2#3, 1#2, 4#11]),
      (X = 3; X = 1; X = 2)), S, [reverse(true)]), R),
   R == [4#11-[3, 1, 2], 2#3-[3, 1, 2], 1#2-[3, 1, 2]].
runner:case(bagof_callback, 4, extend_struct, 'XLOG 2.4.5, XLOG 4') :-
   findall(Y-S, bagof(X, (member(Y, [2#3, 1#2, 4#11]),
      (X = 3; X = 1; X = 2)), S, [type(callback), comparator(number_compare), reverse(true)]), R),
   R == [2#3-[3, 1, 2], 1#2-[3, 1, 2], 4#11-[3, 1, 2]].
runner:case(bagof_callback, 4, extend_struct, 'XLOG 2.4.5, XLOG 5') :-
   catch(bagof(X, (member(_, [a, b]), (X = 3; X = 1; X = 2)), _,
      [type(callback), comparator(_)]), error(E, _), true),
   E == instantiation_error.

/* setof(T, A1^...^An^G, L, O), with callback */
/* cannot yet test comparator, wont traverse terms */
runner:ref(setof_callback, 4, extend_struct, 'XLOG 2.4.6').
runner:case(setof_callback, 4, extend_struct, 'XLOG 2.4.6, XLOG 1') :-
   findall(Y-S, setof(X, (member(Y, [2#3, 1#2, 4#11]),
      (X = 3; X = 1; X = 2)), S), R),
   R == [1#2-[1, 2, 3], 2#3-[1, 2, 3], 4#11-[1, 2, 3]].
runner:case(setof_callback, 4, extend_struct, 'XLOG 2.4.6, XLOG 2') :-
   findall(Y-S, setof(X, (member(Y, [2#3, 1#2, 4#11]),
      (X = 3; X = 1; X = 2)), S, [type(callback), comparator(number_compare)]), R),
   R == [4#11-[1, 2, 3], 1#2-[1, 2, 3], 2#3-[1, 2, 3]].
runner:case(setof_callback, 4, extend_struct, 'XLOG 2.4.6, XLOG 3') :-
   findall(Y-S, setof(X, (member(Y, [2#3, 1#2, 4#11]),
      (X = 3; X = 1; X = 2)), S, [reverse(true)]), R),
   R == [4#11-[1, 2, 3], 2#3-[1, 2, 3], 1#2-[1, 2, 3]].
runner:case(setof_callback, 4, extend_struct, 'XLOG 2.4.6, XLOG 4') :-
   findall(Y-S, setof(X, (member(Y, [2#3, 1#2, 4#11]),
      (X = 3; X = 1; X = 2)), S, [type(callback), comparator(number_compare), reverse(true)]), R),
   R == [2#3-[1, 2, 3], 1#2-[1, 2, 3], 4#11-[1, 2, 3]].
runner:case(setof_callback, 4, extend_struct, 'XLOG 2.4.6, XLOG 5') :-
   catch(setof(X, (member(_, [a, b]), (X = 3; X = 1; X = 2)), _,
      [type(callback), comparator(1)]), error(E, _), true),
   E == type_error(callable, 1).

/****************************************************************/
/* aggregate.p extras                                           */
/****************************************************************/

/* aggregate_all(A, G, S): */

runner:ref(aggregate_all, 3, extend_struct, 'XLOG 2.5.1').
runner:case(aggregate_all, 3, extend_struct, 'XLOG 2.5.1, XLOG 1') :-
   aggregate_all(sum(X), between(1, 10, X), S),
   S == 55.
runner:case(aggregate_all, 3, extend_struct, 'XLOG 2.5.1, XLOG 2') :-
   aggregate_all((sum(X), max(X)), between(1, 10, X), S),
   S == (55, 10).
runner:case(aggregate_all, 3, extend_struct, 'XLOG 2.5.1, XLOG 3') :-
   aggregate_all(sum(1), fail, S),
   S == 0.
runner:case(aggregate_all, 3, extend_struct, 'XLOG 2.5.1, XLOG 4') :-
   catch(aggregate_all(_, between(1, 10, _), _), error(E, _), true),
   E == instantiation_error.
runner:case(aggregate_all, 3, extend_struct, 'XLOG 2.5.1, XLOG 5') :-
   catch(aggregate_all(sum(_), 1, _), error(E, _), true),
   E == type_error(callable, 1).
runner:case(aggregate_all, 3, extend_struct, 'XLOG 2.5.1, XLOG 6') :-
   aggregate_all(first(@<, X), member(X, [goedel, escher, bach]), S),
   S == bach.

/* aggregate(A, G, S): */

runner:ref(aggregate, 3, extend_struct, 'XLOG 2.5.2').
runner:case(aggregate, 3, extend_struct, 'XLOG 2.5.2, XLOG 1a') :-
   findall(Y-S, aggregate(sum(X), ((Y = 2; Y = 1; Y = 2),
      between(1, 3, X)), S), [R|_]),
   R == 1-6.
runner:case(aggregate, 3, extend_struct, 'XLOG 2.5.2, XLOG 1b') :-
   findall(Y-S, aggregate(sum(X), ((Y = 2; Y = 1; Y = 2),
      between(1, 3, X)), S), [_, R|_]),
   R == 2-12.
runner:case(aggregate, 3, extend_struct, 'XLOG 2.5.2, XLOG 1c') :-
   findall(Y-S, aggregate(sum(X), ((Y = 2; Y = 1; Y = 2),
      between(1, 3, X)), S), [_, _]).
runner:case(aggregate, 3, extend_struct, 'XLOG 2.5.2, XLOG 2a') :-
   findall((Y-S, A-B), aggregate((sum(X), max(X)), ((Y = A; Y = B; Y = A),
      between(1, 3, X)), S), [(R, A-B)|_]),
   R == A-(12, 3).
runner:case(aggregate, 3, extend_struct, 'XLOG 2.5.2, XLOG 2b') :-
   findall((Y-S, A-B), aggregate((sum(X), max(X)), ((Y = A; Y = B; Y = A),
      between(1, 3, X)), S), [_, (R, A-B)|_]),
   R == B-(6, 3).
runner:case(aggregate, 3, extend_struct, 'XLOG 2.5.2, XLOG 2c') :-
   findall((Y-S, A-B), aggregate((sum(X), max(X)), ((Y = A; Y = B; Y = A),
      between(1, 3, X)), S), [_, _]).
runner:case(aggregate, 3, extend_struct, 'XLOG 2.5.2, XLOG 3') :-
   \+ aggregate(sum(1), fail, _).
runner:case(aggregate, 3, extend_struct, 'XLOG 2.5.2, XLOG 4') :-
   aggregate(mul(X), between(1, 10, X), S),
   S == 3628800.
runner:case(aggregate, 3, extend_struct, 'XLOG 2.5.2, XLOG 5') :-
   catch(aggregate(_, ((Y = 1; Y = 2), between(1, 10, _)), _), error(E, _), true),
   E == instantiation_error.
runner:case(aggregate, 3, extend_struct, 'XLOG 2.5.2, XLOG 6') :-
   catch(aggregate(sum(_), ((Y = 1; Y = 2), 1), _), error(E, _), true),
   E == type_error(callable, 1).
runner:case(aggregate, 3, extend_struct, 'XLOG 2.5.2, XLOG 7') :-
   aggregate(last(@<, X), member(X, [goedel, escher, bach]), S),
   S == goedel.

/* type(hash) and eager(true) */

/* aggregate_all(A, G, S, O): */
/* derived from aggregate_all/3 test cases, returns results earlier */

runner:ref(aggregate_all, 4, extend_struct, 'XLOG 2.5.3').
runner:case(aggregate_all, 4, extend_struct, 'XLOG 2.5.3, XLOG 1a') :-
   findall(N, aggregate_all(max(X), (X = 1; X = 2; X = 1), N, [type(hash), eager(true)]), [R|_]),
   R == 1.
runner:case(aggregate_all, 4, extend_struct, 'XLOG 2.5.3, XLOG 1b') :-
   findall(N, aggregate_all(max(X), (X = 1; X = 2; X = 1), N, [type(hash), eager(true)]), [_, R|_]),
   R == 2.
runner:case(aggregate_all, 4, extend_struct, 'XLOG 2.5.3, XLOG 1c') :-
   findall(N, aggregate_all(max(X), (X = 1; X = 2; X = 1), N, [type(hash), eager(true)]), [_, _]).

/* aggregate(A, G, S, O): */
/* derived from aggregate/3 test cases, returns results earlier */

runner:ref(aggregate, 4, extend_struct, 'XLOG 2.5.4').
runner:case(aggregate, 4, extend_struct, 'XLOG 2.5.4, XLOG 1a') :-
   findall(Y-N, aggregate(max(X), ((Y = 2; Y = 1), (X = 1; X = Y; X = 1)), N, [type(hash), eager(true)]), [R|_]),
   R == 2-1.
runner:case(aggregate, 4, extend_struct, 'XLOG 2.5.4, XLOG 1b') :-
   findall(Y-N, aggregate(max(X), ((Y = 2; Y = 1), (X = 1; X = Y; X = 1)), N, [type(hash), eager(true)]), [_, R|_]),
   R == 2-2.
runner:case(aggregate, 4, extend_struct, 'XLOG 2.5.4, XLOG 1c') :-
   findall(Y-N, aggregate(max(X), ((Y = 2; Y = 1), (X = 1; X = Y; X = 1)), N, [type(hash), eager(true)]), [_, _, R|_]),
   R == 1-1.
runner:case(aggregate, 4, extend_struct, 'XLOG 2.5.4, XLOG 1d') :-
   findall(Y-N, aggregate(max(X), ((Y = 2; Y = 1), (X = 1; X = Y; X = 1)), N, [type(hash), eager(true)]), [_, _, _]).

/* type(tree) or type(collate) */

runner:ref(aggregate_tree, 4, extend_struct, 'XLOG 2.5.5').
runner:case(aggregate_tree, 4, extend_struct, 'XLOG 2.5.5, XLOG 1') :-
   findall(Y-N, aggregate(max(X), (member(Y, [a, 'A', £, '0']),
      (X = 2; X = 1; X = 3)), N), R),
   R == ['0'-3, 'A'-3, a-3, £ -3].
runner:case(aggregate_tree, 4, extend_struct, 'XLOG 2.5.5, XLOG 2') :-
   findall(Y-N, aggregate(max(X), (member(Y, [a, 'A', £, '0']),
      (X = 2; X = 1; X = 3)), N, [type(collator), locale(en_GB)]), R),
   R == [£ -3, '0'-3, a-3, 'A'-3].
runner:case(aggregate_tree, 4, extend_struct, 'XLOG 2.5.5, XLOG 3') :-
   findall(Y-N, aggregate(max(X), (member(Y, [a, 'A', £, '0']),
      (X = 2; X = 1; X = 3)), N, [reverse(true)]), R),
   R == [£ -3, a-3, 'A'-3, '0'-3].
runner:case(aggregate_tree, 4, extend_struct, 'XLOG 2.5.5, XLOG 4') :-
   findall(Y-N, aggregate(max(X), (member(Y, [a, 'A', £, '0']),
      (X = 2; X = 1; X = 3)), N, [type(collator), locale(en_GB), reverse(true)]), R),
   R == ['A'-3, a-3, '0'-3, £ -3].

/* type(callback) */

runner:ref(aggregate_callback, 4, extend_struct, 'XLOG 2.5.6').
runner:case(aggregate_callback, 4, extend_struct, 'XLOG 2.5.6, XLOG 1') :-
   findall(Y-N, aggregate(max(X), (member(Y, [2#3, 1#2, 4#11]),
      (X = 2; X = 1; X = 3)), N), R),
   R == [1#2-3, 2#3-3, 4#11-3].
runner:case(aggregate_callback, 4, extend_struct, 'XLOG 2.5.6, XLOG 2') :-
   findall(Y-N, aggregate(max(X), (member(Y, [2#3, 1#2, 4#11]),
      (X = 2; X = 1; X = 3)), N, [type(callback), comparator(number_compare)]), R),
   R == [4#11-3, 1#2-3, 2#3-3].
runner:case(aggregate_callback, 4, extend_struct, 'XLOG 2.5.6, XLOG 3') :-
   findall(Y-N, aggregate(max(X), (member(Y, [2#3, 1#2, 4#11]),
      (X = 2; X = 1; X = 3)), N, [reverse(true)]), R),
   R == [4#11-3, 2#3-3, 1#2-3].
runner:case(aggregate_callback, 4, extend_struct, 'XLOG 2.5.6, XLOG 4') :-
   findall(Y-N, aggregate(max(X), (member(Y, [2#3, 1#2, 4#11]),
      (X = 2; X = 1; X = 3)), N, [type(callback), comparator(number_compare), reverse(true)]), R),
   R == [2#3-3, 1#2-3, 4#11-3].
runner:case(aggregate_callback, 4, extend_struct, 'XLOG 2.5.6, XLOG 5') :-
   catch(aggregate(max(X), (member(_, [a, b]), (X = 2; X = 1; X = 3)), _,
      [type(callback), comparator(1)]), error(E, _), true),
   E == type_error(callable, 1).
