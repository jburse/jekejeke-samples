/**
 * Prolog code for the t.b.d. test cases.
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

:- use_module(library(advanced/aggregate)).
:- use_module(library(advanced/arith)).

/* aggregate_all(A, G, S): */

runner:ref(aggregate_all, 3, extend_struct, 'XLOG 5.1').
runner:case(aggregate_all, 3, extend_struct, 'XLOG 5.1, XLOG 1') :-
   aggregate_all(sum(X), between(1, 10, X), S),
   S == 55.
runner:case(aggregate_all, 3, extend_struct, 'XLOG 5.1, XLOG 2') :-
   aggregate_all((sum(X),max(X)), between(1, 10, X), S),
   S == (55,10).
runner:case(aggregate_all, 3, extend_struct, 'XLOG 5.1, XLOG 3') :-
   aggregate_all(sum(1), fail, S),
   S == 0.
runner:case(aggregate_all, 3, extend_struct, 'XLOG 5.1, XLOG 4') :-
   catch(aggregate_all(_, between(1, 10, _), _), error(E,_), true),
   E == instantiation_error.
runner:case(aggregate_all, 3, extend_struct, 'XLOG 5.1, XLOG 5') :-
   catch(aggregate_all(sum(_), 1, _), error(E,_), true),
   E == type_error(callable,1).

/* aggregate(A, G, S): */

runner:ref(aggregate, 3, extend_struct, 'XLOG 5.2').
runner:case(aggregate, 3, extend_struct, 'XLOG 5.2, XLOG 1a') :-
   findall(Y-S, aggregate(sum(X), (  (  Y = 1
                                     ;  Y = 2
                                     ;  Y = 1),
                                     between(1, 3, X)), S), [R|_]),
   R == 1-12.
runner:case(aggregate, 3, extend_struct, 'XLOG 5.2, XLOG 1b') :-
   findall(Y-S, aggregate(sum(X), (  (  Y = 1
                                     ;  Y = 2
                                     ;  Y = 1),
                                     between(1, 3, X)), S), [_,R|_]),
   R == 2-6.
runner:case(aggregate, 3, extend_struct, 'XLOG 5.2, XLOG 1c') :-
   findall(Y-S, aggregate(sum(X), (  (  Y = 1
                                     ;  Y = 2
                                     ;  Y = 1),
                                     between(1, 3, X)), S), [_,_]).
runner:case(aggregate, 3, extend_struct, 'XLOG 5.2, XLOG 2a') :-
   findall((Y-S,A-B), aggregate((sum(X),max(X)), (  (  Y = A
                                                    ;  Y = B
                                                    ;  Y = A),
                                                    between(1, 3, X)), S), [(R,A-B)|_]),
   R == A-(12,3).
runner:case(aggregate, 3, extend_struct, 'XLOG 5.2, XLOG 2b') :-
   findall((Y-S,A-B), aggregate((sum(X),max(X)), (  (  Y = A
                                                    ;  Y = B
                                                    ;  Y = A),
                                                    between(1, 3, X)), S), [_,(R,A-B)|_]),
   R == B-(6,3).
runner:case(aggregate, 3, extend_struct, 'XLOG 5.2, XLOG 2c') :-
   findall((Y-S,A-B), aggregate((sum(X),max(X)), (  (  Y = A
                                                    ;  Y = B
                                                    ;  Y = A),
                                                    between(1, 3, X)), S), [_,_]).
runner:case(aggregate, 3, extend_struct, 'XLOG 5.2, XLOG 3') :-
   \+ aggregate(sum(1), fail, _).
runner:case(aggregate, 3, extend_struct, 'XLOG 5.2, XLOG 4') :-
   catch(aggregate(_, (  (  Y = 1
                         ;  Y = 2),
                         between(1, 10, _)), _), error(E,_), true),
   E == instantiation_error.
runner:case(aggregate, 3, extend_struct, 'XLOG 5.2, XLOG 5') :-
   catch(aggregate(sum(_), (  (  Y = 1
                              ;  Y = 2), 1), _), error(E,_), true),
   E == type_error(callable,1).
