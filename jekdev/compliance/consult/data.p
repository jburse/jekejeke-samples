/**
 * Prolog code for the consult data theory test cases.
 *
 * Source of test cases is the following standard:
 *   - Prolog General Core ISO/IUEC 13211-1
 *   - Draft Technical Corrigendum 2, WG17, Ulrich Neumerkel
 *     <a href="http://www.complang.tuwien.ac.at/ulrich/iso-prolog/dtc2">www.complang.tuwien.ac.at/ulrich/iso-prolog/dtc2</a>
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

:- use_module(library(system/charsio)).

/****************************************************************/
/* Dynamic Database                                             */
/****************************************************************/

/* dynamic(P) */

/* clause(H, B) */

runner:ref(clause, 2, consult_data, 'ISO 8.8.1.4').

:- dynamic cat/0.
cat.

runner:case(clause, 2, consult_data, 'ISO 8.8.1.4, ISO 1') :-
   clause(cat, true).
runner:case(clause, 2, consult_data, 'ISO 8.8.1.4, ISO 2') :-
   clause(dog, true).
runner:case(clause, 2, consult_data, 'ISO 8.8.1.4, ISO 3') :-
   clause(legs(I, 6), Body),
   Body == insect(I).
runner:case(clause, 2, consult_data, 'ISO 8.8.1.4, ISO 4') :-
   clause(legs(C, 7), Body),
   Body == (call(C),call(C)).
runner:case(clause, 2, consult_data, 'ISO 8.8.1.4, ISO 5') :-
   clause(insect(I), T), !,
   I == ant,
   T == true.
runner:case(clause, 2, consult_data, 'ISO 8.8.1.4, ISO 6') :-
   findall(I-T, clause(insect(I), T), [_,I-T|_]),
   I == bee,
   T == true.
runner:case(clause, 2, consult_data, 'ISO 8.8.1.4, ISO 7') :-
   \+ clause(x, _).
runner:case(clause, 2, consult_data, 'ISO 8.8.1.4, ISO 8') :-
   catch(clause(_, _), error(E,_), true),
   E == instantiation_error.
runner:case(clause, 2, consult_data, 'ISO 8.8.1.4, ISO 9') :-
   catch(clause(4, _), error(E,_), true),
   E == type_error(callable,4).
runner:case(clause, 2, consult_data, 'ISO 8.8.1.4, ISO 10') :-
   catch(clause(elk(_), _), error(E,_), true),
   E == permission_error(access,private_procedure,elk/1).
runner:case(clause, 2, consult_data, 'ISO 8.8.1.4, ISO 11') :-
   catch(clause(atom(_), _), error(E,_), true),
   E == permission_error(access,private_procedure,atom/1).
% clause(legs(A,6), insect(f(A)))

/* retract(C) */

runner:ref(retract, 1, consult_data, 'ISO 8.9.3.4').
runner:case(retract, 1, consult_data, 'ISO 8.9.3.4, ISO 1') :-
   assertz(legs(octopus, 8)),
   retract(legs(octopus, 8)).
runner:case(retract, 1, consult_data, 'ISO 8.9.3.4, ISO 2') :-
   \+ retract(legs(spider, 6)).
runner:case(retract, 1, consult_data, 'ISO 8.9.3.4, ISO 3') :-
   assertz((legs(B, 2) :-
              bird(B))),
   retract((legs(X, 2) :- T)),
   T == bird(X).

teardown_olegs :-
   retract((legs(_, _) :- _)), fail; true.
setup_legs :-
   assertz((legs(A, 4) :-
              animal(A))),
   assertz((legs(A, 6) :-
              insect(A))),
   assertz(legs(spider, 8)).
setup_olegs :-
   assertz((legs(A, 6) :-
              insect(A))),
   assertz((legs(A, 7) :- A,
              call(A))).
setup_insect :-
   assertz(insect(ant)),
   assertz(insect(bee)).

runner:case(retract, 1, consult_data, 'ISO 8.9.3.4, ISO 4') :- teardown_olegs, setup_legs,
   findall(X-Y-Z, retract((legs(X, Y) :- Z)), [X-Y-Z|_]),
   Y == 4,
   Z == animal(X).
runner:case(retract, 1, consult_data, 'ISO 8.9.3.4, ISO 5') :- setup_legs,
   findall(X-Y-Z, retract((legs(X, Y) :- Z)), [_,X-Y-Z|_]),
   Y == 6,
   Z == insect(X).
runner:case(retract, 1, consult_data, 'ISO 8.9.3.4, ISO 6') :- setup_legs,
   findall(X-Y-Z, retract((legs(X, Y) :- Z)), [_,_,X-Y-Z|_]),
   X == spider,
   Y == 8,
   Z == true.
runner:case(retract, 1, consult_data, 'ISO 8.9.3.4, ISO 7') :- setup_legs,
   findall(X-Y-Z, retract((legs(X, Y) :- Z)), [_,_,_]).
runner:case(retract, 1, consult_data, 'ISO 8.9.3.4, ISO 8') :-
   \+ retract((legs(_, _) :- _)), setup_olegs.
runner:case(retract, 1, consult_data, 'ISO 8.9.3.4, ISO 9') :-
   with_output_to(atom(X),
      (  retract(insect(I)),
         write(I),
         retract(insect(bee)))), !,
   X == ant, setup_insect.
runner:case(retract, 1, consult_data, 'ISO 8.9.3.4, ISO 10') :-
   findall(X, with_output_to(atom(X),
                 (  retract(insect(I)),
                    write(I),
                    retract(insect(bee)); true)), [_,X|_]),
   X == bee, setup_insect.
runner:case(retract, 1, consult_data, 'ISO 8.9.3.4, ISO 11') :-
   findall(X, with_output_to(atom(X),
                 (  retract(insect(I)),
                    write(I),
                    retract(insect(bee)); true)), [_,_]), setup_insect.
% retract((foo(A) :- A, call(A)))
runner:case(retract, 1, consult_data, 'ISO 8.9.3.4, ISO 12') :-
   assertz((foo(X) :- X
           -> call(X))),
   retract((foo(C) :- A -> B)),
   A == call(C),
   B == call(C).
runner:case(retract, 1, consult_data, 'ISO 8.9.3.4, ISO 13') :-
   catch(retract((_ :-
                    insect(_))), error(E,_), true),
   E == instantiation_error.
runner:case(retract, 1, consult_data, 'ISO 8.9.3.4, ISO 14') :-
   catch(retract((4 :- _)), error(E,_), true),
   E == type_error(callable,4).
runner:case(retract, 1, consult_data, 'ISO 8.9.3.4, ISO 15') :-
   catch(retract((atom(_) :-
                    _ == [])), error(E,_), true),
   E == permission_error(modify,static_procedure,atom/1).

/* asserta(C) */

runner:ref(asserta, 1, consult_data, 'ISO 8.9.1.4').

:- dynamic legs/2.
legs(A, 6) :-
   insect(A).
legs(A, 7) :- A,
   call(A).

runner:case(asserta, 1, consult_data, 'ISO 8.9.1.4, ISO 1') :-
   asserta(legs(octopus, 8)),
   retract(legs(octopus, 8)).
runner:case(asserta, 1, consult_data, 'ISO 8.9.1.4, ISO 2') :-
   asserta((legs(A, 4) :-
              animal(A))),
   retract((legs(A, 4) :-
              animal(A))).
runner:case(asserta, 1, consult_data, 'ISO 8.9.1.4, ISO 3') :-
   asserta((baz(X) :- X,
              call(X))),
   retract((baz(X) :-
              call(X),
              call(X))).
runner:case(asserta, 1, consult_data, 'ISO 8.9.1.4, ISO 4') :-
   catch(asserta(_), error(E,_), true),
   nonvar(E),
   E = instantiation_error.
runner:case(asserta, 1, consult_data, 'ISO 8.9.1.4, ISO 5') :-
   catch(asserta(4), error(E,_), true),
   nonvar(E),
   E = type_error(callable,4).
runner:case(asserta, 1, consult_data, 'ISO 8.9.1.4, ISO 6') :-
   catch((  asserta((foo :- 4)),
            retract((foo :- 4))), error(E,_), true),
   nonvar(E),
   E = type_error(callable,4).
runner:case(asserta, 1, consult_data, 'ISO 8.9.1.4, ISO 7') :-
   catch(asserta((atom(_) :- true)), error(E,_), true),
   nonvar(E),
   E = permission_error(_,_,atom/1).

/* assertz(C) */

runner:ref(assertz, 1, consult_data, 'ISO 8.9.2.4').
runner:case(assertz, 1, consult_data, 'ISO 8.9.2.4, ISO 1') :-
   assertz(legs(spider, 8)),
   retract(legs(spider, 8)).
runner:case(assertz, 1, consult_data, 'ISO 8.9.2.4, ISO 2') :-
   assertz((legs(B, 2) :-
              bird(B))),
   retract((legs(B, 2) :-
              bird(B))).
runner:case(assertz, 1, consult_data, 'ISO 8.9.2.4, ISO 3') :-
   assertz((baz(X) :- X
           -> call(X))),
   retract((baz(X) :-
              call(X)
           -> call(X))).
runner:case(assertz, 1, consult_data, 'ISO 8.9.2.4, ISO 4') :-
   catch(assertz(_), error(E,_), true),
   nonvar(E),
   E = instantiation_error.
runner:case(assertz, 1, consult_data, 'ISO 8.9.2.4, ISO 5') :-
   catch(assertz(4), error(E,_), true),
   nonvar(E),
   E = type_error(callable,4).
runner:case(assertz, 1, consult_data, 'ISO 8.9.2.4, ISO 6') :-
   catch((  assertz((foo :- 4)),
            retract((foo :- 4))), error(E,_), true),
   nonvar(E),
   E = type_error(callable,4).
runner:case(assertz, 1, consult_data, 'ISO 8.9.2.4, ISO 7') :-
   catch(assertz((atom(_) :- true)), error(E,_), true),
   nonvar(E),
   E = permission_error(_,_,atom/1).

/* abolish(P) */

runner:ref(abolish, 1, consult_data, 'ISO 8.9.4.4').

:- multifile foo/2.
:- dynamic foo/2.
foo(a, b).

runner:case(abolish, 1, consult_data, 'ISO 8.9.4.4, ISO 1') :-
   abolish(foo/2),
   catch(foo(_, _), error(E,_), true),
   nonvar(E),
   E = existence_error(procedure,foo/2),
   assertz(foo(a, b)).
runner:case(abolish, 1, consult_data, 'ISO 8.9.4.4, ISO 2') :-
   catch(abolish(foo/_), error(E,_), true),
   nonvar(E),
   E = instantiation_error.
runner:case(abolish, 1, consult_data, 'ISO 8.9.4.4, ISO 3') :-
   catch(abolish(foo), error(E,_), true),
   nonvar(E),
   E = type_error(predicate_indicator,foo).
runner:case(abolish, 1, consult_data, 'ISO 8.9.4.4, ISO 4') :-
   catch(abolish(foo(_)), error(E,_), true),
   nonvar(E),
   E = type_error(predicate_indicator,foo(_)).
runner:case(abolish, 1, consult_data, 'ISO 8.9.4.4, ISO 5') :-
   catch(abolish(abolish/1), error(E,_), true),
   nonvar(E),
   E = permission_error(_,_,abolish/1).

/* retractall(H) */

setup_bee :-
   assertz(insect(bee)).

runner:ref(retractall, 1, consult_data, 'Corr.2 8.9.5.4').
runner:case(retractall, 1, consult_data, 'Corr.2 8.9.5.4, ISO 1') :-
   retractall(insect(bee)),
   \+ insect(bee), setup_bee.
runner:case(retractall, 1, consult_data, 'Corr.2 8.9.5.4, ISO 2') :-
   retractall(insect(_)),
   \+ insect(_), setup_insect.
runner:case(retractall, 1, consult_data, 'Corr.2 8.9.5.4, ISO 3') :-
   retractall(insect(spider)),
   insect(_).
runner:case(retractall, 1, consult_data, 'Corr.2 8.9.5.4, ISO 4') :-
   retractall(mammal(_)).
runner:case(retractall, 1, consult_data, 'Corr.2 8.9.5.4, ISO 5') :-
   catch(retractall(3), error(E,_), true),
   nonvar(E),
   E = type_error(callable,_).
runner:case(retractall, 1, consult_data, 'Corr.2 8.9.5.4, ISO 6') :-
   catch(retractall(retractall(_)), error(E,_), true),
   nonvar(E),
   E = permission_error(modify,_,_).
