/**
 * Prolog code for the occurs test cases.
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

:- use_module(library(misc/residue)).
:- use_module(library(term/herbrand)).
:- use_module(library(term/suspend)).

% sto(+Term)
runner:ref(sto, 1, term_delay, 'Term 0.9.3, 1.1').
runner:case(sto, 1, term_delay, 'Term 0.9.3, 1.1, XLOG 1') :-
   call_residue(sto(X), L),
   L == [sto(X)].
runner:case(sto, 1, term_delay, 'Term 0.9.3, 1.1, XLOG 2') :-
   call_residue((  sto(X),
                   X = f(Y,Z,T)), L),
   L == [sto(Y),sto(Z),sto(T)].
runner:case(sto, 1, term_delay, 'Term 0.9.3, 1.1, XLOG 3') :-
   \+ (  sto(X),
         X = f(X)).
runner:case(sto, 1, term_delay, 'Term 0.9.3, 1.1, XLOG 4') :-
   \+ (  sto(X),
         X = g(Y),
         Y = f(Y)).
runner:case(sto, 1, term_delay, 'Term 0.9.3, 1.1, XLOG 5') :-
   call_residue((  sto(_), fail; true), L),
   L == [].
runner:case(sto, 1, term_delay, 'Term 0.9.3, 1.1, XLOG 6a') :-
   findall(L-X-Y, call_residue((  sto(X)
                               ;  sto(Y)), L), R),
   R = [[sto(A)]-B-_|_],
   A == B.
runner:case(sto, 1, term_delay, 'Term 0.9.3, 1.1, XLOG 6b') :-
   findall(L-X-Y, call_residue((  sto(X)
                               ;  sto(Y)), L), R),
   R = [_,[sto(A)]-_-B|_],
   A == B.
runner:case(sto, 1, term_delay, 'Term 0.9.3, 1.1, XLOG 6c') :-
   findall(L-X-Y, call_residue((  sto(X)
                               ;  sto(Y)), L), R),
   R = [_,_].

% neq(+Term, +Term)
runner:ref(neq, 2, term_delay, 'Term 1.0.0, 1.2').
runner:case(neq, 2, term_delay, 'Term 1.0.0, 1.2, SWI7 1') :-
   neq(1, X),
   \+ X = 1.
runner:case(neq, 2, term_delay, 'Term 1.0.0, 1.2, SWI7 2') :-
   neq(1, X),
   neq(X, 2),
   \+ X = 1,
   \+ X = 2.
runner:case(neq, 2, term_delay, 'Term 1.0.0, 1.2, SWI7 3') :-
   neq(X, Y),
   X = 1,
   \+ Y = 1.
runner:case(neq, 2, term_delay, 'Term 1.0.0, 1.2, SWI7 4') :-
   neq(X, Y),
   \+ X = Y.
runner:case(neq, 2, term_delay, 'Term 1.0.0, 1.2, SWI7 5') :-
   neq(X-Z, a-b),
   neq(X-_, b-b),
   X = a,
   \+ Z = b.
runner:case(neq, 2, term_delay, 'Term 1.0.0, 1.2, SWI7 6') :-
   neq(X-Z, a-b),
   neq(X-Y, b-b),
   Y = b,
   Z = b,
   \+ X = a,
   \+ X = b.
