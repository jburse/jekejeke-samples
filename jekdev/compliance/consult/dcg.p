/**
 * Prolog code for the consult dcg theory test cases.
 *
 * Source of test cases is the following standard:
 *   - Draft Definite Clause Grammar Rules, WG17, Jonathan Hodgson
 *     <a href="http://www.complang.tuwien.ac.at/ulrich/iso-prolog/dcgs">www.complang.tuwien.ac.at/ulrich/iso-prolog/dcgs</a>
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

:- use_module(library(testing/charsio)).

/****************************************************************/
/* Grammar Rules                                                */
/****************************************************************/

/* terminals (grammer), WG17 DCGD 12.3 */

:- meta_predicate @(2,?,?).
@(X) --> X.

runner:ref('.', 4, consult_dcg, 'WG17 DCGD 12.3').
runner:case('.', 4, consult_dcg, 1) :-
   @([], [abc,xyz], X),
   X == [abc,xyz].
runner:case('.', 4, consult_dcg, 2) :-
   \+ @([def], [abc,xyz], _).
runner:case('.', 4, consult_dcg, 3) :-
   @([abc], [abc,xyz], X),
   X == [xyz].
runner:case('.', 4, consult_dcg, 4) :-
   @([3.2, {}, a(b)], X, []),
   X == [3.2,{},a(b)].
runner:case('.', 4, consult_dcg, 5) :-
   @([X], [abc,xyz], _),
   X == abc.
runner:case('.', 4, consult_dcg, 6) :-
   @("abc", X, []),
   X == "abc".

/* non-terminals (grammar), WG17 DCGD 12.3 */

p -->
   @([abc]).
q(X) -->
   @((  p,
        [X])).
r -->
   @([]).
r -->
   @([abc]).

runner:ref(-->, 2, consult_dcg, 'WG17 DCGD 12.3').
runner:case(-->, 2, consult_dcg, 1) :-
   p([abc,xyz], X),
   X == [xyz].
runner:case(-->, 2, consult_dcg, 2) :-
   p([abc,xyz], [xyz]).
runner:case(-->, 2, consult_dcg, 3) :-
   \+ p([abc,xyz], []).
runner:case(-->, 2, consult_dcg, 4) :-
   q(X, [abc,xyz], []),
   X == xyz.
runner:case(-->, 2, consult_dcg, 5) :-
   r([abc], X),
   X == [abc].
runner:case(-->, 2, consult_dcg, 6) :-
   findall(X, r([abc], X), [_,X|_]),
   X == [].

/* A, B (gammar), WG17 DCGD 12.3 */

s -->
   @((  [abc],
        {write(hello)})).
t(X) -->
   @((  r,
        [X])).

runner:ref(',', 4, consult_dcg, 'WG17 DCGD 12.3').
runner:case(',', 4, consult_dcg, 1) :-
   @((  [], p), [abc], []).
runner:case(',', 4, consult_dcg, 2) :-
   \+ @((  fail, p), [abc], []).
runner:case(',', 4, consult_dcg, 3) :-
   with_output_to(atom(Y), \+ s([xyz], [])), !,
   Y == ''.
runner:case(',', 4, consult_dcg, 4) :-
   with_output_to(atom(Y), (  s([abc,xyz], X),
                              X == [xyz])), !,
   Y == hello.
runner:case(',', 4, consult_dcg, 5) :-
   with_output_to(atom(Y), s([abc,xyz], [xyz])), !,
   Y == hello.
runner:case(',', 4, consult_dcg, 6) :-
   with_output_to(atom(Y), \+ s([abc,xyz], [])), !,
   Y == hello.
runner:case(',', 4, consult_dcg, 7) :-
   t(X, [abc,xyz,abc,xyz], Y),
   t(X, Y, _),
   X == xyz.

/* A ; B (grammar), WG17 DCGD 12.3 */

runner:ref(;, 4, consult_dcg, 'WG17 DCGD 12.3').
runner:case(;, 4, consult_dcg, 1) :-
   @((  []
     ;  [abc]), [abc], X),
   X == [abc].
runner:case(;, 4, consult_dcg, 2) :-
   findall(X, @((  []
                ;  [abc]), [abc], X), [_,X|_]),
   X == [].
runner:case(;, 4, consult_dcg, 3) :-
   @((  []; p), [abc], X),
   X == [abc].
runner:case(;, 4, consult_dcg, 4) :-
   findall(X, @((  []; p), [abc], X), [_,X|_]),
   X == [].
runner:case(;, 4, consult_dcg, 5) :-
   @((  [xyz]; p), X, []),
   X == [xyz].
runner:case(;, 4, consult_dcg, 6) :-
   findall(X, @((  [xyz]; p), X, []), [_,X|_]),
   X == [abc].
runner:case(;, 4, consult_dcg, 7) :-
   with_output_to(atom(Y), @((  s; []), [xyz], [xyz])), !,
   Y == ''.
runner:case(;, 4, consult_dcg, 8) :-
   with_output_to(atom(Y), @((  s; []), [abc,xyz], [abc,xyz])), !,
   Y == hello.

/* A -> B; C (grammar), WG17 DCGD 12.3 */

runner:ref(->, 4, consult_dcg, 'WG17 DCGD 12.3').
runner:case(->, 4, consult_dcg, 1) :-
   @((  p -> []; []), [abc], X),
   X == [].
runner:case(->, 4, consult_dcg, 2) :-
   \+ findall(X, @((  p -> []; []), [abc], X), [_,X|_]).
runner:case(->, 4, consult_dcg, 3) :-
   @((  t(X),
        t(X) -> []; []), [abc,xyz,abc,xyz], _),
   X == xyz.
runner:case(->, 4, consult_dcg, 4) :-
   \+ @((  t(X)
        -> t(X); []), [abc,xyz,abc,xyz], _).
runner:case(->, 4, consult_dcg, 5) :-
   @((  []
     -> t(X),
        t(X); []), [abc,xyz,abc,xyz], _),
   X == xyz.
runner:case(->, 4, consult_dcg, 6) :-
   @((  [xyz] -> []
     ;  t(X),
        t(X)), [abc,xyz,abc,xyz], _),
   X == xyz.

/* \+ A (grammar), WG17 DCGD 12.3 */

runner:ref(\+, 3, consult_dcg, 'WG17 DCGD 12.3').
runner:case(\+, 3, consult_dcg, 1) :-
   @((  r,
        \+ fail), [abc], X),
   X == [abc].
runner:case(\+, 3, consult_dcg, 2) :-
   findall(X, @((  r,
                   \+ fail), [abc], X), [_,X|_]),
   X == [].
runner:case(\+, 3, consult_dcg, 3) :-
   \+ @((  r,
           \+ p), [abc,abc], _).
runner:case(\+, 3, consult_dcg, 4) :-
   @((  r,
        \+ p), [abc,xyz], X),
   X == [xyz].
runner:case(\+, 3, consult_dcg, 5) :-
   \+ findall(X, @((  r,
                      \+ p), [abc,xyz], X), [_,X|_]).
runner:case(\+, 3, consult_dcg, 6) :-
   \+ @((  r,
           \+ (  t(X),
                 t(X))), [abc,abc,xyz,abc,xyz], _).
runner:case(\+, 3, consult_dcg, 7) :-
   @((  r,
        \+ (  t(X),
              t(X))), [abc,abc,xyz,abc,abc], Y),
   Y == [abc,xyz,abc,abc].
runner:case(\+, 3, consult_dcg, 8) :-
   \+ findall(Y, @((  r,
                      \+ (  t(X),
                            t(X))), [abc,abc,xyz,abc,abc], Y), [_,Y|_]).
runner:case(\+, 3, consult_dcg, 9) :-
   @((  r,
        \+ (  t(X),
              t(X))), [abc,abc,xyz,abc,abc], _),
   var(X).

/* ! (grammar), WG17 DCGD 12.3 */

runner:ref(!, 2, consult_dcg, 'WG17 DCGD 12.3').
runner:case(!, 2, consult_dcg, 1) :-
   @((  r, !, p), [abc], X),
   X == [].
runner:case(!, 2, consult_dcg, 2) :-
   \+ findall(X, @((  r, !, p), [abc], X), [_,X|_]).
runner:case(!, 2, consult_dcg, 3) :-
   \+ @((  t(X), !,
           t(X)), [abc,xyz,abc,xyz], _).
runner:case(!, 2, consult_dcg, 4) :-
   @((  p -> r, !, p; []), [abc,abc], X),
   X == [].
runner:case(!, 2, consult_dcg, 5) :-
   \+ findall(X, @((  p -> r, !, p; []), [abc,abc], X), [_,X|_]).
runner:case(!, 2, consult_dcg, 6) :-
   \+ @((  p
        -> t(X), !,
           t(X); []), [abc,abc,xyz,abc,xyz], _).
runner:case(!, 2, consult_dcg, 7) :-
   @((  r,
        (  !; p)), [abc], X),
   X == [abc].
runner:case(!, 2, consult_dcg, 8) :-
   \+ findall(X, @((  r,
                      (  !; p)), [abc], X), [_,X|_]).

/* aux (grammar), WG17 DCGD 12.3 */

runner:ref({}, 3, consult_dcg, 'WG17 DCGD 12.3').
runner:case({}, 3, consult_dcg, 1) :-
   @((  p,
        {1 < 2}, p), [abc,abc], X),
   X == [].
runner:case({}, 3, consult_dcg, 2) :-
   \+ @((  p,
           {1 > 2}, p), [abc,abc], _).
runner:case({}, 3, consult_dcg, 3) :-
   @((  t(X),
        {X == xyz}, p), [abc,xyz,abc], Y),
   Y == [].
runner:case({}, 3, consult_dcg, 4) :-
   \+ @((  t(X),
           {X == abc}, p), [abc,xyz,abc], _).
runner:case({}, 3, consult_dcg, 5) :-
   with_output_to(atom(X), @((  p,
                                {write(hello)
                              ;  write(world)}), [abc], [])), !,
   X == hello.
runner:case({}, 3, consult_dcg, 6) :-
   findall(X, with_output_to(atom(X), @((  p,
                                           {write(hello)
                                         ;  write(world)}), [abc], [])), [_,X|_]),
   X == world.
