/**
 * Prolog code for the extend grammar theory test cases.
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
:- use_module(library(standard/dcg)).

/****************************************************************/
/* Grammar Rules                                                */
/****************************************************************/

/* terminals (grammer), WG17 DCGD 7.14.2 */

runner:ref('.', 4, extend_grammar, 'WG17 DCGD 7.14.2').
runner:case('.', 4, extend_grammar, 'WG17 DCGD 7.14.2, XLOG 1') :-
   phrase([], [abc,xyz], X),
   X == [abc,xyz].
runner:case('.', 4, extend_grammar, 'WG17 DCGD 7.14.2, XLOG 2') :-
   \+ phrase([def], [abc,xyz], _).
runner:case('.', 4, extend_grammar, 'WG17 DCGD 7.14.2, XLOG 3') :-
   phrase([abc], [abc,xyz], X),
   X == [xyz].
runner:case('.', 4, extend_grammar, 'WG17 DCGD 7.14.2, XLOG 4') :-
   phrase([3.2, {}, a(b)], X, []),
   X == [3.2,{},a(b)].
runner:case('.', 4, extend_grammar, 'WG17 DCGD 7.14.2, XLOG 5') :-
   phrase([X], [abc,xyz], _),
   X == abc.
runner:case('.', 4, extend_grammar, 'WG17 DCGD 7.14.2, XLOG 6') :-
   phrase("abc", X, []),
   X == "abc".

/* non-terminals (grammar), WG17 DCGD 7.13.1 */

p -->
   [abc].
q(X) --> p,
   [X].
r --> [].
r -->
   [abc].

runner:ref(-->, 2, extend_grammar, 'WG17 DCGD 7.13.1').
runner:case(-->, 2, extend_grammar, 'WG17 DCGD 7.13.1, XLOG 1') :-
   p([abc,xyz], X),
   X == [xyz].
runner:case(-->, 2, extend_grammar, 'WG17 DCGD 7.13.1, XLOG 2') :-
   p([abc,xyz], [xyz]).
runner:case(-->, 2, extend_grammar, 'WG17 DCGD 7.13.1, XLOG 3') :-
   \+ p([abc,xyz], []).
runner:case(-->, 2, extend_grammar, 'WG17 DCGD 7.13.1, XLOG 4') :-
   q(X, [abc,xyz], []),
   X == xyz.
runner:case(-->, 2, extend_grammar, 'WG17 DCGD 7.13.1, XLOG 5') :-
   r([abc], X),
   X == [abc].
runner:case(-->, 2, extend_grammar, 'WG17 DCGD 7.13.1, XLOG 6') :-
   findall(X, r([abc], X), [_,X|_]),
   X == [].

/* A, B (gammar), WG17 DCGD 7.14.3 */

s -->
   [abc],
   {write(hello)}.
t(X) --> r,
   [X].

runner:ref(',', 4, extend_grammar, 'WG17 DCGD 7.14.3').
runner:case(',', 4, extend_grammar, 'WG17 DCGD 7.14.3, XLOG 1') :-
   phrase((  [], p), [abc], []).
runner:case(',', 4, extend_grammar, 'WG17 DCGD 7.14.3, XLOG 2') :-
   \+ phrase((  fail, p), [abc], []).
runner:case(',', 4, extend_grammar, 'WG17 DCGD 7.14.3, XLOG 3') :-
   with_output_to(atom(Y),
      \+ s([xyz], [])), !,
   Y == ''.
runner:case(',', 4, extend_grammar, 'WG17 DCGD 7.14.3, XLOG 4') :-
   with_output_to(atom(Y),
      (  s([abc,xyz], X),
         X == [xyz])), !,
   Y == hello.
runner:case(',', 4, extend_grammar, 'WG17 DCGD 7.14.3, XLOG 5') :-
   with_output_to(atom(Y),
      s([abc,xyz], [xyz])), !,
   Y == hello.
runner:case(',', 4, extend_grammar, 'WG17 DCGD 7.14.3, XLOG 6') :-
   with_output_to(atom(Y),
      \+ s([abc,xyz], [])), !,
   Y == hello.
runner:case(',', 4, extend_grammar, 'WG17 DCGD 7.14.3, XLOG 7') :-
   t(X, [abc,xyz,abc,xyz], Y),
   t(X, Y, _),
   X == xyz.

/* A ; B (grammar), WG17 DCGD 7.14.4 */

runner:ref(;, 4, extend_grammar, 'WG17 DCGD 7.14.4').
runner:case(;, 4, extend_grammar, 'WG17 DCGD 7.14.4, XLOG 1') :-
   phrase((  []
          ;  [abc]), [abc], X),
   X == [abc].
runner:case(;, 4, extend_grammar, 'WG17 DCGD 7.14.4, XLOG 2') :-
   findall(X, phrase((  []
                     ;  [abc]), [abc], X), [_,X|_]),
   X == [].
runner:case(;, 4, extend_grammar, 'WG17 DCGD 7.14.4, XLOG 3') :-
   phrase((  []; p), [abc], X),
   X == [abc].
runner:case(;, 4, extend_grammar, 'WG17 DCGD 7.14.4, XLOG 4') :-
   findall(X, phrase((  []; p), [abc], X), [_,X|_]),
   X == [].
runner:case(;, 4, extend_grammar, 'WG17 DCGD 7.14.4, XLOG 5') :-
   phrase((  [xyz]; p), X, []),
   X == [xyz].
runner:case(;, 4, extend_grammar, 'WG17 DCGD 7.14.4, XLOG 6') :-
   findall(X, phrase((  [xyz]; p), X, []), [_,X|_]),
   X == [abc].
runner:case(;, 4, extend_grammar, 'WG17 DCGD 7.14.4, XLOG 7') :-
   with_output_to(atom(Y), phrase((  s; []), [xyz], [xyz])), !,
   Y == ''.
runner:case(;, 4, extend_grammar, 'WG17 DCGD 7.14.4, XLOG 8') :-
   with_output_to(atom(Y), phrase((  s; []), [abc,xyz], [abc,xyz])), !,
   Y == hello.

/* A -> B; C (grammar), WG17 DCGD 7.14.12 */

runner:ref(->, 4, extend_grammar, 'WG17 DCGD 7.14.12').
runner:case(->, 4, extend_grammar, 'WG17 DCGD 7.14.12, XLOG 1') :-
   phrase((  p -> []; []), [abc], X),
   X == [].
runner:case(->, 4, extend_grammar, 'WG17 DCGD 7.14.12, XLOG 2') :-
   \+ findall(X, phrase((  p -> []; []), [abc], X), [_,X|_]).
runner:case(->, 4, extend_grammar, 'WG17 DCGD 7.14.12, XLOG 3') :-
   phrase((  t(X),
             t(X) -> []; []), [abc,xyz,abc,xyz], _),
   X == xyz.
runner:case(->, 4, extend_grammar, 'WG17 DCGD 7.14.12, XLOG 4') :-
   \+ phrase((  t(X)
             -> t(X); []), [abc,xyz,abc,xyz], _).
runner:case(->, 4, extend_grammar, 'WG17 DCGD 7.14.12, XLOG 5') :-
   phrase((  []
          -> t(X),
             t(X); []), [abc,xyz,abc,xyz], _),
   X == xyz.
runner:case(->, 4, extend_grammar, 'WG17 DCGD 7.14.12, XLOG 6') :-
   phrase((  [xyz] -> []
          ;  t(X),
             t(X)), [abc,xyz,abc,xyz], _),
   X == xyz.

/* \+ A (grammar), WG17 DCGD 7.14.11 */

runner:ref(\+, 3, extend_grammar, 'WG17 DCGD 7.14.11').
runner:case(\+, 3, extend_grammar, 'WG17 DCGD 7.14.11, XLOG 1') :-
   phrase((  r,
             \+ fail), [abc], X),
   X == [abc].
runner:case(\+, 3, extend_grammar, 'WG17 DCGD 7.14.11, XLOG 2') :-
   findall(X, phrase((  r,
                        \+ fail), [abc], X), [_,X|_]),
   X == [].
runner:case(\+, 3, extend_grammar, 'WG17 DCGD 7.14.11, XLOG 3') :-
   \+ phrase((  r,
                \+ p), [abc,abc], _).
runner:case(\+, 3, extend_grammar, 'WG17 DCGD 7.14.11, XLOG 4') :-
   phrase((  r,
             \+ p), [abc,xyz], X),
   X == [xyz].
runner:case(\+, 3, extend_grammar, 'WG17 DCGD 7.14.11, XLOG 5') :-
   \+ findall(X, phrase((  r,
                           \+ p), [abc,xyz], X), [_,X|_]).
runner:case(\+, 3, extend_grammar, 'WG17 DCGD 7.14.11, XLOG 6') :-
   \+ phrase((  r,
                \+ (  t(X),
                      t(X))), [abc,abc,xyz,abc,xyz], _).
runner:case(\+, 3, extend_grammar, 'WG17 DCGD 7.14.11, XLOG 7') :-
   phrase((  r,
             \+ (  t(X),
                   t(X))), [abc,abc,xyz,abc,abc], Y),
   Y == [abc,xyz,abc,abc].
runner:case(\+, 3, extend_grammar, 'WG17 DCGD 7.14.11, XLOG 8') :-
   \+ findall(Y, phrase((  r,
                           \+ (  t(X),
                                 t(X))), [abc,abc,xyz,abc,abc], Y), [_,Y|_]).
runner:case(\+, 3, extend_grammar, 'WG17 DCGD 7.14.11, XLOG 9') :-
   phrase((  r,
             \+ (  t(X),
                   t(X))), [abc,abc,xyz,abc,abc], _),
   var(X).

/* ! (grammar), WG17 DCGD 7.14.10 */

runner:ref(!, 2, extend_grammar, 'WG17 DCGD 7.14.10').
runner:case(!, 2, extend_grammar, 'WG17 DCGD 7.14.10, XLOG 1') :-
   phrase((  r, !, p), [abc], X),
   X == [].
runner:case(!, 2, extend_grammar, 'WG17 DCGD 7.14.10, XLOG 2') :-
   \+ findall(X, phrase((  r, !, p), [abc], X), [_,X|_]).
runner:case(!, 2, extend_grammar, 'WG17 DCGD 7.14.10, XLOG 3') :-
   \+ phrase((  t(X), !,
                t(X)), [abc,xyz,abc,xyz], _).
runner:case(!, 2, extend_grammar, 'WG17 DCGD 7.14.10, XLOG 4') :-
   phrase((  p -> r, !, p; []), [abc,abc], X),
   X == [].
runner:case(!, 2, extend_grammar, 'WG17 DCGD 7.14.10, XLOG 5') :-
   \+ findall(X, phrase((  p -> r, !, p; []), [abc,abc], X), [_,X|_]).
runner:case(!, 2, extend_grammar, 'WG17 DCGD 7.14.10, XLOG 6') :-
   \+ phrase((  p
             -> t(X), !,
                t(X); []), [abc,abc,xyz,abc,xyz], _).
runner:case(!, 2, extend_grammar, 'WG17 DCGD 7.14.10, XLOG 7') :-
   phrase((  r,
             (  !; p)), [abc], X),
   X == [abc].
runner:case(!, 2, extend_grammar, 'WG17 DCGD 7.14.10, XLOG 8') :-
   \+ findall(X, phrase((  r,
                           (  !; p)), [abc], X), [_,X|_]).

/* aux (grammar), WG17 DCGD 7.14.7 */

runner:ref({}, 3, extend_grammar, 'WG17 DCGD 7.14.7').
runner:case({}, 3, extend_grammar, 'WG17 DCGD 7.14.7, XLOG 1') :-
   phrase((  p,
             {1 < 2}, p), [abc,abc], X),
   X == [].
runner:case({}, 3, extend_grammar, 'WG17 DCGD 7.14.7, XLOG 2') :-
   \+ phrase((  p,
                {1 > 2}, p), [abc,abc], _).
runner:case({}, 3, extend_grammar, 'WG17 DCGD 7.14.7, XLOG 3') :-
   phrase((  t(X),
             {X == xyz}, p), [abc,xyz,abc], Y),
   Y == [].
runner:case({}, 3, extend_grammar, 'WG17 DCGD 7.14.7, XLOG 4') :-
   \+ phrase((  t(X),
                {X == abc}, p), [abc,xyz,abc], _).
runner:case({}, 3, extend_grammar, 'WG17 DCGD 7.14.7, XLOG 5') :-
   with_output_to(atom(X),
      phrase((  p,
                {write(hello)
              ;  write(world)}), [abc], [])), !,
   X == hello.
runner:case({}, 3, extend_grammar, 'WG17 DCGD 7.14.7, XLOG 6') :-
   findall(X, with_output_to(atom(X),
                 phrase((  p,
                           {write(hello)
                         ;  write(world)}), [abc], [])), [_,X|_]),
   X == world.
