/**
 * Prolog code for the lambda-DCG with attributes example.
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

:- use_module(library(experiment/abstract)).
:- use_module(library(standard/dcg)).

:- meta_predicate repetition(3,?,?,?).
repetition(G, [X|Y]) -->
   call(G, X),
   repetition(G, Y).
repetition(_, []) --> [].

fruit(apple) --> "apple".
fruit(orange) --> "orange".
fruit(pear) --> "pear".

fruits([X|Y]) -->
   fruit(X),
   repetition(Z \ (  ",",
                     fruit(Z)), Y).

% ?- phrase(fruits(L),"appleorange").
% No

% ?- phrase(fruits(L),"apple,orange,apple").
% L = [apple, orange, apple]

% ?- phrase(fruits([apple, orange]),X), atom_codes(A,X).
% A = 'apple,orange',
% X = [97, 112, 112, 108, 101, 44, 111, 114, 97, 110, 103, 101]
