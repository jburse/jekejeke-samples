/**
 * Peano Arithmetic Factorial, occurs_check flag.
 *
 * Peano numerals 0’..’ can be represented as s(..s(n)) in Prolog.
 * The Peano arithmetic addition and multiplication can be realized
 * as predicates add/3 and mul/3. Peano arithmetic can prove 1+x ≠ x,
 * but without the occurs check we could derive:
 *
 * ?- add2(s(n),X,X).
 * X = &lt;cyclic term&gt;
 *
 * The occurs checks prevents such a derivation:
 *
 * ?- set_prolog_flag(occurs_check,true).
 * Yes
 *
 * ?- add2(s(n),X,X).
 * No
 *
 * The factorial can be realized by a predicate fac/2.
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

add2(n, X, X).
add2(s(X), Y, s(Z)) :- add2(X, Y, Z).

mul2(n, _, n).
mul2(s(X), Y, Z) :- mul2(X, Y, H), add2(Y, H, Z).

fac2(n, s(n)).
fac2(s(X), Y) :- fac2(X, H), mul2(s(X), H, Y).

peano2 :-
   fac2(s(s(s(s(s(s(s(s(n)))))))), _).
