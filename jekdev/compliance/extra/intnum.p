/**
 * Prolog code for the extra integer number test cases.
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

/****************************************************************/
/* bits.p extras                                                */
/****************************************************************/

/* gcd(X, Y) */

runner:ref(gcd, -3, extra_intnum, 'XLOG 5.1.1').
runner:case(gcd, -3, extra_intnum, 'XLOG 5.1.1, XLOG 1') :-
   X is gcd(12, 18), X == 6.
runner:case(gcd, -3, extra_intnum, 'XLOG 5.1.1, XLOG 2') :-
   X is gcd(-12, 18), X == 6.
runner:case(gcd, -3, extra_intnum, 'XLOG 5.1.1, XLOG 3') :-
   X is gcd(12*2^100, -18*2^100), X =:= 6*2^100.
runner:case(gcd, -3, extra_intnum, 'XLOG 5.1.1, XLOG 4') :-
   X is gcd(-12*2^100, -18*2^100), X =:= 6*2^100.
runner:case(gcd, -3, extra_intnum, 'XLOG 5.1.1, XLOG 5') :-
   X is gcd(3, 0), X == 3.
runner:case(gcd, -3, extra_intnum, 'XLOG 5.1.1, XLOG 6') :-
   X is gcd(0, 3*2^100), X =:= 3*2^100.

/* lcm(X, Y) */

runner:ref(lcm, -3, extra_intnum, 'XLOG 5.1.2').
runner:case(lcm, -3, extra_intnum, 'XLOG 5.1.2, XLOG 1') :-
   X is lcm(12, 18), X == 36.
runner:case(lcm, -3, extra_intnum, 'XLOG 5.1.2, XLOG 2') :-
   X is lcm(12, -18), X == -36.
runner:case(lcm, -3, extra_intnum, 'XLOG 5.1.2, XLOG 3') :-
   X is lcm(-12*2^100, 18*2^100), X =:= -36*2^100.
runner:case(lcm, -3, extra_intnum, 'XLOG 5.1.2, XLOG 4') :-
   X is lcm(-12*2^100, -18*2^100), X =:= 36*2^100.
runner:case(lcm, -3, extra_intnum, 'XLOG 5.1.2, XLOG 5') :-
   X is lcm(0, 3), X == 0.
runner:case(lcm, -3, extra_intnum, 'XLOG 5.1.2, XLOG 6') :-
   X is lcm(3*2^100, 0), X == 0.

/* msb(X) */

runner:ref(msb, -2, extra_intnum, 'XLOG 5.1.3').
runner:case(msb, -2, extra_intnum, 'XLOG 5.1.3, XLOG 1') :-
   X is msb(12), X == 3.
runner:case(msb, -2, extra_intnum, 'XLOG 5.1.3, XLOG 2') :-
   X is msb(- 12), X == 3.
runner:case(msb, -2, extra_intnum, 'XLOG 5.1.3, XLOG 3') :-
   X is msb(12*2^100), X == 103.
runner:case(msb, -2, extra_intnum, 'XLOG 5.1.3, XLOG 4') :-
   X is msb(- 12*2^100), X == 103.
runner:case(msb, -2, extra_intnum, 'XLOG 5.1.3, XLOG 5') :-
   X is msb(0), X == -1.
runner:case(msb, -2, extra_intnum, 'XLOG 5.1.3, XLOG 6') :-
   X is msb(- 128), X == 6.
runner:case(msb, -2, extra_intnum, 'XLOG 5.1.3, XLOG 7') :-
   X is msb(- 2^53), X == 52.

/* lsb(X) */

runner:ref(lsb, -2, extra_intnum, 'XLOG 5.1.4').
runner:case(lsb, -2, extra_intnum, 'XLOG 5.1.4, XLOG 1') :-
   X is lsb(12), X == 2.
runner:case(lsb, -2, extra_intnum, 'XLOG 5.1.4, XLOG 2') :-
   X is lsb(-12), X == 2.
runner:case(lsb, -2, extra_intnum, 'XLOG 5.1.4, XLOG 3') :-
   X is lsb(12*2^100), X == 102.
runner:case(lsb, -2, extra_intnum, 'XLOG 5.1.4, XLOG 4') :-
   X is lsb(-12*2^100), X == 102.

/* popcount(X) */

runner:ref(popcount, -2, extra_intnum, 'XLOG 5.1.5').
runner:case(popcount, -2, extra_intnum, 'XLOG 5.1.5, XLOG 1') :-
   X is popcount(12), X == 2.
runner:case(popcount, -2, extra_intnum, 'XLOG 5.1.5, XLOG 2') :-
   X is popcount(-12), X == 3.
runner:case(popcount, -2, extra_intnum, 'XLOG 5.1.5, XLOG 3') :-
   X is popcount(11*2^100), X == 3.
runner:case(popcount, -2, extra_intnum, 'XLOG 5.1.5, XLOG 4') :-
   X is popcount(-12*2^100), X == 103.

/****************************************************************/
/* round.p extras                                               */
/****************************************************************/

/* integer(X)  */

runner:ref(integer, -2, extra_intnum, 'XLOG 5.2.1').
runner:case(integer, -2, extra_intnum, 'XLOG 5.2.1, XLOG 1') :-
   0 is integer(-0.5).
runner:case(integer, -2, extra_intnum, 'XLOG 5.2.1, XLOG 2') :-
   7 is integer(7.6).
runner:case(integer, -2, extra_intnum, 'XLOG 5.2.1, XLOG 3') :-
   catch(_ is integer(foobar), error(E, _), true),
   E == type_error(evaluable, foobar/0).
runner:case(integer, -2, extra_intnum, 'XLOG 5.2.1, XLOG 4') :-
   300000000000000008388608 is integer(3.0E23).

/* divmod(X,Y,D,M) */

runner:ref(divmod, 4, extra_intnum, 'XLOG 5.2.2').
runner:case(divmod, 4, extra_intnum, 'XLOG 5.2.2, XLOG 1') :-
   divmod(7, 35, D, M), D == 0, M == 7.
runner:case(divmod, 4, extra_intnum, 'XLOG 5.2.2, XLOG 2') :-
   divmod(140, 14, D, M), D == 10, M == 0.
runner:case(divmod, 4, extra_intnum, 'XLOG 5.2.2, XLOG 3') :-
   divmod(7, -2, D, M), D == -4, M == -1.
runner:case(divmod, 4, extra_intnum, 'XLOG 5.2.2, XLOG 4') :-
   divmod(-5, 2, D, M), D == -3, M == 1.
runner:case(divmod, 4, extra_intnum, 'XLOG 5.2.2, XLOG 5') :-
   divmod(-15211807202738, -1394415660251, D, M),
   D == 10, M == -1267650600228.
