/**
 * Prolog code for the structure lexical theory test cases.
 *
 * Source of test cases are the following standards:
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

/****************************************************************/
/* Lexical Comparison                                           */
/****************************************************************/

/* X == Y */

runner:ref(==, 2, structure_compare, 'ISO 8.4.1.4').
runner:case(==, 2, structure_compare, 'ISO 8.4.1.4, ISO 13') :-
   X == X.
runner:case(==, 2, structure_compare, 'ISO 8.4.1.4, ISO 15') :-
   \+ _ == _.

/* X \== Y */

runner:ref(\==, 2, structure_compare, 'ISO 8.4.1.4').
runner:case(\==, 2, structure_compare, 'ISO 8.4.1.4, ISO 3') :-
   \+ 1 \== 1.
runner:case(\==, 2, structure_compare, 'ISO 8.4.1.4, ISO 16') :-
   _ \== _.

/* X @< Y */

runner:ref(@<, 2, structure_compare, 'ISO 8.4.1.4').
runner:case(@<, 2, structure_compare, 'ISO 8.4.1.4, ISO 8') :-
   \+ foo(a, b) @< north(a).
runner:case(@<, 2, structure_compare, 'ISO 8.4.1.4, ISO 10') :-
   foo(a, _) @< foo(b, _).
runner:case(@<, 2, structure_compare, 'ISO 8.4.1.4, ISO 11') :-
   foo(X, a) @< foo(Y, b); foo(Y, a) @< foo(X, b).

/* X @> Y */

runner:ref(@>, 2, structure_compare, 'ISO 8.4.1.4').
runner:case(@>, 2, structure_compare, 'ISO 8.4.1.4, ISO 9') :-
   foo(b) @> foo(a).

/* X @=< Y */

runner:ref(@=<, 2, structure_compare, 'ISO 8.4.1.4').
runner:case(@=<, 2, structure_compare, 'ISO 8.4.1.4, ISO 1') :-
   1.0 @=< 1.
runner:case(@=<, 2, structure_compare, 'ISO 8.4.1.4, ISO 4') :-
   aardvark @=< zebra.
runner:case(@=<, 2, structure_compare, 'ISO 8.4.1.4, ISO 5') :-
   short @=< short.
runner:case(@=<, 2, structure_compare, 'ISO 8.4.1.4, ISO 6') :-
   short @=< shorter.
runner:case(@=<, 2, structure_compare, 'ISO 8.4.1.4, ISO 12') :-
   X @=< X.
runner:case(@=<, 2, structure_compare, 'ISO 8.4.1.4, ISO 14') :-
   X @=< Y; Y @=< X.

/* X @>= Y */

runner:ref(@>=, 2, structure_compare, 'ISO 8.4.1.4').
runner:case(@>=, 2, structure_compare, 'ISO 8.4.1.4, ISO 7') :-
   \+ short @>= shorter.
runner:case(@>=, 2, structure_compare, 'ISO 8.4.1.4, XLOG 1') :-
   X @>= X.

/* compare(0, X, Y) */

runner:ref(compare, 3, structure_compare, 'Corr.2 8.4.2.4').
runner:case(compare, 3, structure_compare, 'Corr.2 8.4.2.4, XLOG 1') :-
   compare(<, 1.0, 1).
runner:case(compare, 3, structure_compare, 'Corr.2 8.4.2.4, XLOG 2') :-
   compare(<, aardvark, zebra).
runner:case(compare, 3, structure_compare, 'Corr.2 8.4.2.4, XLOG 3') :-
   compare(=, short, short).
runner:case(compare, 3, structure_compare, 'Corr.2 8.4.2.4, XLOG 4') :-
   compare(=, X, X).
runner:case(compare, 3, structure_compare, 'Corr.2 8.4.2.4, XLOG 5') :-
   compare(>, foo(a, b), north(a)).
runner:case(compare, 3, structure_compare, 'Corr.2 8.4.2.4, XLOG 6') :-
   compare(<, foo(a, _), foo(b, _)).
runner:case(compare, 3, structure_compare, 'Corr.2 8.4.2.4, XLOG 7') :-
   compare(>, a, 'A').
runner:case(compare, 3, structure_compare, 'Corr.2 8.4.2.4, XLOG 8') :-
   compare(>, œ, ü).
