/**
 * Prolog code for the consult file theory test cases.
 *
 * Source of test cases is the following standards:
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
/* Clause Indexing                                              */
/****************************************************************/

/****************************************************************/
/* Theory Files                                                 */
/****************************************************************/

/* consult(R) */

/* [R1, ..., Rn] */

/* op(L, M, O) */

runner:ref(op, 3, consult_file, 'ISO 8.14.3.4, Corr.2 8.14.3.4').
runner:case(op, 3, consult_file, 'ISO 8.14.3.4, ISO 1') :-
   op(30, xfy, ++), current_op(30, xfy, ++).
runner:case(op, 3, consult_file, 'ISO 8.14.3.4, ISO 2') :-
   op(0, yfx, ++), \+ current_op(30, xfy, ++).
runner:case(op, 3, consult_file, 'ISO 8.14.3.4, ISO 3') :-
   catch(op(max, xfy, ++), error(E, _), true),
   E == type_error(integer, max).
runner:case(op, 3, consult_file, 'ISO 8.14.3.4, ISO 4') :-
   catch(op(-30, xfy, ++), error(E, _), true),
   E == representation_error(not_less_than_zero).
runner:case(op, 3, consult_file, 'ISO 8.14.3.4, ISO 5') :-
   catch(op(1201, xfy, ++), error(E, _), true),
   E == domain_error(operator_priority, 1201).
runner:case(op, 3, consult_file, 'ISO 8.14.3.4, ISO 6') :-
   catch(op(30, _, ++), error(E, _), true),
   E == instantiation_error.
runner:case(op, 3, consult_file, 'ISO 8.14.3.4, ISO 7') :-
   catch(op(30, yfy, ++), error(E, _), true),
   E == domain_error(operator_specifier, yfy).
runner:case(op, 3, consult_file, 'ISO 8.14.3.4, ISO 8') :-
   catch(op(30, xfy, 0), error(E, _), true),
   nonvar(E), E = type_error(_, 0).
runner:case(op, 3, consult_file, 'ISO 8.14.3.4, ISO 9') :-
   op(30, xfy, ++), op(40, xfx, ++), current_op(40, xfx, ++), op(0, xfx, ++).
runner:case(op, 3, consult_file, 'ISO 8.14.3.4, ISO 10') :-
   op(30, xfy, ++), catch(op(50, yf, ++), error(E, _), true),
   nonvar(E), E = permission_error(create, operator, _).
runner:case(op, 3, consult_file, 'Corr.2 6.3.4.3, ISO 1') :-
   catch(op(1000, xfy, {}), error(E, _), true),
   E == permission_error(create, operator, {}).
runner:case(op, 3, consult_file, 'Corr.2 6.3.4.3, ISO 3') :-
   catch(op(1000, xfy, '|'), error(E, _), true),
   nonvar(E), E = permission_error(_, _, _).

/* current_op(L, M, O), ISO 8.14.4.4 */

/* discontiguous(P) */

/* multifile(P) */

/* listing */

/* listing(P) */

/* source_file(S) */