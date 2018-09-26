/**
 * Prolog code for some common test data.
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

/****************************************************************/
/* Used by set.p and structure.p                                */
/****************************************************************/

a(1, f(_)).
a(2, f(_)).

b(1, 1).
b(1, 1).
b(1, 2).
b(2, 1).
b(2, 2).
b(2, 2).

d(1, 1).
d(1, 2).
d(1, 1).
d(2, 2).
d(2, 1).
d(2, 2).
