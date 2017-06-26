/**
 * Prolog code for the compliance assessment suite.
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

:- ensure_loaded('../decimal/precision.p').
:- ensure_loaded('../decimal/addmul.p').
:- ensure_loaded('../decimal/expsin.p').

:- ensure_loaded('../gauss/onedim.p').
:- ensure_loaded('../gauss/exchstep.p').
:- ensure_loaded('../gauss/samebef.p').
:- ensure_loaded('../gauss/utilpoly.p').

:- ensure_loaded('../groebner/parteval.p').
:- ensure_loaded('../groebner/simplify.p').
:- ensure_loaded('../groebner/nonfrac.p').

:- ensure_loaded('../leibniz/varchange.p').
:- ensure_loaded('../leibniz/autodiff.p').
:- ensure_loaded('../leibniz/expansion.p').
