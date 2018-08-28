/**
 * Prolog code for the Call-out Call-in Solution Limiter example.
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

:- use_package(foreign(jekpro/tools/call)).
:- use_package(foreign(jekpro/tools/term)).

:- foreign(employee5/1, 'example03.OutTable', employee('CallOut')).

:- foreign(limit/2, 'example05.OutInLimit',
      limit('Interpreter','CallOut','AbstractTerm',int)).

:- limit(employee5(X), 3),
   write(X), nl, fail; true.

:- nl.

:- limit(employee5(X), 4),
   write(X), nl.

:- nl.

:- limit(employee5(X), 6),
   write(X), nl, fail; true.


