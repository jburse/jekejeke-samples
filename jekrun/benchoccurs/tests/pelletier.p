/**
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

:- ensure_loaded(prepare).

pcase(1, 'Pelletier p18', exist(A, all(B, (p(A) -: p(B))))).
pcase(2, 'Pelletier p19', exist(A, all(B, all(C, ((p(B) -: q(C)) -:
      (p(A) -: q(A))))))).
pcase(3, 'Pelletier p20', (all(A, all(B, exist(C, all(D, (p(A), q(B) -:
         r(C), u(D)))))) -: (exist(E, exist(F, (p(E), q(F)))) -:
         exist(G, r(G))))).
pcase(7, 'Pelletier p24', (-exist(A, (u(A), q(A))), all(B, (p(B) -:
         q(B); r(B))), -exist(C, (p(C) -: exist(D, q(D)))),
   all(E, (q(E), r(E) -: u(E))) -: exist(F, (p(F), r(F))))).
pcase(8, 'Pelletier p25', (exist(A, p(A)), all(B, (u(B) -:
         -g(B), r(B))), all(C, (p(C) -: g(C), u(C))),
   (all(D, (p(D) -: q(D))); exist(E, (q(E), p(E)))) -:
      exist(F, (q(F), p(F))))).
pcase(10, 'Pelletier p27', (exist(A, (p(A), -q(A))), all(B, (p(B) -:
         r(B))), all(C, (u(C), v(C) -: p(C))),
   exist(D, (r(D), -q(D))) -: (all(E, (v(E) -: -r(E))) -:
         all(F, (u(F) -: -v(F)))))).
pcase(11, 'Pelletier p28', (all(A, (p(A) -: all(B, q(B)))),
   (all(C, (q(C); r(C))) -: exist(D, (q(D), r(D)))),
   (exist(E, r(E)) -: all(F, (l(F) -: m(F)))) -:
      all(G, (p(G), l(G) -: m(G))))).
