/**
 * Prolog code for the database query benchmark.
 *
 * This is the benchmark of page 226 of:
 * Warren, D.H.D. (1983): Applied Logic - Its Use and
 * Implementation as a Programming Tool,
 * Technical Note 290, SRI International, 1983
 *
 * We use integer  division instead of float division,
 * since float division is not yet available in Jekejeke
 * Prolog 0.8.4. Further complication by two temporary
 * variables T1 and T2.
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

% pop(-Atom, -Integer)
pop(china,      8250).
pop(india,      5863).
pop(ussr,       2521).
pop(usa,        2119).
pop(indonesia,  1276).
pop(japan,      1097).
pop(brazil,     1042).
pop(bangladesh,  750).
pop(pakistan,    682).
pop(w_germany,   620).
pop(nigeria,     613).
pop(mexico,      581).
pop(uk,          559).
pop(italy,       554).
pop(france,      525).
pop(philippines, 415).
pop(thailand,    410).
pop(turkey,      383).
pop(egypt,       364).
pop(spain,       352).
pop(poland,      337).
pop(s_korea,     335).
pop(iran,        320).
pop(ethiopia,    272).
pop(argentina,   251).

% area(-Atom, -Integer)
area(china,     3380).
area(india,     1139).
area(ussr,      8708).
area(usa,       3609).
area(indonesia,  570).
area(japan,      148).
area(brazil,    3288).
area(bangladesh,  55).
area(pakistan,   311).
area(w_germany,   96).
area(nigeria,    373).
area(mexico,     764).
area(uk,          86).
area(italy,      116).
area(france,     213).
area(philippines, 90).
area(thailand,   200).
area(turkey,     296).
area(egypt,      386).
area(spain,      190).
area(poland,     121).
area(s_korea,     37).
area(iran,       628).
area(ethiopia,   350).
area(argentina, 1080).

% density(-Atom, -Integer)
density(C, D) :-
  pop(C, P),
  area(C, A),
  D is P*100 // A.

% query
query :-
  density(_, D1),
  density(_, D2),
  D1 > D2,
  T1 is 20 * D1,
  T2 is 21 * D2,
  T1 < T2.