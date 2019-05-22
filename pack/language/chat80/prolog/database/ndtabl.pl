/**
 * Prolog text ndtabl from Chat80 as a module.
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

/**
 * Obtained rights comment in Prolog text and text from LICENSE file:
 *
 * @(#)ndtabl.pl	24.1 2/23/88
 *
 * Copyright 1986, Fernando C.N. Pereira and David H.D. Warren,
 *
 * All Rights Reserved
 *
 * This program may be used, copied, altered or included in other programs
 * only for academic purposes and provided that the authorship of the
 * initial program is acknowledged. Use for commercial purposes without the
 * previous written agreement of the authors is forbidden.
 */

:- if(current_prolog_flag(dialect,jekejeke)).

:- package(library(database)).

:- endif.

:- module(ndtabl, [nd/3,nd/4,nd/5]).

:- discontiguous nd/3.
:- discontiguous nd/4.
:- discontiguous nd/5.

% :-mode
%    nd(+,-,-),
%    nd(+,-,-,-),
%    nd(+,-,-,-,-).

nd(african, 19, 26).
nd(american, 19, 26).
nd(area, 51, 51).
nd(area, 22, 22, 51).
nd(asian, 21, 26).
nd(aggregate, 103, 3, 100, 51).
nd(one_of, 99, 200, -99).
nd(ratio, 99, 51, 51, 3).
nd(card, 99, 100, 3).
nd(borders, 29, 22, 22).
nd(capital, 22, 22).
nd(capital, 22, 22, 23).
nd(city, 18, 18).
nd(continent, 8, 8).
nd(country, 22, 22).
nd(drains, 16, 16, 10).
nd(eastof, 40, 22, 22).
nd(european, 19, 26).
nd(exceeds, 99, 51, 51).
nd(flows, 19, 16, 22).
nd(flows, 19, 16, 22, 22).
nd(in, 29, 26, 15).
nd(latitude, 23, 23).
nd(latitude, 22, 22, 23).
nd(longitude, 26, 26).
nd(longitude, 22, 22, 26).
nd(northof, 40, 22, 22).
nd(ocean, 7, 7).
nd(population, 51, 51).
nd(population, 23, 23, 51).
nd(region, 12, 12).
nd(rises, 16, 16, 22).
nd(river, 16, 16).
nd(sea, 8, 8).
nd(place, 23, 23).
nd(seamass, 10, 10).
nd(southof, 40, 22, 22).
nd(westof, 40, 22, 22).
nd(=<, 99, 51, 51).
nd(<, 99, 51, 51).
nd(>, 99, 51, 51).
nd(>=, 99, 51, 51).

