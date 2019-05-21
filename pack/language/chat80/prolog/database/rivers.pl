/**
 * Prolog text rivers from Chat80 as a module.
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
 * @(#)rivers.pl	24.1 2/23/88
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

:- module(rivers, [river/2]).

% Facts about rivers.
% ------------------

river(amazon, [atlantic,brazil,peru]).
river(amu_darya, [aral_sea,soviet_union,afghanistan]).
river(amur, [pacific,soviet_union,china,mongolia]).
river(brahmaputra, [indian_ocean,bangladesh,china]).
river(colorado, [pacific,mexico,united_states]).
river(congo_river, [atlantic,zaire,zambia]).
river(cubango, [botswana,south_africa,angola]).
river(danube, [black_sea,romania,yugoslavia,hungary,czechoslovakia,austria,west_germany]).
river(don, [black_sea,soviet_union]).
river(elbe, [atlantic,west_germany,east_germany,czechoslovakia]).
river(euphrates, [persian_gulf,iraq,syria,turkey]).
river(ganges, [indian_ocean,india,china]).
river(hwang_ho, [pacific,china]).
river(indus, [indian_ocean,pakistan,india,china]).
river(irrawaddy, [indian_ocean,burma]).
river(lena, [arctic_ocean,soviet_union]).
river(limpopo, [indian_ocean,mozambique,south_africa]).
river(mackenzie, [arctic_ocean,canada]).
river(mekong, [pacific,vietnam,cambodia,laos,china]).
river(mississippi, [atlantic,united_states]).
river(murray, [indian_ocean,australia]).
river(niger_river, [atlantic,nigeria,niger,mali,guinea]).
river(nile, [mediterranean,egypt,sudan,uganda]).
river(ob, [arctic_ocean,soviet_union]).
river(oder, [baltic,poland,czechoslovakia]).
river(orange, [atlantic,south_africa,lesotho]).
river(orinoco, [atlantic,venezuela,colombia]).
river(parana, [atlantic,argentina,paraguay,brazil]).
river(rhine, [atlantic,netherlands,west_germany,switzerland]).
river(rhone, [mediterranean,france,switzerland]).
river(rio_grande, [atlantic,mexico,united_states]).
river(salween, [indian_ocean,burma,china]).
river(senegal_river, [atlantic,senegal,mali,guinea]).
river(tagus, [atlantic,portugal,spain]).
river(vistula, [baltic,poland]).
river(volga, [black_sea,soviet_union]).
river(volta, [atlantic,ghana,upper_volta]).
river(yangtze, [pacific,china]).
river(yenisei, [arctic_ocean,soviet_union,mongolia]).
river(yukon, [pacific,united_states,canada]).
river(zambesi, [indian_ocean,mozambique,zambia,angola]).

