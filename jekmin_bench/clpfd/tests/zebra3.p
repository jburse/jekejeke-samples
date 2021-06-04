/**
 * CLP(FD) code for the Zebra riddle.
 *
 * First published as "Who owns the Zebra?" in the Life International
 * magazine on December 17, 1962 with solution given in the March 25,
 * 1963 issue. The riddle given here is not a verbatim copy.
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

:- current_prolog_flag(dialect, jekejeke) -> true
;  use_module(library(clpfd)).
:- current_prolog_flag(dialect, jekejeke)
-> use_module(library(finite/clpfd)); true.

% zebra3(-List)
zebra3(Variables) :-
   Nationality = [Brit,Swede,Dane,Norwegian,German],
   Pet = [Dog,Bird,Cat,Horse,_],
   Cigarette = [Pallmall,Dunhill,Rothmans,Marlboro,Winfield],
   Drink = [Tea,Coffee,Milk,Beer,Water],
   Color = [Red,White,Green,Yellow,Blue],

   term_variables([Nationality,Pet,Cigarette,Drink,Color], Variables),
   Variables ins 1..5,

   all_different(Nationality),
   all_different(Pet),
   all_different(Cigarette),
   all_different(Drink),
   all_different(Color),

   Norwegian #= 1,
   Milk #= 3,
   Brit #= Red,
   Swede #= Dog,
   Dane #= Tea,
   Green+1 #= White,
   Coffee #= Green,
   Bird #= Pallmall,
   Dunhill #= Yellow,
   Dunhill-Horse+1 #= Dist1,
   Marlboro-Cat+1 #= Dist2,
   Marlboro-Water+1 #= Dist3,
   Winfield #= Beer,
   German #= Rothmans,
   Norwegian-Blue+1 #= Dist4,
   [Dist1,Dist2,Dist3,Dist4] ins 0\/2,

   label(Variables).
