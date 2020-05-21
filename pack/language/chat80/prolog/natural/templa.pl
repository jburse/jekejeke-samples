/**
 * Prolog text templa from Chat80 as a module.
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
 * @(#)templa.pl	24.1 2/23/88
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

:- if(current_prolog_flag(dialect, jekejeke)).

:- package(library(natural)).

:- endif.

:- module(templa, [property/9, thing/6, aggr_noun/4,
      
      meta_noun/7, trans/9, intrans/6,
      
      restriction/4, attribute/6, aggr_adj/4,
      
      adjunction/4, measure/4, units/2,
      
      chat_sign/2, comparator/5, name_template/2]).
:- use_module('../database/chatops').
:- use_module('../database/world0').

/* Nouns */

property(area, measure&area, X, feature&place&_, Y, area(Y, X), [], _, _).
property(capital, feature&city, X, feature&place&country, Y,
   capital(Y, X), [], _, _).
property(latitude,
   measure&position, X, feature&_, Y, latitude(Y, X), [], _, _).
property(longitude, measure&position, X, feature&_, Y,
   longitude(Y, X), [], _, _).
property(population,
   measure&heads, X, feature&_, Y, population(Y, X), [], _, _).

thing(place, feature&place&_, X, place(X), [], _).
thing(area, measure&area, X, area(X), [], _).
thing(capital, feature&city, X, capital(X), [], _).
thing(city, feature&city, X, city(X), [], _).
thing(continent, feature&place&continent, X, continent(X), [], _).
thing(country, feature&place&country, X, country(X), [], _).
thing(latitude, measure&position, X, latitude(X), [], _).
thing(longitude, measure&position, X, longitude(X), [], _).
thing(ocean, feature&place&seamass, X, ocean(X), [], _).
% thing(person,_,X,person(X),[],_).
thing(population, measure&heads, X, population(X), [], _).
thing(region, feature&place&_, X, region(X), [], _).
thing(river, feature&river, X, river(X), [], _).
thing(sea, feature&place&seamass, X, sea(X), [], _).
thing(seamass, feature&place&seamass, X, seamass(X), [], _).

aggr_noun(average, _, _, average).
aggr_noun(sum, _, _, total).
aggr_noun(total, _, _, total).

meta_noun(number, _, V, feature&_, X, P, numberof(X, P, V)).

/* Proper nouns */

name_template(X, feature&circle) :- database(circle_of_latitude(X)).
name_template(X, feature&city) :- database(city(X)).
name_template(X, feature&place&continent) :- database(continent(X)).
name_template(X, feature&place&country) :- database(country(X)).
name_template(X, feature&place&_) :- database(region(X)).
name_template(X, feature&river) :- database(river(X)).
name_template(X, feature&place&seamass) :- database(seamass(X)).

/* Verbs */

trans(border,
   feature&place&_, X, feature&place&_, Y, borders(X, Y), [], _, _).
trans(contain, feature&place&_, X, feature&_, Y, in(Y, X), [], _, _).
trans(exceed, measure&Type, X, measure&Type, Y, exceeds(X, Y), [], _, _).

intrans(drain, feature&river, X, drains(X, Y),
   [slot(prep(into), feature&place&_, Y, _, free)], _).
intrans(flow, feature&river, X, flows(X, Y),
   [slot(prep(through), feature&place&_, Y, _, free)], _).
intrans(flow, feature&river, X, flows(X, Y, Z),
   [slot(prep(into), feature&place&_, Z, _, free),
   
   slot(prep(from), feature&place&_, Y, _, free)], _).
intrans(rise, feature&river, X, rises(X, Y),
   [slot(prep(in), feature&place&_, Y, _, free)], _).

/* Adjectives */

restriction(african, feature&_, X, african(X)).
restriction(american, feature&_, X, american(X)).
restriction(asian, feature&_, X, asian(X)).
restriction(european, feature&_, X, european(X)).

attribute(large, feature&place&_, X, measure&area, Y, area(X, Y)).
attribute(small, feature&place&_, X, measure&area, Y, area(X, Y)).
attribute(great, measure&Type, X, measure&Type, Y, X = Y).
attribute(populous, feature&_, X, measure&heads, Y, population(Y, X)).

aggr_adj(average, _, _, average).
aggr_adj(total, _, _, total).
aggr_adj(minimum, _, _, minimum).
aggr_adj(maximum, _, _, maximum).

/* Prepositions */

adjunction(in, feature&_-X, feature&place&_-Y, in(X, Y)).
adjunction(eastof, feature&_-X, feature&_-Y, eastof(X, Y)).
adjunction(westof, feature&_-X, feature&_-Y, westof(X, Y)).
adjunction(northof, feature&_-X, feature&_-Y, northof(X, Y)).
adjunction(southof, feature&_-X, feature&_-Y, southof(X, Y)).

/* Measure */

measure(ksqmile, measure&area, [], ksqmiles).
measure(sqmile, measure&area, [], sqmiles).
measure(degree, measure&position, [], degrees).
measure(thousand, measure&heads, [], thousand).
measure(million, measure&heads, [], million).

units(large, measure&_).
units(small, measure&_).

chat_sign(large, +).
chat_sign(small, -).
chat_sign(great, +).

/* Proportions and the like */

comparator(proportion, _, V, [], proportion(V)).
comparator(percentage, _, V, [], proportion(V)).
