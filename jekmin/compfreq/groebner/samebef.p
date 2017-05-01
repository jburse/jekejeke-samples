/**
 * Prolog test cases for the symbolic same and before.
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

:- use_package(library(jekdev/reference/testing)).

:- multifile runner:ref/4.
:- discontiguous runner:ref/4.

:- multifile runner:case/4.
:- discontiguous runner:case/4.

:- use_module(library(groebner/generic)).

% eval_eq/2
runner:ref(eval_eq, 2, groebner_samebef, 'groebner 0.9.2, 3.1').
runner:case(eval_eq, 2, groebner_samebef, 'groebner 0.9.2, 3.1, XLOG 1') :-
   -3 =:= -3.
runner:case(eval_eq, 2, groebner_samebef, 'groebner 0.9.2, 3.1, XLOG 2') :-
   \+ 1 =:= 3/5.
runner:case(eval_eq, 2, groebner_samebef, 'groebner 0.9.2, 3.1, XLOG 3') :-
   \+ 4/7 =:= 5/11.
runner:case(eval_eq, 2, groebner_samebef, 'groebner 0.9.2, 3.1, XLOG 4') :-
   catch(1 =:= _, error(E,_), true),
   E == evaluation_error(ordered).

% eval_nq/2
runner:ref(eval_nq, 2, groebner_samebef, 'groebner 0.9.2, 3.2').
runner:case(eval_nq, 2, groebner_samebef, 'groebner 0.9.2, 3.2, XLOG 1') :-
   1 =\= 2.
runner:case(eval_nq, 2, groebner_samebef, 'groebner 0.9.2, 3.2, XLOG 2') :-
   3/5 =\= 1.
runner:case(eval_nq, 2, groebner_samebef, 'groebner 0.9.2, 3.2, XLOG 3') :-
   \+ 3/4 =\= 3/4.
runner:case(eval_nq, 2, groebner_samebef, 'groebner 0.9.2, 3.2, XLOG 4') :-
   catch(_+_ =\= 7/5, error(E,_), true),
   E == existence_error(procedure,polynom:gen_eq/2).

% eval_ls/2
runner:ref(eval_ls, 2, groebner_samebef, 'groebner 0.9.2, 3.3').
runner:case(eval_ls, 2, groebner_samebef, 'groebner 0.9.2, 3.3, XLOG 1') :-
   \+ -3 < -3.
runner:case(eval_ls, 2, groebner_samebef, 'groebner 0.9.2, 3.3, XLOG 2') :-
   2 < 7/3.
runner:case(eval_ls, 2, groebner_samebef, 'groebner 0.9.2, 3.3, XLOG 3') :-
   -5/4 < -1.
runner:case(eval_ls, 2, groebner_samebef, 'groebner 0.9.2, 3.3, XLOG 4') :-
   \+ 2/7 < 1/6.
runner:case(eval_ls, 2, groebner_samebef, 'groebner 0.9.2, 3.3, XLOG 5') :-
   catch(_+_ < 7/5, error(E,_), true),
   E == existence_error(procedure,polynom:gen_ls/2).

% eval_lq/2
runner:ref(eval_lq, 2, groebner_samebef, 'groebner 0.9.2, 3.4').
runner:case(eval_lq, 2, groebner_samebef, 'groebner 0.9.2, 3.4, XLOG 1') :-
   -3 =< 12.
runner:case(eval_lq, 2, groebner_samebef, 'groebner 0.9.2, 3.4, XLOG 2') :-
   \+ 1 =< 5/6.
runner:case(eval_lq, 2, groebner_samebef, 'groebner 0.9.2, 3.4, XLOG 3') :-
   6/11 =< 1.
runner:case(eval_lq, 2, groebner_samebef, 'groebner 0.9.2, 3.4, XLOG 4') :-
   3/4 =< 3/4.
runner:case(eval_lq, 2, groebner_samebef, 'groebner 0.9.2, 3.4, XLOG 5') :-
   catch(_/_ =< 3/4, error(E,_), true),
   E == evaluation_error(ordered).

% eval_gr/2
runner:ref(eval_gr, 2, groebner_samebef, 'groebner 0.9.2, 3.5').
runner:case(eval_gr, 2, groebner_samebef, 'groebner 0.9.2, 3.5, XLOG 1') :-
   \+ -3 > 12.
runner:case(eval_gr, 2, groebner_samebef, 'groebner 0.9.2, 3.5, XLOG 2') :-
   \+ 2 > 7/3.
runner:case(eval_gr, 2, groebner_samebef, 'groebner 0.9.2, 3.5, XLOG 3') :-
   \+ -5/4 > -1.
runner:case(eval_gr, 2, groebner_samebef, 'groebner 0.9.2, 3.5, XLOG 4') :-
   \+ 3/4 > 3/4.
runner:case(eval_gr, 2, groebner_samebef, 'groebner 0.9.2, 3.5, XLOG 5') :-
   catch(7 > _/_, error(E,_), true),
   E == existence_error(procedure,fraction:gen_ls/2).

% eval_gq/2
runner:ref(eval_gq, 2, groebner_samebef, 'groebner 0.9.2, 3.6').
runner:case(eval_gq, 2, groebner_samebef, 'groebner 0.9.2, 3.6, XLOG 1') :-
   -3 >= -3.
runner:case(eval_gq, 2, groebner_samebef, 'groebner 0.9.2, 3.6, XLOG 2') :-
   1 >= 5/6.
runner:case(eval_gq, 2, groebner_samebef, 'groebner 0.9.2, 3.6, XLOG 3') :-
   \+ 6/11 >= 1.
runner:case(eval_gq, 2, groebner_samebef, 'groebner 0.9.2, 3.6, XLOG 4') :-
   2/7 >= 1/6.
runner:case(eval_gq, 2, groebner_samebef, 'groebner 0.9.2, 3.6, XLOG 5') :-
   catch(1 >= _, error(E,_), true),
   E == evaluation_error(ordered).


