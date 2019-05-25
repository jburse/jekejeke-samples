/**
 * Prolog text clotab from Chat80 as a module.
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
 * @(#)clotab.pl	24.1 2/23/88
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

:- package(library(natural)).

:- endif.

:- module(clotab, [adv/1,do_trace/1,do_trace/2,compl_case/1,empty/1,
                     is_adv/1,is_pp/1,is_pred/1,minus/3,
                     my_plus/3,np_all/1,np_no_trace/1,role/3,
                     s_all/1,subj_case/1,prep_case/1,verb_case/1]).

% Normal form masks

is_pp(#(1,_,_,_)).

is_pred(#(_,1,_,_)).

is_trace(#(_,_,1,_)).

is_adv(#(_,_,_,1)).

do_trace(#(_,_,1,_), #(0,0,0,0)).

do_trace(#(0,0,1,0)).

adv(#(0,0,0,1)).

empty(#(0,0,0,0)).

np_all(#(1,1,1,0)).

s_all(#(1,0,1,1)).

np_no_trace(#(1,1,0,0)).

% Mask operations

my_plus(#(B1,B2,B3,B4), #(C1,C2,C3,C4), #(D1,D2,D3,D4)) :-
   or(B1, C1, D1),
   or(B2, C2, D2),
   or(B3, C3, D3),
   or(B4, C4, D4).

minus(#(B1,B2,B3,B4), #(C1,C2,C3,C4), #(D1,D2,D3,D4)) :-
   anot(B1, C1, D1),
   anot(B2, C2, D2),
   anot(B3, C3, D3),
   anot(B4, C4, D4).

or(1, _, 1).
or(0, 1, 1).
or(0, 0, 0).

anot(X, 0, X).
anot(_, 1, 0).

% Noun phrase position features

role(subj, _, #(1,0,0)).
role(compl, _, #(0,_,_)).
role(undef, main, #(_,0,_)).
role(undef, aux, #(0,_,_)).
role(undef, decl, _).
role(nil, _, _).

subj_case(#(1,0,0)).
verb_case(#(0,1,0)).
prep_case(#(0,0,1)).
compl_case(#(0,_,_)).
