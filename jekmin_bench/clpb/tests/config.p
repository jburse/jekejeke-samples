/**
 * CLP(B) test configuration.
 *
 * Automotive configuration - Küchlin, 2013
 * http://ceur-ws.org/Vol-1128/paper3.pdf
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
;  use_module(library(clpb)).
:- current_prolog_flag(dialect, jekejeke)
-> use_module(library(finite/clpb)); true.
:- current_prolog_flag(dialect, jekejeke)
-> use_module(library(basic/lists)); true.

% config(-Integer)
config(M) :-
   carmin(_, M).
config(M) :-
   carmax(_, M).

% carmin(-List, -Integer)
carmin(L, M) :-
   car(L),
   prices(P),
   maplist(-, P, Q),
   weighted_maximum(Q, L, N),
   M is -N.

% carmax(-List, -Integer)
carmax(L, M) :-
   car(L),
   prices(P),
   weighted_maximum(P, L, M).


% prices(-List)
prices([4000,2500,4500,
          500,800,300,
          800,2000,1500,1600,1200,
          300,500,600,450,
          100,150,130,
          180,100,90,
          300,250,
          100,80,200,180,150]).

% car(-List)
car([E1,E2,E3,
       G1,G2,G3,
       C1,C2,C3,C4,C5,
       D1,D2,D3,D4,
       N1,N2,N3,
       A1,A2,A3,
       S1,S2,
       R1,R2,R3,R4,R5]) :-
   sat(card([1],[E1,E2,E3])),
   sat(card([1],[G1,G2,G3])),
   sat(card([1],[C1,C2,C3,C4,C5])),
   sat(card([1],[D1,D2,D3,D4])),
   sat(card([0-1],[N1,N2,N3])),
   sat(card([0-1],[A1,A2,A3])),
   sat(card([0-1],[S1,S2])),
   sat(card([0-1],[R1,R2,R3,R4,R5])),
   sat(G1=<E1+E2),
   sat(N1+N2=<D1),
   sat(N3=<D2+D3),
   sat(A1+A3=<D1+D2),
   sat(S1=<D2+D3),
   sat(R1+R2+R5=<D1+D4).


