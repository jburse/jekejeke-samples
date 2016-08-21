/**
 * Prolog code for the naïve reverse benchmark.
 *
 * This is the benchmark of page 219 of:
 * Warren, D.H.D. (1983): Applied Logic - Its Use and
 * Implementation as a Programming Tool,
 * Technical Note 290, SRI International, 1983
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


/*****************************************************************/
/* Normal Test Cases                                             */
/*****************************************************************/

% nrev
nrev :- datanrev(X), rev(X, _).

% datanrev(-List)
datanrev([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,
			   21,22,23,24,25,26,27,28,29,30]).

/*****************************************************************/
/* Reduced Test Cases                                            */
/*****************************************************************/

% rnrev
rnrev :- rdatanrev(X), rev(X, _).

% rdatanrev(-List)
rdatanrev([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17]).

/*****************************************************************/
/* The Pre3dicates                                               */
/*****************************************************************/

% rev(+List, -List)
rev([], []).
rev([X|Rest], Ans) :- rev(Rest, L), concatenate(L, [X], Ans).

% concatenate(+List, +List, -List)
concatenate([], L, L).
concatenate([X|L1], L2, [X|L3]) :- concatenate(L1, L2, L3).