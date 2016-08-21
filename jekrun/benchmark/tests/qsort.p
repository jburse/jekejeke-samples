/**
 * Prolog code for the quick sort benchmark.
 *
 * This is the benchmark of page 220 of:
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

% qsort
qsort :-
    dataqsort(X),
    qsort(X, [], _).

% dataqsort(+List)
dataqsort([27,74,17,33,94,18,46,83,65,2,32,53,28,85,99,
           47,28,82,6,11,55,29,39,81,90,37,10,0,66,51,
           7,21,85,27,31,63,75,4,95,99,11,28,61,74,18,
           92,40,53,59,8]).

/*****************************************************************/
/* Reduced Test Cases                                            */
/*****************************************************************/

% rqsort
rqsort :-
    rdataqsort(X),
    qsort(X, [], _).

% rdataqsort(+List)
rdataqsort([27,74,17,33,94,18,46,83,65,2,32,53,28,85,99,
           47,28,82,6,11,55,29,39,81,90,37,10]).

/*****************************************************************/
/* The Sort Algorithm                                            */
/*****************************************************************/

% partition(+List, +Integer, -List, -List)
partition([], _, [], []).
partition([X|L], Y, [X|L1], L2) :-
	X =< Y, !,
	partition(L, Y, L1, L2).
partition([X|L], Y, L1, [X|L2]) :-
	partition(L, Y, L1, L2).

% qsort(+List, +List, -List)
qsort([], R, R).
qsort([X|L], R0, R) :-
	partition(L, X, L1, L2),
	qsort(L2, R0, R1),
	qsort(L1, [X|R1], R).

