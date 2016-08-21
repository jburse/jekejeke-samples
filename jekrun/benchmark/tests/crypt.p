/**
 * Prolog code for the crypt riddle benchmark.
 *
 * This is problem 223 from:
 * Trigg, W. C. (1985): Mathematical Quickies,
 * Dover Publications, Inc., New York, 1985
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

% sum2(+List, +List, -List)
sum2(AL, BL, CL) :-
	sum2(AL, BL, 0, CL).

% sum2(+List, +List, +Integer, -List)
sum2([A | AL], [B | BL], Carry, [C | CL]) :-
	X is (A + B + Carry),
	C is X rem 10,
	NewCarry is X // 10,
	sum2(AL, BL, NewCarry, CL).
sum2([], BL, 0, BL) :- !.
sum2(AL, [], 0, AL) :- !.
sum2([], [B | BL], Carry, [C | CL]) :-
	X is B + Carry,
	NewCarry is X // 10,
	C is X rem 10,
	sum2([], BL, NewCarry, CL).
sum2([A | AL], [], Carry, [C | CL]) :-
	X is A + Carry,
	NewCarry is X // 10,
	C is X rem 10,
	sum2([], AL, NewCarry, CL).
sum2([], [], Carry, [Carry]).

% mult(+List, +Integer, -List)
mult(AL, D, BL) :- mult(AL, D, 0, BL).

% mult(+List, +Integer, +Integer, -List)
mult([], _, Carry, [C, Cend]) :-
	C is Carry rem 10,
	Cend is Carry // 10.
mult([A | AL], D, Carry, [B | BL] ) :-
	X is A * D + Carry,
	B is X rem 10,
	NewCarry is X // 10,
	mult(AL, D, NewCarry, BL).

% zero(+List)
zero([]).
zero([0 | L]) :- zero(L).

% odd(-Integer)
odd(1).
odd(3).
odd(5).
odd(7).
odd(9).

% even(-Integer)
even(0).
even(2).
even(4).
even(6).
even(8).

% lefteven(-Integer)
lefteven(2).
lefteven(4).
lefteven(6).
lefteven(8).

% crypt
crypt :-
	odd(A), even(B), even(C), even(E),
	mult([C, B, A], E, [I, H, G, F | X]),
	lefteven(F), odd(G), even(H), even(I), zero(X), lefteven(D),
	mult([C, B, A], D, [L, K, J | Y]),
	lefteven(J), odd(K), even(L), zero(Y),
	sum2([I, H, G, F], [0, L, K, J], [P, O, N, M | Z]),
	odd(M), odd(N), even(O), even(P), zero(Z).