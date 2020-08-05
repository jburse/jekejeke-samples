/**
 * Prolog code for the deterministic tracing example.
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

rev(X, Y) :- rev(X, [], Y).

rev([], X, X).
rev([X|Y], Z, T) :- rev(Y, [X|Z], T).

% ?- rev([1,2,3], X).
% X = [3,2,1]

% ?- trace.
% Yes

% ?- rev([1,2,3], X).
%     0 Call rev([1,2,3], X) ?
%     1 Call rev([1,2,3], [], X) ?
%     2 Call rev([2,3], [1], X) ?
%     3 Call rev([3], [2,1], X) ?
%     4 Call rev([], [3,2,1], X) ?
%     4 Exit rev([], [3,2,1], [3,2,1]) ?
%     3 Exit rev([3], [2,1], [3,2,1]) ?
%     2 Exit rev([2,3], [1], [3,2,1]) ?
%     1 Exit rev([1,2,3], [], [3,2,1]) ?
%     0 Exit rev([1,2,3], [3,2,1]) ?
% X = [3,2,1]
