/**
 * Term Logic Test Cases
 * https://planetmath.org/aristotelianlogic
 *
 * Warranty & Liability
 * To the extent permitted by applicable law and unless explicitly
 * otherwise agreed upon, XLOG Technologies GmbH makes no warranties
 * regarding the provided information.XLOG Technologies GmbH assumes
 * no liability that any problems might be solved with the information
 * provided by XLOG Technologies GmbH.
 *
 * Rights & License
 * All industrial property rights regarding the information - copyright
 * and patent rights in particular - are the sole property of XLOG
 * Technologies GmbH.If the company was not the originator of some
 * excerpts, XLOG Technologies GmbH has at least obtained the right to
 * reproduce, change and translate the information.
 *
 * Reproduction is restricted to the whole unaltered document.Reproduction
 * of the information is only allowed for non-commercial uses.Selling,
 * giving away or letting of the execution of the library is prohibited.
 * The library can be distributed as part of your applications and libraries
 * for execution provided this comment remains unchanged.
 *
 * Restrictions
 * Only to be distributed with programs that add significant and primary
 * functionality to the library.Not to be distributed with additional
 * software intended to replace any components of the library.
 *
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */

% :- encoding(utf8).

:- ensure_loaded(prepare).

:- op(100, xfx, ∈).
:- op(100, xfx, a).
:- op(100, xfx, o).
:- op(100, xfx, e).
:- op(100, xfx, i).
:- op(100, fx, n).

case(1, quantity_conv1, (s e p -: p e s)).
case(2, quantity_conv2, (s i p -: p i s)).
case(3, quantity_conv3, (s a p, n s -: p i s)).
case(4, quantity_conv4, (s e p, n p -: p o s)).
case(5, square_contradict1, (s e p -: -s i p)).
case(6, square_contradict2, (-s i p -: s e p)).
case(7, square_contradict3, (s a p -: -s o p)).
case(8, square_contradict4, (-s o p -: s a p)).

case(11, barbara, (m a p, s a m -: s a p)).
case(12, celarent, (m e p, s a m -: s e p)).
case(13, darii, (m a p, s i m -: s i p)).
case(14, ferio, (m e p, s i m -: s o p)).
case(15, cesare, (p e m, s a m -: s e p)).
case(16, camestres, (p a m, s e m -: s e p)).
case(17, festino, (p e m, s i m -: s o p)).
case(18, baroco, (p a m, s o m -: s o p)).
case(19, darapti, (m a p, m a s, n m -: s i p)).
case(20, felapton, (m e p, m a s, n m -: s o p)).
case(21, disamis, (m i p, m a s -: s i p)).
case(22, datisi, (m a p, m i s -: s i p)).
case(23, bocardo, (m o p, m a s -: s o p)).
case(24, ferison, (m e p, m i s -: s o p)).
case(25, bramantip, (p a m, m a s, n p -: s i p)).
case(26, camenes, (p a m, m e s -: s e p)).
case(27, dimaris, (p i m, m a s -: s i p)).
case(28, fesapo, (p e m, m a s, n m -: s o p)).
case(29, fresison, (p e m, m i s -: s o p)).

case(31, singular_term1, (m a p, s∈m -: s∈p)).
case(32, singular_term2, (m∈s, m∈p -: s i p)).
case(33, singular_term3, (m e p, s∈m -: -s∈p)).
case(34, singular_term4, (m∈s, -m∈p -: s o p)).
case(35, singular_conv1, (p a s, m∈p -: s i p)).
case(36, singular_conv2, (s e p, m∈p -: p o s)).
