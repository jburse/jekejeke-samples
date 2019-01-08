/**
 * Prolog code for the stream open theory test cases.
 *
 * Source of test cases is the following standard:
 *   - Prolog General Core ISO/IUEC 13211-1
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

:- use_package(library(jekdev/reference/testing)).

:- multifile runner:ref/4.
:- discontiguous runner:ref/4.

:- multifile runner:case/4.
:- discontiguous runner:case/4.

/****************************************************************/
/* Stream Control                                               */
/****************************************************************/

/* absolute_file_name(R, S) */

/* absolute_file_name(R, S, O) */

/* current_input(S), ISO 8.11.1 */

/* current_output(S), ISO 8.11.2 */

/* set_input(X), ISO 8.11.3 */

/* set_output(X), ISO 8.11.4 */

/* open(P, M, S), ISO 8.11.5.4 */

/* open(P, M, S, O), ISO 8.11.5.4 */

runner:ref(open, 4, stream_open, 'ISO 8.11.5.4').
runner:case(open, 4, stream_open, 'ISO 8.11.5.4, XLOG 1') :-
   open('../data.txt', read, D, [type(binary)]),
   close(D).
runner:case(open, 4, stream_open, 'ISO 8.11.5.4, XLOG 2') :-
   open('../data2.txt', write, _, [alias(editor)]),
   put_code(editor, 0xE54),
   close(editor).
runner:case(open, 4, stream_open, 'ISO 8.11.5.4, XLOG 3') :-
   open('../data2.txt', read, D, []),
   get_code(D, C),
   close(D),
   C == 0xE54.

/* close(X), ISO 8.11.6 */

/* close(X, O), ISO 8.11.6 */

/* stream_property(X, P), ISO 8.11.8 */

runner:ref(stream_property, 2, stream_open, 'ISO 8.11.8').
runner:case(stream_property, 2, stream_open, 'ISO 8.11.8, ISO 1') :-
   open('../data2.txt', read, D, [type(binary)]),
   stream_property(D, file_name(F)),
   close(D),
   F \== ''.
runner:case(stream_property, 2, stream_open, 'ISO 8.11.8, ISO 2') :-
   open('../data2.txt', write, D, [type(binary)]),
   put_byte(D, 65),
   stream_property(D, output),
   close(D).
runner:case(stream_property, 2, stream_open, 'ISO 8.11.8, XLOG 1') :-
   stream_property(_, input).
runner:case(stream_property, 2, stream_open, 'ISO 8.11.8, XLOG 3') :-
   open('../data2.txt', read, D, [type(binary),reposition(true)]),
   get_byte(D, _),
   stream_property(D, position(P)),
   close(D),
   P == 1.

/* set_stream_position(S, P), ISO 8.11.9 */

runner:ref(set_stream_position, 2, stream_open, 'ISO 8.11.9').
runner:case(set_stream_position, 2, stream_open, 'ISO 8.11.9, XLOG 1') :-
   open('../data2.txt', write, D, [type(binary)]),
   put_byte(D, 65),
   put_byte(D, 66),
   put_byte(D, 67),
   close(D),
   open('../data2.txt', read, E, [type(binary),reposition(true)]),
   set_stream_position(E, 1),
   get_byte(E, C),
   close(E),
   C == 66.
runner:case(set_stream_position, 2, stream_open, 'ISO 8.11.9, XLOG 2') :-
   open('../data2.txt', write, D, [type(binary),reposition(true)]),
   set_stream_position(D, 1),
   put_byte(D, 68),
   close(D),
   open('../data2.txt', read, E, [type(binary)]),
   get_byte(E, A),
   get_byte(E, B),
   get_byte(E, C),
   close(E),
   A == 65,
   B == 68,
   C == 67.
