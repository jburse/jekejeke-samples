/**
 * Prolog code for the stream byte theory test cases.
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

:- use_module(library(system/charsio)).

/****************************************************************/
/* Byte Input / Output                                          */
/****************************************************************/

/* put_byte(B) */

runner:ref(put_byte, 1, stream_binary, 'ISO 8.13.3').
runner:case(put_byte, 1, stream_binary, 'ISO 8.13.3, ISO 1') :-
   with_output_to(bytes(L), put_byte(116)),
   L == [116].
runner:case(put_byte, 1, stream_binary, 'ISO 8.13.3, ISO 3') :-
   catch(put_byte(_), error(E,_), true),
   nonvar(E),
   E = instantiation_error.
runner:case(put_byte, 1, stream_binary, 'ISO 8.13.3, ISO 4') :-
   catch(put_byte(ty), error(E,_), true),
   nonvar(E),
   E = type_error(_,ty).

/* peek_byte(B) */

runner:ref(peek_byte, 1, stream_binary, 'ISO 8.13.2').
runner:case(peek_byte, 1, stream_binary, 'ISO 8.13.2, ISO 1') :-
   with_input_from(bytes("qwerty "), peek_byte(B)),
   B == 113.
runner:case(peek_byte, 1, stream_binary, 'ISO 8.13.2, ISO 3a') :-
   \+ with_input_from(bytes("qwerty "), peek_byte(114)).
runner:case(peek_byte, 1, stream_binary, 'ISO 8.13.2, ISO 3b') :-
   with_input_from(bytes("qwerty "), (  peek_byte(114)
                                     ;  get_byte(B))),
   B == 113.
runner:case(peek_byte, 1, stream_binary, 'ISO 8.13.2, ISO 4') :-
   with_input_from(bytes(""), peek_byte(B)),
   B == -1.

/* peek_byte(S,B) */

runner:ref(peek_byte, 2, stream_binary, 'ISO 8.13.2').
runner:case(peek_byte, 2, stream_binary, 'ISO 8.13.2, ISO 5') :-
   current_output(O),
   catch(peek_byte(O, _), error(E,_), true),
   nonvar(E),
   E = permission_error(_,_,_).

/* get_byte(B) */

runner:ref(get_byte, 1, stream_binary, 'ISO 8.13.1').
runner:case(get_byte, 1, stream_binary, 'ISO 8.13.1, ISO 1') :-
   with_input_from(bytes("qwerty "), get_byte(B)),
   B == 113.
runner:case(get_byte, 1, stream_binary, 'ISO 8.13.1, ISO 3a') :-
   \+ with_input_from(bytes("qwerty "), get_byte(114)).
runner:case(get_byte, 1, stream_binary, 'ISO 8.13.1, ISO 3b') :-
   with_input_from(bytes("qwerty "), (  get_byte(114)
                                     ;  get_byte(B))),
   B == 119.
runner:case(get_byte, 1, stream_binary, 'ISO 8.13.1, ISO 4') :-
   with_input_from(bytes(""), get_byte(B)),
   B == -1.

/* get_byte(S,B) */

runner:ref(get_byte, 2, stream_binary, 'ISO 8.13.1').
runner:case(get_byte, 2, stream_binary, 'ISO 8.13.1, ISO 5') :-
   current_output(O),
   catch(get_byte(O, _), error(E,_), true),
   nonvar(E),
   E = permission_error(_,_,_).

/* flush_output, ISO 8.11.7 */

/* at_end_of_stream, ISO 8.11.8 */

runner:ref(at_end_of_stream, 0, stream_binary, 'ISO 8.11.8').
runner:case(at_end_of_stream, 0, stream_binary, 'ISO 8.11.8, XLOG 1') :-
   \+ with_input_from(bytes("qwerty"), at_end_of_stream).
runner:case(at_end_of_stream, 0, stream_binary, 'ISO 8.11.8, XLOG 2') :-
   with_input_from(atom(''), at_end_of_stream).

