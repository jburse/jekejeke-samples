/**
 * Prolog code for the stream char theory test cases.
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
/* Character Input / Output                                     */
/****************************************************************/

/* nl */

runner:ref(nl, 0, stream_text, 'ISO 8.12.3.4').
runner:case(nl, 0, stream_text, 'ISO 8.12.3.4, ISO 5') :-
   with_output_to(atom(X), nl),
   (  X == '\r'
   ;  X == '\r\n'
   ;  X == '\n').

/* put_char(C) */

runner:ref(put_char, 1, stream_text, 'ISO 8.12.3.4').
runner:case(put_char, 1, stream_text, 'ISO 8.12.3.4, ISO 1') :-
   with_output_to(atom(X), put_char(t)),
   X == t.
runner:case(put_char, 1, stream_text, 'ISO 8.12.3.4, ISO 8') :-
   catch(put_char(_), error(E,_), true),
   E == instantiation_error.
runner:case(put_char, 1, stream_text, 'ISO 8.12.3.4, ISO 9') :-
   catch(put_char(ty), error(E,_), true),
   E == type_error(character,ty).

/* put_code(C) */

runner:ref(put_code, 1, stream_text, 'ISO 8.12.3.4').
runner:case(put_code, 1, stream_text, 'ISO 8.12.3.4, ISO 3') :-
   with_output_to(atom(X), put_code(116)),
   X == t.
runner:case(put_code, 1, stream_text, 'ISO 8.12.3.4, ISO 10') :-
   catch(put_code(_), error(E,_), true),
   E == instantiation_error.
runner:case(put_code, 1, stream_text, 'ISO 8.12.3.4, ISO 11') :-
   catch(put_code(ty), error(E,_), true),
   E == type_error(integer,ty).

/* peek_char(C) */

runner:ref(peek_char, 1, stream_text, 'ISO 8.12.2.4').
runner:case(peek_char, 1, stream_text, 'ISO 8.12.2.4, ISO 1') :-
   with_input_from(atom('qwerty '), peek_char(X)),
   X == q.
runner:case(peek_char, 1, stream_text, 'ISO 8.12.2.4, XLOG 1') :-
   with_input_from(atom('qwerty '), (  peek_char(_),
                                       get_char(X))),
   X == q.
runner:case(peek_char, 1, stream_text, 'ISO 8.12.2.4, ISO 5') :-
   with_input_from(atom('''qwerty'' '), peek_char(X)),
   X == ''''.
runner:case(peek_char, 1, stream_text, 'ISO 8.12.2.4, ISO 8') :-
   \+ with_input_from(atom('qwerty '), peek_char(p)).
runner:case(peek_char, 1, stream_text, 'ISO 8.12.2.4, ISO 10') :-
   with_input_from(atom(''), peek_char(X)),
   X == end_of_file.

/* peek_code(C) */

runner:ref(peek_code, 1, stream_text, 'ISO 8.12.2.4').
runner:case(peek_code, 1, stream_text, 'ISO 8.12.2.4, ISO 2') :-
   with_input_from(atom('qwerty '), peek_code(X)),
   X == 113.
runner:case(peek_code, 1, stream_text, 'ISO 8.12.2.4, XLOG 2') :-
   with_input_from(atom('qwerty '), (  peek_code(_),
                                       get_code(X))),
   X == 113.
runner:case(peek_code, 1, stream_text, 'ISO 8.12.2.4, ISO 6') :-
   with_input_from(atom('''qwerty'' '), peek_code(X)),
   X == 39.
runner:case(peek_code, 1, stream_text, 'ISO 8.12.2.4, ISO 9') :-
   \+ with_input_from(atom('qwerty '), peek_code(112)).
runner:case(peek_code, 1, stream_text, 'ISO 8.12.2.4, ISO 11') :-
   with_input_from(atom(''), peek_code(X)),
   X == -1.

/* get_char(C) */

runner:ref(get_char, 1, stream_text, 'ISO 8.12.1.4').
runner:case(get_char, 1, stream_text, 'ISO 8.12.1.4, ISO 1') :-
   with_input_from(atom('qwerty '), get_char(X)),
   X == q.
runner:case(get_char, 1, stream_text, 'ISO 8.12.1.4, XLOG 1') :-
   with_input_from(atom('qwerty '), (  get_char(_),
                                       get_char(X))),
   X == w.
runner:case(get_char, 1, stream_text, 'ISO 8.12.1.4, ISO 5') :-
   with_input_from(atom('''qwerty'' '), get_char(X)),
   X == ''''.
runner:case(get_char, 1, stream_text, 'ISO 8.12.1.4, ISO 7') :-
   \+ with_input_from(atom('qwerty '), get_char(p)).
runner:case(get_char, 1, stream_text, 'ISO 8.12.1.4, ISO 9') :-
   with_input_from(atom(''), get_char(X)),
   X == end_of_file.

/* get_code(C) */

runner:ref(get_code, 1, stream_text, 'ISO 8.12.1.4').
runner:case(get_code, 1, stream_text, 'ISO 8.12.1.4, ISO 2') :-
   with_input_from(atom('qwerty '), get_code(X)),
   X == 113.
runner:case(get_code, 1, stream_text, 'ISO 8.12.1.4, XLOG 2') :-
   with_input_from(atom('qwerty '), (  get_code(_),
                                       get_code(X))),
   X == 119.
runner:case(get_code, 1, stream_text, 'ISO 8.12.1.4, ISO 6') :-
   with_input_from(atom('''qwerty'' '), get_code(X)),
   X == 39.
runner:case(get_code, 1, stream_text, 'ISO 8.12.1.4, ISO 8') :-
   \+ with_input_from(atom('qwerty '), get_code(112)).
runner:case(get_code, 1, stream_text, 'ISO 8.12.1.4, ISO 10') :-
   with_input_from(atom(''), get_code(X)),
   X == -1.

