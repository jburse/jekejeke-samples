/**
 * Prolog code for the extend codec test cases.
 *
 * Source of test cases are the following standards:
 *   - The HTML Syntax, "8.2.4.72. Character reference state"
 *     http://w3c.github.io/html/syntax.html#character-reference-state
 *   - RFC 4648, "The Base16, Base32, and Base64 Data Encodings", October 2006
 *     https://tools.ietf.org/html/rfc4648
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

:- use_module(library(stream/xml)).

/* text_escape(T, E) */

runner:ref(text_escape, 2, extend_codec, 'XLOG 6.1').
runner:case(text_escape, 2, extend_codec, 'XLOG 6.1, XLOG 1') :-
   text_escape(X, 'This is an "experiment"'),
   X = 'This is an "experiment"'.
runner:case(text_escape, 2, extend_codec, 'XLOG 6.1, XLOG 2') :-
   text_escape(X, 'This is an &quot;experiment&quot;'),
   X = 'This is an "experiment"'.
runner:case(text_escape, 2, extend_codec, 'XLOG 6.1, XLOG 3') :-
   text_escape(X, 'This is an &#x22;experiment&#x22;'),
   X = 'This is an "experiment"'.
runner:case(text_escape, 2, extend_codec, 'XLOG 6.1, XLOG 4') :-
   text_escape(X, 'This is an &#34;experiment&#34;'),
   X = 'This is an "experiment"'.
runner:case(text_escape, 2, extend_codec, 'XLOG 6.1, XLOG 5') :-
   text_escape('This is an "experiment"', X),
   X == 'This is an &quot;experiment&quot;'.
runner:case(text_escape, 2, extend_codec, 'XLOG 6.1, XLOG 6') :-
   catch(text_escape(123, _), error(E,_), true),
   E == type_error(atom,123).
runner:case(text_escape, 2, extend_codec, 'XLOG 6.1, XLOG 7') :-
   catch(text_escape(_, _), error(E,_), true),
   E == instantiation_error.

/* base64_block(T, E) */

runner:ref(base64_block, 2, extend_codec, 'XLOG 6.2').
runner:case(base64_block, 2, extend_codec, 'XLOG 6.2, XLOG 1') :-
   block_bytes(B, "foob"),
   base64_block(C, B),
   C == 'Zm9vYg=='.
runner:case(base64_block, 2, extend_codec, 'XLOG 6.2, XLOG 2') :-
   block_bytes(B, "The quick brown fox jumps over the lazy dog."),
   base64_block(C, B),
   C == 'VGhlIHF1aWNrIGJyb3duIGZveCBqdW1wcyBvdmVy\nIHRoZSBsYXp5IGRvZy4='.
runner:case(base64_block, 2, extend_codec, 'XLOG 6.2, XLOG 3') :-
   base64_block('Zm9vYg==', B),
   block_bytes(B, C),
   C == "foob".
runner:case(base64_block, 2, extend_codec, 'XLOG 6.2, XLOG 4') :-
   base64_block('VGhlIHF1aWNrIGJyb3duIGZveCBqdW1wcyBvdmVy\nIHRoZSBsYXp5IGRvZy4=', B),
   block_bytes(B, C),
   C == "The quick brown fox jumps over the lazy dog.".
runner:case(base64_block, 2, extend_codec, 'XLOG 6.2, XLOG 5') :-
   catch(block_bytes(_, foo), error(E,_), true),
   E == type_error(list,foo).
runner:case(base64_block, 2, extend_codec, 'XLOG 6.2, XLOG 6') :-
   catch(base64_block(_, _), error(E,_), true),
   E == instantiation_error.
