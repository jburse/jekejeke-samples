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
:- use_module(library(system/domain)).

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

/* hex_block(T, E) */

runner:ref(hex_block, 2, extend_codec, 'XLOG 6.2').
runner:case(hex_block, 2, extend_codec, 'XLOG 6.2, XLOG 1') :-
   atom_block(foob, B),
   hex_block(C, B),
   C == '666F6F62'.
runner:case(hex_block, 2, extend_codec, 'XLOG 6.2, XLOG 2') :-
   hex_block('666F6F62', B),
   atom_block(C, B),
   C = foob.
runner:case(hex_block, 2, extend_codec, 'XLOG 6.2, XLOG 3') :-
   catch(hex_block(_, 123), error(E,_), true),
   E == type_error(ref,123).
runner:case(hex_block, 2, extend_codec, 'XLOG 6.2, XLOG 4') :-
   catch(hex_block(_, _), error(E,_), true),
   E == instantiation_error.
runner:case(hex_block, 2, extend_codec, 'XLOG 6.2, XLOG 5') :-
   catch(hex_block(foo, _), error(E,_), true),
   nonvar(E),
   E = representation_error(_).
runner:case(hex_block, 2, extend_codec, 'XLOG 6.2, XLOG 6') :-
   catch(hex_block('Ff', _), error(E,_), true),
   nonvar(E),
   E = representation_error(_).

/* base64_block(T, E) */

runner:ref(base64_block, 2, extend_codec, 'XLOG 6.3').
runner:case(base64_block, 2, extend_codec, 'XLOG 6.3, XLOG 1') :-
   atom_block(foob, B),
   base64_block(C, B),
   C == 'Zm9vYg=='.
runner:case(base64_block, 2, extend_codec, 'XLOG 6.3, XLOG 2') :-
   atom_block('The quick brown fox jumps over the lazy dog.', B),
   base64_block(C, B),
   C == 'VGhlIHF1aWNrIGJyb3duIGZveCBqdW1wcyBvdmVy\nIHRoZSBsYXp5IGRvZy4='.
runner:case(base64_block, 2, extend_codec, 'XLOG 6.3, XLOG 3') :-
   base64_block('Zm9vYg==', B),
   atom_block(C, B),
   C == foob.
runner:case(base64_block, 2, extend_codec, 'XLOG 6.3, XLOG 4') :-
   base64_block('VGhlIHF1aWNrIGJyb3duIGZveCBqdW1wcyBvdmVy\nIHRoZSBsYXp5IGRvZy4=', B),
   atom_block(C, B),
   C == 'The quick brown fox jumps over the lazy dog.'.
runner:case(base64_block, 2, extend_codec, 'XLOG 6.3, XLOG 5') :-
   catch(base64_block(_, 123), error(E,_), true),
   E == type_error(ref,123).
runner:case(base64_block, 2, extend_codec, 'XLOG 6.3, XLOG 6') :-
   catch(base64_block(_, _), error(E,_), true),
   E == instantiation_error.
runner:case(base64_block, 2, extend_codec, 'XLOG 6.3, XLOG 7') :-
   catch(base64_block(foo, _), error(E,_), true),
   nonvar(E),
   E = representation_error(_).
runner:case(base64_block, 2, extend_codec, 'XLOG 6.3, XLOG 8') :-
   catch(base64_block('Z_8=', _), error(E,_), true),
   nonvar(E),
   E = representation_error(_).

/* uri_puny(S, P) */

runner:ref(uri_puny, 2, extend_codec, 'XLOG 6.4').
runner:case(uri_puny, 2, extend_codec, 'XLOG 6.4, XLOG 1') :-
   uri_puny('http://zürich.ch/robots.txt', X),
   X == 'http://xn--zrich-kva.ch/robots.txt'.
runner:case(uri_puny, 2, extend_codec, 'XLOG 6.4, XLOG 2') :-
   uri_puny(X, 'http://xn--zrich-kva.ch/robots.txt'),
   X == 'http://zürich.ch/robots.txt'.
runner:case(uri_puny, 2, extend_codec, 'XLOG 6.4, XLOG 3') :-
   catch(uri_puny(_, 123), error(E,_), true),
   E == type_error(atom,123).
runner:case(uri_puny, 2, extend_codec, 'XLOG 6.4, XLOG 4') :-
   catch(uri_puny(_, _), error(E,_), true),
   E == instantiation_error.

/* sha1_hash(B, H) */

runner:ref(sha1_hash, 2, extend_codec, 'XLOG 6.5').
runner:case(sha1_hash, 2, extend_codec, 'XLOG 6.5, XLOG 1') :-
   atom_block('hello world', B),
   sha1_hash(B, C),
   hex_block(D, C),
   D == '2AAE6C35C94FCFB415DBE95F408B9CE91EE846ED'.
runner:case(sha1_hash, 2, extend_codec, 'XLOG 6.5, XLOG 2') :-
   atom_block('GeeksForGeeks', B),
   sha1_hash(B, C),
   hex_block(D, C),
   D == 'ADDF120B430021C36C232C99EF8D926AEA2ACD6B'.
runner:case(sha1_hash, 2, extend_codec, 'XLOG 6.5, XLOG 3') :-
   catch(sha1_hash(123, _), error(E,_), true),
   E == type_error(ref,123).
runner:case(sha1_hash, 2, extend_codec, 'XLOG 6.5, XLOG 4') :-
   catch(sha1_hash(_, _), error(E,_), true),
   E == instantiation_error.

/* term_atom(T, A, O) */

runner:ref(term_atom, 3, extend_codec, 'XLOG 6.6').
runner:case(term_atom, 3, extend_codec, 'XLOG 6.6, XLOG 1') :-
   term_atom(X, '[1,2,3]', [double_quotes(string)]),
   X == [1,2,3].
runner:case(term_atom, 3, extend_codec, 'XLOG 6.6, XLOG 2') :-
   term_atom('1<2', X, [double_quotes(string)]),
   X == '''1<2'''.
runner:case(term_atom, 3, extend_codec, 'XLOG 6.6, XLOG 3') :-
   term_atom(X, '"foo"', [double_quotes(string)]),
   X == '$STR'(foo).
runner:case(term_atom, 3, extend_codec, 'XLOG 6.6, XLOG 4') :-
   term_atom('$STR'(bar), X, [double_quotes(string)]),
   X == '"bar"'.
runner:case(term_atom, 3, extend_codec, 'XLOG 6.6, XLOG 5') :-
   term_atom('$STR'('\xFFFD\'), X, [double_quotes(string)]),
   X == '"\\uFFFD"'.
runner:case(term_atom, 3, extend_codec, 'XLOG 6.6, XLOG 6') :-
   term_atom(X, '"\\""', [double_quotes(string)]),
   X == '$STR'('"').
runner:case(term_atom, 3, extend_codec, 'XLOG 6.6, XLOG 7') :-
   catch(term_atom(_, "abc", [double_quotes(string)]), error(E,_), true),
   E == type_error(atom,[97,98,99]).
