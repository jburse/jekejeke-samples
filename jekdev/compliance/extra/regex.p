/**
 * Prolog code for the msic text test cases.
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

:- use_module(library(misc/text)).

/* char_type(C, N) */
runner:ref(char_type, 2, extra_regex, 'XLOG 1.2.1').
runner:case(char_type, 2, extra_regex, 'XLOG 1.2.1, XLOG 1') :-
   char_type(' ', N),
   N == white.
runner:case(char_type, 2, extra_regex, 'XLOG 1.2.1, XLOG 2') :-
   char_type('\n', N),
   N == control.
runner:case(char_type, 2, extra_regex, 'XLOG 1.2.1, XLOG 3') :-
   char_type('\xFFFE\', N),
   N == inval.
runner:case(char_type, 2, extra_regex, 'XLOG 1.2.1, XLOG 4') :-
   char_type(!, N),
   N == solo.
runner:case(char_type, 2, extra_regex, 'XLOG 1.2.1, XLOG 5') :-
   char_type('A', N),
   N == upper.

/* code_type(C, N) */
runner:ref(code_type, 2, extra_regex, 'XLOG 1.2.2').
runner:case(code_type, 2, extra_regex, 'XLOG 1.2.2, XLOG 1') :-
   code_type(0'_, N),
   N == score.
runner:case(code_type, 2, extra_regex, 'XLOG 1.2.2, XLOG 2') :-
   code_type(0'a, N),
   N == lower.
runner:case(code_type, 2, extra_regex, 'XLOG 1.2.2, XLOG 3') :-
   code_type(0'¼, N),
   N == other.
runner:case(code_type, 2, extra_regex, 'XLOG 1.2.2, XLOG 4') :-
   code_type(0'0, N),
   N == digit.
runner:case(code_type, 2, extra_regex, 'XLOG 1.2.2, XLOG 5') :-
   code_type(0'=, N),
   N == graph.

/* code_upper(C, D) */
runner:ref(code_upper, 2, extra_regex, 'XLOG 1.2.3').

/* code_lower(C, D) */
runner:ref(code_lower, 2, extra_regex, 'XLOG 1.2.4').

/* atom_upper(A, B) */
runner:ref(atom_upper, 2, extra_regex, 'XLOG 1.2.5').

/* atom_lower(A, B) */
runner:ref(atom_lower, 2, extra_regex, 'XLOG 1.2.6').

/* pattern_match(S, P) */
runner:ref(pattern_match, 2, extra_regex, 'XLOG 1.2.7').

/* pattern_replace(S, P, R, T) */
runner:ref(pattern_replace, 4, extra_regex, 'XLOG 1.2.8').

/* last_pattern_replace(S, P, R, T) */
runner:ref(last_pattern_replace, 4, extra_regex, 'XLOG 1.2.9').
