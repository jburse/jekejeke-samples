/**
 * Bidirectional parser via CLP(FD) in Prolog.
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

/**
 * Obtained rights, copyright notice found in ZIP by Michael Hendricks
 * http://github.com/mndrix/roman/archive/v0.1.0.zip from 2015-07-10:
 * This is free and unencumbered software released into the public domain.
 *
 * Anyone is free to copy, modify, publish, use, compile, sell, or
 * distribute this software, either in source code form or as a compiled
 * binary, for any purpose, commercial or non-commercial, and by any
 * means.
 *
 * In jurisdictions that recognize copyright laws, the author or authors
 * of this software dedicate any and all copyright interest in the
 * software to the public domain. We make this dedication for the benefit
 * of the public at large and to the detriment of our heirs and
 * successors. We intend this dedication to be an overt act of
 * relinquishment in perpetuity of all present and future rights to this
 * software under copyright law.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
 * OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
 * ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
 * OTHER DEALINGS IN THE SOFTWARE.
 *
 * For more information, please refer to http://unlicense.org/
 */

:- module(roman, [roman/2]).

:- current_prolog_flag(dialect, jekejeke)
-> use_module(library(standard/dcg)); true.
:- current_prolog_flag(dialect, jekejeke)
-> use_module(library(finite/clpfd)); true.
:- current_prolog_flag(dialect, swi)
-> use_module(library(clpfd)); true.

% roman(+-Integer, -+List)
roman(Arabic, Roman) :-
   phrase(digits(Arabic), Roman).

% digits(+-Integer, -+List, +-List)
digits(Arabic) -->
   {Arabic in 1..3999},
   digits2(Arabic), !.

% digits2(+-Integer, -+List, +-List)
digits2(Total) -->
   {Rest #>= 0, Total #= Value+Rest},
   digit(Value),
   digits2(Rest).
digits2(0) --> [].

% digit(+-Integer, -+List, +-List)
digit(1000) --> "M".
digit(900) --> "CM".
digit(500) --> "D".
digit(400) --> "CD".
digit(100) --> "C".
digit(90) --> "XC".
digit(50) --> "L".
digit(40) --> "XL".
digit(10) --> "X".
digit(9) --> "IX".
digit(5) --> "V".
digit(4) --> "IV".
digit(1) --> "I".

% ?- roman(14, X), atom_codes(Y, X).
% X = [88,73,86],
% Y = 'XIV'

% ?- atom_codes('XIV', Y), roman(X, Y).
% Y = [88, 73, 86],
% X = 14.