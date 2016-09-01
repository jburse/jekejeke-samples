/**
 * Prolog code for the stream read theory test cases.
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

:- use_module(library(stream/console)).
:- use_module(library(testing/charsio)).
:- use_module(library(advanced/sets)).

/****************************************************************/
/* Term Input / Output                                          */
/****************************************************************/

/* write(X), ISO 8.14.2.4 */

/* writeq(X) */

runner:ref(writeq, 1, stream_read, 'ISO 8.14.2.4, ISO 6.3.4.2, ISO 6.4.2').
runner:case(writeq, 1, stream_read, 1) :-
   with_output_to(atom(X), writeq('1<2')),
   X == '\'1<2\''.
runner:case(writeq, 1, stream_read, 2) :-
   with_output_to(atom(X), writeq('$VAR'(0))),
   X == 'A'.
runner:case(writeq, 1, stream_read, 3) :-
   op(9, fy, fy),
   op(9, yf, yf),
   with_output_to(atom(X), writeq(yf(fy(1)))),
   X == '(fy 1)yf'.
runner:case(writeq, 1, stream_read, 4) :-
   op(9, fy, fy),
   op(9, yfx, yfx),
   with_output_to(atom(X), writeq(yfx(fy(1),2))),
   X == '(fy 1)yfx 2'.
runner:case(writeq, 1, stream_read, 5) :-
   op(9, fy, fy),
   op(9, xfy, xfy),
   with_output_to(atom(X), writeq(yf(xfy(1,2)))),
   X == '(1 xfy 2)yf'.
runner:case(writeq, 1, stream_read, 6) :-
   with_output_to(atom(X), writeq(//*)),
   X == //* .

/* write_canonical(X) */

runner:ref(write_canonical, 1, stream_read, 'ISO 8.14.2.4, ISO 6.3.3.1').
runner:case(write_canonical, 1, stream_read, 1) :-
   with_output_to(atom(X), write_canonical([1,2,3])),
   X == '\'.\'(1,\'.\'(2,\'.\'(3,[])))'.
runner:case(write_canonical, 1, stream_read, 2) :-
   with_output_to(atom(X), write_canonical((a,b))),
   X == '\',\'(a,b)'.

/* write_term(X,L) */

runner:ref(write_term, 2, stream_read, 'ISO 8.14.2.4, ISO 6.3.3.1').
runner:case(write_term, 2, stream_read, 1) :-
   with_output_to(atom(X), write_term([1,2,3], [])),
   X == '[1,2,3]'.
runner:case(write_term, 2, stream_read, 2) :-
   with_output_to(atom(X), write_term('1<2', [])),
   X == '1<2'.
runner:case(write_term, 2, stream_read, 3) :-
   with_output_to(atom(X), write_term('$VAR'(1), [numbervars(false)])),
   X == '$VAR(1)'.
runner:case(write_term, 2, stream_read, 4) :-
   with_output_to(atom(X), write_term('$VAR'(51), [numbervars(true)])),
   X == 'Z1'.
runner:case(write_term, 2, stream_read, 5) :-
   with_output_to(atom(X), write_term(f(',',a), [quoted(true)])),
   X == 'f(\',\',a)'.
runner:case(write_term, 2, stream_read, 6) :-
   with_output_to(atom(X), write_term((a;b|c), [quoted(true)])),
   X == 'a;b|c'.

/* read(X) */

runner:ref(read, 1, stream_read, 'ISO 8.14.1.4, ISO 6.3.3.1').
runner:case(read, 1, stream_read, 'ISO 8.14.1.4, ISO 1a') :-
   with_input_from(atom('term1. term2. '), read(X)),
   X == term1.
runner:case(read, 1, stream_read, 'ISO 8.14.1.4, ISO 1b') :-
   findall(X, with_input_from(atom('term1. term2. '), (  read(_)
                                                      ;  read_line(X))), [_,X|_]),
   X == 'term2. '.
runner:case(read, 1, stream_read, 'ISO 8.14.1.4, ISO 4') :-
   \+ with_input_from(atom('3.1. term2. '), read(4.1)).
runner:case(read, 1, stream_read, 'ISO 8.14.1.4, ISO 5') :-
   catch(with_input_from(atom('foo 123. term2. '), read(_)), error(E,_), true),
   E = syntax_error(_).
runner:case(read, 1, stream_read, 'ISO 8.14.1.4, ISO 6') :-
   catch(with_input_from(atom('3.1 '), read(_)), error(E,_), true),
   E = syntax_error(_).
runner:case(read, 1, stream_read, 'ISO 8.14.1.4, XLOG 1') :-
   with_input_from(atom('3.1 '), (  catch(read(_), error(_,_), fail)
                                 ;  read_line(end_of_file))).
runner:case(read, 1, stream_read, 'ISO 6.3.3.1, XLOG 1') :-
   catch(with_input_from(atom('[a,b|,].'), read(_)), error(E,_), true),
   E == syntax_error(cannot_start_term).
runner:case(read, 1, stream_read, 'ISO 6.3.3.1, XLOG 2') :-
   catch(with_input_from(atom('{,}.'), read(_)), error(E,_), true),
   E == syntax_error(cannot_start_term).
runner:case(read, 1, stream_read, 'ISO 6.3.3.1, XLOG 3') :-
   catch(with_input_from(atom('\'\\N\'.'), read(_)), error(E,_), true),
   E == syntax_error(illegal_escape).
runner:case(read, 1, stream_read, 'ISO 6.3.3.1, XLOG 4') :-
   catch(with_input_from(atom('X = |.'), read(_)), error(E,_), true),
   E == syntax_error(cannot_start_term).

/* read_term(X,L) */

runner:ref(read_term, 2, stream_read, 'ISO 8.14.1.4, ISO 6.4.5, ISO 6.3.4.2').
runner:case(read_term, 2, stream_read, 1) :-
   with_input_from(atom('foo(A+Roger,A+_). term2. '), read_term(X,
                                                         [variables(VL),variable_names(VN),singletons(VS)])),
   X = foo(X1+X2,X1+X3),
   permutation(VL, [X1,X2,X3]),
   permutation(VN, ['A'=X1,'Roger'=X2]),
   VS == ['Roger'=X2].
runner:case(read_term, 2, stream_read, 2) :-
   findall(X, with_input_from(atom('term1. term2. '), (  read_term(X,
                                                            [variables(_),variable_names(_),singletons(_)])
                                                      ;  read_line(X))), [_,X|_]),
   X == 'term2. '.
runner:case(read_term, 2, stream_read, 3) :-
   op(9, xf, e),
   with_input_from(atom('1e-9.'), read_term(X, [])),
   X == e(1)-9.
runner:case(read_term, 2, stream_read, 4) :-
   op(9, xf, e),
   with_input_from(atom('1.0e- 9.'), read_term(X, [])),
   X == e(1.0)-9.
runner:case(read_term, 2, stream_read, 5) :-
   op(9, fy, fy),
   op(9, yf, yf),
   with_input_from(atom('fy 1 yf.'), read_term(X, [])),
   X == fy(yf(1)).
runner:case(read_term, 2, stream_read, 6) :-
   op(9, fy, fy),
   op(9, yfx, yfx),
   with_input_from(atom('fy 1 yfx 2.'), read_term(X, [])),
   X == fy(yfx(1,2)).
runner:case(read_term, 2, stream_read, 7) :-
   op(9, fy, fy),
   op(9, xfy, xfy),
   with_input_from(atom('1 xfy 2 yf.'), read_term(X, [])),
   X == xfy(1,yf(2)).

/* numbervars(X, N, M) */

