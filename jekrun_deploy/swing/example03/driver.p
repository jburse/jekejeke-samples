/**
 * Prolog code for the driver.
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

:- use_package(foreign(jekpro/tools/call)).
:- use_package(foreign(java/sql)).
:- use_package(foreign(example03)).
:- use_module(library(advanced/signal)).

% drive(+Firstname, +Name, +AgeFrom, +AgeTo, +SalaryFrom, +SalaryTo, -Compound)
drive(F, N, AF, AT, SF, ST, employee(X, Y, Z, T)) :-
   str_cond(F, 'firstname = ', [], L1),
   str_cond(N, 'name = ', L1, L2),
   num_cond(AF, 'age >= ', L2, L3),
   num_cond(AT, 'age <= ', L3, L4),
   num_cond(SF, 'salary >= ', L4, L5),
   num_cond(ST, 'salary <= ', L5, L6),
   make_where(L6, W),
   atom_concat('SELECT * FROM employee', W, Q),
   setup_call_cleanup(create_statement(S),
      execute_query(S, Q, [X, Y, Z, T]),
      close_statement(S)).

% str_cond(+String, +ColumnOperator, +WhereList, -WhereList).
str_cond('', _, W, W) :- !.
str_cond(L, CO, W, [COE|W]) :-
   literal_encode(L, E),
   atom_concat(CO, E, COE).

% num_cond(+Number, +ColumnOperator, +WhereList, -WhereList).
num_cond('', _, W, W) :- !.
num_cond(N, CO, W, [CON|W]) :-
   atom_concat(CO, N, CON).

% make_where(+WhereList, -WhereClause)
make_where([], '').
make_where([COL|W], S3) :-
   make_rest(W, S1),
   atom_concat(S1, COL, S2),
   atom_concat(' WHERE ', S2, S3).

% make_rest(+WhereList, -AndCondition).
make_rest([], '').
make_rest([COL|W], S3) :-
   make_rest(W, S1),
   atom_concat(S1, COL, S2),
   atom_concat(S2, ' AND ', S3).

/***********************************************************/
/* Foreign Functions                                       */
/***********************************************************/

% create_statement(-Statement)
:- foreign(create_statement/1, 'StatementAPI', createStatement).

% execute_query(+Statement, +String, -List)
:- foreign(execute_query/3, 'StatementAPI',
      executeQuery('CallOut', 'Statement', 'String')).

% close_statement(+Statement)
:- foreign(close_statement/1, 'StatementAPI', closeStatement('Statement')).

% liiteral_encode(+String, -String)
:- foreign(literal_encode/2, 'StatementAPI', literalEncode('String')).
