/**
 * Prolog code for the agent.
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

:- use_package(foreign(example04)).
:- use_module(library(advanced/signal)).
:- use_module(library(system/uri)).
:- use_module(library(stream/console)).

% act(+Firstname, +Name, +AgeFrom, +AgeTo, +SalaryFrom, +SalaryTo, -Compound)
act(F, N, AF, AT, SF, ST, R) :-
    make_link('http://localhost:8080/deployweb/servlet/example01.Plain',
         ['firstname'-F,
          'name'-N,
          'agefrom'-AF,
          'ageto'-AT,
          'salaryfrom'-SF,
          'salaryto'-ST], '', L),
    setup_call_cleanup(open(L, read, T),
                       fetch(T, R),
                       close(T)).

% fetch(+Stream, -Compound)
fetch(T, employee(F,N,A,S)) :-
    read_line(T, _),
    repeat,
    (read_line(T, L) ->
        atom_split(L, '\t', [F,N,A,S]);
    !, fail).