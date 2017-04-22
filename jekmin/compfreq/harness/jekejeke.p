/**
 * Jekejeke Prolog code for the compliance diagnosis.
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

:- ensure_loaded(suite).
:- ensure_loaded(util).
:- use_module(library(testing/runner)).
:- use_module(library(testing/diagnose)).
:- use_module(library(testing/result)).

uptime(X) :-
   statistics(uptime, X).

gctime(X) :-
   statistics(gctime, X).

% run_test
run_test :-
   bench(runner_batch).

% run_diagnose
run_diagnose :- diagnose_online.

% run_report
run_report :-
   set_prolog_flag(sys_locale, de),
   set_prolog_flag(base_url, '/Projects/Shop/Prototyping/webapps/idatab/prod/docs/15_min/15_stdy/08_compfreq/09_results/'),
   result_batch('../../../../../../../blog/docs/15_min/08_compfreq/'),
   set_prolog_flag(sys_locale, en),
   set_prolog_flag(base_url, '/Projects/Shop/Prototyping/webapps/idatab/prod/en/docs/15_min/15_stdy/08_compfreq/09_results/'),
   result_batch('../../../../../../../../blog/en/docs/15_min/08_compfreq/').
