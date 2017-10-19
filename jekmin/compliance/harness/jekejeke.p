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

:- use_package(library(jekdev/reference/testing)).

:- sys_init_capability('jekmin.platform.headless.CapabilityMinlog').

% :- sys_add_path('file:/Projects/Jekejeke/Prototyping/experiment/other/clp/').

:- ensure_loaded(suite).
:- use_module(library(testing/runner)).
:- use_module(library(testing/diagnose)).
:- use_module(library(testing/result)).
:- use_module(library(testing/tracker)).
:- use_module(library(testing/cover)).
:- use_module(library(testing/summary)).

/****************************************************************/
/* Results                                                      */
/****************************************************************/

% run_test
run_test :-
   write('% running test cases.'), nl,
   time(runner_batch).

% run_diagnose
run_diagnose :- diagnose_online.

% run_report
run_report :-
   set_prolog_flag(sys_locale, de),
   set_prolog_flag(base_url, '/Projects/Shop/Prototyping2/webapps/idatab/prod/docs/15_min/15_stdy/07_compliance/09_results/'),
   result_batch('../../../../../../../blog/docs/15_min/07_compliance/'),
   set_prolog_flag(sys_locale, en),
   set_prolog_flag(base_url, '/Projects/Shop/Prototyping2/webapps/idatab/prod/en/docs/15_min/15_stdy/07_compliance/09_results/'),
   result_batch('../../../../../../../../blog/en/docs/15_min/07_compliance/').

/****************************************************************/
/* Coverage                                                     */
/****************************************************************/

:- multifile tracker:text/1.

tracker:text(library(finite/clpfd)).
tracker:text(library(finite/intset)).
tracker:text(library(finite/linform)).
tracker:text(library(finite/reify)).
tracker:text(library(finite/enum)).

tracker:text(library(finite/clpb)).
tracker:text(library(finite/tree)).

tracker:text(library(misc/elem)).

tracker:text(library(term/herbrand)).
tracker:text(library(term/suspend)).
tracker:text(library(term/verify)).
tracker:text(library(term/unify)).
tracker:text(library(term/state)).

% run_tracker
run_tracker :-
   write('% performing coverage analysis.'), nl,
   time(tracker_batch), analyze_batch.

% run_control
run_control :- list_cover_source.

% run_cover
run_cover :-
   set_prolog_flag(sys_locale, de),
   set_prolog_flag(base_url, '/Projects/Shop/Prototyping2/webapps/idatab/prod/docs/15_min/15_stdy/07_compliance/07_coverage/'),
   cover_batch('../../../../../../../blog/docs/15_min/02_reference/'),
   set_prolog_flag(sys_locale, en),
   set_prolog_flag(base_url, '/Projects/Shop/Prototyping2/webapps/idatab/prod/en/docs/15_min/15_stdy/07_compliance/07_coverage/'),
   cover_batch('../../../../../../../../blog/en/docs/15_min/02_reference/').

% run_summary
run_summary :-
   set_prolog_flag(sys_locale, de),
   set_prolog_flag(base_url, '/Projects/Shop/Prototyping2/webapps/idatab/prod/docs/15_min/15_stdy/07_compliance/'), summary_batch,
   set_prolog_flag(sys_locale, en),
   set_prolog_flag(base_url, '/Projects/Shop/Prototyping2/webapps/idatab/prod/en/docs/15_min/15_stdy/07_compliance/'), summary_batch.
