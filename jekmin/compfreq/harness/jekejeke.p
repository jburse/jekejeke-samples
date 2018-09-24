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
 * Restrictions
 * Only to be distributed with programs that add significant and primary
 * functionality to the library. Not to be distributed with additional
 * software intended to replace any components of the library.
 *
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */

:- use_package(library(jekdev/reference/testing)).

% :- use_module(library(bootload/toolkit)).
% :- sys_init_capability('jekmin.platform.headless.CapabilityMinlog').

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
   set_prolog_flag(base_url, '/D:/Projects/Shop/Prototyping3/webapps/idatab/prod/docs/15_min/15_stdy/08_compfreq/09_results/'),
   result_batch('../../../../../../../repo/docs/15_min/08_compfreq/'),
   set_prolog_flag(sys_locale, en),
   set_prolog_flag(base_url, '/D:/Projects/Shop/Prototyping3/webapps/idatab/prod/en/docs/15_min/15_stdy/08_compfreq/09_results/'),
   result_batch('../../../../../../../../repo/en/docs/15_min/08_compfreq/').

/****************************************************************/
/* Coverage                                                     */
/****************************************************************/

:- multifile tracker:text/1.

tracker:text(library(decimal/helper)).
tracker:text(library(decimal/multi)).
tracker:text(library(decimal/poly)).
tracker:text(library(decimal/trigo)).
tracker:text(library(groebner/generic)).
tracker:text(library(groebner/integer)).
tracker:text(library(groebner/rational)).
tracker:text(library(groebner/variable)).
tracker:text(library(groebner/polynom)).
tracker:text(library(groebner/fraction)).
tracker:text(library(gauss/element)).
tracker:text(library(gauss/vector)).
tracker:text(library(gauss/matrice)).
tracker:text(library(gauss/ordered)).
tracker:text(library(gauss/ring)).
tracker:text(library(leibniz/subst)).
tracker:text(library(leibniz/deriv)).
tracker:text(library(leibniz/series)).
tracker:text(library(leibniz/radical)).

% run_tracker
run_tracker :-
   write('% performing coverage analysis.'), nl,
   time(tracker_batch), analyze_batch.

% run_control
run_control :- list_cover_source.

% run_cover
run_cover :-
   set_prolog_flag(sys_locale, de),
   set_prolog_flag(base_url, '/D:/Projects/Shop/Prototyping3/webapps/idatab/prod/docs/15_min/15_stdy/08_compfreq/07_coverage/'),
   cover_batch('../../../../../../../blog/docs/15_min/04_frequent/'),
   set_prolog_flag(sys_locale, en),
   set_prolog_flag(base_url, '/D:/Projects/Shop/Prototyping3/webapps/idatab/prod/en/docs/15_min/15_stdy/08_compfreq/07_coverage/'),
   cover_batch('../../../../../../../../blog/en/docs/15_min/04_frequent/').

% run_summary
run_summary :-
   set_prolog_flag(sys_locale, de),
   set_prolog_flag(base_url, '/D:/Projects/Shop/Prototyping3/webapps/idatab/prod/docs/15_min/15_stdy/08_compfreq/'), summary_batch,
   set_prolog_flag(sys_locale, en),
   set_prolog_flag(base_url, '/D:/Projects/Shop/Prototyping3/webapps/idatab/prod/en/docs/15_min/15_stdy/08_compfreq/'), summary_batch.
