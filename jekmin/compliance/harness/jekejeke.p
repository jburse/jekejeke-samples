/**
 * Jekejeke Prolog code for the compliance diagnosis.
 *
 * Copyright 2011-2016, XLOG Technologies GmbH, Switzerland
 * Jekejeke Prolog 0.9.0 (a fast and small prolog interpreter)
 */

:- use_package(library(jekdev/reference/testing)).

:- sys_init_capability('jekmin.platform.headless.CapabilityMinlog').

% :- sys_add_path('file:/Projects/Jekejeke/Prototyping/experiment/other/clp/').

:- ensure_loaded(suite).
:- ensure_loaded(util).
:- use_module(library(testing/runner)).
:- use_module(library(testing/diagnose)).
:- use_module(library(testing/result)).
:- use_module(library(testing/tracker)).
:- use_module(library(testing/cover)).

:- multifile tracker:text/1.

tracker:text(library(finite/clpfd)).
tracker:text(library(finite/intset)).
tracker:text(library(finite/linform)).
tracker:text(library(finite/reify)).
tracker:text(library(finite/enum)).

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
   set_prolog_flag(base_url, '/Projects/Shop/Prototyping/webapps/idatab/prod/docs/15_min/15_stdy/07_compliance/09_results/'),
   result_batch('../../../../../../../blog/docs/15_min/07_compliance/'),
   set_prolog_flag(sys_locale, en),
   set_prolog_flag(base_url, '/Projects/Shop/Prototyping/webapps/idatab/prod/en/docs/15_min/15_stdy/07_compliance/09_results/'),
   result_batch('../../../../../../../../blog/en/docs/15_min/07_compliance/').

% run_tracker
run_tracker :-
   text(X),
   absolute_file_name(X, Y),
   reset_source_property(Y, sys_notrace), fail.
run_tracker :-
   bench(tracker_batch), analyze_batch, list_cover_source.

% run_cover
run_cover :-
   set_prolog_flag(sys_locale, de),
   set_prolog_flag(base_url, '/Projects/Shop/Prototyping/webapps/idatab/prod/docs/15_min/15_stdy/07_compliance/07_coverage/'),
   cover_batch('../../../../../../../blog/docs/15_min/02_reference/'),
   set_prolog_flag(sys_locale, en),
   set_prolog_flag(base_url, '/Projects/Shop/Prototyping/webapps/idatab/prod/en/docs/15_min/15_stdy/07_compliance/07_coverage/'),
   cover_batch('../../../../../../../../blog/en/docs/15_min/02_reference/').
