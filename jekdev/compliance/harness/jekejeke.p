/**
 * Jekejeke Prolog code for the compliance diagnosis.
 *
 * Copyright 2011-2016, XLOG Technologies GmbH, Switzerland
 * Jekejeke Prolog 0.9.0 (a fast and small prolog interpreter)
 */

:- ensure_loaded(suite).
:- use_module(library(testing/runner)).
:- use_module(library(testing/diagnose)).
:- use_module(library(testing/result)).

% run_test
run_test :-
   set_prolog_flag(base_url, '/Projects/Jekejeke/Prototyping/samples/jekdev/compliance/classes/stream'), runner_batch.

% run_diagnose
run_diagnose :- nl, diagnose_online.

% run_report
run_report :- nl,
   set_prolog_flag(sys_locale, de),
   set_prolog_flag(base_url, '/Projects/Shop/Prototyping/webapps/idatab/prod/docs/10_dev/15_stdy/07_compliance/09_results/'),
   result_batch('../../../../../../../blog/docs/10_dev/07_compliance/'),
   set_prolog_flag(sys_locale, en),
   set_prolog_flag(base_url, '/Projects/Shop/Prototyping/webapps/idatab/prod/en/docs/10_dev/15_stdy/07_compliance/09_results/'),
   result_batch('../../../../../../../../blog/en/docs/10_dev/07_compliance/').
