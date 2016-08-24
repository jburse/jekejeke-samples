/**
 * Ciao Prolog code for the test harness.
 * Use ensure_loaded/1 on toplevel so that code will be interpreted.
 *
 * Copyright 2011-2014, XLOG Technologies GmbH, Switzerland
 * Jekejeke Prolog 0.9.0 (a fast and small prolog interpreter)
 */

% :- ensure_loaded('C:\\Projects\\Jekejeke\\Prototyping\\samples\\jekrun\\benchmark\\harness\\ciao.p').

uptime(Y) :-
   statistics(walltime, [X|_]),
   Y is round(X).

gctime(S) :-
   statistics(garbage_collection, [_,_,T]),
   S is round(T).

:- ensure_loaded('../compat/ciaosuite.p').
