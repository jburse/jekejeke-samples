/**
 * Prolog code for the trace example.
 *
 * Copyright 2012, XLOG Technologies GmbH, Switzerland
 * Jekejeke Prolog 0.9.7 (a fast and small prolog interpreter)
 */

p.
r :- p.
q :- p.
s :- q, r.

% ?- s.
% Yes

% ?- trace.
% Yes

% ?- s.
%     0 Call s ?
%     1 Call q ?