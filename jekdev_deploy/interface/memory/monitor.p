/**
 * Prolog code for the memory monitor.
 *
 * Copyright 2012, XLOG Technologies GmbH, Switzerland
 * Jekejeke Prolog 0.9.3 (a fast and small prolog interpreter)
 */

:- use_package(foreign(memory)).

:- foreign(start_monitor/0, 'MemoryFrame', startMonitor).
:- foreign(gc/0, 'MemoryFrame', gc).

