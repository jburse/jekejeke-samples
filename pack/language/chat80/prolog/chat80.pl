/**
 * Prolog text chattop from Chat80 as a module.
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

/**
 * Obtained rights comment in Prolog text and text from LICENSE file:
 *
 * Copyright 1986, Fernando C.N. Pereira and David H.D. Warren,
 *
 * All Rights Reserved
 *
 * This program may be used, copied, altered or included in other programs
 * only for academic purposes and provided that the authorship of the
 * initial program is acknowledged. Use for commercial purposes without the
 * previous written agreement of the authors is forbidden.
 */

:- module(chat80, [hi/0]).
:- current_prolog_flag(dialect, jekejeke)
-> use_module(library(preprocessor)); true.
:- use_module(readin).
:- use_module(database/chatops).
:- use_module(database/world0).
:- use_module(database/ndtabl).
:- use_module(natural/slots).
:- use_module(natural/newdic).
:- use_module(natural/newg).
:- use_module(natural/scopes).
:- use_module(natural/ptree).
:- use_module(natural/qplan).
:- use_module(natural/talkr).

/* ----------------------------------------------------------------------
	Top level for runtime version, and interactive demonstrations
   ---------------------------------------------------------------------- */

hi :-
   hi(user).

hi(File) :- repeat,
   ask(File, P),
   control(P), !,
   end(File).

ask(user, P) :- !,
   write('Question: '), flush_output,
   read_in(P).
% ask(File,P) :-
%    seeing(Old),
%    see(File),
%    read_in(P),
%    nl,
%    doing(P,0),
%    nl,
%    see(Old).
%
% doing([],_) :- !.
% doing([X|L],N0) :-
%    out(X),
%    advance(X,N0,N),
%    doing(L,N).
%
% out(nb(X)) :- !,
%    write(X).
% out(A) :-
%    write(A).
%
% advance(X,N0,N) :-
%    uses(X,K),
%    M is N0+K,
%  ( M>72, !,
%       nl,
%       N is 0;
%    N is M+1,
%       put(" ")).
%
% uses(nb(X),N) :- !,
%    chars(X,N).
% uses(X,N) :-
%    chars(X,N).
%
% chars(X,N) :- atomic(X), !,
%    name(X,L),
%    length(L,N).
% chars(_,2).

end(user) :- !.
% end(F) :-
%    close(F).

control([bye,'.']) :- !,
   write('Cheerio.'), nl.
control([trace,'.']) :- !,
   tracing ~= on,
   write('Tracing from now on!'), nl, fail.
control([do,not,trace,'.']) :- !,
   tracing ~= off,
   write('No longer tracing.'), nl, fail.
% control([do,mini,demo,'.']) :- !,
%    write('Executing mini demo...'), nl,
%    demo(mini), fail.
% control([do,main,demo,'.']) :- !,
%    write('Executing main demo...'), nl,
%    demo(main), fail.
% control([test,chat,'.']) :- !,
%    test_chat, fail.
control(U0) :-
   check_words(U0, U),
   process(U), fail.

process(U) :-
   uptime(TParse1),
   sentence(E, U, [], [], []),
   uptime(TParse2),
   TParse is TParse2-TParse1,
   report(E, 'Parse', TParse, tree),

   uptime(TSemantics1),
   i_sentence(E, QT),
   clausify(QT, UE),
   simplify(UE, S),
   uptime(TSemantics2),
   TSemantics is TSemantics2-TSemantics1,
   report(S, 'Semantics', TSemantics, expr),

   uptime(TPlanning1),
   qplan(S, S1), !,
   uptime(TPlanning2),
   TPlanning is TPlanning2-TPlanning1,
   report(S1, 'Planning', TPlanning, expr),

   uptime(TReply1),
   answer(S1), !, nl,
   uptime(TReply2),
   TReply is TReply2-TReply1,
   report(_, 'Reply', TReply, none).
process(_) :- failure.

failure :-
   write('I don''t understand!'), nl.

report(Item, Label, Time, Mode) :-
   tracing =: on, !, nl,
   write(Label),
   write(': '),
   write(Time),
   write('msec.'), nl,
   report_item(Mode, Item).
report(_, _, _, _).

report_item(none, _).
report_item(expr, Item) :-
   write_tree(Item), nl.
report_item(tree, Item) :-
   print_tree(Item), nl.
% report_item(quant,Item) :-
%    pp_quant(Item,2), nl.

:- if(current_prolog_flag(dialect,jekejeke)).

uptime(X) :-
   statistics(uptime, X).

:- else.

uptime(T) :-
   statistics(walltime, [T|_]).

:- endif.
