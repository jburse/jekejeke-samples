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
 * https://www.kaggle.com/nltkdata/chat-80/home
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
-> use_module(library(edinburgh)); true.
:- current_prolog_flag(dialect, jekejeke)
-> use_module(library(preprocessor)); true.
:- current_prolog_flag(dialect, jekejeke)
-> use_module(library(advanced/arith)); true.
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
out(nb(X)) :- !,
   write(X).
out(A) :-
   write(A).
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
control([do,mini,demo,'.']) :- !,
   write('Executing mini demo...'), nl,
   demo(mini), fail.
control([do,main,demo,'.']) :- !,
   write('Executing main demo...'), nl,
   demo(main), fail.
control([test,chat,'.']) :- !, test_chat, fail.
control(U0) :-
   check_words(U0, U),
   process(U), fail.

process(U) :-
   sentence(E, U, [], [], []), !,
   report(E, 'Parse', tree),

   i_sentence(E, QT),
   clausify(QT, UE),
   simplify(UE, S), !,
   report(S, 'Semantics', expr),

   qplan(S, S1), !,
   report(S1, 'Planning', expr),

   answer(S1), !, nl,
   report(_, 'Reply', none).
process(_) :- failure.

failure :-
   write('I don''t understand!'), nl.

report(Item, Label, Mode) :-
   tracing =: on, !,
   write(Label),
   write(': '), nl,
   report_item(Mode, Item).
report(_, _, _).

report_item(none, _).
report_item(expr, Item) :-
   write_tree(Item), nl.
report_item(tree, Item) :-
   print_tree(Item), nl.
% report_item(quant,Item) :-
%    pp_quant(Item,2), nl.

/* ----------------------------------------------------------------------
	Simple questions
	These question do not require setof/3 and are useful for early
	testing of a system.
   ---------------------------------------------------------------------- */

eg([does,america,contain,new_york,?]).
eg([does,mexico,border,the,united_states,?]).
eg([is,the,population,of,china,greater,than,nb(200),million,?]).
eg([does,the,population,of,china,exceed,nb(1000),million,?]).
eg([is,the,population,of,china,nb(840),million,?]).
eg([does,the,population,of,china,exceed,the,population,of,
      india,?]).
eg([is,spain,bordered,by,the,pacific,?]).
eg([does,the,atlantic,border,spain,?]).
eg([is,the,rhine,in,switzerland,?]).
eg([is,the,united_kingdom,in,europe,?]).

/* ----------------------------------------------------------------------
	Standard question set
	This is the standard chat question set, originally put together
	by David and Fernando and use in their papers. Quintus uses this
	set as a standard for performance comparisons.
   ---------------------------------------------------------------------- */

ed(1, [what,rivers,are,there,?],

   [amazon,amu_darya,amur,brahmaputra,colorado,
      congo_river,cubango,danube,don,elbe,euphrates,ganges,
      hwang_ho,indus,irrawaddy,lena,limpopo,mackenzie,
      mekong,mississippi,murray,niger_river,nile,ob,oder,
      orange,orinoco,parana,rhine,rhone,rio_grande,salween,
      senegal_river,tagus,vistula,volga,volta,yangtze,
      yenisei,yukon,zambesi]).

ed(2, [does,afghanistan,border,china,?],

   [true]).

ed(3, [what,is,the,capital,of,upper_volta,?],

   [ouagadougou]).

ed(4, [where,is,the,largest,country,?],

   [asia,northern_asia]).

ed(5, [which,countries,are,european,?],

   [albania,andorra,austria,belgium,bulgaria,cyprus,
      czechoslovakia,denmark,east_germany,eire,finland,
      france,greece,hungary,iceland,italy,liechtenstein,
      luxembourg,malta,monaco,netherlands,norway,poland,
      portugal,romania,san_marino,spain,sweden,switzerland,
      united_kingdom,west_germany,yugoslavia]).

ed(6, [which,country,'''',s,capital,is,london,?],

   [united_kingdom]).

ed(7, [which,is,the,largest,african,country,?],

   [sudan]).

ed(8, [how,large,is,the,smallest,american,country,?],

   [0--ksqmiles]).

ed(9, [what,is,the,ocean,that,borders,african,countries,
         and,that,borders,asian,countries,?],

   [indian_ocean]).

ed(10, [what,are,the,capitals,of,the,countries,bordering,the,
          baltic,?],

   [[[denmark]:[copenhagen],[east_germany]:[east_berlin],
       [finland]:[helsinki],[poland]:[warsaw],
       [soviet_union]:[moscow],[sweden]:[stockholm],
       [west_germany]:[bonn]]]).

ed(11, [which,countries,are,bordered,by,two,seas,?],

   [egypt,iran,israel,saudi_arabia,turkey]).

ed(12, [how,many,countries,does,the,danube,flow,through,?],

   [6]).

ed(13, [what,is,the,total,area,of,countries,south,of,the,equator,
          and,not,in,australasia,?],

   [10228--ksqmiles]).

ed(14, [what,is,the,average,area,of,the,countries,in,each,
          continent,?],

   [[africa,233--ksqmiles],[america,496--ksqmiles],
      [asia,485--ksqmiles],[australasia,543--ksqmiles],
      [europe,58--ksqmiles]]).

ed(15, [is,there,more,than,one,country,in,each,continent,?],

   [false]).

ed(16, [is,there,some,ocean,that,does,not,border,any,country,?],

   [true]).

ed(17, [what,are,the,countries,from,which,a,river,flows,into,
          the,black_sea,?],

   [[romania,soviet_union]]).

ed(18, [what,are,the,continents,no,country,in,which,contains,more,
          than,two,cities,whose,population,exceeds,nb(1),million,?],

   [[africa,antarctica,australasia]]).

ed(19, [which,country,bordering,the,mediterranean,borders,a,country,
          that,is,bordered,by,a,country,whose,population,exceeds,
          the,population,of,india,?],

   [turkey]).

ed(20, [which,countries,have,a,population,exceeding,nb(10),
          million,?],

   [afghanistan,algeria,argentina,australia,bangladesh,
      brazil,burma,canada,china,colombia,czechoslovakia,
      east_germany,egypt,ethiopia,france,india,indonesia,
      iran,italy,japan,kenya,mexico,morocco,nepal,
      netherlands,nigeria,north_korea,pakistan,peru,
      philippines,poland,south_africa,south_korea,
      soviet_union,spain,sri_lanka,sudan,taiwan,tanzania,
      thailand,turkey,united_kingdom,united_states,venezuela,
      vietnam,west_germany,yugoslavia,zaire]).

ed(21, [which,countries,with,a,population,exceeding,nb(10),million,
          border,the,atlantic,?],

   [argentina,brazil,canada,colombia,france,mexico,
      morocco,netherlands,nigeria,south_africa,spain,
      united_kingdom,united_states,venezuela,west_germany,
      zaire]).

ed(22, [what,percentage,of,countries,border,each,ocean,?],

   [[arctic_ocean,2],[atlantic,35],[indian_ocean,14],
      [pacific,20]]).

ed(23, [what,countries,are,there,in,europe,?],

   [albania,andorra,austria,belgium,bulgaria,cyprus,
      czechoslovakia,denmark,east_germany,eire,finland,
      france,greece,hungary,iceland,italy,liechtenstein,
      luxembourg,malta,monaco,netherlands,norway,poland,
      portugal,romania,san_marino,spain,sweden,switzerland,
      united_kingdom,west_germany,yugoslavia]).

/* ----------------------------------------------------------------------
	Simple Access to demonstrations
   ---------------------------------------------------------------------- */

demo(Type) :-
   demo(Type, L),
   inform(L),
   check_words(L, S),
   process(S).

demo(mini, List) :-
   eg(List).
demo(main, List) :-
   ed(_, List, _).

inform(L) :-
   write('Question: '),
   inform1(L), nl.

inform1([]).
inform1([H|T]) :-
   out(H),
   put(" "),
   inform1(T).

/* ----------------------------------------------------------------------
	Top level processing for verification and performance analysis
   ---------------------------------------------------------------------- */

test_chat :-
   process(1, Time1),
   show_result('Parse', 0, Time1),
   process(2, Time2),
   show_result('Semantics', Time1, Time2),
   process(3, Time3),
   show_result('Planning', Time2, Time3),
   process(4, Time4),
   show_result('Reply', Time3, Time4).

show_result(Label, TimeA, TimeB) :-
   Time is TimeB-TimeA,
   write(Label),
   write(': '),
   write(Time),
   write(' ms'), nl.

process(Level, Time) :-
   uptime(Time1),
   (  between(1, 150, _),
      ed(_, Sentence, _),
      process(Sentence, _, Level), fail; true),
   uptime(Time2),
   Time is Time2-Time1.

process(Sentence, Answer, Level) :-
   (  Level > 0
   -> sentence(E, Sentence, [], [], []), !; true),

   (  Level > 1
   -> i_sentence(E, QT),
      clausify(QT, UE),
      simplify(UE, S), !; true),

   (  Level > 2
   -> qplan(S, S1), !; true),

   (  Level > 3
   -> answer(S1, Answer), !; true).

/* ----------------------------------------------------------------------
	Timing Utility
   ---------------------------------------------------------------------- */

:- if(current_prolog_flag(dialect,jekejeke)).

uptime(X) :-
   statistics(uptime, X).

:- else.

uptime(T) :-
   statistics(walltime, [T|_]).

:- endif.




