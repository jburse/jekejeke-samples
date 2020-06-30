/**
 * Prolog code for the multimedia hello HTTP server.
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

:- package(library(example01)).

:- module(hello, []).
:- reexport(library(container/http)).
:- use_module(library(stream/xml)).
:- use_module(library(advanced/signal)).

/**
 * dispatch(O, P, R, S):
 * The predicate succeeds in dispatching the request for object
 * O, with path P, with request R and the socket S.
 */
% dispatch(+Object, +Spec, +Request, +Socket)
:- override dispatch/4.
:- public dispatch/4.
dispatch(_, '/example01/piglet.gif', Request, Session) :- !,
   dispatch_binary(library(example01/piglet), Request, Session).
dispatch(_, '/example01/hello.jsp', Request, Session) :- !,
   dispatch_hello(Request, Session).
dispatch(Object, Spec, Request, Session) :-
   container/http:dispatch(Object, Spec, Request, Session).

% dispatch_hello(+Request, +Socket)
:- private dispatch_hello/2.
dispatch_hello(Request, Session) :-
   http_parameter(Request, name, Name), !,
   catch(handle_hello(Name, Session), _, true).
dispatch_hello(_, Session) :-
   dispatch_error(415, Session).

% handle_hello(+Atom, +Socket)
:- private handle_hello/2.
handle_hello(Name, Session) :-
   setup_call_cleanup(
      open(Session, write, Response),
      send_hello(Name, Response),
      close(Response)).

% send_hello(+Atom, +Socket)
:- private send_hello/2.
send_hello(Name, Response) :-
   response_text(200, ['Content-Type'-
      'text/html; charset=UTF-8'], Response),
   atom_split(Title, ' ', ['Hello', Name]),
   html_begin(Response, Title),
   write(Response, '  <center><img src="piglet.gif">\r\n'),
   write(Response, '<h1>Happy New Year 2019, '),
   html_escape(Response, Name),
   write(Response, '</h1></center>\r\n'),
   html_end(Response).

% html_begin(+Stream, +Atom)
:- private html_begin/2.
html_begin(Response, Title) :-
   write(Response, '<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">\r\n'),
   write(Response, '<html>\r\n'),
   write(Response, '  <head>\r\n'),
   write(Response, '      <meta http-equiv="Content-Type" content="text/html; charset=UTF-8">\r\n'),
   write(Response, '      <meta name="viewport" content="width=device-width, initial-scale=1.0">\r\n'),
   write(Response, '      <title>'), html_escape(Response, Title), write(Response, '</title>\r\n'),
   write(Response, '  </head>\r\n'),
   write(Response, '  <body>\r\n').

% html_end(+Stream)
:- private html_end/1.
html_end(Response) :-
   write(Response, '   </body>\r\n'),
   write(Response, '</html>\r\n').

% ?- run_http(example01/hello, 'localhost:8085'), write('.'), flush_output, fail; true.

% Point browser to: http://localhost:8085/example01/hello.jsp?name=Fritz
