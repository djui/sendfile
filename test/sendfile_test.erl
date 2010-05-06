%%% Based on original test code from ifile project
%%%----------------------------------------------------------------------
%%% Created : 6 Nov 2007 by <tobbe@tornkvist.org>
%%% Desc.   : sendfile test program
%%%----------------------------------------------------------------------
-module(sendfile_test).
-export([test/3]).

%%% Shell  1:  nc -l -p 4422 localhost
%%%            or if you nc version is different try
%%% Shell  1:  nc -4 -l localhost 4422
%%% Eshell 2:  sendfile_test:test("localhost", 4422, "/somefile").
test(Host, Port, Fname) ->
  io:format("sendfile() enabled: ~p~n", [sendfile:enabled()]),
  sendfile:start_link(),
  {ok,Sock} = gen_tcp:connect(Host,Port, [binary,{packet,0}]),
  sendfile:send(Sock, Fname).
