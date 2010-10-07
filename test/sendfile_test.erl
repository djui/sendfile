%%% -*- tab-width: 4;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%%% ex: ts=4 sw=4 sts=4 et
%%%
%%% Copyright 2010 Tuncer Ayaz. All Rights Reserved.
%%% Use of this source code is governed by a BSD-style
%%% license that can be found in the LICENSE file.

-module(sendfile_test).
-author(tuncerayaz).
-export([send/0, send/1, send/3, server/1]).

-include_lib("kernel/include/file.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(HOST,"localhost").
-define(PORT,4422).
-define(TESTFILE,"../test/testfile").
-define(TIMEOUT, 10000).

basic_test() ->
  ok = send().

send() ->
  send(?HOST, ?PORT, ?TESTFILE).

send(File) ->
  send(?HOST, ?PORT, File).

send(Host, Port, File) ->
  FileInfo = file_info(File),
  spawn_link(?MODULE, server, [self()]),
  {ok, _} = sendfile:start_link(),
  {ok, Sock} = gen_tcp:connect(Host, Port, [binary,{packet,0}]),
  {ok, _} = sendfile:send(Sock, File),
  ok = gen_tcp:close(Sock),
  receive
    {ok, Bin} ->
      FileInfo = bin_info(Bin)
  after ?TIMEOUT ->
      ?assert(failure =:= timeout)
  end,
  ok = sendfile:stop().

server(ClientPid) ->
  {ok, LSock} = gen_tcp:listen(?PORT, [binary, {packet, 0},
                                       {active, false},
                                       {reuseaddr, true}]),
  {ok, Sock} = gen_tcp:accept(LSock),
  {ok, Bin} = do_recv(Sock, []),
  ok = gen_tcp:close(Sock),
  ClientPid ! {ok, Bin}.

do_recv(Sock, Bs) ->
  case gen_tcp:recv(Sock, 0, ?TIMEOUT) of
    {ok, B} ->
      do_recv(Sock, [B|Bs]);
    {error, closed} ->
      {ok, lists:reverse(Bs)}
  end.

file_info(File) ->
  {ok, #file_info{size = Size}} = file:read_file_info(File),
  {ok, Data} = file:read_file(File),
  Crc32 = erlang:crc32(Data),
  {Size, Crc32}.

bin_info(Data) ->
  Size = lists:foldl(fun(E,Sum) -> size(E) + Sum end, 0, Data),
  Crc32 = erlang:crc32(Data),
  {Size, Crc32}.
