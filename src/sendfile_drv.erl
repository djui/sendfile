%%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%%% ex: ts=4 sw=4 sts=4 et
%%%
%%% Copyright 2008 Steve Vinoski. All Rights Reserved.
%%% Copyright 2010 Tuncer Ayaz. All Rights Reserved.
%%% Use of this source code is governed by a BSD-style
%%% license that can be found in the LICENSE file.
%%%
%%% Based on original code from yaws
%%%
%%% File    : yaws_sendfile.erl
%%% Author  : Steve Vinoski <vinoski@ieee.org>
%%% Description : interface to sendfile linked-in driver for Yaws
%%% Created :  9 Nov 2008 by Steve Vinoski <vinoski@ieee.org>
%%%
%%% Renamed to sendfile_drv and modified: Tuncer Ayaz in May 2010

-module(sendfile_drv).
-export([start_link/0, init/1, stop/0, send/2, send/3, send/4]).

-include_lib("kernel/include/file.hrl").

start_link() ->
    Shlib = "sendfile_drv",
    Dir = filename:dirname(code:which(?MODULE)) ++ "/../priv",
    case erl_ddll:load_driver(Dir, Shlib) of
        ok -> ok;
        {error, already_loaded} -> ok;
        _ -> exit({error, could_not_load_driver})
    end,
    {ok, spawn_link(?MODULE, init, [Shlib])}.

init(Shlib) ->
    register(?MODULE, self()),
    process_flag(trap_exit, true),
    Port = open_port({spawn, Shlib}, [binary]),
    loop(Port).

stop() ->
    ?MODULE ! stop,
    unregister(?MODULE),
    ok.

send(Out, Filename) ->
    send(Out, Filename, 0, 0).
send(Out, Filename, Offset) ->
    send(Out, Filename, Offset, 0).
send(Out, Filename, Offset, Count) ->
    Count2 = case Count of
                 0 ->
                     case file:read_file_info(Filename) of
                         {ok, #file_info{size = Size}} ->
                             Size - Offset;
                         Error ->
                             Error
                     end;
                 _ ->
                     Count
             end,
    case Count2 of
        {error, _}=Error2 ->
            Error2;
        _ ->
            case prim_inet:getfd(Out) of
                {ok, Socket_fd} ->
                    call_port(
                      Socket_fd,
                      list_to_binary(
                        [<<Offset:64, Count2:64, Socket_fd:32>>,
                         Filename, <<0:8>>]));
                Error3 ->
                    Error3
            end
    end.

call_port(Socket_id, Msg) ->
    ?MODULE ! {call, self(), Socket_id, Msg},
    receive
        {?MODULE, Reply} ->
            Reply
    end.

loop(Port) ->
    receive
        {call, Caller, Id, Msg} ->
            put(Id, Caller),
            erlang:port_command(Port, Msg),
            loop(Port);
        {Port, {data, <<Cnt:64, Id:32, Res:8, Err/binary>>}} ->
            Response = case Res of
                           1 ->
                               {ok, Cnt};
                           0 ->
                               {error,
                                list_to_atom(
                                  lists:takewhile(fun(El) -> El =/= 0 end,
                                                  binary_to_list(Err)))}
                       end,
            Caller = erase(Id),
            Caller ! {?MODULE, Response},
            loop(Port);
        stop ->
            erlang:port_close(Port),
            receive {'EXIT', Port, _Reason} -> ok
            after 0 -> ok
            end;
        {'EXIT', _, shutdown} ->
            erlang:port_close(Port),
            unregister(?MODULE),
            exit(shutdown);
        {'EXIT', Port, Posix_error} ->
            error_logger:format("Fatal error: sendfile port died, error ~p~n",
                                [Posix_error]),
            exit(Posix_error);
        {'EXIT', error, Reason} ->
            error_logger:format("Fatal error: sendfile driver failure: ~p~n",
                                [Reason]),
            exit(Reason)
    end.
