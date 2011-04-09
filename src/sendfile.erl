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
%%% File    : yaws_sendfile_compat.erl
%%% Author  : Claes Wikstrom, klacke@hyber.org
%%% Description : Conditional OS dependent call to sendfile
%%% Created :  Sat Dec 20 20:00:11 CET 2008
%%%
%%% Renamed to sendfile and modified: Tuncer Ayaz in May 2010

-module(sendfile).

-behaviour(gen_server).

-define(SERVER, ?MODULE).

%% TODO: maybe expose as app-config
-define(CHUNK_SIZE, 10240).

-include_lib("kernel/include/file.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, send/2, send/3, send/4, enabled/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-ifdef(HAVE_SENDFILE).
enabled() ->
    true.

send(Out, Filename) ->
    gen_server:call(?MODULE, {send, Out, Filename}).

send(Out, Filename, Offset) ->
    gen_server:call(?MODULE, {send, Out, Filename, Offset}).

send(Out, Filename, Offset, Count) ->
    gen_server:call(?MODULE, {Out, Filename, Offset, Count}).

-else.

enabled() ->
    false.

send(Out, Filename) ->
    send(Out, Filename, 0, all).

send(Out, Filename, Offset) ->
    send(Out, Filename, Offset, all).

send(Out, Filename, Offset, Count) ->
    gen_server:call(?MODULE, {Out, Filename, Offset, Count}).
-endif.



%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------


%% Will be defined for Linux, FreeBSD, DragonflyBSD, Solaris and Mac OS X
-ifdef(HAVE_SENDFILE).
init(Args) ->
    {ok, _} = sendfile_drv:start_link(),
    {ok, Args}.

handle_call({send, Out, FileName}, _From, State) ->
    Reply = case sendfile_drv:send(Out, FileName) of
                {error, eoverflow} ->
                    compat_send(Out, FileName, 0, all);
                Other ->
                    Other
            end,
    {reply, Reply, State};
handle_call({send, Out, FileName, Offset}, _From, State) ->
    Reply = case sendfile_drv:send(Out, FileName, Offset) of
                {error, eoverflow} ->
                    compat_send(Out, FileName, Offset, all);
                Other ->
                    Other
            end,
    {reply, Reply, State};
handle_call({send, Out, FileName, Offset, Count}, _From, State) ->
    Reply = case sendfile_drv:send(Out, FileName, Offset, Count) of
                {error, eoverflow} ->
                    compat_send(Out, FileName, Offset, Count);
                Other ->
                    Other
            end,
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    {noreply, ok, State}.
terminate(_Reason, _State) ->
    ok = sendfile_drv:stop().

-else.

init(Args) ->
    {ok, Args}.

handle_call({send, Out, FileName, Offset, Count}, _From, State) ->
    Reply = compat_send(Out, Filename, Offset, Count),
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    {noreply, ok, State}.

terminate(_Reason, _State) ->
    ok.
-endif.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

compat_send(Out, Filename, Offset, Count) ->
    case file:open(Filename, [read, binary, raw]) of
        {ok, Fd} ->
            {ok, _} = file:position(Fd, {bof, Offset}),
            ChunkSize = ?CHUNK_SIZE,
            Ret = loop_send(Fd, ChunkSize, file:read(Fd, ChunkSize), Out, Count),
            ok = file:close(Fd),
            Ret;
        Err ->
            Err
    end.

loop_send(Fd, ChunkSize, {ok, Bin}, Out, all) ->
    case gen_tcp:send(Out, Bin) of
        ok ->
            loop_send(Fd, ChunkSize, file:read(Fd, ChunkSize), Out, all);
        Err ->
            Err
    end;
loop_send(_Fd, _ChunkSize, eof, _Out, _) ->
    ok;
loop_send(Fd, ChunkSize, {ok, Bin}, Out, Count) ->
    Sz = size(Bin),
    if Sz < Count ->
            case gen_tcp:send(Out, Bin) of
                ok ->
                    loop_send(Fd, ChunkSize, file:read(Fd, ChunkSize),
                              Out, Count-Sz);
                Err ->
                    Err
            end;
       Sz == Count ->
            gen_tcp:send(Out, Bin);
       Sz > Count ->
            <<Deliver:Count/binary , _/binary>> = Bin,
            gen_tcp:send(Out, Deliver)
    end;
loop_send(_Fd, _, Err, _,_) ->
    Err.
