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
-export([start_link/0, init/1, stop/0, send/2, send/3, send/4, enabled/0]).

-include_lib("kernel/include/file.hrl").

%% TODO: maybe expose as app-config
-define(CHUNK_SIZE, 10240).

%% will be true for MacOsX, FreeBSD, Linux
-ifdef(HAVE_SENDFILE).

enabled() ->
    true.
start_link() ->
    sendfile_drv:start_link().
stop() ->
    sendfile_drv:stop().
init(ShLib) ->
    sendfile_drv:init(ShLib).
send(Out, FileName) ->
    case sendfile_drv:send(Out, FileName) of
        {error, eoverflow} ->
            compat_send(Out, FileName, 0, all);
        Other ->
            Other
    end.
send(Out, FileName, Offset) ->
    case sendfile_drv:send(Out, FileName, Offset) of
        {error, eoverflow} ->
            compat_send(Out, FileName, Offset, all);
        Other ->
            Other
    end.
send(Out, FileName, Offset, Count) ->
    case sendfile_drv:send(Out, FileName, Offset, Count) of
        {error, eoverflow} ->
            compat_send(Out, FileName, Offset, Count);
        Other ->
            Other
    end.

-else.

%% Emulate sendfile, this is true for win32, qnx, solaris. OpenBSD,NetBSD I
%% still don't know

enabled() ->
    false.
start_link() ->
    ignore.
stop() ->
    ok.
init(_) ->
    ok.
send(Out, Filename) ->
    send(Out, Filename, 0, all).
send(Out, Filename, Offset) ->
    send(Out, Filename, Offset, all).
send(Out, Filename, Offset, Count) ->
    compat_send(Out, Filename, Offset, Count).

-endif.

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


