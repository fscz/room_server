%M% -*- tab-width: 4;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%%% ex: ft=erlang ts=4 sw=4 et
%%%
%%% This file is part of mochicow released under the MIT license.
%%% See the NOTICE for more information.
%%%
-module(mochicow_upgrade).

-export([upgrade/4]).


upgrade(Req, _Env, _Handler, Opts) ->
    {loop, HttpLoop} = proplists:lookup(loop, Opts),

    [Socket,
     Transport,
     Method,
     Version,
     Host,
     Port,
     Path,
     QS,
     Headers,
     Buffer] = cowboy_req:get([socket, transport, method, version,
                              host,port, path, qs, headers, buffer],
                              Req),

    MochiSocket = mochiweb_socket(Transport, Socket),
    DefaultPort = default_port(Transport:name()),
    MochiHost = case Port of
        DefaultPort ->
            binary_to_list(Host);
        _ ->
            %% fix raw host
            binary_to_list(Host) ++ ":" ++ integer_to_list(Port)
    end,

    MochiHeaders = lists:foldl(fun
                ({'Host'=K, _V}, T) ->
                    mochiweb_headers:insert(K, MochiHost, T);
                ({K, V}, T) when is_binary(K) ->
                    mochiweb_headers:insert(binary_to_list(K),
                                         binary_to_list(V), T);
                ({K, V}, T) ->
                    mochiweb_headers:insert(K, binary_to_list(V), T)
            end, mochiweb_headers:empty(), Headers),

    %% fix raw path
    Path1 = case Path of
        <<>> ->
            <<"/">>;
        _ ->
            Path
    end,
    RawPath = case QS of
        <<>> ->
            Path1;
        _ ->
            << Path1/binary, "?", QS/binary >>
    end,
    AtomicMethod = if is_binary(Method) ->
            list_to_atom(binary_to_list(Method));
        true -> Method
    end,
    MochiReq = mochicow_request:new(MochiSocket,
                                    AtomicMethod,
                                    binary_to_list(RawPath),
                                    Version,
                                    MochiHeaders,
                                    Buffer),

    case catch call_body(HttpLoop, MochiReq) of
        {'EXIT', _Reason} ->
            %%closed;
            {halt, Req};
        _ ->
            after_response(Req, MochiReq)
    end.

mochiweb_socket(ranch_ssl, Socket) ->
    {ssl, Socket};
mochiweb_socket(_Transport, Socket) ->
    Socket.

call_body({M, F, A}, Req) ->
    erlang:apply(M, F, [Req | A]);
call_body({M, F}, Req) ->
    M:F(Req);
call_body(Body, Req) ->
    Body(Req).

after_response(Req, MochiReq) ->
    Req2 = cowboy_req:set([{resp_state, done}, {body_state, done},
                           {buffer, MochiReq:get(buffer)}], Req),

    case MochiReq:should_close() of
        true ->
            {ok, cowboy_req:set([{connection, close}], Req2), [{result, ok}]};
        _ ->
            MochiReq:cleanup(),
            erlang:garbage_collect(),
            {ok, cowboy_req:set([{connection, keepalive}], Req2), [{result, ok}]}
    end.

-spec default_port(atom()) -> 80 | 443.
default_port(ssl) -> 443;
default_port(_) -> 80.
