-module(roomserver_room_websocket, [Req, SessionId]).
-behaviour(boss_service_handler).

-record(state, {users, rooms}).

-compile(export_all).
%-import(ws_lib, [notify_all/2, notify_all_except_me/3]).

% helpers

format_str(String, List) ->
    lists:flatten(io_lib:format(String, List)).

rand() ->
    random:uniform(134217728).


room_notify(WebSocketId, Notification) ->
    WebSocketId ! {text, jsx:encode(Notification)}.


room_notify_other_members(WebSocketId, Members, Notification, State) ->
    lists:foreach(fun(Member) -> if
				     Member /= WebSocketId ->
					 room_notify(Member, Notification);
				     true ->
					 ok
				 end 
		  end, Members).


room_ok(WebSocketId, Type) ->
    room_ok(WebSocketId, Type, []).

room_ok(WebSocketId, Type, Params) ->
    room_notify(WebSocketId, [{<<"type">>, list_to_binary(Type)} | [{<<"status">>, <<"ok">>} | Params]]).

room_error(WebSocketId, Type, Message) ->    
    room_notify(WebSocketId, [{<<"type">>, list_to_binary(Type)}, {<<"status">>, <<"error">>}, {<<"reason">>, list_to_binary(Message)}]).

room_create(WebSocketId, State) ->
    #state{users=Users, rooms=Rooms} = State,
    case dict:find(WebSocketId, Users) of
	{ok, RoomId} -> % User already in room, so deny
	    room_error(WebSocketId, "create", format_str("cannot join more than one room. already in: ~p", [RoomId])),
	    {noreply, State};
	error ->
	    F = fun(Self, R) -> 		     		      
			case dict:find(R, Rooms) of
			    {ok, Value} ->
				Self(rand());
			    error ->
				R
			end
		end,
	    RoomId = F(F, rand()),
	    room_ok(WebSocketId, "create", [{<<"room">>, RoomId}]),
	    {noreply, #state{users=dict:store(WebSocketId, RoomId, Users), rooms=dict:store(RoomId, [WebSocketId], Rooms)}}
    end.

room_leave(WebSocketId, State) ->
    #state{users=Users, rooms=Rooms} = State,
    case dict:find(WebSocketId, Users) of
	{ok, RoomId} -> %erase cannot fail if user found		
	    NewMembers = lists:delete(WebSocketId, dict:fetch(RoomId, Rooms)),
	    NewUsers = dict:erase(WebSocketId, Users),
	    room_ok(WebSocketId, "leave", [{<<"room">>, RoomId}]),
	    if 
		NewMembers == [] ->
		    {noreply, #state{users=NewUsers, rooms=dict:erase(RoomId, Rooms)}};
		true ->
		    {noreply, #state{users=NewUsers, rooms=dict:store(RoomId, NewMembers, Rooms)}}
	    end;
	error -> 
	    room_error(WebSocketId, "leave", "cannot leave room. user unknown."),
	    {noreply, State}
    end.

room_join(WebSocketId, MessageMap, State) ->
    #state{users=Users, rooms=Rooms} = State,
    case dict:find(WebSocketId, Users) of
	{ok, RoomId} -> % cannot join several rooms
	    room_error(WebSocketId, "join", format_str("cannot join more than one room. already in: ~p", [RoomId])),
	    {noreply, State};	       
	error ->
	    case maps:find(<<"room">>, MessageMap) of
		{ok, RoomId} ->
		    if
			is_integer(RoomId) -> 			    
			    case dict:find(RoomId, Rooms) of
				{ok, Members} -> % ok, room exists, add user
				    room_ok(WebSocketId, "join", [{<<"room">>, RoomId}]),
				    {noreply, #state{users=dict:store(WebSocketId, RoomId, Users), rooms=dict:store(RoomId, [WebSocketId|Members], Rooms)}};
				error -> % error, unknown room id
				    room_error(WebSocketId, "join", "room unknown"),
				    {noreply, State}
			    end;
			true ->
			    room_error(WebSocketId, "join", "field \"room\" must be integer")
		    end;
		error ->
		    room_error(WebSocketId, "join", "message missing \"room\" field"),
		    {noreply, State}
	    end
    end.    

room_message(WebSocketId, MessageMap, State) ->
    #state{users=Users, rooms=Rooms} = State,
    case maps:find(<<"content">>, MessageMap) of
	{ok, Content} ->
	    case dict:find(WebSocketId, Users) of
		{ok, RoomId} ->
		    Members = dict:fetch(RoomId, Rooms),
		    room_notify_other_members(WebSocketId, Members, MessageMap, State);
		error ->
		    room_error(WebSocketId, "message", "not registered to a room")
	    end;
	error ->
	    room_error(WebSocketId, "message", "message missing \"content\" field")
    end,
    {noreply, State}.

% protocol methods

init() ->    
    random:seed(),
    {ok, #state{users=dict:new(), rooms=dict:new()}}.

handle_join(ServiceName, WebSocketId, State) ->
    {noreply, State}.

handle_close(ServiceName, WebSocketId, State) ->    
    {noreply, NewState} = room_leave(WebSocketId, State),
    {reply, ok, NewState}.

handle_incoming(ServiceName, WebSocketId, Message, State) ->
    MessageMap = jsx:decode(Message, [return_maps]),
    
    case maps:find(<<"type">>, MessageMap) of
	{ok, <<"create">>} ->
	    room_create(WebSocketId, State);
	{ok, <<"join">>} ->
	    room_join(WebSocketId, MessageMap, State);	    
	{ok, <<"leave">>} ->
	    room_leave(WebSocketId, State);
	{ok, <<"message">>} ->
	    room_message(WebSocketId, MessageMap, State);
	_ -> 
 	    room_error(WebSocketId, "unknown", "unknown message format"),
	    {noreply, State}
    end.

handle_info(_,     State) ->
  {noreply, State}.

terminate(Reason, State) ->
    ok.
