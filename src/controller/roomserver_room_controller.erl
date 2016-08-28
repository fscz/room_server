-module(roomserver_room_controller, [Req, Sess]).
-compile(export_all).

index('GET', []) ->
  {ok, []}.

