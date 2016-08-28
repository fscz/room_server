-module(client, [Id, RoomId, SocketId, CreatedAt, UpdatedAt]).
-compile(export_all).

-belongs_to(room).
