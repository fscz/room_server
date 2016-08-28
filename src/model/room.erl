-module(room, [Id, Number, CreatedAt, UpdatedAt]).
-compile(export_all).

-has({clients, many}).
