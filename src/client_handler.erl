-module(client_handler).

-export([handle_client/3,remove_client_from_room/3]).

handle_client(Socket, undefined, _) ->
    gen_tcp:send(Socket, <<"Write your name: ">>),
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            Name = string:trim(binary_to_list(Data)),
            gen_server:cast(chat_server, {register_client, Socket, Name}),
            handle_client(Socket, Name, undefined);
        {error, closed} -> 
            io:format("Client disconnected while entering name.~n")
    end;

handle_client(Socket, Name, Room) ->
    WelcomeMessage = "Welcome " ++ Name ++ "!\n"
                    "Commands:"
                    "  list_rooms"
                    "  create_room <name>"
                    "  join_room <name>"
                    "  leave_room"
                    "  destroy_room <name>"
                    "  quit\n",
    gen_tcp:send(Socket, list_to_binary(WelcomeMessage)),
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} -> 
            Message = string:trim(binary_to_list(Data)),
            io:format("Received message from ~s: ~s~n", [Name, Message]),

            case Message of
                "quit" -> 
                    io:format("~s disconnected.~n", [Name]),
                    gen_server:cast(chat_server, {disconnect_client, self()}),
                    gen_tcp:send(Socket, <<"You left!\n">>),
                    gen_tcp:close(Socket); 
                "list_rooms" -> 
                    gen_server:cast(chat_server, {list_rooms, self()}),
                    handle_client(Socket, Name, Room);
                "create_room " ++ RoomName -> 
                    TrimmedRoomName = string:trim(RoomName),
                    gen_server:cast(chat_server, {create_room, self(), TrimmedRoomName}),
                    handle_client(Socket, Name, Room);
                "join_room " ++ RoomName -> 
                    TrimmedRoomName = string:trim(RoomName),
                    gen_server:cast(chat_server, {join_room, self(), TrimmedRoomName}),
                    handle_client(Socket, Name, TrimmedRoomName);
                "leave_room" -> 
                    gen_server:cast(chat_server, {leave_room, self()}),
                    handle_client(Socket, Name, undefined);
                "destroy_room " ++ RoomName -> 
                    TrimmedRoomName = string:trim(RoomName),
                    gen_server:cast(chat_server, {destroy_room, self(), TrimmedRoomName}),
                    handle_client(Socket, Name, Room);
                _ -> 
                    case Room of
                        undefined -> 
                            gen_tcp:send(Socket, <<"You are not in a room. Join a room to send messages.\n">>),
                            handle_client(Socket, Name, Room);
                        _ -> 
                            chat_server:broadcast(Room, Name, Message),
                            handle_client(Socket, Name, Room)
                    end
            end;
        {error, closed} -> 
            io:format("~s disconnected unexpectedly.~n", [Name]),
            gen_server:cast(chat_server, {disconnect_client, self()})
    end.

remove_client_from_room(Room, ClientPid, Rooms) ->
    case maps:get(Room, Rooms, undefined) of
        undefined -> Rooms;
        {CreatorPid, ClientPids} -> 
            NewClientPids = lists:delete(ClientPid, ClientPids),
            maps:put(Room, {CreatorPid, NewClientPids}, Rooms)
    end.