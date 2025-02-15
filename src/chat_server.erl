-module(chat_server).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, broadcast/3]).

-record(state, {
    listener,
    rooms = #{},
    private_rooms = #{},
    clients = #{}
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

    init([]) ->
        case application:get_env(chat_server, port) of
            {ok, Port} ->
                {ok, ListenSocket} = gen_tcp:listen(Port, [binary, {packet, line}, {active, false}, {reuseaddr, true}]),
                io:format("Server started on port ~p~n", [Port]),
                spawn(fun() -> accept_loop(ListenSocket) end),
                {ok, #state{listener = ListenSocket}};
            
            error ->
                %% Port is not defined in sys.config
                io:format("ERROR: Port not defined in sys.config~n"),
                {stop, port_not_found}
    end.

accept_loop(ListenSocket) ->
    {ok, ClientSocket} = gen_tcp:accept(ListenSocket),
    gen_server:cast(?MODULE, {new_client, ClientSocket}),
    accept_loop(ListenSocket).

handle_cast({new_client, Socket}, State) ->
    ClientPid = spawn(fun() ->client_handler:handle_client(Socket, undefined, undefined) end),
    {noreply, State#state{clients = maps:put(ClientPid, {Socket, undefined, undefined}, State#state.clients)}};

handle_cast({broadcast, Room, From, Message}, State) ->
    io:format("Broadcasting message in room ~s from ~s: ~s~n", [Room, From, Message]),
    case maps:get(Room, State#state.rooms, undefined) of
        undefined ->
            case maps:get(Room, State#state.private_rooms, undefined) of
                undefined ->
                    io:format("Room ~s does not exist~n", [Room]),
                    {noreply, State};
                {_CreatorPid, Members} ->
                    lists:foreach(
                        fun(MemberPid) ->
                            case maps:get(MemberPid, State#state.clients, undefined) of
                                undefined -> ok;
                                {ClientSocket, _Name, _Room} -> 
                                    gen_tcp:send(ClientSocket, io_lib:format("~s: ~s~n", [From, Message]))
                            end
                        end, Members),
                    {noreply, State}
            end;
        {_CreatorPid, ClientPids} ->
            lists:foreach(
                fun(ClientPid) ->
                    case maps:get(ClientPid, State#state.clients, undefined) of
                        undefined -> ok;
                        {ClientSocket, _Name, _Room} -> 
                            gen_tcp:send(ClientSocket, io_lib:format("~s: ~s~n", [From, Message]))
                    end
                end, ClientPids),
            {noreply, State}
    end;

handle_cast({register_client, Socket, Name}, State) ->
    io:format("~s joined the chat.~n", [Name]),
    {noreply, State#state{clients = maps:put(self(), {Socket, Name, undefined}, State#state.clients)}};

handle_cast({disconnect_client, ClientPid}, State) ->
    case maps:get(ClientPid, State#state.clients, undefined) of
        undefined -> {noreply, State};
        {_Socket, Name, Room} ->
            io:format("~s disconnected.~n", [Name]),
            NewClients = maps:remove(ClientPid, State#state.clients),
            NewRooms = case Room of
                undefined -> State#state.rooms;
                _ -> client_handler:remove_client_from_room(Room, ClientPid, State#state.rooms)
            end,
            {noreply, State#state{clients = NewClients, rooms = NewRooms}}
    end;

handle_cast({create_room, ClientPid, RoomName}, State) ->
    io:format("Attempting to create room: ~s by client ~p~n", [RoomName, ClientPid]),
    case maps:get(RoomName, State#state.rooms, undefined) of
        undefined ->
            io:format("Room ~s created by ~p~n", [RoomName, ClientPid]),
            NewRooms = maps:put(RoomName, {ClientPid, []}, State#state.rooms),
            {noreply, State#state{rooms = NewRooms}};
        _ ->
            io:format("Room ~s already exists~n", [RoomName]),
            {noreply, State}
    end;

handle_cast({destroy_room, ClientPid, RoomName}, State) ->
    case maps:get(RoomName, State#state.rooms, undefined) of
        undefined -> {noreply, State};
        {CreatorPid, _ClientPids} ->
            if
                CreatorPid =:= ClientPid ->
                    io:format("Room ~s destroyed by ~p~n", [RoomName, ClientPid]),
                    NewRooms = maps:remove(RoomName, State#state.rooms),
                    {noreply, State#state{rooms = NewRooms}};
                true ->
                    io:format("Client ~p is not the creator of room ~s~n", [ClientPid, RoomName]),
                    {noreply, State}
            end
    end;

handle_cast({join_room, ClientPid, RoomName}, State) ->
    io:format("Attempting to join room: ~s by client ~p~n", [RoomName, ClientPid]),
    case maps:get(RoomName, State#state.rooms, undefined) of
        undefined ->
            io:format("Room ~s does not exist~n", [RoomName]),
            case maps:get(ClientPid, State#state.clients, undefined) of
                undefined -> {noreply, State};
                {Socket, _Name, _Room} -> 
                    gen_tcp:send(Socket, io_lib:format("Room ~s does not exist~n", [RoomName])),
                    {noreply, State}
            end;
        {CreatorPid, ClientPids} ->
            io:format("Client ~p joined room ~s~n", [ClientPid, RoomName]),
            NewRooms = maps:put(RoomName, {CreatorPid, [ClientPid | ClientPids]}, State#state.rooms),
            NewClients = maps:update(ClientPid, {element(1, maps:get(ClientPid, State#state.clients)), element(2, maps:get(ClientPid, State#state.clients)), RoomName}, State#state.clients),
            {noreply, State#state{rooms = NewRooms, clients = NewClients}}
    end;

handle_cast({private_message, From, To, Message}, State) ->
    case find_client_by_name(To, State#state.clients) of
        undefined ->
            case maps:get(self(), State#state.clients, undefined) of
                undefined -> {noreply, State};
                {Socket, _Name, _Room} -> 
                    gen_tcp:send(Socket, io_lib:format("User ~s not found.~n", [To])),
                    {noreply, State}
            end;
        {RecipientSocket, _Name, _Room} ->
            gen_tcp:send(RecipientSocket, io_lib:format("[Private from ~s]: ~s~n", [From, Message])),
            {noreply, State}
    end;

handle_cast({leave_room, ClientPid}, State) ->
    io:format("Attempting to leave room by client ~p~n", [ClientPid]),
    case maps:get(ClientPid, State#state.clients, undefined) of
        undefined -> {noreply, State};
        {_Socket, _Name, Room} ->
            io:format("Client ~p left room ~s~n", [ClientPid, Room]),
            NewRooms = client_handler:remove_client_from_room(Room, ClientPid, State#state.rooms),
            NewClients = maps:update(ClientPid, {element(1, maps:get(ClientPid, State#state.clients)), element(2, maps:get(ClientPid, State#state.clients)), undefined}, State#state.clients),
            {noreply, State#state{rooms = NewRooms, clients = NewClients}}
    end;

handle_cast({list_rooms, ClientPid}, State) ->
    PublicRooms = maps:keys(State#state.rooms),
    PrivateRooms = maps:fold(fun(RoomName, {_CreatorPid, Members}, Acc) ->
        case lists:member(ClientPid, Members) of
            true -> [RoomName | Acc];
            false -> Acc
        end
    end, [], State#state.private_rooms),
    AllRooms = PublicRooms ++ PrivateRooms,
    case maps:get(ClientPid, State#state.clients, undefined) of
        undefined -> {noreply, State};
        {Socket, _Name, _Room} ->
            gen_tcp:send(Socket, io_lib:format("Available rooms: ~p~n", [AllRooms])),
            {noreply, State}
    end;

handle_cast({create_private_room, ClientPid, RoomName}, State) ->
    io:format("Attempting to create private room: ~s by client ~p~n", [RoomName, ClientPid]),
    case maps:get(RoomName, State#state.rooms, undefined) of
        undefined ->
            case maps:get(RoomName, State#state.private_rooms, undefined) of
                undefined ->
                    io:format("Private room ~s created by ~p~n", [RoomName, ClientPid]),
                    NewPrivateRooms = maps:put(RoomName, {ClientPid, [ClientPid]}, State#state.private_rooms),
                    {noreply, State#state{private_rooms = NewPrivateRooms}};
                _ ->
                    io:format("Private room ~s already exists~n", [RoomName]),
                    {noreply, State}
            end;
        _ ->
            io:format("Room ~s already exists as a public room~n", [RoomName]),
            {noreply, State}
    end;

handle_cast({invite_to_private_room, ClientPid, RoomName, InviteeName}, State) ->
    io:format("Attempting to invite ~s to private room ~s by client ~p~n", [InviteeName, RoomName, ClientPid]),
    case maps:get(RoomName, State#state.private_rooms, undefined) of
        undefined ->
            io:format("Private room ~s does not exist~n", [RoomName]),
            {noreply, State};
        {CreatorPid, Members} ->
            if
                CreatorPid =:= ClientPid ->
                    case find_client_by_name(InviteeName, State#state.clients) of
                        undefined ->
                            io:format("User ~s not found~n", [InviteeName]),
                            {noreply, State};
                        {InviteeSocket, _Name, _Room} ->
                            io:format("User ~s invited to private room ~s~n", [InviteeName, RoomName]),
                            gen_tcp:send(InviteeSocket, io_lib:format("You have been invited to join private room ~s. Use 'join_private_room ~s' to join.~n", [RoomName, RoomName])),
                            NewPrivateRooms = maps:put(RoomName, {CreatorPid, [InviteeSocket | Members]}, State#state.private_rooms),
                            {noreply, State#state{private_rooms = NewPrivateRooms}}
                    end;
                true ->
                    io:format("Client ~p is not the creator of private room ~s~n", [ClientPid, RoomName]),
                    {noreply, State}
            end
    end;

handle_cast({join_private_room, ClientPid, RoomName}, State) ->
    io:format("Attempting to join private room: ~s by client ~p~n", [RoomName, ClientPid]),
    case maps:get(RoomName, State#state.private_rooms, undefined) of
        undefined ->
            io:format("Private room ~s does not exist~n", [RoomName]),
            {noreply, State};
        {_CreatorPid, Members} ->
            case lists:member(ClientPid, Members) of
                true ->
                    io:format("Client ~p joined private room ~s~n", [ClientPid, RoomName]),
                    NewClients = maps:update(ClientPid, {element(1, maps:get(ClientPid, State#state.clients)), element(2, maps:get(ClientPid, State#state.clients)), RoomName}, State#state.clients),
                    {noreply, State#state{clients = NewClients}};
                false ->
                    io:format("Client ~p is not invited to private room ~s~n", [ClientPid, RoomName]),
                    {noreply, State}
            end
    end.


handle_call(_Request, _From, State) ->
    {reply, ok, State}.

terminate(_Reason, _State) ->
    ok.

broadcast(Room, From, Message) ->
    gen_server:cast(?MODULE, {broadcast, Room, From, Message}).

find_client_by_name(Name, Clients) ->
    maps:fold(fun(_Pid, {Socket, ClientName, Room}, Acc) ->
        case ClientName =:= Name of
            true -> {Socket, ClientName, Room};
            false -> Acc
        end
    end, undefined, Clients).