-module(chat_server).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).


-record(state, {
    listener,
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
    ClientPid = spawn(fun() -> handle_client(Socket, undefined) end),
    {noreply, State#state{clients = maps:put(ClientPid, {Socket, undefined}, State#state.clients)}};

handle_cast({register_client, Socket, Name}, State) ->
    io:format("~s joined the chat.~n", [Name]),
    {noreply, State#state{clients = maps:put(self(), {Socket, Name}, State#state.clients)}};

handle_cast({disconnect_client, ClientPid}, State) ->
    case maps:get(ClientPid, State#state.clients, undefined) of
        undefined -> {noreply, State};
        {_Socket, Name} ->
            io:format("~s disconnected.~n", [Name]),
            NewClients = maps:remove(ClientPid, State#state.clients),
            {noreply, State#state{clients = NewClients}}
    end;

handle_cast({broadcast, From, Message}, State) ->
    lists:foreach(
        fun(ClientPid) ->
            case maps:get(ClientPid, State#state.clients, undefined) of
                undefined -> ok;
                {ClientSocket, _Name} ->
                    gen_tcp:send(ClientSocket, io_lib:format("~s: ~s~n", [From, Message]))
            end
        end, maps:keys(State#state.clients)),
    {noreply, State}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

terminate(_Reason, _State) ->
    ok.

handle_client(Socket, undefined) ->
    gen_tcp:send(Socket, <<"Write your name: ">>),
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            Name = string:trim(binary_to_list(Data)),
            gen_server:cast(?MODULE, {register_client, Socket, Name}),
            handle_client(Socket, Name);
        {error, closed} ->
            io:format("Client disconnected while entering name.~n")
    end;

handle_client(Socket, Name) ->
WelcomeMessage = "Welcome " ++ Name ++ "!\n" ++
                "You can start chatting now.\n" ++
                "Type 'quit' to exit.\n\n",
    gen_tcp:send(Socket, WelcomeMessage),
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} -> 
            Message = string:trim(binary_to_list(Data)),
            io:format("Received message from ~s: ~s~n", [Name, Message]),

            case Message of
                "quit" -> 
                    io:format("~s disconnected.~n", [Name]),
                    gen_server:cast(?MODULE, {disconnect_client, self()}),
                    gen_tcp:send(Socket, <<"You left!\n">>),
                    gen_tcp:close(Socket); 
                _ -> 
                    gen_server:cast(?MODULE, {broadcast, Name, Message}),
                    handle_client(Socket, Name)
            end;
        {error, closed} -> 
            io:format("~s disconnected unexpectedly.~n", [Name]),
            gen_server:cast(?MODULE, {disconnect_client, self()})
    end.