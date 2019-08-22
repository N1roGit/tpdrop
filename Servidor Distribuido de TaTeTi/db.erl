-module(db).
-compile(export_all).

%% Guarda el nombre del jugador y el nodo donde esta su psocket
-record(nombres,{name,nodo,psocket}).

%% Guarda el id del juego, sus jugadores, los espectadores y el nodo donde esta el psocket del jg1 (Host de la partida)
-record(juegos,{id,jg1,jg2,spect=[],nodo,pid}).

%%Creo el scheme, si ya esta creado lo vuelvo a hacer, me fijo si estan las tablas
%%Si estan las tablas las vacio, sino las creo
install(Nodes) ->
    %%Creo la database en ram only ya que no necesito guardar nada "Permanentemente"
    application:set_env(mnesia,schema_location, ram),
    try mnesia:create_schema(Nodes)
    catch
    exit:_->mnesia:delete_schema(Nodes),mnseia:create_schema(Nodes)
    end,
    rpc:multicall(Nodes, application, start, [mnesia]),
    try mnesia:create_table(nombres,
        [{attributes, record_info(fields, nombres)},
        {type, set},
        {disc_copies,[]},
        {ram_copies, Nodes}])
    catch
        exit:_->mnesia:clear_table(nombres)
    end,
    try mnesia:create_table(juegos,
        [{attributes, record_info(fields, juegos)},
        {ram_copies, Nodes},
        {disc_copies,[]},
        {type, set}])
    catch
        exit:_->mnesia:clear_table(juegos)
    end,
    rpc:multicall(Nodes, application, start, [mnesia]),
    mnesia:change_config(extra_db_nodes,nodes()),
   %% rpc:multicall(Nodes--[node()],db,send_confirm,[Nodes--[node()]]),
    ok.

add_node()->
    receive
        {add,Pid,AddNode} ->
        case mnesia:change_config(extra_db_nodes,[AddNode])of
            {error,P} -> Pid ! {error,P};
            _ -> ok
        end,
      %%  case mnesia:add_table_copy(schema, AddNode, ram_copies) of
       %%         {atomic,ok} ->ok;
       %%         {aborted,Re} -> Pid ! {error,Re}
       %% end,
        case mnesia:add_table_copy(nombres, AddNode, ram_copies) of
            {atomic,ok} ->ok;
            {aborted,Reason} -> Pid ! {error,Reason}
        end,
        case mnesia:add_table_copy(juegos, AddNode, ram_copies) of
            {atomic,ok} -> Pid ! ok;
            {aborted,R} -> Pid ! {error,R}
        end
    end,
    add_node().



add_self()->
        case mnesia:add_table_copy(schema, node(), ram_copies) of
                {atomic,ok} ->ok;
                {aborted,Re} -> io:format("~p~n",[Re])
        end,
        case mnesia:add_table_copy(nombres, node(), ram_copies) of
            {atomic,ok} ->ok;
            {aborted,Reason} -> io:format("~p~n",[Reason])
        end,
        case mnesia:add_table_copy(juegos, node(), ram_copies) of
            {atomic,ok} -> ok;
            {aborted,R} -> io:format("~p~n",[R])
        end.


uninstall(Node)->
    mnesia:del_table_copy(nombres,Node), 
    mnesia:del_table_copy(juegos,Node),
    mnesia:del_table_copy(schema,Node).

send_confirm([]) -> ok;
send_confirm([H|T]) ->
    {node_server , H} ! start,
    send_confirm(T).

