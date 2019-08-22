-module(server).
-compile(export_all).

%% Guarda el nombre del jugador y el nodo donde esta su psocket
-record(nombres,{name,nodo,psocket}).

%% Guarda el id del juego, sus jugadores, los espectadores y el nodo donde esta el psocket del jg1 (Host de la partida),
%%tambien guarda el pid de donde se esta ejecutando el codigo de la partida
-record(juegos,{id,jg1,jg2,spect=[],nodo,pid}).


init(Port,Brother,Fin)->
	register(node_server,spawn(?MODULE,server,[Port,Brother,Fin])).

kill() -> exit(normal).

server(Port,Brother,Fin)->
	case Brother of
		false -> ok;
		_     -> case net_adm:ping(Brother) of
						pong -> io:format("Conexion exitosa a ~p~n",[Brother]);
						pang -> io:format("Error de conexion a ~p ~n",[Brother]),exit(error)
					end
	end,
	case Fin of
		%% Es el ultimo nodo que va a entrar en el servidor, agarro y inicializo la db
		fin -> ok = db:install([node()]++nodes()),
			   %%ok = db:send_confirm(nodes()),io:format("Nodos ~p~n",[nodes()]);
			   lists:foreach(fun(Node)-> {node_server,Node} ! start end ,nodes());
		%%Si ya tengo mi red de mnesia funcionando y quiero agregar otro nodo uso esto
		add -> 	ok = mnesia:start(),
				{addnodes,hd(nodes())} ! {add,self(),node()},
				receive
				ok -> io:format("Conexion a la red exitosa~n"),mnesia:info();
				{error,R} -> io:format("Algo salio mal -> ~p~n",[R]),exit(normal)
				end;
		%% En caso de no ser el ultimo nodo, espero hasta que el nodo final aparezca y cree la db 
	_  ->receive start->ok end
	end,
	register(addnodes,spawn(db,add_node,[])),
	io:format("Ready~n"),
	register(balance,spawn(?MODULE,pbalance,[dict:new()])),
	register(stat,spawn(?MODULE,pstat,[nodes()++[node()]])),
	listen(Port).

listen(Port) ->
	{ok, ListenSocket} = gen_tcp:listen(Port, [binary, {active, false},{nodelay, true}]),
	dispatcher(ListenSocket).

dispatcher(ListenSocket) ->
	{ok, Socket} = gen_tcp:accept(ListenSocket),
	Pid = spawn(?MODULE, psocket, [Socket," "]),
	gen_tcp:controlling_process(Socket, Pid),
	dispatcher(ListenSocket).
	
	
psocket(Socket,Nombre)->
	io:format("Conexion por ~p ~n ",[Socket]),
	inet:setopts(Socket, [{active, once}]),
	Pid = self(),
	receive 
		{tcp,S,Msg} -> {balance,node()} ! {request,Pid,Msg},
		receive
			{execute,Node,Msg} -> spawn(Node,?MODULE,pcomando,[self(),Nombre,Msg]),
								  psocket(Socket,Nombre)
		end;
		{login,ID} -> gen_tcp:send(Socket,ID),psocket(Socket,ID);
		
		{send,Response} -> io:format("~p ~p ~p ~n",[gen_tcp:send(Socket,Response++"-"),Socket,Response]),psocket(Socket,Nombre);
		
		{tcp_closed,Socket} -> io:format("bye ~p~n",[Nombre]),delete_nombre(Nombre),
							   end_games(Nombre),exit(normal)
	 
	end.

	
pcomando(Psocket,Name,Msg) ->
	COMM = string:tokens(binary_to_list(Msg)," \n"),
	%%COMM = string:tokens(Msg," \n"),
	case COMM of 
		%%  _	-> io:format("~p~n",[COMM]);
		["CON",Nombre] ->spawn(?MODULE,new_nombre,[Nombre,Psocket]);

		["LSG"] -> show_games(Psocket);

		["NEW",ID] -> new_game(ID,Psocket,Name);

		["ACC",ID] -> enter_game(ID,Name,Psocket);
		
		["PLA",ID,F] ->play_game(ID,Name,Psocket,F) ;
		
		["OBS",ID] ->spect_game(ID,Name,Psocket);
		
		["LEA",ID] ->leave_spect(ID,Name,Psocket);
		
		"BYE" ->ok;
		
		_ -> io:format("~p, ~p ~n",[nodes(),statistics(total_active_tasks)])
	
	end.%%,spam(0).

spam(N) -> spam(N+1).

%%envı́a a itervalos regulares información de carga a los pbalance de los otros nodos

pstat([]) ->
	timer:sleep(1000),
	pstat(lists:append(nodes(),[node()]));
	
pstat(Nodos = [H|T])-> 
	Carga = statistics(total_active_tasks),
	{balance , H } ! {node(),Carga},
	pstat(T).

	

pbalance(Diccionario)-> %%devuelve el nodo con menos carga
	N = node(),
	receive
%% Agarra el diccionario, lo pasa a lista, lo ordena segun carga,toma la primera tupla, extrae el nodo y se lo manda de vuelta
	{request,Pid,Msg} -> Pid ! {execute,element( 1 ,hd(lists:keysort(2,dict:to_list(Diccionario)))),Msg},pbalance(Diccionario);
	
	{nodedown, Node} -> io:format("Un nodo murio~n"),
					F = fun() -> case mnesia:match_object(juegos,#juegos{id = '_',jg1 = '_',jg2 ='_' ,spect ='_',nodo=Node,pid= '_'},read) of
									[] -> ok;
									List -> end_game_s(List)
								end,
								case mnesia:match_object(nombres,#nombres{name = '_',nodo=Node,psocket = '_'},read) of
									[] -> ok;
									List2 ->lists:foreach(fun(X) -> mnesia:delete_object(nombres,X,write) end,List2)
								end
							end,
							spawn(mnesia,activity,[transaction,F]),db:uninstall(Node),pbalance(dict:erase(Node,Diccionario));
	{N,B} -> pbalance(dict:store(N,B,Diccionario));
	
	{A,B}	->	case dict:is_key(A, Diccionario) of
					false -> monitor_node(A, true), pbalance(dict:store(A,B,Diccionario));
					   _  -> pbalance(dict:store(A,B,Diccionario))
			end	
	end.

new_nombre("error",Pid) -> Pid ! {send,"error"};
new_nombre(Nombre,Pid) ->
	case (string:find(Nombre,"@") == nomatch) and (string:find(Nombre,"&") == nomatch) and (string:find(Nombre,"¬") == nomatch) and (string:find(Nombre,"-") == nomatch) of
		false -> Pid ! {send,"error"},exit(normal);
		true -> ok
	end,
	F = fun() -> case mnesia:read(nombres,Nombre) of
					[] -> A = mnesia:write(nombres,#nombres{name = Nombre,nodo=node(Pid),psocket= Pid},write)
						,Pid ! {login,Nombre}, io:format("aa~p~n",[A]) ;
					A  -> Pid ! {send,"error"}, io:format("Nametaken ~p~n",[A])
				end
			end,
	mnesia:activity(transaction,F),
	io:format("ok~n").

delete_nombre(Nombre) -> 
	F = fun() -> case mnesia:read(nombres,Nombre) of
			[] -> ok;
			A  -> mnesia:delete(nombres,Nombre,write)
			end
		end,
		mnesia:activity(transaction,F).

%% Intenta crear un juego con el id ID, si esta usado genera el usuario id@Nombre, si ese tambien esta usado manda error
new_game(ID,Psocket,Name) ->
	F = fun() ->	case mnesia:read(juegos,ID) of
					[] ->mnesia:write(juegos,#juegos{id = ID,jg1 = Name,jg2 = " " ,spect = [],nodo=node(Psocket),pid = " "},write)
						,Psocket ! {send,ID};
					A  -> case string:find(ID,"@") of
							nomatch -> new_game(ID++"@"++Name,Psocket,Name);
							_ -> Psocket ! {send,"Ese Nombre esa usado"}, io:format("Nametaken ~p~n",[A])
					end
				end
			end,
	mnesia:activity(transaction,F),
	io:format("ok~n").

displayer([],String,N)-> String;
displayer([H|T],String,N) ->
	%%Me fijo si hay player 2
	case H#juegos.jg2 of
	   " " -> A = "Join "++H#juegos.id++"|";
		_  -> A = "Spect "++H#juegos.id++"|"
	end,
	case N of
		0 -> B = A ++ "~n";
		_ -> B = A
	end,
	displayer(T,String++B, ((N+1) rem 10) ).

show_games(Psocket)->
	F = fun() -> case mnesia:match_object(juegos,#juegos{id = '_',jg1 = '_',jg2 ='_' ,spect ='_',nodo='_',pid='_'},read) of
					[] ->Psocket ! {send,"No hay Juegos disponibles"};
					List ->Psocket ! {send,displayer(List,"",1)}
				end
		end,
		mnesia:activity(transaction,F),
		io:format("ok~n").

enter_game(Id,Name,Psocket) ->
	F = fun() -> case mnesia:wread({juegos,Id}) of
		[] -> Psocket ! {send,"Not Found"};
		[A] -> case (A#juegos.jg2 == " ") and not(A#juegos.jg1 == Name ) of
				true ->Table = ttt:faux(),
					   Pid = spawn(ttt,partida,[Id,Table,0," "," "," "," "]),
					   P = A#juegos{jg2= Name},
					   P2 = P#juegos{pid= Pid},
					   mnesia:write(P2),
					   Psocket ! {send,"ok"};

				 _  -> Psocket ! {send,"Ocupado"}
			end
		end
	end,mnesia:transaction(F),io:format("ok~n").

spect_game(Id,Name,Psocket) ->
		F = fun() -> case mnesia:wread({juegos,Id}) of
			[] -> Psocket ! {send,"Not Found"};
			[A] -> case lists:member({Name,Psocket},A#juegos.spect) of
					false -> List = [{Name,Psocket}]++A#juegos.spect, P = A#juegos{spect = List},mnesia:write(P),
						   Psocket ! {send,"ok"};
					 _  -> Psocket ! {send,"Ocupado"}
				end
			end
		end,mnesia:transaction(F),io:format("ok~n").

leave_spect(Id,Name,Psocket) ->
		F = fun() -> case mnesia:wread({juegos,Id}) of
			[] -> Psocket ! {send,"Not Found"};
			[A] -> case lists:member({Name,Psocket},A#juegos.spect) of
					true -> List = A#juegos.spect--[{Name,Psocket}], P = A#juegos{spect = List},mnesia:write(P),
						   Psocket ! {send,"ok"};
					 _  -> Psocket ! {send,"ok"}
						end
			end
		end,mnesia:transaction(F),io:format("ok~n").


kill_game(ID) -> 
	F = fun() ->
			case mnesia:wread({juegos,ID}) of
				[] -> ok;
				_ -> mnesia:delete({juegos,ID})
			end
		end,
	mnesia:activity(transaction,F).



play_game(ID,Name,Psocket,Jugada) ->
	F = fun()-> 
			case mnesia:read({juegos,ID}) of
				[] -> Psocket ! {send,"No"};
				[A] -> case (A#juegos.jg1 == Name) or (A#juegos.jg2 == Name) of
						false -> Psocket ! {send,"NoTuyo"};
						true -> 
							case A#juegos.pid of
							 	" " -> case Jugada of
										"-1" ->kill_game(ID),Psocket ! {send,"Elm"};
										 _ -> Psocket ! {send,"Inact"}
										end;
								Pid -> Pid ! {Name,Jugada}
						end
					end
				end
			end,
		mnesia:activity(transaction,F),io:format("ok~n").


end_games(Nombre)-> 
	F = fun()-> case mnesia:match_object(juegos,#juegos{id = '_',jg1 = Nombre,jg2 ='_' ,spect ='_',nodo='_',pid='_'},read)
				++ mnesia:match_object(juegos,#juegos{id = '_',jg1 = '_',jg2 =Nombre ,spect ='_',nodo='_',pid='_'},read) of
				[] -> ok;
				A -> end_game(A,Nombre)
			end
		end,
		mnesia:transaction(F).


end_game_s([])->ok;
end_game_s([H|T])->
	case H#juegos.pid of
		" " -> kill_game(H#juegos.id);
		Pid -> Pid ! {H#juegos.jg1,"-1"},kill_game(H#juegos.id),exit(Pid,kill)
	end,
		end_game_s(T).


end_game([],Nombre)->ok;
end_game([H|T],Nombre)->
	case H#juegos.pid of
		" " -> kill_game(H#juegos.id);
		Pid -> Pid ! {Nombre,"-1"}%%,kill_game(H#juegos.id)
	end,
	end_game(T,Nombre).