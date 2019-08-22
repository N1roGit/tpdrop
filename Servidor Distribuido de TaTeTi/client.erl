-module(client).
-compile(export_all).

init(Ip,Port) ->
    case gen_tcp:connect(Ip, Port,[binary, {active,true},{nodelay, true}]) of
    	
    	{ok,Sock} -> io:format("Conectado a servidor~n"),register(ngame,spawn(?MODULE,ngames,[0])),pcomando(Sock,"  ");%%register(conn,spawn(?MODULE,client,[Sock]));%%,mensajero(Sock);
    	
    	{error,Reason} ->io:format("Error: ~p~n",[Reason])
    	end.
    %%ok = gen_tcp:close(Sock).

%%Separo los mensajes que me llegan por - y en caso de que lleguen pegados los separo y los despacho
post_office(Pid,Sock,s)->
	register(post,self()),
	post_office(Pid,Sock,1);

post_office(Pid,Sock,_) -> 
	ok = gen_tcp:controlling_process(Sock,self()),
	ok = inet:setopts(Sock, [{active, once}]), %%cambiar a true si no anda
	receive
		{tcp_closed, Socket} -> io:format("Perdiste la conexion al servidor~n"),
								exit(whereis(ngame),kill),exit(Pid,kill),exit(normal);
		{tcp,S,Msj} -> A = string:tokens(binary_to_list(Msj),"-"),
					lists:foreach(fun(M) -> mailman(M,Pid) end, A)
	end,post_office(Pid,Sock,1).

%%Agarra todos los mensajes, si son notificaciones de partida lo hace y listo
%%Si son otros mensajes los manda a pcomando y que haga lo que tenga que hacer
mailman(Msj,Pid) ->
	A = string:tokens(Msj," \n"),
	case lists:last(A) of
		%%Se cerro una partida
		"¬¬" -> ngame ! del;
		%%Mensajes de los juegos
		"¬" -> io:format((Msj--"¬")++"~n");
		%%Tableros
		"&" -> io:format((Msj--"&")++ "~n");
		%%Respuesta a un comando
		 _ -> Pid ! {tcp,a,Msj}%%,io:format("~p~n",[Msj])
		end.



    
pcomando(Sock,"  ")->
	Cmd = io:get_line("~~> "),
	Aux = string:tokens(Cmd," \n"),
	%%io:format("~p~n",[Cmd]),
	inet:setopts(Sock, [{active, once}]),
	case Aux of 
		["CON",Nombre] ->case length(Nombre)<16 of
					   true ->gen_tcp:send(Sock,Cmd),
					   	receive
							{tcp,_, <<"error-">>} -> io:format("Error, intente de nuevo~n"),pcomando(Sock,"  ");
							{tcp,_,Name} -> io:fwrite("Nombre aceptado~nBienvenido "++binary_to_list(Name)++"~n"),
							gen_tcp:controlling_process(Sock,spawn(?MODULE,post_office,[self(),Sock,s])),
							pcomando(Sock,binary_to_list(Name))
						after 10000 -> io:format("Error de conexion~n"),gen_tcp:close(Sock),exit(normal)
						end;
					   false -> io:format("Nombre demasiado largo~n"),pcomando(Sock,"  ")
					end;
		["BYE"] -> gen_tcp:close(Sock),exit(whereis(ngame),kill),exit(self(),kill);
		["HELP"] -> io:format(help()),pcomando(Sock,"  "); 
		_	-> io:format("Por favor conectese primero~n"),pcomando(Sock,"  ")
	end;


pcomando(Sock,Nombre) ->
	Cmd = io:get_line("~~> "),
	Aux = string:tokens(Cmd," \n"),
	case Aux of 
		["CON",_] -> io:fwrite("Ya esta conectado como "++Nombre++"~n");

		["LSG"] -> gen_tcp:send(Sock,Cmd),
					receive
						{tcp,_,List}->io:fwrite("Juegos~n"++List++"~n");
						A->io:format("~p~n",[A])
						after 5000 -> io:format("Algo salio mal~n")
					end;

		["NEW",Id] -> case (length(Id)<15) and (string:find(Id,"@")==nomatch) and  (string:find(Id,"&")==nomatch) and  (string:find(Id,"¬")==nomatch) and  (string:find(Id,"-")==nomatch) of
						true -> ngame ! {check,self()},
								receive A -> ok end,
								case (A < 6) of
									true-> 	
										gen_tcp:send(Sock,Cmd),
										receive
											{tcp,_, "Ese Nombre esta usado" } -> io:format("Ese Nombre esta usado~n");
											{tcp,_, "Hubo un error" } -> io:format("Hubo un error creando el juego~n");
											{tcp,_,ID} -> io:format("Juego creado con ID: "++ID++"~n"), ngame ! add
											after 2000 -> io:format("Hubo un error~n")
										end;
									false -> io:format("Ya creo demasiados juegos~n")
								end;
							
						false -> io:format("Nombre invalido~n")
						end;
						
		["ACC",ID] -> gen_tcp:send(Sock,Cmd),
						receive
							{tcp,_, "Not Found" } -> io:format("No se pudo encontrar el juego~n");
							{tcp,_, "Ocupado" } -> io:format("Ese juego ya tiene todos los jugadores necesarios o usted es el primer jugador~n");
							{tcp,_, "ok" } -> io:format("Te metiste al juego "++ID++" ~n")
						after 1000 -> io:format("Error ~n")
						end;
		
		["PLA",ID,Jugada] ->gen_tcp:send(Sock,Cmd),
							receive
								{tcp,_, "ok" } ->ok;
								{tcp,_, "No" } ->io:format("No existe ese juego ~n");
								{tcp,_, "NoTuyo" } ->io:format("No estas participando en ese juego ~n");
								{tcp,_, "Ilegal" } ->io:format("Jugada ilegal~n");
								{tcp,_, "Inact" } ->io:format("La partida aun no empieza~n");								
								{tcp,_, "NoTurno" } ->io:format("No es tu turno~n");
								{tcp,_, "Elm" } ->io:format("Eliminaste el juego "++ID++"~n"),ngame ! del;
								{tcp,_,A} -> io:format("LLego ~p~n",[A])
								after 1000 -> io:format("Algo salio mal, intente de nuevo~n")
							end;
							
		["OBS",ID] ->gen_tcp:send(Sock,Cmd),
						receive
							{tcp,_, "Not Found" } -> io:format("No se pudo encontrar el juego~n");
							{tcp,_, "Ocupado" } -> io:format("Ya estas anotado en ese juego~n");
							{tcp,_, "ok" } -> io:format("Estas mirando el juego "++ID++"~n")
						after 1000 -> io:format("Error ~n")
						end;
		
		["LEA",ID] ->gen_tcp:send(Sock,Cmd),
					receive
						{tcp,_, "Not Found" } -> io:format("No se pudo encontrar el juego~n");
						{tcp,_, "ok" } -> io:format("No estas mirando el juego "++ID++"~n")
					after 1000 -> io:format("Error ~n")
					end;

		["BYE"] ->client_quit(Sock),exit(self(),kill);
		
		["HELP"] -> io:format(help()); 

		_ -> io:format("Comando invalido~n")
		
		end,
		pcomando(Sock,Nombre).


ngames(N)->
	receive 
		{check,Pid} -> Pid ! N ,M = N;
		add -> M = N+1;
		del -> M = N-1
	end,
	ngames(M).

client_quit(Sock) ->
		gen_tcp:close(Sock),exit(whereis(ngame),kill),exit(whereis(post),kill).

help()->
	Str = 
	"CON name -----> Te conectas con el nombre name (Si esta disponible)~n"++
	"LSG ----------> Lista de juegos disponibles~n"++
	"NEW juego-----> Crea un juego de nombre juego~n"++
	"ACC juego-----> Te une al juego~n"++
	"OBS juego-----> Te permite ver el juego~n"++
	"LEA juego-----> Dejas de ver el juego~n"++
	"PLA juego N---> Haces una jugada en el casillero N, Con -1 eliminas el juego o abandonas~n"++
	"BYE -----------> Salis del programa~n",
	Str.