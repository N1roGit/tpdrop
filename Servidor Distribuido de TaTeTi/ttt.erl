-module(ttt).
-compile(export_all).
-record(nombres,{name,nodo,psocket}).
-record(juegos,{id,jg1,jg2,spect=[],nodo,pid}).

%%Si se cae la partida por alguna razon, le avisa a crash y borra el registro

crash(Pid,ID,Psocket1,Psocket2)->
    erlang:monitor(process, Pid),
    receive
        {'DOWN', _ , _, _, normal} ->ok;
        {'DOWN', _ , _, _, _} -> server:kill_game(ID),Psocket1!{send,"Algo salio mal con "++ID++"~n ¬"},
                                Psocket2!{send,"Algo salio mal con "++ID++"~n ¬"},Psocket1!{send,"¬¬"}
    end.

%%{" "," "," "," "," "," "," "," "," "}
%% Si turno es impar le toca jugar al jugador 1, sino al 2 y si llega a cierta cantidad de turnos cierro todo y declaro empate
partida(ID,Tablero,Turno,Name1,Psocket1,Name2,Psocket2) ->
   %%Inicializa los datos del juego
    io:format("self :~p ~p ~p ~p ~p ~p ~p ~p~n",[self(),ID,Tablero,Turno,Name1,Psocket1,Name2,Psocket2]),
     case (Psocket1 == " ") of
        true -> 
            F = fun() -> 
                    case mnesia:read(juegos,ID) of
                        [P1] ->
                            case mnesia:read(nombres,P1#juegos.jg1) of
                                [] -> P2 = 0,exit(normal);
                                [A] -> P2 = A#nombres.psocket
                            end,
                            case mnesia:read(nombres,P1#juegos.jg2) of
                                [] ->P3 = 0, exit(normal);
                                [B] -> P3 = B#nombres.psocket
                            end,
                            Res = {P1#juegos.jg1,P2,P1#juegos.jg2,P3};

                            _  -> Res = ok,exit(normal)                         
                            end,
                            Res 
                    end,
            {atomic,Arg} = mnesia:transaction(F),
            element(2,Arg) ! {send,ID++": "++element(3,Arg)++" acepto tu partida ~n ¬"},
            spawn(?MODULE,crash,[self(),ID,element(2,Arg),element(4,Arg)]),
            partida(ID,Tablero,Turno+1,element(1,Arg),element(2,Arg),element(3,Arg),element(4,Arg));
        _ -> ok
    end,
    case (gano(Tablero,"X")) of
        true -> Aux = ID++": Gano "++Name1++"!! ¬",spect_sender(get_spects(ID),Aux),
            Psocket1 ! {send,ID++": Ganaste!! ¬"}, Psocket2 ! {send,Aux},
            Psocket1 !{send,"¬¬"},
            server:kill_game(ID),exit(normal);
            %%gano player1
        _ -> ok
    end,
    case(gano(Tablero,"O")) of
        true -> Aux2 = ID++": Gano "++Name2++"!! ¬",spect_sender(get_spects(ID),Aux2),
        Psocket1 !{send,"¬¬"},
        Psocket2 ! {send,ID++": Ganaste!! ¬"}, Psocket1 ! {send,Aux2},server:kill_game(ID),exit(normal);
    _ -> ok
    end,

    %%Esta el tablero lleno y no gano nadie
    case(Turno==10)of
        true -> Send = ID++": Empate!!~n ¬",spect_sender(get_spects(ID),Send),
            Psocket2 ! {send,Send}, Psocket1 ! {send,Send},
            Psocket1 !{send,"¬¬"}, 
            server:kill_game(ID),exit(normal);
        _ -> ok
    end,

    %%Se fija si ya esta el turno que se empata
    receive
            {Pj,"-1"} -> case Pj == Name1 of
                            true -> Aux3 = ID++": Gano "++Name2++" por abandono!! ¬",spect_sender(get_spects(ID),Aux3),
                                    Psocket1 !{send,"ok"},Psocket1 !{send,"¬¬"},
                                    Psocket2 ! {send,ID++": Ganaste por abandono!! ¬"}, Psocket1 ! {send,Aux3},server:kill_game(ID),exit(normal);

                            false -> Aux3 = ID++": Gano "++Name1++" por abandono!! ¬",spect_sender(get_spects(ID),Aux3),
                                    Psocket2 !{send,"ok"},Psocket1 !{send,"¬¬"}, 
                                    Psocket1 ! {send,ID++": Ganaste por abandono!! ¬"}, Psocket2 ! {send,Aux3},server:kill_game(ID),exit(normal)
                        end;

            {Pj,C}   -> Posicion = element(1,string:to_integer(C)),
                        case (Name1 == Pj) and ((Turno rem 2) == 1) of
                            true -> case (not(Posicion == error) and (Turno>0) and (0 < Posicion)  and (Posicion < 10 )) andalso (dict:fetch(Posicion,Tablero) == " ") of

                                        true -> TableroU = dict:store(Posicion,"X",Tablero),
                                                Psocket1 !{send,"ok"},
                                                %%Manda pantallazo a los jugadores/espectadores y le avisa a P2 que es su turno
                                                D = table_display(ID,Name1,Name2,TableroU),
                                                Psocket1 ! {send,D} , Psocket2 ! {send,D} ,Psocket2 ! {send,ID++":Tu turno ¬"},
                                                spect_sender(get_spects(ID),D),
                                            partida(ID,TableroU,Turno+1,Name1,Psocket1,Name2,Psocket2);

                                        _  -> Psocket1 ! {send,"Ilegal"},
                                            partida(ID,Tablero,Turno,Name1,Psocket1,Name2,Psocket2)
                                    end;
                            false -> case (Name1 == Pj) of
                                    true ->Psocket1 !{send,"NoTurno"},
                                           partida(ID,Tablero,Turno,Name1,Psocket1,Name2,Psocket2); %%No es tu turno
                                    false -> ok
                                    end
                        end,


                        case (Name2 == Pj) and ((Turno rem 2) == 0) of
                            true -> case (not(Posicion == error) and (Turno>0) and (Posicion > 0) and (Posicion < 10)) andalso (dict:fetch(Posicion,Tablero) == " ") of
                                        true -> TableroU2 = dict:store(Posicion,"O",Tablero),
                                                Psocket2 ! {send,"ok"},
                                                %%Manda pantallazo a los jugadores/espectadores y le avisa a P1 que es su turno
                                                D2 = table_display(ID,Name1,Name2,TableroU2),
                                                Psocket2 ! {send,D2} , Psocket1 ! {send,D2} ,Psocket1 ! {send,ID++":Tu turno ¬"},
                                                spect_sender(get_spects(ID),D2),
                                                partida(ID,TableroU2,Turno+1,Name1,Psocket1,Name2,Psocket2);

                                        _  -> Psocket2 ! {send,"Ilegal"},
                                            partida(ID,Tablero,Turno,Name1,Psocket1,Name2,Psocket2)
                                    end;
                            false -> Psocket2 !{send,"NoTurno"},
                                    partida(ID,Tablero,Turno,Name1,Psocket1,Name2,Psocket2) %%No es tu turno
                        end
        after 180000 -> server:kill_game(ID),Psocket1 !{send,"¬¬"},Psocket1 ! {send,ID++": Timeout~n ¬"},Psocket2 ! {send,ID++": Timeout~n ¬"},exit(normal)
    end,
    partida(ID,Tablero,Turno,Name1,Psocket1,Name2,Psocket2).

get_spects(ID)->
    F = fun() -> [A] = mnesia:read(juegos,ID),A#juegos.spect end,
    case mnesia:transaction(F) of
    {atomic,Res} -> ok;
        _        -> Res = []
    end,
    Res.

table_display(ID,J1,J2,Dict) -> 
    Str = 
    "<<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>>~n"
    ++ J1 ++" X" ++" < VS > "++ J2 ++ " O   En juego: " ++ ID ++ "~n" ++
    dict:fetch(1,Dict) ++ " | " ++ dict:fetch(2,Dict) ++ " | " ++ dict:fetch(3,Dict) ++ "~n" ++
    dict:fetch(4,Dict) ++ " | " ++ dict:fetch(5,Dict) ++ " | " ++ dict:fetch(6,Dict) ++ "~n" ++
    dict:fetch(7,Dict) ++ " | " ++ dict:fetch(8,Dict) ++ " | " ++ dict:fetch(9,Dict) ++ "~n" ++
    "<<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>>~n"++" & ",
    Str.

gano(T,X) ->
	(vertical(T,X) or horizontal(T,X)) or diagonal(T,X).

vertical(T,X) ->
	((dict:fetch(1,T) == X) and (dict:fetch(4,T) == X) and (dict:fetch(7,T) == X)) or
	((dict:fetch(2,T) == X) and (dict:fetch(5,T) == X) and (dict:fetch(8,T) == X)) or
	((dict:fetch(3,T) == X) and (dict:fetch(6,T) == X) and (dict:fetch(9,T) == X)).

horizontal(T,X) ->
    ((dict:fetch(1,T) == X) and (dict:fetch(2,T) == X) and (dict:fetch(3,T) == X)) or
	((dict:fetch(4,T) == X) and (dict:fetch(5,T) == X) and (dict:fetch(6,T) == X)) or
	((dict:fetch(7,T) == X) and (dict:fetch(8,T) == X) and (dict:fetch(9,T) == X)).

diagonal(T,X) ->
	(((dict:fetch(1,T) == X) and (dict:fetch(5,T) == X)) and (dict:fetch(9,T) == X)) or
    ((dict:fetch(7,T) == X) and (dict:fetch(5,T) == X) and (dict:fetch(3,T) == X)).

spect_sender([],_)-> ok;
spect_sender([H|T],Tablero) -> 
    element(2,H) ! {send,Tablero},
    spect_sender(T,Tablero).

%%spect_sender(List,Tablero) -> 
 %%list:foreach(fun(X)-> element(2,X) ! {send,Tablero} end,List).

faux()->
	T1 = dict:store(1," ",dict:new()),
	T2 = dict:store(2," ",T1),
	T3 = dict:store(3," ",T2),
	T4 = dict:store(4," ",T3),
	T5 = dict:store(5," ",T4),
	T6 = dict:store(6," ",T5),
	T7 = dict:store(7," ",T6),
	T8 = dict:store(8," ",T7),
	T9 = dict:store(9," ",T8),
	T9.