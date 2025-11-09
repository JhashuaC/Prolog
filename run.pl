:- initialization(main, main).
:- set_prolog_flag(encoding, utf8).

:- ensure_loaded('server/paths.pl').
:- use_module('server/server.pl', [server/1, stop_all/0]).

main :-
	% Obtener el puerto asignado por Railway
(getenv('PORT', PortAtom) ->
	atom_number(PortAtom, Port);
	Port = 8080% Fallback local
	),
	% Detener servidores activos previos
	catch(stop_all, _, true),
	% Iniciar servidor
	format('🚀 Iniciando servidor en puerto ~w~n', [Port]),
	catch(server(Port),
		E,
		(print_message(error, E),
			halt(1))),
	% Mantener proceso vivo (Railway necesita esto)
	thread_get_message(quit).
