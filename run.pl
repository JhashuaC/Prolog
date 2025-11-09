:- initialization(main, main).
:- set_prolog_flag(encoding, utf8).

:- ensure_loaded('server/paths.pl').
:- use_module('server/server.pl', [server/1, stop_all/0]).

main :-
	(getenv('PORT', PortAtom) ->
	atom_number(PortAtom, Port);
	Port = 8080),
	catch(stop_all, _, true),
	format('🚀 Iniciando servidor en puerto ~w~n', [Port]),
	catch(server(Port),
		E,
		(print_message(error, E),
			halt(1))),
	repeat,
	thread_get_message(_),
	fail.
