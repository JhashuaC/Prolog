% run.pl
:- initialization(main, main).
:- set_prolog_flag(encoding, utf8).

:- ensure_loaded('server/paths.pl').
:- use_module('server/server.pl', [server/0, stop/0]).

main :-
	catch(stop, _, true),
	format('Iniciando servidor (Railway, puerto desde env PORT)~n', []),
	catch(server,
		E,
		(print_message(error, E),
			halt(1))),
	repeat,
	thread_get_message(_),
	fail.
