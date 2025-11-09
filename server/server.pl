:- module(server, [server/1, stop/0, stop_all/0]).
:- set_prolog_flag(encoding, utf8).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_cors)).

:- use_module(server(routes/api_diagnose)).
:- use_module(server(routes/api_symptoms)).
:- use_module(server(routes/ui_page)).
:- use_module(server(utils/logger)).
:- use_module(server(routes/api_queries)).

:- set_setting(http:cors, [*]).

:- http_handler(root(.), ui_page, []).
:- http_handler(root('app.js'), serve_app_js, []).

:- http_handler(root(api/ping), api_ping_handler, [method(get)]).
:- http_handler(root(api/symptoms), api_symptoms_handler, [method(get)]).
:- http_handler(root(api), handle_api_request, []).  % handler genérico para todo /api/*
:- http_handler(root(api), cors_options_handler, [method(options), prefix]).

add_cors_headers :-
	format('Access-Control-Allow-Origin: *~n'),
	format('Access-Control-Allow-Methods: GET, POST, PUT, DELETE, OPTIONS~n'),
	format('Access-Control-Allow-Headers: Content-Type, Authorization, Accept~n').

cors_options_handler(_Request) :-
	add_cors_headers,
	format('Status: 204~n~n').

api_ping_handler(_Request) :-
	add_cors_headers,
	reply_json_dict(_{ok : true, ts : now}).

api_symptoms_handler(Request) :-
	add_cors_headers,
	api_symptoms(Request).

handle_api_request(Request) :-
	member(path(Path),
		Request),
	(sub_atom(Path, _, _, 0, '/api/diagnose') ->
	add_cors_headers,
		(member(method(post),
				Request) ->
	catch(api_diagnose(Request),
				E,
				(message_to_string(E, Msg),
					reply_json_dict(_{
							error : Msg
							},
						[status(500)])));
	member(method(options),
				Request) ->
	cors_options_handler(Request);
	reply_json_dict(_{
					error : 'Método no permitido'
					},
				[status(405)]));
	sub_atom(Path, _, _, 0, '/api/sintomas_de') ->
	add_cors_headers,
		api_queries : api_sintomas_de(Request);
	sub_atom(Path, _, _, 0, '/api/enfermedades_por_sintoma') ->
	add_cors_headers,
		api_queries : api_enfermedades_por_sintoma(Request);
	sub_atom(Path, _, _, 0, '/api/categoria_enfermedad') ->
	add_cors_headers,
		api_queries : api_categoria_enfermedad(Request);
	sub_atom(Path, _, _, 0, '/api/enfermedades_posibles') ->
	add_cors_headers,
		api_queries : api_enfermedades_posibles(Request);
	reply_json_dict(_{
				error : 'Ruta no encontrada'
				},
			[status(404)])).

serve_app_js(_Request) :-
	http_reply_file('./static/app.js',
		[unsafe(true), mime_type('application/javascript')],
		[]).

server(Port) :-
	ignore(stop_all),
	catch(http_server(http_dispatch,
			[port(Port), ip('0.0.0.0'), encoding(utf8)]),
		E,
		(log(red, 'Error iniciando servidor: ~w~n', [E]),
			fail)),
	log(green, 'Servidor iniciado en puerto ~w (escuchando en 0.0.0.0)~n', [Port]).

stop :-
	stop_all.

stop_all :-
	findall(Port,
		(current_prolog_flag(http_server, Servers),
			member(_{port : Port}, Servers)),
		Ports),
	(Ports == [] ->
	log(yellow, 'No hay servidores activos.~n', []);
	forall(member(P, Ports),
			(log(red, 'Cerrando servidor en puerto ~w...~n', [P]),
				catch(http_stop_server(P,
						[force(true)]),
					_,
					true)))),
	sleep(0.2),
	repeat,
	findall(Id - Name,
		(thread_property(Id,
				alias(Name)),
			(sub_atom(Name, 0, _, _, 'http@');
	sub_atom(Name, 0, _, _, 'httpd@'))),
		List),
	(List == [] ->
	!;
	forall(member(Id - Name, List),
			((catch(thread_signal(Id, abort),
						_,
						true),
					catch(thread_join(Id, _),
						_,
						true)) ->
	log(yellow, ' Hilo residual terminado: ~w~n', [Name]);
	log(gray, 'Esperando cierre natural: ~w~n', [Name]))),
		sleep(0.1),
		fail),
	log(green, ' Todos los hilos HTTP terminados correctamente.~n', []).
