:- module(server, [server/0, stop/0]).
:- set_prolog_flag(encoding, utf8).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_cors)).

:- use_module(server(routes/api_diagnose)).
:- use_module(server(routes/api_symptoms)).
:- use_module(server(routes/ui_page)).
:- use_module(server(routes/api_queries)).
:- use_module(server(utils/logger)).

% ===========================
%  CORS CONFIG — TODO PERMITIDO
% ===========================

:- set_setting(http:cors, [*]).  % permitir todos los orígenes

enable_cors(Request) :-
	cors_enable(Request,
		[
			methods([get, post, options]),
			headers(['Content-Type', 'Authorization', 'Accept'])]).

% ===========================
%  HANDLERS
% ===========================

:- http_handler('/', ui_page, [prefix]).
:- http_handler('/app.js', serve_app_js, [prefix]).

:- http_handler('/api/ping', api_ping_handler, [prefix]).
:- http_handler('/api/symptoms', api_symptoms_handler, [prefix]).
:- http_handler('/api/diagnose', api_diagnose_handler, [prefix]).
:- http_handler('/api/sintomas_de', api_sintomas_de_handler, [prefix]).
:- http_handler('/api/enfermedades_por_sintoma', api_enfermedades_por_sintoma_handler, [prefix]).
:- http_handler('/api/categoria_enfermedad', api_categoria_enfermedad_handler, [prefix]).
:- http_handler('/api/enfermedades_posibles', api_enfermedades_posibles_handler, [prefix]).
:- http_handler('/', cors_options_handler, [method(options), prefix]).

% ===========================
%  OPTIONS (Preflight)
% ===========================

cors_options_handler(Request) :-
	enable_cors(Request),
	format('Content-type: text/plain~n~n'),
	format('OK~n').

% ===========================
%  API ROUTES
% ===========================

api_ping_handler(Request) :-
	enable_cors(Request),
	reply_json_dict(_{ok : true, ts : now}).

api_symptoms_handler(Request) :-
	enable_cors(Request),
	api_symptoms(Request).

api_diagnose_handler(Request) :-
	enable_cors(Request),
	catch(api_diagnose(Request),
		E,
		(message_to_string(E, Msg),
			reply_json_dict(_{
					error : Msg
					},
				[status(500)]))).

api_sintomas_de_handler(Request) :-
	enable_cors(Request),
	api_queries : api_sintomas_de(Request).

api_enfermedades_por_sintoma_handler(Request) :-
	enable_cors(Request),
	api_queries : api_enfermedades_por_sintoma(Request).

api_categoria_enfermedad_handler(Request) :-
	enable_cors(Request),
	api_queries : api_categoria_enfermedad(Request).

api_enfermedades_posibles_handler(Request) :-
	enable_cors(Request),
	api_queries : api_enfermedades_posibles(Request).

% ===========================
%  STATIC FILES
% ===========================

serve_app_js(Request) :-
	enable_cors(Request),
	http_reply_file('./static/app.js',
		[unsafe(true), mime_type('application/javascript')],
		[]).

% ===========================
%  SERVER CONTROL
% ===========================

server :-
	(getenv('PORT', PortAtom) ->
	atom_number(PortAtom, Port);
	Port = 3000),
	catch(http_server(http_dispatch,
			[port(Port), ip('0.0.0.0'), encoding(utf8)]),
		E,
		(log(red, 'Error iniciando servidor: ~w~n', [E]),
			fail)),
	log(green, 'Servidor iniciado en puerto ~w (modo abierto)~n', [Port]).

stop :-
	findall(P,
		(current_prolog_flag(http_server, S),
			member(_{port : P}, S)),
		Ports),
	forall(member(P, Ports),
		(log(red, 'Deteniendo servidor ~w~n', [P]),
			catch(http_stop_server(P,
					[force(true)]),
				_,
				true))),
	log(green, 'Servidores detenidos.~n', []).
