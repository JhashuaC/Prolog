% server/server.pl
:- module(server, [server/1, stop/0, stop_all/0]).
:- set_prolog_flag(encoding, utf8).

% --- Librerías HTTP ---
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_server)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_cors)).  % CORS

% --- Rutas internas ---
:- use_module(server(routes/api_diagnose)).
:- use_module(server(routes/api_symptoms)).
:- use_module(server(routes/ui_page)).
:- use_module(server(utils/logger)).
:- use_module(server(routes/api_queries)).  % Consultas avanzadas

% =====================================
% Handlers HTTP (con wrappers CORS)
% =====================================

% Página UI
:- http_handler(root(.),                  ui_page,                           []).

% Static app.js
:- http_handler(root('app.js'),           serve_app_js,                      []).

% API básica
:- http_handler(root(api/ping),           api_ping,                          [method(get)]).
:- http_handler(root(api/symptoms),       h_api_symptoms,                    [method(get)]).
:- http_handler(root(api/diagnose),       h_api_diagnose,                    [method(post)]).

% API consultas avanzadas (todas por POST)
:- http_handler(root(api/sintomas_de),              h_api_sintomas_de,              [method(post)]).
:- http_handler(root(api/enfermedades_por_sintoma), h_api_enfermedades_por_sintoma, [method(post)]).
:- http_handler(root(api/categoria_enfermedad),     h_api_categoria_enfermedad,     [method(post)]).
:- http_handler(root(api/enfermedades_posibles),    h_api_enfermedades_posibles,    [method(post)]).

% Preflight CORS para /api/*
:- http_handler(root(api), options_cors, [method(options), prefix]).



% =====================================
% Wrappers que habilitan CORS y delegan
% =====================================

allow_cors(Req) :- cors_enable(Req, [methods([get,post,options])]).

api_ping(Req) :-
    allow_cors(Req),
    reply_json_dict(_{ok:true, ts:now}).

h_api_symptoms(Req) :-
    allow_cors(Req),
    api_symptoms(Req).

h_api_diagnose(Req) :-
    allow_cors(Req),
    api_diagnose(Req).

h_api_sintomas_de(Req) :-
    allow_cors(Req),
    api_queries:api_sintomas_de(Req).

h_api_enfermedades_por_sintoma(Req) :-
    allow_cors(Req),
    api_queries:api_enfermedades_por_sintoma(Req).

h_api_categoria_enfermedad(Req) :-
    allow_cors(Req),
    api_queries:api_categoria_enfermedad(Req).

h_api_enfermedades_posibles(Req) :-
    allow_cors(Req),
    api_queries:api_enfermedades_posibles(Req).

options_cors(Req) :-
    allow_cors(Req),
    reply_json_dict(_{}, [status(204)]).

% =====================================
% Servidor
% =====================================

server(Port) :-
    ignore(stop_all),
    catch(
        http_server(http_dispatch, [port(Port), encoding(utf8)]),
        E,
        ( log(red, 'Error iniciando servidor: ~w~n', [E]), fail )
    ),
    log(green, 'Servidor iniciado en puerto ~w~n', [Port]).

stop :- stop_all.

% stop_all/0 — Detiene todos los HTTP activos
stop_all :-
    % --- Detener servidores activos ---
    findall(Port,
        ( current_prolog_flag(http_server, Servers),
          member(_{port:Port}, Servers)
        ),
        Ports),
    ( Ports == [] ->
        log(yellow, 'No hay servidores activos.~n', [])
    ; forall(member(P, Ports),
          ( log(red, 'Cerrando servidor en puerto ~w...~n', [P]),
            catch(http_stop_server(P, [force(true)]), E,
                  log(red, ' Error al cerrar servidor (~w): ~w~n', [P, E]))
          )
      ),
      log(green, 'Servidores HTTP detenidos correctamente.~n', [])
    ),

    % --- Espera breve para evitar race condition ---
    sleep(0.2),

    % --- Forzar terminación de hilos residuales http@ / httpd@ ---
    repeat,
        findall(Id-Name, (
            thread_property(Id, alias(Name)),
            ( sub_atom(Name, 0, _, _, 'http@')
            ; sub_atom(Name, 0, _, _, 'httpd@')
            )
        ), List),
        ( List == [] ->
            !
        ; forall(member(Id-Name, List),
              (
                ( catch(thread_signal(Id, abort), _, true),
                  catch(thread_join(Id, _), _, true)
                ->  log(yellow, ' Hilo residual terminado: ~w~n', [Name])
                ;   log(gray, 'Esperando cierre natural: ~w~n', [Name])
                )
              )
          ),
          sleep(0.1),
          fail
        ),
    log(green, ' Todos los hilos HTTP terminados correctamente.~n', []).

% Servir app.js (frontend)
serve_app_js(_Req) :-
    http_reply_file('./static/app.js',
        [unsafe(true), mime_type('application/javascript')], []).