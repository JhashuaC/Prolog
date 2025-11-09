% server/routes/api_queries.pl
:- module(api_queries, [
    api_sintomas_de/1,
    api_enfermedades_por_sintoma/1,
    api_categoria_enfermedad/1,
    api_enfermedades_posibles/1
]).

:- use_module(library(http/http_json)).
:- use_module(library(apply)).

% Trae los predicados del core
:- use_module(core(kb_central), [
    sintoma_de/2,
    categoria_de_enfermedad/2,
    tiene/2
]).
:- use_module(core(kb_inferencia), [
    enfermedades_posibles/2
]).
:- use_module(server(utils/logger)).

% -------- utilidades --------
to_atom_value(V, A) :- (atom(V) -> A = V ; atom_string(A, V)).
to_norm_atom(V, AOut) :-
    to_atom_value(V, A0),
    downcase_atom(A0, AOut).   % minúsculas; evita problemas de "Neumonia" vs "neumonia"

% ---------------------------
% Síntomas de una enfermedad
% ---------------------------
api_sintomas_de(Req) :-
    http_read_json_dict(Req, D),
    to_norm_atom(D.get(enfermedad), Enfer),
    log(cyan, '[api] sintomas_de ~w~n', [Enfer]),
    ( setof(S, sintoma_de(Enfer,S), Ss) -> true ; Ss = [] ),
    reply_json_dict(_{enfermedad:Enfer, sintomas:Ss}).

% ---------------------------
% Enfermedades por síntoma
% ---------------------------
api_enfermedades_por_sintoma(Req) :-
    http_read_json_dict(Req, D),
    to_norm_atom(D.get(sintoma), Sint),
    log(cyan, '[api] enf_por_sintoma ~w~n', [Sint]),
    ( setof(E, sintoma_de(E,Sint), Es) -> true ; Es = [] ),
    reply_json_dict(_{sintoma:Sint, enfermedades:Es}).

% ---------------------------
% Categoría de una enfermedad
% ---------------------------
api_categoria_enfermedad(Req) :-
    http_read_json_dict(Req, D),
    to_norm_atom(D.get(enfermedad), Enfer),
    log(cyan, '[api] categoria_enfermedad ~w~n', [Enfer]),
    ( categoria_de_enfermedad(Enfer, Cat) -> true ; Cat = desconocida ),
    reply_json_dict(_{enfermedad:Enfer, categoria:Cat}).

% ---------------------------
% Enfermedades posibles por síntomas
% ---------------------------
api_enfermedades_posibles(Req) :-
    http_read_json_dict(Req, D),
    maplist(to_norm_atom, D.get(sintomas), Syms),

    retractall(kb_central:tiene(temp,_)),
    forall(member(S, Syms), assertz(kb_central:tiene(temp,S))),

    ( enfermedades_posibles(temp, Es) -> true ; Es = [] ),
    reply_json_dict(_{sintomas:Syms, enfermedades_posibles:Es}).
