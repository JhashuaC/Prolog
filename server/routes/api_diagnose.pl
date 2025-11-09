% server/routes/api_diagnose.pl
:- module(api_diagnose, [api_diagnose/1]).
:- use_module(library(http/http_json)).
:- use_module(core(kb_inferencia)).
:- use_module(core(kb_central)).
:- use_module(server(utils/logger)).

api_diagnose(Req) :-
    http_read_json_dict(Req, Dict),
    Syms0 = Dict.symptoms,
    maplist(to_atom, Syms0, Syms),

    retractall(kb_central:tiene(temp, _)),
    forall(member(S, Syms), assertz(kb_central:tiene(temp, S))),

    (   tipo_probable(temp, Tipo) -> true ; Tipo = desconocido ),
    (   enfermedades_posibles(temp, Enferms) -> true ; Enferms = [] ),
    (   explicacion_categoria(temp, Tipo, Detalle) -> true ; Detalle = _{} ),

    reply_json_dict(_{
        tipo: Tipo,
        enfermedades: Enferms,
        detalle: Detalle
    }).

to_atom(X, A) :- (atom(X) -> A=X ; atom_string(A, X)).
