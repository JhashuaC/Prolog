% server/routes/api_symptoms.pl
:- module(api_symptoms, [api_symptoms/1]).
:- use_module(library(http/http_json)).
:- use_module(library(apply)).             % para maplist/3 y exclude/3
:- use_module(core(kb_central), [sintoma_de/2]).
:- use_module(server(utils/logger)).

% Endpoint GET /api/symptoms
api_symptoms(_Req) :-
    catch(
        (
            setof(S, E^(sintoma_de(E,S)), All0),  % usa setof para evitar duplicados
            exclude(var, All0, Clean),             % elimina variables sin instanciar
            length(Clean, N),
            log(cyan, '→ Enviando ~w síntomas~n', [N]),
            reply_json_dict(_{symptoms: Clean})
        ),
        E,
        (
            log(red, ' Error en api_symptoms: ~w~n', [E]),
            reply_json_dict(_{error: 'No se pudieron obtener los síntomas'}, [status(500)])
        )
    ).
