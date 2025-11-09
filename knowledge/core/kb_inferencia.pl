% knowledge/core/kb_inferencia.pl

:- module(kb_inferencia, [
    tipo_probable/2,
    enfermedades_posibles/2,
    explicacion_categoria/3
]).

:- use_module('./kb_central').
:- use_module(core(kb_base), [normalizar_sintoma/2]).
:- use_module(library(lists)).

% ------------------------------------------------------
% Determina el tipo probable según el mayor puntaje por categoría
% ------------------------------------------------------
tipo_probable(Paciente, Tipo) :-
    mejor_categoria_por_score(Paciente, Tipo),
    Tipo \= desconocido, !.
tipo_probable(_, desconocido).

% ------------------------------------------------------
% Enfermedades candidatas (≥2 síntomas coincidentes)
% ------------------------------------------------------
enfermedades_posibles(Paciente, Enfermedades) :-
    findall(Enfermedad,
        (
            sintoma_de(Enfermedad, _),
            score_enfermedad(Paciente, Enfermedad, Score),
            Score >= 2
        ),
        L),
    sort(L, Enfermedades).

% ------------------------------------------------------
% Explicación detallada (formato JSON para frontend)
% ------------------------------------------------------
explicacion_categoria(Paciente, Tipo, _{
    categoria: Tipo,
    score_categoria: Score,
    top_enfermedades: TopJSON
}) :-
    ( categoria_score(Paciente, Tipo, Score0) -> Score = Score0 ; Score = 0 ),
    findall([Puntaje, Enfermedad],
        (
            sintoma_de(Enfermedad, _),
            score_enfermedad(Paciente, Enfermedad, Puntaje),
            Puntaje > 0
        ),
        Pares0),
    ( Pares0 == [] ->
        TopJSON = []
    ;
        sort(Pares0, Desc),
        reverse(Desc, Top),
        maplist(pair_to_json, Top, TopJSON)
    ).

pair_to_json([Score, Enfermedad], _{score: Score, enfermedad: Enfermedad}).

% ------------------------------------------------------
% Calcula qué categoría tiene el mejor puntaje
% ------------------------------------------------------
mejor_categoria_por_score(Paciente, MejorCategoria) :-
    findall(Score-Cat,
        (
            categoria_de_enfermedad(_, Cat),
            categoria_score(Paciente, Cat, Score)
        ),
        Pares0),
    exclude(=(0-_), Pares0, Pares),
    ( Pares == [] ->
        MejorCategoria = desconocido
    ;
        sort(Pares, Ordenadas),
        last(Ordenadas, _-MejorCategoria)
    ).

% ------------------------------------------------------
% Calcula el puntaje total de una categoría
% ------------------------------------------------------
categoria_score(Paciente, Categoria, TotalScore) :-
    findall(Score,
        (
            categoria_de_enfermedad(Enfermedad, Categoria),
            score_enfermedad(Paciente, Enfermedad, Score)
        ),
        Scores),
    sum_list(Scores, TotalScore), !.
categoria_score(_, _, 0).

% ------------------------------------------------------
% Puntaje de coincidencia de síntomas con una enfermedad
% ------------------------------------------------------
score_enfermedad(Paciente, Enfermedad, Score) :-
    findall(SintomaNormalizado,
        (
            tiene(Paciente, SintomaUsuario),
            normalizar_sintoma(SintomaUsuario, SintomaNormalizado),
            sintoma_de(Enfermedad, SintomaNormalizado)
        ),
        ListaCoincidencias),
    length(ListaCoincidencias, Score), !.
score_enfermedad(_, _, 0).
