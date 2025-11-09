% knowledge/core/kb_central.pl

:- module(kb_central, [
    sintoma_de/2,
    tiene/2,
    categoria_de_enfermedad/2
]).

:- use_module(core(kb_base), [tiene/2]).
:- use_module(knowledge(infecciosas/kb_infecciones), []).
:- use_module(knowledge(traumatologicas/kb_traumatologicas), []).
:- use_module(knowledge(cardiovasculares/kb_cardiovasculares), []).
:- use_module(knowledge(neurologicas/kb_neurologicas), []).
:- use_module(knowledge(digestivas/kb_digestivas), []).
:- use_module(knowledge(respiratorias/kb_respiratorias), []).

% ------------------------------------
% Unificar acceso a todos los síntomas
% ------------------------------------
sintoma_de(E, S) :- kb_infecciones:sintoma_de(E, S).
sintoma_de(E, S) :- kb_traumatologicas:sintoma_de(E, S).
sintoma_de(E, S) :- kb_cardiovasculares:sintoma_de(E, S).
sintoma_de(E, S) :- kb_neurologicas:sintoma_de(E, S).
sintoma_de(E, S) :- kb_digestivas:sintoma_de(E, S).
sintoma_de(E, S) :- kb_respiratorias:sintoma_de(E, S).

% ------------------------------------
% Clasificación de enfermedades
% ------------------------------------
categoria_de_enfermedad(E, infecciosa)     :- kb_infecciones:es_dolencia_infecciosa(E).
categoria_de_enfermedad(E, traumatica)     :- kb_traumatologicas:es_dolencia_traumatica(E).
categoria_de_enfermedad(E, cardiovascular) :- kb_cardiovasculares:es_dolencia_cardiovascular(E).
categoria_de_enfermedad(E, neurologica)    :- kb_neurologicas:es_dolencia_neurologica(E).
categoria_de_enfermedad(E, digestiva)      :- kb_digestivas:es_dolencia_digestiva(E).
categoria_de_enfermedad(E, respiratoria)   :- kb_respiratorias:es_dolencia_respiratoria(E).
