% knowledge/core/kb_base.pl
:- module(kb_base, [
    tiene/2,
    sintoma_de/2,
    normalizar_sintoma/2
]).

:- dynamic tiene/2.

% ------------------------------------------------
% Ejemplo base
% ------------------------------------------------
sintoma_de(desconocido, ninguno).

tiene(ana, fiebre).
tiene(ana, tos).

% ------------------------------------------------
% Normalización de síntomas equivalentes
% ------------------------------------------------
% Permite agrupar síntomas con nombres distintos pero mismo significado.
% ------------------------------------------------

% Casos de fiebre
normalizar_sintoma(fiebre, fiebre).
normalizar_sintoma(fiebre_alta, fiebre).
normalizar_sintoma(fiebre_leve, fiebre).
normalizar_sintoma(fiebre_prolongada, fiebre).

% Casos respiratorios
normalizar_sintoma(tos, tos).
normalizar_sintoma(tos_seca, tos).
normalizar_sintoma(tos_con_flema, tos).
normalizar_sintoma(dificultad_respirar, disnea).
normalizar_sintoma(disnea, disnea).
normalizar_sintoma(opresion_toracica, dolor_toracico_opresivo).
normalizar_sintoma(dolor_toracico_opresivo, dolor_toracico_opresivo).
normalizar_sintoma(dolor_pecho, dolor_toracico_opresivo).
normalizar_sintoma(dolor_toracico, dolor_toracico_opresivo).

% Casos digestivos
normalizar_sintoma(nauseas, nausea).
normalizar_sintoma(nausea, nausea).
normalizar_sintoma(vomito, vomito).
normalizar_sintoma(vomito_con_sangre, vomito_con_sangre).
normalizar_sintoma(diarrea, diarrea).
normalizar_sintoma(diarrea_profusa, diarrea).
normalizar_sintoma(dolor_abdomen, dolor_abdominal).
normalizar_sintoma(dolor_abdominal_intenso, dolor_abdominal).
normalizar_sintoma(colicos_abdominales, dolor_abdominal).
normalizar_sintoma(perdida_apetito, perdida_apetito).

% Casos neurológicos
normalizar_sintoma(cefalea, dolor_cabeza).
normalizar_sintoma(cefalea_intensa, dolor_cabeza).
normalizar_sintoma(dolor_cabeza, dolor_cabeza).
normalizar_sintoma(convulsiones, convulsiones).
normalizar_sintoma(perdida_conciencia, perdida_conciencia).
normalizar_sintoma(desorientacion, confusion).
normalizar_sintoma(confusion, confusion).
normalizar_sintoma(perdida_equilibrio, perdida_equilibrio).
normalizar_sintoma(hormigueo_extremidades, hormigueo).
normalizar_sintoma(hormigueo_manos_pies, hormigueo).
normalizar_sintoma(hormigueo_pies, hormigueo).
normalizar_sintoma(vision_borrosa, vision_borrosa).
normalizar_sintoma(fotofobia, fotofobia).
normalizar_sintoma(dificultad_hablar, dificultad_hablar).
normalizar_sintoma(cambios_conducta, alteracion_conducta).
normalizar_sintoma(perdida_memoria, perdida_memoria).
normalizar_sintoma(zumbido_oidos, zumbido_oidos).

% Casos traumatológicos
normalizar_sintoma(dolor_localizado, dolor_localizado).
normalizar_sintoma(dolor_muscular, dolor_muscular).
normalizar_sintoma(inflamacion, hinchazon).
normalizar_sintoma(hinchazon, hinchazon).
normalizar_sintoma(hematoma_visible, hematoma).
normalizar_sintoma(hematoma_local, hematoma).
normalizar_sintoma(deformidad, deformidad).
normalizar_sintoma(sangrado_abundante, sangrado).
normalizar_sintoma(sangrado_leve, sangrado).
normalizar_sintoma(herida_visible, herida).
normalizar_sintoma(herida_punzante, herida).
normalizar_sintoma(herida_infectada, herida).

% Casos cardiovasculares
normalizar_sintoma(dolor_toracico_opresivo, dolor_toracico_opresivo).
normalizar_sintoma(dolor_pecho_esfuerzo, dolor_toracico_opresivo).
normalizar_sintoma(dolor_toracico_reposo, dolor_toracico_opresivo).
normalizar_sintoma(disnea, disnea).
normalizar_sintoma(fatiga, fatiga).
normalizar_sintoma(cansancio, fatiga).
normalizar_sintoma(mareo, mareo).
normalizar_sintoma(taquicardia, palpitaciones).
normalizar_sintoma(palpitaciones_irregulares, palpitaciones).
normalizar_sintoma(palpitaciones, palpitaciones).
normalizar_sintoma(dolor_pecho_subito, dolor_toracico_opresivo).
normalizar_sintoma(cefalea, dolor_cabeza).

% Casos infecciosos / sistémicos
normalizar_sintoma(fiebre_persistente, fiebre).
normalizar_sintoma(fiebre_alta, fiebre).
normalizar_sintoma(escalofrios, escalofrios).
normalizar_sintoma(malestar_general, malestar_general).
normalizar_sintoma(erupcion_cutanea, sarpullido).
normalizar_sintoma(sarpullido, sarpullido).
normalizar_sintoma(ulcera_cutanea, ulcera).
normalizar_sintoma(ictericia, ictericia).
normalizar_sintoma(heces_negras, heces_negras).
normalizar_sintoma(sangrado, sangrado).

% Casos musculoesqueléticos
normalizar_sintoma(rigidez_muscular, rigidez_muscular).
normalizar_sintoma(rigidez_cuello, rigidez_cuello).
normalizar_sintoma(espasmos_musculares, espasmos_musculares).
normalizar_sintoma(dolor_articulaciones, dolor_articulaciones).

% Casos oculares
normalizar_sintoma(dolor_ojos, dolor_ojos).
normalizar_sintoma(conjuntivitis, inflamacion_ocular).

% Casos cutáneos
normalizar_sintoma(enrojecimiento_piel, enrojecimiento_piel).
normalizar_sintoma(prurito, picazon).
normalizar_sintoma(picazon, picazon).

% Casos generales
normalizar_sintoma(perdida_peso, perdida_peso).
normalizar_sintoma(perdida_fuerza, debilidad_muscular).
normalizar_sintoma(debilidad_muscular, debilidad_muscular).
normalizar_sintoma(fatiga_extrema, fatiga).
normalizar_sintoma(sudoracion_excesiva, sudoracion_excesiva).
normalizar_sintoma(dolor_general, dolor).

% Por defecto: mantiene el mismo término
normalizar_sintoma(S, S).
