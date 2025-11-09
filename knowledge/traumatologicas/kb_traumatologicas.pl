% knowledge/traumatologicas/kb_traumatologicas.pl
:- module(kb_traumatologicas, [es_dolencia_traumatica/1, sintoma_de/2]).

% -------------------------------------------------------
% Dolencias traumatológicas (50 en total)
% -------------------------------------------------------

% Fracturas
es_dolencia_traumatica(fractura).
es_dolencia_traumatica(fractura_abierta).
es_dolencia_traumatica(fractura_cerrada).
es_dolencia_traumatica(fractura_colles).
es_dolencia_traumatica(fractura_femur).
es_dolencia_traumatica(fractura_clavicula).
es_dolencia_traumatica(fractura_tibia).
es_dolencia_traumatica(fractura_costilla).
es_dolencia_traumatica(fractura_muneca).
es_dolencia_traumatica(fractura_vertebral).

% Esguinces y luxaciones
es_dolencia_traumatica(esguince_tobillo).
es_dolencia_traumatica(esguince_muneca).
es_dolencia_traumatica(esguince_cervical).
es_dolencia_traumatica(luxacion_hombro).
es_dolencia_traumatica(luxacion_codo).
es_dolencia_traumatica(luxacion_rodilla).

% Contusiones y golpes
es_dolencia_traumatica(contusion).
es_dolencia_traumatica(contusion_muscular).
es_dolencia_traumatica(contusion_craneoencefalica).
es_dolencia_traumatica(contusion_toracica).
es_dolencia_traumatica(contusion_abdominal).

% Cortes, heridas y laceraciones
es_dolencia_traumatica(cortadura).
es_dolencia_traumatica(laceracion).
es_dolencia_traumatica(herida_profunda).
es_dolencia_traumatica(herida_punzante).
es_dolencia_traumatica(herida_infectada).

% Lesiones musculares y tendinosas
es_dolencia_traumatica(distension_muscular).
es_dolencia_traumatica(desgarro_muscular).
es_dolencia_traumatica(rotura_tendon_aquiles).
es_dolencia_traumatica(tendinitis_rotuliana).
es_dolencia_traumatica(tendinitis_bicipital).
es_dolencia_traumatica(tenosinovitis).

% Lesiones articulares
es_dolencia_traumatica(artrosis_postraumática).
es_dolencia_traumatica(bursitis).
es_dolencia_traumatica(subluxacion).
es_dolencia_traumatica(sindrome_patelofemoral).

% Lesiones óseas y vertebrales
es_dolencia_traumatica(compresion_vertebral).
es_dolencia_traumatica(fractura_pelvis).
es_dolencia_traumatica(hernia_discal_traumatica).
es_dolencia_traumatica(escoliosis_traumatica).
es_dolencia_traumatica(dolor_lumbar_post_impacto).

% Lesiones por esfuerzo o sobreuso
es_dolencia_traumatica(fascitis_plantar).
es_dolencia_traumatica(sindrome_tunel_carpiano_traumatico).
es_dolencia_traumatica(sindrome_banda_iliotibial).
es_dolencia_traumatica(periostitis_tibial).
es_dolencia_traumatica(epicondilitis_lateral).
es_dolencia_traumatica(epitrocleitis).

% Otras lesiones
es_dolencia_traumatica(amputacion_parcial).
es_dolencia_traumatica(quemadura).
es_dolencia_traumatica(congelacion).
es_dolencia_traumatica(hematoma_profundo).

% -------------------------------------------------------
% Síntomas asociados
% -------------------------------------------------------

% Fracturas
sintoma_de(fractura, dolor_localizado).
sintoma_de(fractura, inflamacion).
sintoma_de(fractura, deformidad).
sintoma_de(fractura, imposibilidad_movimiento).

sintoma_de(fractura_abierta, herida_visible).
sintoma_de(fractura_abierta, sangrado_abundante).
sintoma_de(fractura_abierta, dolor_intenso).

sintoma_de(fractura_cerrada, hematoma_visible).
sintoma_de(fractura_cerrada, inflamacion).
sintoma_de(fractura_cerrada, sensibilidad_extrema).

sintoma_de(fractura_colles, dolor_muneca).
sintoma_de(fractura_colles, deformidad_dorsal_mano).
sintoma_de(fractura_colles, limitacion_movimiento_muneca).

sintoma_de(fractura_femur, dolor_pierna).
sintoma_de(fractura_femur, pierna_acortada).
sintoma_de(fractura_femur, incapacidad_caminar).

sintoma_de(fractura_clavicula, dolor_hombro).
sintoma_de(fractura_clavicula, bulto_visible).
sintoma_de(fractura_clavicula, dificultad_movimiento_brazo).

sintoma_de(fractura_costilla, dolor_toracico).
sintoma_de(fractura_costilla, dolor_respirar).
sintoma_de(fractura_costilla, hematoma_torax).

sintoma_de(fractura_vertebral, dolor_espalda_intenso).
sintoma_de(fractura_vertebral, perdida_movilidad_tronco).
sintoma_de(fractura_vertebral, entumecimiento_extremidades).

% Esguinces
sintoma_de(esguince_tobillo, inflamacion_tobillo).
sintoma_de(esguince_tobillo, dolor_pisar).
sintoma_de(esguince_tobillo, hematoma_tobillo).

sintoma_de(esguince_muneca, dolor_muneca).
sintoma_de(esguince_muneca, debilidad_mano).

sintoma_de(esguince_cervical, rigidez_cuello).
sintoma_de(esguince_cervical, dolor_movimiento_cabeza).
sintoma_de(esguince_cervical, mareo_post_golpe).

% Luxaciones
sintoma_de(luxacion_hombro, deformidad_hombro).
sintoma_de(luxacion_hombro, dolor_agudo_hombro).
sintoma_de(luxacion_hombro, imposibilidad_movimiento_brazo).

sintoma_de(luxacion_codo, deformidad_codo).
sintoma_de(luxacion_codo, inflamacion).
sintoma_de(luxacion_codo, entumecimiento_dedos).

sintoma_de(luxacion_rodilla, dolor_rodilla).
sintoma_de(luxacion_rodilla, hinchazon_inmediata).
sintoma_de(luxacion_rodilla, incapacidad_caminar).

% Contusiones
sintoma_de(contusion, hematoma_visible).
sintoma_de(contusion, dolor_moderado).
sintoma_de(contusion, inflamacion_leve).

sintoma_de(contusion_craneoencefalica, dolor_cabeza).
sintoma_de(contusion_craneoencefalica, perdida_conciencia).
sintoma_de(contusion_craneoencefalica, nauseas).

sintoma_de(contusion_toracica, dolor_pecho).
sintoma_de(contusion_toracica, dificultad_respirar).

sintoma_de(contusion_abdominal, dolor_abdomen).
sintoma_de(contusion_abdominal, hematoma_abdomen).

% Cortes y heridas
sintoma_de(cortadura, sangrado).
sintoma_de(cortadura, dolor_localizado).
sintoma_de(cortadura, apertura_piel).

sintoma_de(herida_punzante, dolor_profundo).
sintoma_de(herida_punzante, sangrado_leve).
sintoma_de(herida_punzante, posible_infeccion).

sintoma_de(herida_infectada, enrojecimiento).
sintoma_de(herida_infectada, secrecion_pus).
sintoma_de(herida_infectada, fiebre).

% Lesiones musculares
sintoma_de(distension_muscular, dolor_muscular).
sintoma_de(distension_muscular, rigidez_muscular).

sintoma_de(desgarro_muscular, dolor_intenso).
sintoma_de(desgarro_muscular, hematoma_local).
sintoma_de(desgarro_muscular, perdida_fuerza).

sintoma_de(rotura_tendon_aquiles, imposibilidad_apoyar_pie).
sintoma_de(rotura_tendon_aquiles, chasquido_pie).
sintoma_de(rotura_tendon_aquiles, hinchazon_talon).

sintoma_de(tendinitis_rotuliana, dolor_rodilla).
sintoma_de(tendinitis_rotuliana, sensibilidad_rotula).
sintoma_de(tendinitis_rotuliana, dolor_subir_escaleras).

% Bursitis
sintoma_de(bursitis, inflamacion_articulacion).
sintoma_de(bursitis, dolor_presion).
sintoma_de(bursitis, rigidez_matutina).

% Compresión vertebral
sintoma_de(compresion_vertebral, dolor_espalda).
sintoma_de(compresion_vertebral, hormigueo_piernas).
sintoma_de(compresion_vertebral, dificultad_moverse).

% Hernia discal traumática
sintoma_de(hernia_discal_traumatica, dolor_lumbar).
sintoma_de(hernia_discal_traumatica, ciatica).
sintoma_de(hernia_discal_traumatica, hormigueo_piernas).

% Fascitis plantar
sintoma_de(fascitis_plantar, dolor_talon).
sintoma_de(fascitis_plantar, dolor_matutino).
sintoma_de(fascitis_plantar, dificultad_caminar).

% Quemaduras
sintoma_de(quemadura, enrojecimiento_piel).
sintoma_de(quemadura, ampollas).
sintoma_de(quemadura, dolor_intenso).
sintoma_de(quemadura, tejido_carbonizado).

% Congelación
sintoma_de(congelacion, piel_palida).
sintoma_de(congelacion, hormigueo).
sintoma_de(congelacion, entumecimiento).
sintoma_de(congelacion, necrosis_tejido).

% Amputación parcial
sintoma_de(amputacion_parcial, sangrado_abundante).
sintoma_de(amputacion_parcial, dolor_extremo).
sintoma_de(amputacion_parcial, shock).

% Hematoma profundo
sintoma_de(hematoma_profundo, dolor_constante).
sintoma_de(hematoma_profundo, inflamacion).
sintoma_de(hematoma_profundo, cambio_color_piel).

% Epitrocleitis / Epicondilitis
sintoma_de(epicondilitis_lateral, dolor_codo).
sintoma_de(epicondilitis_lateral, debilidad_mano).
sintoma_de(epicondilitis_lateral, dolor_agarrar_objetos).

sintoma_de(epitrocleitis, dolor_codo_interno).
sintoma_de(epitrocleitis, sensibilidad_brazo_interno).
