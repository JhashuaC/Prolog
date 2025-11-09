% knowledge/cardiovasculares/kb_cardiovasculares.pl
:- module(kb_cardiovasculares, [es_dolencia_cardiovascular/1, sintoma_de/2]).

% -------------------------------------------------------
% Dolencias cardiovasculares (50 en total)
% -------------------------------------------------------

% Enfermedades coronarias
es_dolencia_cardiovascular(infarto_agudo_miocardio).
es_dolencia_cardiovascular(angina_estable).
es_dolencia_cardiovascular(angina_inestable).
es_dolencia_cardiovascular(enfermedad_coronaria).
es_dolencia_cardiovascular(espasmo_coronario).

% Hipertensión y trastornos relacionados
es_dolencia_cardiovascular(hipertension).
es_dolencia_cardiovascular(hipotension).
es_dolencia_cardiovascular(crisis_hipertensiva).
es_dolencia_cardiovascular(hipertension_pulmonar).

% Arritmias
es_dolencia_cardiovascular(fibrilacion_auricular).
es_dolencia_cardiovascular(fibrilacion_ventricular).
es_dolencia_cardiovascular(taquicardia_supraventricular).
es_dolencia_cardiovascular(bradicardia).
es_dolencia_cardiovascular(bloqueo_cardiaco).

% Insuficiencia y fallos cardíacos
es_dolencia_cardiovascular(insuficiencia_cardiaca_congestiva).
es_dolencia_cardiovascular(insuficiencia_ventricular_derecha).
es_dolencia_cardiovascular(insuficiencia_ventricular_izquierda).
es_dolencia_cardiovascular(edema_pulmonar_cardiogenico).
es_dolencia_cardiovascular(cardiomiopatia_dilatada).
es_dolencia_cardiovascular(cardiomiopatia_hipertrofica).
es_dolencia_cardiovascular(cardiomiopatia_restrictiva).

% Enfermedades valvulares
es_dolencia_cardiovascular(est_enosis_mitral).
es_dolencia_cardiovascular(insuficiencia_mitral).
es_dolencia_cardiovascular(est_enosis_aortica).
es_dolencia_cardiovascular(insuficiencia_aortica).
es_dolencia_cardiovascular(prolapso_valvula_mitral).

% Enfermedades de vasos sanguíneos
es_dolencia_cardiovascular(arterioesclerosis).
es_dolencia_cardiovascular(aterosclerosis).
es_dolencia_cardiovascular(trombosis_venosa_profunda).
es_dolencia_cardiovascular(embolia_pulmonar).
es_dolencia_cardiovascular(aneurisma_aorta).
es_dolencia_cardiovascular(insuficiencia_venosa_cronica).
es_dolencia_cardiovascular(varices).

% Inflamatorias e infecciosas
es_dolencia_cardiovascular(endocarditis_infecciosa).
es_dolencia_cardiovascular(miocarditis).
es_dolencia_cardiovascular(pericarditis).
es_dolencia_cardiovascular(tamponamiento_cardiaco).
es_dolencia_cardiovascular(fiebre_reumatica).

% Otras afecciones cardiovasculares
es_dolencia_cardiovascular(sindrome_marcapasos_enfermo).
es_dolencia_cardiovascular(choque_cardiogenico).
es_dolencia_cardiovascular(sindrome_bajo_gasto_cardiaco).
es_dolencia_cardiovascular(sindrome_corazon_roto).
es_dolencia_cardiovascular(enfermedad_pericardica).
es_dolencia_cardiovascular(enfermedad_aortica).
es_dolencia_cardiovascular(sindrome_postinfarto).
es_dolencia_cardiovascular(isquemia_miocardica_silente).

% -------------------------------------------------------
% Síntomas asociados
% -------------------------------------------------------

% Infarto agudo de miocardio
sintoma_de(infarto_agudo_miocardio, dolor_toracico_opresivo).
sintoma_de(infarto_agudo_miocardio, disnea).
sintoma_de(infarto_agudo_miocardio, sudoracion_excesiva).
sintoma_de(infarto_agudo_miocardio, nauseas).
sintoma_de(infarto_agudo_miocardio, palidez).
sintoma_de(infarto_agudo_miocardio, ansiedad_muerte_inminente).

% Angina
sintoma_de(angina_estable, dolor_pecho_esfuerzo).
sintoma_de(angina_estable, alivio_reposo).
sintoma_de(angina_inestable, dolor_toracico_reposo).
sintoma_de(angina_inestable, irradiacion_brazo_izquierdo).

% Hipertensión
sintoma_de(hipertension, cefalea).
sintoma_de(hipertension, zumbido_oidos).
sintoma_de(hipertension, vision_borrosa).
sintoma_de(hipertension, mareo).
sintoma_de(hipertension, fatiga).

% Hipotensión
sintoma_de(hipotension, mareo).
sintoma_de(hipotension, desmayo).
sintoma_de(hipotension, debilidad_general).
sintoma_de(hipotension, palidez_cutanea).

% Fibrilación auricular
sintoma_de(fibrilacion_auricular, palpitaciones_irregulares).
sintoma_de(fibrilacion_auricular, disnea_esfuerzo).
sintoma_de(fibrilacion_auricular, fatiga).
sintoma_de(fibrilacion_auricular, mareo).

% Fibrilación ventricular
sintoma_de(fibrilacion_ventricular, perdida_conciencia).
sintoma_de(fibrilacion_ventricular, paro_cardiaco).
sintoma_de(fibrilacion_ventricular, ausencia_pulso).

% Insuficiencia cardiaca congestiva
sintoma_de(insuficiencia_cardiaca_congestiva, disnea).
sintoma_de(insuficiencia_cardiaca_congestiva, edema_piernas).
sintoma_de(insuficiencia_cardiaca_congestiva, fatiga).
sintoma_de(insuficiencia_cardiaca_congestiva, tos_nocturna).
sintoma_de(insuficiencia_cardiaca_congestiva, aumento_peso_rapido).

% Insuficiencia ventricular izquierda
sintoma_de(insuficiencia_ventricular_izquierda, disnea_esfuerzo).
sintoma_de(insuficiencia_ventricular_izquierda, ortopnea).
sintoma_de(insuficiencia_ventricular_izquierda, estertores_pulmonares).

% Insuficiencia ventricular derecha
sintoma_de(insuficiencia_ventricular_derecha, edema_piernas).
sintoma_de(insuficiencia_ventricular_derecha, distension_yugular).
sintoma_de(insuficiencia_ventricular_derecha, ascitis).

% Edema pulmonar
sintoma_de(edema_pulmonar_cardiogenico, disnea_grave).
sintoma_de(edema_pulmonar_cardiogenico, tos_espumosa).
sintoma_de(edema_pulmonar_cardiogenico, ansiedad).

% Miocarditis
sintoma_de(miocarditis, dolor_pecho).
sintoma_de(miocarditis, fiebre).
sintoma_de(miocarditis, fatiga).
sintoma_de(miocarditis, palpitaciones).

% Endocarditis
sintoma_de(endocarditis_infecciosa, fiebre_prolongada).
sintoma_de(endocarditis_infecciosa, soplo_cardiaco).
sintoma_de(endocarditis_infecciosa, manchas_piel).
sintoma_de(endocarditis_infecciosa, fatiga).

% Pericarditis
sintoma_de(pericarditis, dolor_toracico_punzante).
sintoma_de(pericarditis, fiebre_leve).
sintoma_de(pericarditis, alivio_sentado).
sintoma_de(pericarditis, roce_pericardico).

% Tamponamiento cardíaco
sintoma_de(tamponamiento_cardiaco, hipotension).
sintoma_de(tamponamiento_cardiaco, ingurgitacion_yugular).
sintoma_de(tamponamiento_cardiaco, ruidos_cardiacos_apagados).

% Aneurisma aórtico
sintoma_de(aneurisma_aorta, dolor_abdomen).
sintoma_de(aneurisma_aorta, masa_pulsatil_abdomen).
sintoma_de(aneurisma_aorta, hipotension_repaso).

% Trombosis y embolia
sintoma_de(trombosis_venosa_profunda, dolor_pierna).
sintoma_de(trombosis_venosa_profunda, inflamacion_pierna).
sintoma_de(trombosis_venosa_profunda, calor_local).

sintoma_de(embolia_pulmonar, disnea_súbita).
sintoma_de(embolia_pulmonar, dolor_toracico).
sintoma_de(embolia_pulmonar, taquicardia).
sintoma_de(embolia_pulmonar, tos_con_sangre).

% Aterosclerosis
sintoma_de(aterosclerosis, dolor_pecho).
sintoma_de(aterosclerosis, fatiga).
sintoma_de(aterosclerosis, mareo).

% Varices
sintoma_de(varices, pesadez_piernas).
sintoma_de(varices, dolor_piernas).
sintoma_de(varices, picazon_piel).

% Fiebre reumática
sintoma_de(fiebre_reumatica, fiebre).
sintoma_de(fiebre_reumatica, dolor_articulaciones).
sintoma_de(fiebre_reumatica, soplo_cardiaco).
sintoma_de(fiebre_reumatica, fatiga).

% Choque cardiogénico
sintoma_de(choque_cardiogenico, hipotension_grave).
sintoma_de(choque_cardiogenico, pulso_debil).
sintoma_de(choque_cardiogenico, piel_fria).
sintoma_de(choque_cardiogenico, confusión).

% Síndrome del corazón roto
sintoma_de(sindrome_corazon_roto, dolor_pecho_subito).
sintoma_de(sindrome_corazon_roto, disnea).
sintoma_de(sindrome_corazon_roto, debilidad).
sintoma_de(sindrome_corazon_roto, palpitaciones).

% Insuficiencia venosa crónica
sintoma_de(insuficiencia_venosa_cronica, hinchazon_tobillos).
sintoma_de(insuficiencia_venosa_cronica, pigmentacion_piel).
sintoma_de(insuficiencia_venosa_cronica, ulcera_pierna).

% Síndrome postinfarto
sintoma_de(sindrome_postinfarto, fiebre_leve).
sintoma_de(sindrome_postinfarto, dolor_toracico).
sintoma_de(sindrome_postinfarto, fatiga).

% Cardiomiopatías
sintoma_de(cardiomiopatia_dilatada, disnea).
sintoma_de(cardiomiopatia_dilatada, palpitaciones).
sintoma_de(cardiomiopatia_dilatada, fatiga).

sintoma_de(cardiomiopatia_hipertrofica, dolor_pecho).
sintoma_de(cardiomiopatia_hipertrofica, desmayo).
sintoma_de(cardiomiopatia_hipertrofica, mareo).

sintoma_de(cardiomiopatia_restrictiva, disnea).
sintoma_de(cardiomiopatia_restrictiva, edema_piernas).
sintoma_de(cardiomiopatia_restrictiva, fatiga).
