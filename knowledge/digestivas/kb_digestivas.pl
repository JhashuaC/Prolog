% knowledge/digestivas/kb_digestivas.pl
:- module(kb_digestivas, [es_dolencia_digestiva/1, sintoma_de/2]).

% -------------------------------------------------------
% Dolencias digestivas (≈50)
% -------------------------------------------------------

% Altas / esófago – estómago
es_dolencia_digestiva(reflujo_gastroesofagico).
es_dolencia_digestiva(hernia_hiato).
es_dolencia_digestiva(esofagitis).
es_dolencia_digestiva(esofagitis_eosinofilica).
es_dolencia_digestiva(gastritis).
es_dolencia_digestiva(dispepsia_funcional).
es_dolencia_digestiva(ulcera_gastrica).
es_dolencia_digestiva(ulcera_duodenal).
es_dolencia_digestiva(infeccion_helicobacter_pylori).

% Intestino delgado y colon
es_dolencia_digestiva(gastroenteritis_aguda).
es_dolencia_digestiva(intoxicacion_alimentaria).
es_dolencia_digestiva(diarrea_aguda).
es_dolencia_digestiva(diarrea_cronica).
es_dolencia_digestiva(estrenimiento_cronico).
es_dolencia_digestiva(sindrome_intestino_irritable).
es_dolencia_digestiva(enfermedad_celiaca).
es_dolencia_digestiva(intolerancia_lactosa).
es_dolencia_digestiva(meteorismo).
es_dolencia_digestiva(malabsorcion).
es_dolencia_digestiva(diverticulosis).
es_dolencia_digestiva(diverticulitis).
es_dolencia_digestiva(colitis_ulcerosa).
es_dolencia_digestiva(enfermedad_crohn).
es_dolencia_digestiva(polidipsia_deshidratacion).
es_dolencia_digestiva(sangrado_digestivo_bajo).

% Apendice / peritoneo
es_dolencia_digestiva(apendicitis).
es_dolencia_digestiva(peritonitis).

% Hepatobiliar y páncreas
es_dolencia_digestiva(hepatitis_viral).
es_dolencia_digestiva(higado_graso).
es_dolencia_digestiva(cirrosis).
es_dolencia_digestiva(colelitiasis).           % piedras en vesícula
es_dolencia_digestiva(colecistitis).
es_dolencia_digestiva(colangitis).
es_dolencia_digestiva(ictericia_obstructiva).
es_dolencia_digestiva(enfermedad_biliar_funcional).
es_dolencia_digestiva(pancreatitis_aguda).
es_dolencia_digestiva(pancreatitis_cronica).

% Anorrectales
es_dolencia_digestiva(hemorroides).
es_dolencia_digestiva(fisura_anal).
es_dolencia_digestiva(fistula_anal).
es_dolencia_digestiva(sangrado_digestivo_alto).

% Infecciones/parasitarias
es_dolencia_digestiva(giardiasis).
es_dolencia_digestiva(amebiasis).
es_dolencia_digestiva(parasitosis_intestinal).

% Neoplasias frecuentes
es_dolencia_digestiva(polipos_colon).
es_dolencia_digestiva(cancer_colon).
es_dolencia_digestiva(cancer_estomago).
es_dolencia_digestiva(cancer_pancreas).
es_dolencia_digestiva(varices_esofagicas).

% -------------------------------------------------------
% Síntomas asociados
% -------------------------------------------------------

% Reflujo / esófago – estómago
sintoma_de(reflujo_gastroesofagico, ardor_pecho).
sintoma_de(reflujo_gastroesofagico, regurgitacion_acida).
sintoma_de(reflujo_gastroesofagico, tos_nocturna).
sintoma_de(hernia_hiato, pirosis_postprandial).
sintoma_de(esofagitis, dolor_toracico_ardor).
sintoma_de(esofagitis, disfagia).
sintoma_de(esofagitis_eosinofilica, disfagia_solidos).
sintoma_de(gastritis, dolor_epigastrio).
sintoma_de(gastritis, nausea).
sintoma_de(dispepsia_funcional, plenitud_postprandial).
sintoma_de(ulcera_gastrica, dolor_epigastrio_postcomida).
sintoma_de(ulcera_duodenal, dolor_epigastrio_ayunas_alivia_comer).
sintoma_de(infeccion_helicobacter_pylori, dolor_epigastrio_recurrente).

% Intestino delgado y colon
sintoma_de(gastroenteritis_aguda, diarrea_acuosa).
sintoma_de(gastroenteritis_aguda, vomitos).
sintoma_de(intoxicacion_alimentaria, dolor_abdominal_colico).
sintoma_de(diarrea_aguda, deposiciones_frecuentes).
sintoma_de(diarrea_cronica, perdida_peso).
sintoma_de(estrenimiento_cronico, dificultad_defecar).
sintoma_de(sindrome_intestino_irritable, dolor_abdominal_alivia_defecar).
sintoma_de(sindrome_intestino_irritable, alternancia_diarrea_estrenimiento).
sintoma_de(enfermedad_celiaca, esteatorrea).
sintoma_de(enfermedad_celiaca, perdida_peso).
sintoma_de(intolerancia_lactosa, distension_abdominal).
sintoma_de(intolerancia_lactosa, diarrea_post_lacteos).
sintoma_de(meteorismo, gases_excesivos).
sintoma_de(malabsorcion, deficiencias_nutricionales).
sintoma_de(diverticulosis, dolor_fosa_izquierda_leve).
sintoma_de(diverticulitis, fiebre).
sintoma_de(diverticulitis, dolor_fosa_izquierda_intenso).
sintoma_de(colitis_ulcerosa, diarrea_con_sangre).
sintoma_de(colitis_ulcerosa, urgencia_rectal).
sintoma_de(enfermedad_crohn, dolor_fosa_derecha).
sintoma_de(enfermedad_crohn, fistulas_perianales).
sintoma_de(polidipsia_deshidratacion, sed_intensa).
sintoma_de(polidipsia_deshidratacion, mareo).
sintoma_de(sangrado_digestivo_bajo, sangre_roja_en_heces).

% Apendice / peritoneo
sintoma_de(apendicitis, dolor_periumbilical_migra_fosa_derecha).
sintoma_de(apendicitis, fiebre_leve).
sintoma_de(peritonitis, dolor_abdominal_difuso).
sintoma_de(peritonitis, rigidez_abdominal).

% Hepatobiliar y páncreas
sintoma_de(hepatitis_viral, ictericia).
sintoma_de(hepatitis_viral, coluria).
sintoma_de(higado_graso, cansancio_cronico).
sintoma_de(cirrosis, ascitis).
sintoma_de(cirrosis, encefalopatia_hepatica).
sintoma_de(colelitiasis, colico_biliar_postgrasa).
sintoma_de(colecistitis, dolor_hipocondrio_derecho_fiebre).
sintoma_de(colangitis, fiebre_ictericia_dolor_hd).
sintoma_de(ictericia_obstructiva, heces_acolicas).
sintoma_de(enfermedad_biliar_funcional, dolor_biliar_sin_piedras).
sintoma_de(pancreatitis_aguda, dolor_epigastrio_irradia_espalda).
sintoma_de(pancreatitis_aguda, vomitos_persistentes).
sintoma_de(pancreatitis_cronica, esteatorrea).
sintoma_de(pancreatitis_cronica, dolor_epigastrico_recurrente).

% Anorrectales
sintoma_de(hemorroides, sangrado_rojo_brilante).
sintoma_de(hemorroides, prurito_anal).
sintoma_de(fisura_anal, dolor_intenso_defecacion).
sintoma_de(fistula_anal, supuracion_perianal).
sintoma_de(sangrado_digestivo_alto, melena).
sintoma_de(sangrado_digestivo_alto, hematemesis).

% Infecciones/parasitarias
sintoma_de(giardiasis, diarrea_espumosa).
sintoma_de(giardiasis, flatulencia_fetida).
sintoma_de(amebiasis, diarrea_con_moco_sangre).
sintoma_de(parasitosis_intestinal, prurito_perianal).
sintoma_de(parasitosis_intestinal, colico_abdominal).

% Neoplasias
sintoma_de(polipos_colon, sangrado_oculto).
sintoma_de(cancer_colon, cambio_habito_intestinal).
sintoma_de(cancer_colon, perdida_peso).
sintoma_de(cancer_estomago, saciedad_precoz).
sintoma_de(cancer_estomago, anemia_ferropenica).
sintoma_de(cancer_pancreas, ictericia_piel_prurito).
sintoma_de(varices_esofagicas, hematemesis_masiva).
