% knowledge/respiratorias/kb_respiratorias.pl
:- module(kb_respiratorias, [es_dolencia_respiratoria/1, sintoma_de/2]).

% -------------------------------------------------------
% Dolencias respiratorias (≈50 en total)
% -------------------------------------------------------

% Infecciones respiratorias altas
es_dolencia_respiratoria(resfriado_comun).
es_dolencia_respiratoria(gripe).
es_dolencia_respiratoria(faringitis).
es_dolencia_respiratoria(laringitis).
es_dolencia_respiratoria(amigdalitis).
es_dolencia_respiratoria(sinusitis).
es_dolencia_respiratoria(rinitis).

% Infecciones respiratorias bajas
es_dolencia_respiratoria(bronquitis).
es_dolencia_respiratoria(bronquiolitis).
es_dolencia_respiratoria(neumonia).
es_dolencia_respiratoria(tuberculosis).
es_dolencia_respiratoria(absceso_pulmonar).

% Afecciones obstructivas crónicas
es_dolencia_respiratoria(asma).
es_dolencia_respiratoria(epoc).
es_dolencia_respiratoria(bronquiectasias).

% Alergias y reacciones respiratorias
es_dolencia_respiratoria(rinitis_alergica).
es_dolencia_respiratoria(asma_alergica).
es_dolencia_respiratoria(alergia_polvo).
es_dolencia_respiratoria(alergia_polen).

% Trastornos pleurales
es_dolencia_respiratoria(pleuritis).
es_dolencia_respiratoria(neumotorax).
es_dolencia_respiratoria(derrame_pleural).

% Enfermedades respiratorias graves
es_dolencia_respiratoria(fibrosis_pulmonar).
es_dolencia_respiratoria(cancer_pulmon).
es_dolencia_respiratoria(covid19).
es_dolencia_respiratoria(sindrome_respiratorio_agudo).
es_dolencia_respiratoria(sindrome_respiratorio_cronico).

% Trastornos funcionales y ocupacionales
es_dolencia_respiratoria(bronquitis_cronica).
es_dolencia_respiratoria(tos_cronica).
es_dolencia_respiratoria(enfermedad_pulmonar_ocupacional).
es_dolencia_respiratoria(silicosis).
es_dolencia_respiratoria(asbestosis).

% Trastornos del sueño y respiración
es_dolencia_respiratoria(apnea_sueño).
es_dolencia_respiratoria(hipoventilacion).
es_dolencia_respiratoria(insuficiencia_respiratoria).

% Otros
es_dolencia_respiratoria(micosis_pulmonar).
es_dolencia_respiratoria(sarcoidosis).
es_dolencia_respiratoria(alveolitis).
es_dolencia_respiratoria(laringotraqueitis).
es_dolencia_respiratoria(broncoespasmo).
es_dolencia_respiratoria(traqueitis).
es_dolencia_respiratoria(respiracion_sibilante).
es_dolencia_respiratoria(edema_pulmonar).
es_dolencia_respiratoria(hemoptisis).

% -------------------------------------------------------
% Síntomas asociados
% -------------------------------------------------------

% Resfriado y gripe
sintoma_de(resfriado_comun, estornudos).
sintoma_de(resfriado_comun, congestión_nasal).
sintoma_de(resfriado_comun, dolor_garganta).
sintoma_de(resfriado_comun, tos_leve).

sintoma_de(gripe, fiebre_alta).
sintoma_de(gripe, escalofrios).
sintoma_de(gripe, dolor_muscular).
sintoma_de(gripe, tos_seca).
sintoma_de(gripe, fatiga).

% Bronquitis
sintoma_de(bronquitis, tos_persistente).
sintoma_de(bronquitis, flema_amarillenta).
sintoma_de(bronquitis, dificultad_respirar).
sintoma_de(bronquitis, silbidos_pecho).

% Asma
sintoma_de(asma, dificultad_respirar).
sintoma_de(asma, silbido_respiratorio).
sintoma_de(asma, opresion_pecho).
sintoma_de(asma, tos_nocturna).

% Neumonía
sintoma_de(neumonia, fiebre_alta).
sintoma_de(neumonia, escalofrios).
sintoma_de(neumonia, dolor_toracico).
sintoma_de(neumonia, tos_productiva).
sintoma_de(neumonia, dificultad_respirar).

% Tuberculosis
sintoma_de(tuberculosis, tos_cronica).
sintoma_de(tuberculosis, sudor_nocturno).
sintoma_de(tuberculosis, perdida_peso).
sintoma_de(tuberculosis, fiebre_baja).
sintoma_de(tuberculosis, hemoptisis).

% EPOC
sintoma_de(epoc, dificultad_respirar_esfuerzo).
sintoma_de(epoc, tos_cronica).
sintoma_de(epoc, fatiga).
sintoma_de(epoc, sibilancias).

% COVID-19
sintoma_de(covid19, fiebre).
sintoma_de(covid19, tos_seca).
sintoma_de(covid19, perdida_olfato).
sintoma_de(covid19, dificultad_respirar).
sintoma_de(covid19, dolor_muscular).
sintoma_de(covid19, fatiga).

% Sinusitis
sintoma_de(sinusitis, dolor_frontal).
sintoma_de(sinusitis, congestion_nasal).
sintoma_de(sinusitis, secrecion_nasal_espesa).
sintoma_de(sinusitis, presion_cara).
sintoma_de(sinusitis, perdida_olfato).

% Faringitis / Laringitis
sintoma_de(faringitis, dolor_garganta).
sintoma_de(faringitis, fiebre_leve).
sintoma_de(faringitis, dificultad_tragar).

sintoma_de(laringitis, ronquera).
sintoma_de(laringitis, tos_seca).
sintoma_de(laringitis, perdida_voz).

% Pleuritis
sintoma_de(pleuritis, dolor_toracico).
sintoma_de(pleuritis, tos_seca).
sintoma_de(pleuritis, fiebre_moderada).

% Neumotórax
sintoma_de(neumotorax, dolor_pecho_agudo).
sintoma_de(neumotorax, dificultad_respirar_súbita).
sintoma_de(neumotorax, palidez).
sintoma_de(neumotorax, ansiedad).

% Fibrosis pulmonar
sintoma_de(fibrosis_pulmonar, disnea_progresiva).
sintoma_de(fibrosis_pulmonar, tos_seca_cronica).
sintoma_de(fibrosis_pulmonar, fatiga_cronica).
sintoma_de(fibrosis_pulmonar, dedos_en_palomilla).

% Rinitis alérgica
sintoma_de(rinitis_alergica, estornudos_repetidos).
sintoma_de(rinitis_alergica, picazon_nariz).
sintoma_de(rinitis_alergica, lagrimeo).
sintoma_de(rinitis_alergica, congestión_nasal).

% Cáncer de pulmón
sintoma_de(cancer_pulmon, tos_persistente).
sintoma_de(cancer_pulmon, dolor_toracico).
sintoma_de(cancer_pulmon, hemoptisis).
sintoma_de(cancer_pulmon, perdida_peso).
sintoma_de(cancer_pulmon, fatiga).

% Apnea del sueño
sintoma_de(apnea_sueño, ronquidos_fuertes).
sintoma_de(apnea_sueño, pausas_respiracion).
sintoma_de(apnea_sueño, somnolencia_diurna).
sintoma_de(apnea_sueño, dolor_cabeza_matutino).

% Edema pulmonar
sintoma_de(edema_pulmonar, dificultad_respirar_reposo).
sintoma_de(edema_pulmonar, tos_espumosa).
sintoma_de(edema_pulmonar, ansiedad).
sintoma_de(edema_pulmonar, labios_azulados).
