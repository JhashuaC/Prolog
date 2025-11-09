% knowledge/infecciosas/kb_infecciones.pl
% knowledge/infecciosas/kb_infecciones.pl
:- module(kb_infecciones, [es_dolencia_infecciosa/1, sintoma_de/2]).

% -------------------------------------------------------
% Dolencias infecciosas (50 en total)
% -------------------------------------------------------

% Respiratorias
es_dolencia_infecciosa(gripe).
es_dolencia_infecciosa(covid19).
es_dolencia_infecciosa(bronquitis_infecciosa).
es_dolencia_infecciosa(neumonia_bacteriana).
es_dolencia_infecciosa(tuberculosis).
es_dolencia_infecciosa(faringitis_estrepococica).
es_dolencia_infecciosa(sinusitis_bacteriana).
es_dolencia_infecciosa(otitis_media_infecciosa).

% Víricas comunes
es_dolencia_infecciosa(dengue).
es_dolencia_infecciosa(zika).
es_dolencia_infecciosa(chikungunya).
es_dolencia_infecciosa(hepatitis_a).
es_dolencia_infecciosa(hepatitis_b).
es_dolencia_infecciosa(hepatitis_c).
es_dolencia_infecciosa(mononucleosis).
es_dolencia_infecciosa(varicela).
es_dolencia_infecciosa(sarampion).
es_dolencia_infecciosa(rubeola).
es_dolencia_infecciosa(paperas).
es_dolencia_infecciosa(herpes_simple).

% Infecciones gastrointestinales
es_dolencia_infecciosa(gastroenteritis).
es_dolencia_infecciosa(salmonelosis).
es_dolencia_infecciosa(shigelosis).
es_dolencia_infecciosa(colera).
es_dolencia_infecciosa(amibiasis).
es_dolencia_infecciosa(giardiasis).
es_dolencia_infecciosa(helicobacter_pylori).
es_dolencia_infecciosa(toxoplasmosis).

% Infecciones cutáneas
es_dolencia_infecciosa(impetigo).
es_dolencia_infecciosa(celulitis_infecciosa).
es_dolencia_infecciosa(furunculosis).
es_dolencia_infecciosa(absceso_cutaneo).
es_dolencia_infecciosa(candidiasis_cutanea).
es_dolencia_infecciosa(pie_atleta).

% Infecciones de transmisión sexual
es_dolencia_infecciosa(vih).
es_dolencia_infecciosa(sifilis).
es_dolencia_infecciosa(gonorrea).
es_dolencia_infecciosa(clamidia).
es_dolencia_infecciosa(virus_papiloma_humano).
es_dolencia_infecciosa(herpes_genital).

% Sistémicas o tropicales
es_dolencia_infecciosa(malaria).
es_dolencia_infecciosa(leptospirosis).
es_dolencia_infecciosa(fiebre_tifoidea).
es_dolencia_infecciosa(ebola).
es_dolencia_infecciosa(fiebre_amarilla).
es_dolencia_infecciosa(tetanos).
es_dolencia_infecciosa(rabia).
es_dolencia_infecciosa(leishmaniasis).
es_dolencia_infecciosa(chagas).

% -------------------------------------------------------
% Síntomas asociados
% -------------------------------------------------------

% Gripe
sintoma_de(gripe, fiebre).
sintoma_de(gripe, tos).
sintoma_de(gripe, dolor_garganta).
sintoma_de(gripe, dolor_cabeza).
sintoma_de(gripe, malestar_general).
sintoma_de(gripe, congestion_nasal).
sintoma_de(gripe, escalofrios).

% COVID-19
sintoma_de(covid19, fiebre_alta).
sintoma_de(covid19, tos_seca).
sintoma_de(covid19, disnea).
sintoma_de(covid19, perdida_olfato).
sintoma_de(covid19, perdida_gusto).
sintoma_de(covid19, fatiga).
sintoma_de(covid19, dolor_muscular).

% Bronquitis
sintoma_de(bronquitis_infecciosa, tos_persistente).
sintoma_de(bronquitis_infecciosa, esputo_amarillo).
sintoma_de(bronquitis_infecciosa, dificultad_respirar).
sintoma_de(bronquitis_infecciosa, fiebre_leve).

% Neumonía
sintoma_de(neumonia_bacteriana, fiebre_alta).
sintoma_de(neumonia_bacteriana, tos_con_flema).
sintoma_de(neumonia_bacteriana, dolor_toracico).
sintoma_de(neumonia_bacteriana, dificultad_respirar).
sintoma_de(neumonia_bacteriana, sudoracion_excesiva).

% Tuberculosis
sintoma_de(tuberculosis, tos_cronica).
sintoma_de(tuberculosis, esputo_con_sangre).
sintoma_de(tuberculosis, perdida_peso).
sintoma_de(tuberculosis, fiebre_baja_tarde).
sintoma_de(tuberculosis, sudores_nocturnos).

% Faringitis
sintoma_de(faringitis_estrepococica, dolor_garganta).
sintoma_de(faringitis_estrepococica, fiebre).
sintoma_de(faringitis_estrepococica, dificultad_tragar).
sintoma_de(faringitis_estrepococica, ganglios_inflamados).

% Dengue
sintoma_de(dengue, fiebre_alta).
sintoma_de(dengue, dolor_articulaciones).
sintoma_de(dengue, dolor_muscular).
sintoma_de(dengue, erupcion_cutanea).
sintoma_de(dengue, dolor_cabeza_intenso).

% Zika
sintoma_de(zika, fiebre_leve).
sintoma_de(zika, sarpullido).
sintoma_de(zika, dolor_articulaciones).
sintoma_de(zika, conjuntivitis).

% Chikungunya
sintoma_de(chikungunya, fiebre_alta).
sintoma_de(chikungunya, dolor_articulaciones_severo).
sintoma_de(chikungunya, erupcion_cutanea).
sintoma_de(chikungunya, fatiga).

% Hepatitis
sintoma_de(hepatitis_a, ictericia).
sintoma_de(hepatitis_a, dolor_abdomen).
sintoma_de(hepatitis_a, fatiga).
sintoma_de(hepatitis_b, orina_oscura).
sintoma_de(hepatitis_b, nauseas).
sintoma_de(hepatitis_c, perdida_apetito).

% Mononucleosis
sintoma_de(mononucleosis, fiebre).
sintoma_de(mononucleosis, dolor_garganta).
sintoma_de(mononucleosis, ganglios_inflamados).
sintoma_de(mononucleosis, fatiga).

% Varicela
sintoma_de(varicela, erupcion_cutanea).
sintoma_de(varicela, picazon).
sintoma_de(varicela, fiebre).
sintoma_de(varicela, malestar_general).

% Sarampión
sintoma_de(sarampion, fiebre_alta).
sintoma_de(sarampion, manchas_koplik).
sintoma_de(sarampion, tos).
sintoma_de(sarampion, erupcion_cutanea).

% Rubeola
sintoma_de(rubeola, erupcion_cutanea).
sintoma_de(rubeola, fiebre_leve).
sintoma_de(rubeola, ganglios_inflamados).
sintoma_de(rubeola, dolor_articulaciones).

% Gastroenteritis
sintoma_de(gastroenteritis, diarrea).
sintoma_de(gastroenteritis, vomito).
sintoma_de(gastroenteritis, dolor_abdomen).
sintoma_de(gastroenteritis, fiebre_leve).

% Salmonelosis
sintoma_de(salmonelosis, fiebre).
sintoma_de(salmonelosis, diarrea).
sintoma_de(salmonelosis, colicos_abdominales).
sintoma_de(salmonelosis, nauseas).

% Cólera
sintoma_de(colera, diarrea_profusa).
sintoma_de(colera, deshidratacion).
sintoma_de(colera, calambres_musculares).
sintoma_de(colera, vomito).

% Impétigo
sintoma_de(impetigo, lesiones_piel).
sintoma_de(impetigo, costras_amarillentas).
sintoma_de(impetigo, picazon).

% Celulitis
sintoma_de(celulitis_infecciosa, enrojecimiento_piel).
sintoma_de(celulitis_infecciosa, calor_local).
sintoma_de(celulitis_infecciosa, hinchazon).
sintoma_de(celulitis_infecciosa, fiebre).

% VIH
sintoma_de(vih, fiebre_persistente).
sintoma_de(vih, perdida_peso).
sintoma_de(vih, ganglios_inflamados).
sintoma_de(vih, fatiga).
sintoma_de(vih, infecciones_recurrentes).

% Sífilis
sintoma_de(sifilis, ulcera_genital_indolora).
sintoma_de(sifilis, erupcion_palmas).
sintoma_de(sifilis, fiebre).
sintoma_de(sifilis, perdida_pelo).

% Gonorrea
sintoma_de(gonorrea, flujo_genital).
sintoma_de(gonorrea, ardor_orinar).
sintoma_de(gonorrea, dolor_pelvico).

% Malaria
sintoma_de(malaria, fiebre_alta).
sintoma_de(malaria, escalofrios).
sintoma_de(malaria, sudoracion_excesiva).
sintoma_de(malaria, dolor_muscular).
sintoma_de(malaria, nauseas).

% Leptospirosis
sintoma_de(leptospirosis, fiebre_alta).
sintoma_de(leptospirosis, dolor_cabeza).
sintoma_de(leptospirosis, ictericia).
sintoma_de(leptospirosis, dolor_muscular).

% Fiebre tifoidea
sintoma_de(fiebre_tifoidea, fiebre_prolongada).
sintoma_de(fiebre_tifoidea, dolor_abdomen).
sintoma_de(fiebre_tifoidea, diarrea).
sintoma_de(fiebre_tifoidea, confusion).

% Tétanos
sintoma_de(tetanos, rigidez_mandibula).
sintoma_de(tetanos, espasmos_musculares).
sintoma_de(tetanos, dificultad_tragar).

% Rabia
sintoma_de(rabia, fiebre).
sintoma_de(rabia, espasmos_garganta).
sintoma_de(rabia, agitacion).
sintoma_de(rabia, dificultad_tragar).
sintoma_de(rabia, hidrofobia).

% Ébola
sintoma_de(ebola, fiebre_alta).
sintoma_de(ebola, vomito).
sintoma_de(ebola, diarrea).
sintoma_de(ebola, sangrado).
sintoma_de(ebola, debilidad_extrema).

% Fiebre amarilla
sintoma_de(fiebre_amarilla, fiebre).
sintoma_de(fiebre_amarilla, ictericia).
sintoma_de(fiebre_amarilla, dolor_abdomen).
sintoma_de(fiebre_amarilla, sangrado_nariz).

% Leishmaniasis
sintoma_de(leishmaniasis, ulcera_cutanea).
sintoma_de(leishmaniasis, fiebre_prolongada).
sintoma_de(leishmaniasis, perdida_peso).

% Chagas
sintoma_de(chagas, fiebre).
sintoma_de(chagas, hinchazon_parpado).
sintoma_de(chagas, fatiga).
sintoma_de(chagas, palpitaciones).
