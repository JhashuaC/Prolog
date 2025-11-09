% knowledge/neurologicas/kb_neurologicas.pl
:- module(kb_neurologicas, [es_dolencia_neurologica/1, sintoma_de/2]).

% -------------------------------------------------------
% Dolencias neurológicas (50 en total)
% -------------------------------------------------------

% Degenerativas
es_dolencia_neurologica(alzheimer).
es_dolencia_neurologica(parkinson).
es_dolencia_neurologica(esclerosis_multiple).
es_dolencia_neurologica(esclerosis_lateral_amiotrofica).
es_dolencia_neurologica(demencia_frontotemporal).
es_dolencia_neurologica(atrofia_multisistemica).
es_dolencia_neurologica(coreas).
es_dolencia_neurologica(distonia).
es_dolencia_neurologica(sindrome_tourette).
es_dolencia_neurologica(enfermedad_huntington).

% Vasculares y circulatorias
es_dolencia_neurologica(ictus).
es_dolencia_neurologica(aneurisma_cerebral).
es_dolencia_neurologica(hemorragia_cerebral).
es_dolencia_neurologica(isquemia_transitoria).
es_dolencia_neurologica(malformacion_arteriovenosa).

% Infecciosas e inflamatorias
es_dolencia_neurologica(meningitis).
es_dolencia_neurologica(encefalitis).
es_dolencia_neurologica(absceso_cerebral).
es_dolencia_neurologica(neurocisticercosis).
es_dolencia_neurologica(sindrome_guillain_barre).
es_dolencia_neurologica(neuritis_optica).

% Traumáticas y estructurales
es_dolencia_neurologica(traumatismo_craneoencefalico).
es_dolencia_neurologica(conmocion_cerebral).
es_dolencia_neurologica(hematoma_subdural).
es_dolencia_neurologica(hidrocefalia).
es_dolencia_neurologica(compresion_medular).

% Tumorales
es_dolencia_neurologica(tumor_cerebral).
es_dolencia_neurologica(neurofibromatosis).
es_dolencia_neurologica(meningioma).
es_dolencia_neurologica(glioblastoma).

% Desórdenes del dolor y neuromusculares
es_dolencia_neurologica(migraña).
es_dolencia_neurologica(neuralgia_trigeminal).
es_dolencia_neurologica(neuralgia_occipital).
es_dolencia_neurologica(neuropatia_periferica).
es_dolencia_neurologica(miastenia_gravis).
es_dolencia_neurologica(sindrome_canal_carpiano).
es_dolencia_neurologica(mialgia_neurologica).

% Epilépticas y de convulsión
es_dolencia_neurologica(epilepsia).
es_dolencia_neurologica(sindrome_lennox_gastaut).
es_dolencia_neurologica(convulsion_febril).

% Autoinmunes
es_dolencia_neurologica(sindrome_miller_fisher).
es_dolencia_neurologica(neuromielitis_optica).
es_dolencia_neurologica(encefalomielitis_aguda_difusa).
es_dolencia_neurologica(miopatias_inflamatorias).

% Otras neurológicas
es_dolencia_neurologica(sindrome_rett).
es_dolencia_neurologica(sindrome_west).
es_dolencia_neurologica(sindrome_fatiga_cronica).
es_dolencia_neurologica(tetraplejia).
es_dolencia_neurologica(paraplejia).
es_dolencia_neurologica(sindrome_tunel_tarsiano).
es_dolencia_neurologica(ataxia_cerebelosa).

% -------------------------------------------------------
% Síntomas asociados
% -------------------------------------------------------

% Alzheimer
sintoma_de(alzheimer, perdida_memoria).
sintoma_de(alzheimer, desorientacion).
sintoma_de(alzheimer, dificultad_hablar).
sintoma_de(alzheimer, cambios_personalidad).
sintoma_de(alzheimer, confusion_tiempo_espacio).

% Parkinson
sintoma_de(parkinson, temblor_manos).
sintoma_de(parkinson, rigidez_muscular).
sintoma_de(parkinson, lentitud_movimientos).
sintoma_de(parkinson, dificultad_hablar).
sintoma_de(parkinson, inestabilidad_postural).

% Esclerosis múltiple
sintoma_de(esclerosis_multiple, vision_borrosa).
sintoma_de(esclerosis_multiple, hormigueo_extremidades).
sintoma_de(esclerosis_multiple, debilidad_muscular).
sintoma_de(esclerosis_multiple, perdida_equilibrio).
sintoma_de(esclerosis_multiple, fatiga).

% ELA
sintoma_de(esclerosis_lateral_amiotrofica, debilidad_extremidades).
sintoma_de(esclerosis_lateral_amiotrofica, dificultad_hablar).
sintoma_de(esclerosis_lateral_amiotrofica, problemas_respirar).
sintoma_de(esclerosis_lateral_amiotrofica, perdida_fuerza_manos).

% Demencia frontotemporal
sintoma_de(demencia_frontotemporal, cambios_conducta).
sintoma_de(demencia_frontotemporal, apatia).
sintoma_de(demencia_frontotemporal, dificultad_hablar).

% Migraña
sintoma_de(migraña, cefalea_intensa).
sintoma_de(migraña, fotofobia).
sintoma_de(migraña, nauseas).
sintoma_de(migraña, vomito).
sintoma_de(migraña, sensibilidad_sonido).

% Epilepsia
sintoma_de(epilepsia, convulsiones).
sintoma_de(epilepsia, perdida_conciencia).
sintoma_de(epilepsia, movimientos_involuntarios).
sintoma_de(epilepsia, mirada_fija).
sintoma_de(epilepsia, confusion_postictal).

% Ictus
sintoma_de(ictus, paralisis_facial).
sintoma_de(ictus, dificultad_hablar).
sintoma_de(ictus, perdida_fuerza_un_lado).
sintoma_de(ictus, confusion).
sintoma_de(ictus, perdida_equilibrio).

% Aneurisma cerebral
sintoma_de(aneurisma_cerebral, dolor_cabeza_subito).
sintoma_de(aneurisma_cerebral, vision_doble).
sintoma_de(aneurisma_cerebral, rigidez_cuello).
sintoma_de(aneurisma_cerebral, perdida_conciencia).

% Meningitis
sintoma_de(meningitis, fiebre_alta).
sintoma_de(meningitis, rigidez_cuello).
sintoma_de(meningitis, dolor_cabeza_intenso).
sintoma_de(meningitis, fotofobia).
sintoma_de(meningitis, nauseas).

% Encefalitis
sintoma_de(encefalitis, fiebre_alta).
sintoma_de(encefalitis, dolor_cabeza).
sintoma_de(encefalitis, somnolencia).
sintoma_de(encefalitis, convulsiones).
sintoma_de(encefalitis, confusion).

% Neuralgia trigeminal
sintoma_de(neuralgia_trigeminal, dolor_cara_intenso).
sintoma_de(neuralgia_trigeminal, espasmos_faciales).
sintoma_de(neuralgia_trigeminal, dolor_mandibula).

% Neuralgia occipital
sintoma_de(neuralgia_occipital, dolor_cabeza_punzante).
sintoma_de(neuralgia_occipital, dolor_nuca).
sintoma_de(neuralgia_occipital, sensibilidad_cuero_cabelludo).

% Neuropatía periférica
sintoma_de(neuropatia_periferica, hormigueo_pies).
sintoma_de(neuropatia_periferica, entumecimiento_extremidades).
sintoma_de(neuropatia_periferica, debilidad_muscular).
sintoma_de(neuropatia_periferica, perdida_sensibilidad).

% Síndrome Guillain-Barré
sintoma_de(sindrome_guillain_barre, debilidad_piernas).
sintoma_de(sindrome_guillain_barre, hormigueo_manos_pies).
sintoma_de(sindrome_guillain_barre, dificultad_caminar).

% Hidrocefalia
sintoma_de(hidrocefalia, dolor_cabeza).
sintoma_de(hidrocefalia, confusion).
sintoma_de(hidrocefalia, vision_borrosa).
sintoma_de(hidrocefalia, incontinencia_urinaria).

% Tumor cerebral
sintoma_de(tumor_cerebral, dolor_cabeza_intenso).
sintoma_de(tumor_cerebral, perdida_vision).
sintoma_de(tumor_cerebral, vomito).
sintoma_de(tumor_cerebral, convulsiones).
sintoma_de(tumor_cerebral, cambios_personalidad).

% Neuritis óptica
sintoma_de(neuritis_optica, dolor_ojos).
sintoma_de(neuritis_optica, perdida_vision).
sintoma_de(neuritis_optica, vision_borrosa).

% Síndrome de Tourette
sintoma_de(sindrome_tourette, tics_motores).
sintoma_de(sindrome_tourette, tics_vocales).
sintoma_de(sindrome_tourette, movimientos_repetitivos).

% Compresión medular
sintoma_de(compresion_medular, dolor_espalda).
sintoma_de(compresion_medular, debilidad_piernas).
sintoma_de(compresion_medular, perdida_sensibilidad_miembros).

% Síndrome del túnel carpiano
sintoma_de(sindrome_canal_carpiano, hormigueo_manos).
sintoma_de(sindrome_canal_carpiano, dolor_muneca).
sintoma_de(sindrome_canal_carpiano, debilidad_mano).

% Miastenia Gravis
sintoma_de(miastenia_gravis, debilidad_muscular).
sintoma_de(miastenia_gravis, ptosis_palpebral).
sintoma_de(miastenia_gravis, dificultad_hablar).
sintoma_de(miastenia_gravis, dificultad_respirar).

% Ataxia cerebelosa
sintoma_de(ataxia_cerebelosa, perdida_equilibrio).
sintoma_de(ataxia_cerebelosa, movimientos_torpes).
sintoma_de(ataxia_cerebelosa, dificultad_hablar).

% Enf. Huntington
sintoma_de(enfermedad_huntington, movimientos_involuntarios).
sintoma_de(enfermedad_huntington, cambios_conducta).
sintoma_de(enfermedad_huntington, perdida_memoria).

% Síndrome de Rett
sintoma_de(sindrome_rett, movimientos_repetitivos_manos).
sintoma_de(sindrome_rett, retraso_habla).
sintoma_de(sindrome_rett, perdida_movimientos_coordinados).

% Fatiga crónica
sintoma_de(sindrome_fatiga_cronica, cansancio_extremo).
sintoma_de(sindrome_fatiga_cronica, dolor_cabeza).
sintoma_de(sindrome_fatiga_cronica, dificultad_concentrarse).
