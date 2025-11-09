# Diagnóstico Médico Básico, Prolog + SWI HTTP Server

Este proyecto implementa un sistema experto básico para el diagnóstico de dolencias **infecciosas, traumáticas y cardiovasculares**, desarrollado en **SWI-Prolog** usando su biblioteca HTTP integrada.  
Incluye una interfaz web ligera (`app.js`) que permite seleccionar síntomas y obtener el tipo probable de dolencia mediante reglas lógicas.

----

## Requisitos previos

- [SWI-Prolog](https://www.swi-prolog.org/) versión **≥ 9.0.4**

---

## Cómo ejecutar el sistema

### 1- Abrir el proyecto desde la consola CMD

```bash
cd C:/Users/../PROLOG_PROJECT/
```

*(Ajustar la ruta de descarga del proyecto en cmd, o abres una nueva terminal de VS code)*

---

### 2- Iniciar el servidor completo

```bash
swipl run.pl
```

Esto levanta un servidor local en el puerto **8080**.
---

### 3- Apagar el servidor por completo

```bash
ctrl + C
```
Esto va a cerrar toods los hilos levantados y terminará el proceso.
---

## Uso desde navegador

1. Abre [http://localhost:8080/](http://localhost:8080/)
2. Marca los síntomas que correspondan
3. Haz clic en **“Diagnosticar”**
4. El sistema mostrará el tipo de dolencia y las enfermedades candidatas

---

##  Módulos 

###  `kb_salud.pl`
- Define hechos: `sintoma_de/2`, `es_dolencia_infecciosa/1`, etc.
- Contiene reglas de diagnóstico como:
  ```prolog
  atajo_categoria(P, infecciosa) :-
      tiene(P, fiebre),
      (tiene(P, tos); tiene(P, mialgia); tiene(P, rinorrea)).
  ```

###  `server.pl`
- Expone endpoints REST (`/api/symptoms`, `/api/diagnose`)
- Renderiza la interfaz HTML + JS
- Controla la sesión de síntomas en memoria temporal (`tiene(temp, X)`)

###  `app.js`
- Genera dinámicamente los checkboxes
- Mantiene los síntomas seleccionados entre filtrados
- Envía las solicitudes `fetch()` al servidor

---

 
Universidad Nacional de Costa Rica — Ingeniería en Sistemas de Información  
Sede Regional Brunca – Campus Coto  
2025

