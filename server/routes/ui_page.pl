% server/routes/ui_page.pl
:- module(ui_page, [ui_page/1]).
:- use_module(library(http/html_write)).

ui_page(_Request) :-
    reply_html_page(
        [title('Diagnóstico básico (Prolog)'), meta([charset='UTF-8'])],
        [\styles, \page_body]
    ).

styles -->
    html(style([
      % ====== tokens ======
      ':root{--bg:#0b1220;--panel:#121a2b;--muted:#9fb0c7;--text:#e5ecf7;--accent:#22c55e;--accent-2:#3b82f6;--danger:#ef4444;--stroke:#22304b;--pill:#0f1627}',
      '*,*::before,*::after{box-sizing:border-box}',
      'html,body{height:100%}',
      'body{margin:0;font-family:ui-sans-serif,system-ui,Segoe UI,Roboto,Helvetica,Arial;letter-spacing:.1px;color:var(--text);background:radial-gradient(1200px 600px at 70% -10%,#12345a22,transparent),var(--bg)}',

      % ====== layout ======
      'header{position:sticky;top:0;z-index:40;background:linear-gradient(180deg,#0b1220,#0b1220f5 70%,transparent);backdrop-filter:saturate(140%) blur(4px);border-bottom:1px solid #1b273f}',
      '.wrap{max-width:1100px;margin:0 auto;padding:18px}',
      '.title{display:flex;align-items:center;gap:10px}',
      '.title h1{font-size:20px;margin:0}',
      '.subtitle{color:var(--muted);font-size:13px;margin:2px 0 0}',
      '.grid{display:grid;grid-template-columns:1.1fr .9fr;gap:16px}',
      '@media(max-width:900px){.grid{grid-template-columns:1fr}}',

      % ====== cards ======
      '.card{background:var(--panel);border:1px solid var(--stroke);border-radius:16px;box-shadow:0 10px 30px #00000033;overflow:hidden}',
      '.card .h{display:flex;align-items:center;justify-content:space-between;padding:14px 16px;border-bottom:1px solid #1b273f}',
      '.card .h h2{font-size:16px;margin:0}',
      '.card .b{padding:16px}',

      % ====== controls ======
      '.row{display:flex;gap:10px;flex-wrap:wrap;align-items:center}',
      'input[type=text]{flex:1;min-width:220px;padding:10px 12px;border-radius:10px;background:#0e1628;border:1px solid #23314c;color:var(--text);outline:none}',
      'input[type=text]::placeholder{color:#6b7b94}',
      'button{border:0;border-radius:10px;padding:10px 14px;cursor:pointer;background:var(--accent);color:#06101c;font-weight:600;transition:transform .05s ease,opacity .2s}',
      'button.secondary{background:var(--accent-2);color:white}',
      'button.ghost{background:#0e1628;color:var(--text);border:1px solid #22304b}',
      'button:active{transform:translateY(1px)}',
      'button:disabled{opacity:.55;cursor:not-allowed}',

      % ====== symptoms grid ======
      '.symtoolbar{display:flex;gap:8px;align-items:center;margin-bottom:10px}',
      '.counter{font-size:12px;color:var(--muted)}',
      '.symgrid{display:grid;grid-template-columns:repeat(auto-fill,minmax(200px,1fr));gap:10px;max-height:380px;overflow:auto;padding-right:4px}',
      '.pill{display:flex;align-items:center;gap:10px;background:var(--pill);border:1px solid #21304b;border-radius:999px;padding:8px 12px}',
      '.pill input{appearance:none;width:14px;height:14px;border-radius:3px;border:1px solid #39507a;background:#0b1220;display:inline-block;position:relative}',
      '.pill input:checked{background:var(--accent);border-color:var(--accent)}',
      '.pill span{font-size:13px}',

      % ====== result ======
      '.badge{display:inline-block;padding:4px 10px;border-radius:999px;background:#0e1628;border:1px solid #23314c;font-size:12px}',
      '.badge.ok{background:#072615;border-color:#145a32;color:#b8f3cf}',
      '.badge.blue{background:#0d1a2e;border-color:#274a96;color:#d6e6ff}',
      '.tags{display:flex;flex-wrap:wrap;gap:8px}',
      '.tag{background:#0e1628;border:1px solid #23314c;border-radius:999px;padding:4px 10px;font-size:12px}',
      'pre.json{background:#0b1220;border:1px solid #1b273f;border-radius:12px;padding:12px;overflow:auto}',

      % ====== advanced ======
      '.advgrid{display:grid;grid-template-columns:1fr auto;gap:10px}',
      '.line{display:grid;grid-template-columns:1fr auto;gap:10px;margin-bottom:8px}',

      % ====== toast ======
      '#toast{position:fixed;bottom:16px;right:16px;display:none;padding:10px 14px;border-radius:12px;background:#0e1628;border:1px solid #23314c;color:var(--text);box-shadow:0 10px 30px #00000040;z-index:60}',

      % ---- pretty result ----
      '.section{margin:10px 0 16px}',
      '.title-sm{font-weight:700;margin:0 0 6px;font-size:13px;color:#b8c7df}',
      '.chips{display:flex;flex-wrap:wrap;gap:8px}',
      '.chip{background:#0e1628;border:1px solid #23314c;border-radius:999px;padding:4px 10px;font-size:12px}',
      '.meter{height:8px;border-radius:999px;background:#0f1627;border:1px solid #22304b;overflow:hidden}',
      '.meter>span{display:block;height:100%;background:#22c55e}',
      '.card-mini{border:1px solid #23314c;border-radius:12px;padding:10px;background:#0e1628}',
      '.muted{color:#9fb0c7;font-size:13px}'
    ])).

page_body -->
  html([
    header(div(class(wrap), div(class(title), [
      h1('SaludDiag — Motor Prolog'),
      span(class(subtitle), 'Clasificación por síntomas y consultas médicas')
    ]))),
    div(class(wrap), div(class(grid), [
      div(class(card), [
        div(class(h), [
          h2('Selección de síntomas'),
          span([id(selCount), class(counter)], '0 seleccionados')
        ]),
        div(class(b), [
          div(class(symtoolbar), [
            input([type(text), id(q), placeholder('Filtrar síntomas (ej. fiebre, tos...)'), oninput('filterSyms()')]),
            button([id(btnLoad), class(ghost), onclick('loadSyms()')], 'Cargar síntomas'),
            button([id(btnDiag), class(secondary), onclick('diagnose()'), disabled(true)], 'Diagnosticar')
          ]),
          div([id(symgrid), class(symgrid)], [])
        ])
      ]),
      div(class(card), [
        div(class(h), [
          h2('Resultado'),
          span([id(tipoBadge), class(badge)], 'Aún sin diagnóstico')
        ]),
        div([id(out), class(b)], [
          p(class(counter), 'Selecciona síntomas y presiona Diagnosticar.')
        ])
      ])
    ])),
    div(class(wrap), div(class(card), [
      div(class(h), [h2('Consultas avanzadas')]),
      div(class(b), [
        div(class(line), [
          input([type(text), id(inEnf1), placeholder('Enfermedad: p. ej. neumonia')]),
          button([onclick('uiSintomasDe()')], 'Síntomas de enfermedad')
        ]),
        div(class(line), [
          input([type(text), id(inSint1), placeholder('Síntoma: p. ej. tos_seca')]),
          button([onclick('uiEnfermedadesPorSintoma()')], 'Enfermedades por síntoma')
        ]),
        div(class(line), [
          input([type(text), id(inEnf2), placeholder('Enfermedad: p. ej. asma')]),
          button([onclick('uiCategoriaEnfermedad()')], 'Categoría de enfermedad')
        ]),
        div(class(line), [
          span('Usar síntomas seleccionados'),
          button([onclick('uiEnfermedadesPosibles()')], 'Enfermedades posibles')
        ])
      ])
    ])),
    div([id(toast)], 'Listo'),
    footer(div(class(wrap), span(class(counter),
      'Hecho con SWI-Prolog + library(http).'
    ))),
    script([type('module'), src('/app.js')], [])
  ]).
