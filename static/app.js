// static/app.js

// ===== estado global =====
let ALL_SYMPTOMS = [];
let FILTER = "";

// ===== helpers visuales =====
function $(s){ return document.querySelector(s); }
function setOut(html){ $("#out").innerHTML = html; }
function ul(items){ return `<ul>${(items||[]).map(x=>`<li>${x}</li>`).join("")}</ul>`; }
function setBadge(text, cls=""){ const b=$("#tipoBadge"); if(b){ b.textContent=text; b.className="badge " + cls; } }
function toast(msg){
  const t=$("#toast"); if(!t) return;
  t.textContent=msg; t.style.display="block";
  clearTimeout(window.__t);
  window.__t=setTimeout(()=>t.style.display="none", 1800);
}
function setSelCount(){
  const el=$("#selCount"); if(!el) return;
  const n = obtenerSintomasSeleccionados().length;
  el.textContent = `${n} seleccionados`;
}

// ===== render de síntomas =====
function renderSymptoms(){
  const grid = $("#symgrid");
  const filtered = ALL_SYMPTOMS.filter(s => s.toLowerCase().includes(FILTER.toLowerCase()));
  grid.innerHTML = filtered.map(s => `
    <label class="pill">
      <input type="checkbox" value="${s}" onchange="toggleDiagBtn(); setSelCount()">
      <span>${s}</span>
    </label>
  `).join("");
  toggleDiagBtn();
  setSelCount();
}

// ===== estados =====
function toggleDiagBtn(){
  const any = obtenerSintomasSeleccionados().length > 0;
  const btn = $("#btnDiag");
  if (btn) btn.disabled = !any;
}
function obtenerSintomasSeleccionados(){
  return [...document.querySelectorAll('#symgrid input[type="checkbox"]:checked')].map(i => i.value);
}
window.filterSyms = function(){
  FILTER = $("#q").value.trim();
  renderSymptoms();
};

// ===== llamadas =====
window.loadSyms = async function(){
  $("#symgrid").innerHTML = "<p class='counter'>Cargando síntomas…</p>";
  const res = await fetch("/api/symptoms");
  const data = await res.json();
  ALL_SYMPTOMS = data.symptoms || [];
  renderSymptoms();
  toast(`Se cargaron ${ALL_SYMPTOMS.length} síntomas`);
  setOut(`<p class="counter">Listo. Selecciona algunos y diagnostica.</p>`);
};

window.diagnose = async function(){
  const symptoms = obtenerSintomasSeleccionados();
  if (!symptoms.length){ toast("Selecciona al menos un síntoma"); return; }

  setOut(`<p class="muted">Analizando…</p>`);
  const res = await fetch("/api/diagnose", {
    method:"POST",
    headers:{ "Content-Type":"application/json" },
    body: JSON.stringify({ symptoms })
  });
  const data = await res.json();

  const tipo = data.tipo || "desconocido";
  setBadge(tipo, tipo !== "desconocido" ? "ok" : "");

  // chips de enfermedades (las del array plano)
  const chips = (data.enfermedades || [])
    .map(e => `<span class="chip">${e}</span>`).join(" ") || '<span class="chip">—</span>';

  // top_enfermedades (con barras)
  const top = (data.detalle && data.detalle.top_enfermedades) ? data.detalle.top_enfermedades : [];
  const maxScore = Math.max(1, ...top.map(t => t.score || 0));
  const topHtml = top.length
    ? top.map(t => {
        const sc = t.score || 0;
        const width = Math.round((sc / maxScore) * 100);
        return `
          <div class="card-mini">
            <div class="row" style="justify-content:space-between;margin-bottom:6px">
              <strong>${t.enfermedad}</strong>
              <span class="muted">score: ${sc}</span>
            </div>
            <div class="meter"><span style="width:${width}%"></span></div>
          </div>
        `;
      }).join("")
    : '<p class="muted">Sin ranking disponible.</p>';

  // descripción corta por categoría (fallback si el backend no trae explicación)
  const CAT_DESC = {
    digestiva: "Dolencias del aparato digestivo (estómago, intestino, hígado). Suelen relacionarse con dolor abdominal, náuseas, vómitos o alteraciones del tránsito.",
    respiratoria: "Trastornos que afectan vías respiratorias y pulmones; típicos: tos, disnea, dolor torácico, sibilancias o fiebre.",
    cardiovascular: "Alteraciones del corazón y vasos; signos: dolor opresivo, palpitaciones, disnea, edema.",
    traumatologica: "Lesiones por golpes o esfuerzos: fracturas, esguinces, contusiones, tendinitis.",
    infecciosa: "Procesos causados por microorganismos; fiebre, malestar general y síntomas locales.",
    neurologica: "Sistema nervioso central o periférico; cefalea, mareos, déficit motor/sensitivo, convulsiones."
  };

  const descripcion = (data.detalle && (data.detalle.descripcion || data.detalle.explicacion))
                      || CAT_DESC[tipo] || "Descripción no disponible para esta categoría.";

  setOut(`
    <div class="section">
      <div class="row" style="gap:10px;align-items:center">
        <span class="badge ok">Tipo probable: ${tipo}</span>
      </div>
      <p class="muted" style="margin-top:8px">${descripcion}</p>
    </div>

    <div class="section">
      <p class="title-sm">Enfermedades posibles</p>
      <div class="chips">${chips}</div>
    </div>

    <div class="section">
      <p class="title-sm">Top por puntuación</p>
      <div class="col" style="display:grid;gap:10px">${topHtml}</div>
    </div>
  `);

  toast("Diagnóstico actualizado");
};

// ===== consultas avanzadas =====
window.uiSintomasDe = async function(){
  const enfermedad = $("#inEnf1").value.trim();
  if(!enfermedad) return toast("Ingresa una enfermedad");
  setOut(`<p class="counter">Buscando síntomas de ${enfermedad}…</p>`);
  const res = await fetch("/api/sintomas_de", {
    method:"POST", headers:{ "Content-Type":"application/json" },
    body: JSON.stringify({ enfermedad })
  });
  const data = await res.json();
  setOut(`<h3>Síntomas de <code>${data.enfermedad}</code></h3>${ul(data.sintomas)}`);
};

window.uiEnfermedadesPorSintoma = async function(){
  const sintoma = $("#inSint1").value.trim();
  if(!sintoma) return toast("Ingresa un síntoma");
  setOut(`<p class="counter">Buscando enfermedades con ${sintoma}…</p>`);
  const res = await fetch("/api/enfermedades_por_sintoma", {
    method:"POST", headers:{ "Content-Type":"application/json" },
    body: JSON.stringify({ sintoma })
  });
  const data = await res.json();
  setOut(`<h3>Enfermedades asociadas a <code>${data.sintoma}</code></h3>${ul(data.enfermedades)}`);
};

window.uiCategoriaEnfermedad = async function(){
  const enfermedad = $("#inEnf2").value.trim();
  if(!enfermedad) return toast("Ingresa una enfermedad");
  setOut(`<p class="counter">Buscando categoría…</p>`);
  const res = await fetch("/api/categoria_enfermedad", {
    method:"POST", headers:{ "Content-Type":"application/json" },
    body: JSON.stringify({ enfermedad })
  });
  const data = await res.json();
  setOut(`<p>La enfermedad <code>${data.enfermedad}</code> pertenece a la categoría <span class="badge blue">${data.categoria}</span>.</p>`);
};

window.uiEnfermedadesPosibles = async function(){
  const sintomas = obtenerSintomasSeleccionados();
  if(!sintomas.length) return toast("Selecciona síntomas primero");
  setOut(`<p class="counter">Calculando…</p>`);
  const res = await fetch("/api/enfermedades_posibles", {
    method:"POST", headers:{ "Content-Type":"application/json" },
    body: JSON.stringify({ sintomas })
  });
  const data = await res.json();
  setOut(`<h3>Enfermedades posibles</h3>${ul(data.enfermedades_posibles)}`);
};

// Necesario porque en el HTML usamos onchange="toggleDiagBtn()"
window.toggleDiagBtn = toggleDiagBtn;
