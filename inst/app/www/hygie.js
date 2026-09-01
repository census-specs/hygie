// =====================================================================
// HYGIE v1.0 — Interface logiciel statistique professionnel
// Gestion des menus déroulants + affichage données
// =====================================================================

document.addEventListener("DOMContentLoaded", function () {

  // ── Ouvrir / fermer les menus ──────────────────────────
  document.addEventListener("click", function (e) {
    var trigger  = e.target.closest(".h-menu-trigger");
    var menuItem = trigger ? trigger.closest(".h-menu-item") : null;

    document.querySelectorAll(".h-menu-item.ouvert").forEach(function (m) {
      if (m !== menuItem) m.classList.remove("ouvert");
    });

    if (menuItem) {
      menuItem.classList.toggle("ouvert");
      e.stopPropagation();
    }
  });

  // ── Clic sur un item du menu ───────────────────────────
  document.addEventListener("click", function (e) {
    var item = e.target.closest(".h-dropdown-item");
    if (!item || item.classList.contains("desactive")) return;

    var action = item.dataset.action;
    if (action) {
      Shiny.setInputValue("menu_action", action, { priority: "event" });
    }
    document.querySelectorAll(".h-menu-item.ouvert").forEach(function (m) {
      m.classList.remove("ouvert");
    });
  });

  // ── Clic en dehors → fermer ────────────────────────────
  document.addEventListener("click", function (e) {
    if (!e.target.closest(".h-menu-item")) {
      document.querySelectorAll(".h-menu-item.ouvert").forEach(function (m) {
        m.classList.remove("ouvert");
      });
    }
  });

  // ── Echap → fermer les menus ───────────────────────────
  document.addEventListener("keydown", function (e) {
    if (e.key === "Escape") {
      document.querySelectorAll(".h-menu-item.ouvert").forEach(function (m) {
        m.classList.remove("ouvert");
      });
    }
  });
});

// ── Activer/désactiver les items selon les données ─────────
Shiny.addCustomMessageHandler("majMenus", function (data) {
  document.querySelectorAll(
    ".h-dropdown-item[data-necessite-donnees]"
  ).forEach(function (el) {
    if (data.donnees_chargees) {
      el.classList.remove("desactive");
      el.removeAttribute("disabled");
    } else {
      el.classList.add("desactive");
    }
  });
});

// ── Résumé qualité : construit à partir des badges réellement rendus ──
// Les badges manquants/outliers représentent des cellules/valeurs par
// variable. Pour les doublons, la même quantité est répétée dans chaque
// colonne : on prend donc le maximum plutôt que de sommer.
(function () {
  var observer = null;

  function nombreBadge(el) {
    var m = (el.textContent || "").match(/^\s*(\d+)/);
    return m ? parseInt(m[1], 10) : 0;
  }

  function total(selector, mode) {
    var els = Array.prototype.slice.call(document.querySelectorAll(selector));
    if (!els.length) return 0;
    var valeurs = els.map(nombreBadge);
    return mode === "max" ? Math.max.apply(null, valeurs) : valeurs.reduce(function (a, b) { return a + b; }, 0);
  }

  function injectStyles() {
    if (document.getElementById("hygie-quality-summary-style")) return;
    var style = document.createElement("style");
    style.id = "hygie-quality-summary-style";
    style.textContent = `
      .h-quality-summary {
        display:flex; align-items:stretch; gap:8px; flex-wrap:wrap;
        margin:0 0 8px 0; padding:8px 10px;
        border:1px solid #E4E7EC; border-radius:4px;
        background:#FCFCFD; font-family:'Noto Sans',sans-serif;
      }
      .h-quality-summary-title {
        display:flex; align-items:center; margin-right:4px;
        font-size:10px; font-weight:700; letter-spacing:.05em;
        text-transform:uppercase; color:#667085;
      }
      .h-quality-summary-card {
        display:flex; align-items:center; gap:7px;
        min-width:118px; padding:5px 9px;
        border:1px solid #EAECF0; border-radius:4px;
        background:#fff;
      }
      .h-quality-summary-number { font-size:15px; font-weight:700; line-height:1; }
      .h-quality-summary-label { font-size:10.5px; color:#667085; }
      .h-quality-summary-card.missing .h-quality-summary-number { color:#C2410C; }
      .h-quality-summary-card.outlier .h-quality-summary-number { color:#B42318; }
      .h-quality-summary-card.duplicate .h-quality-summary-number { color:#6941C6; }
      .h-quality-summary-card.ok .h-quality-summary-number { color:#027A48; }
      @media (max-width: 700px) {
        .h-quality-summary-title { width:100%; }
        .h-quality-summary-card { flex:1; min-width:100px; }
      }
    `;
    document.head.appendChild(style);
  }

  function renderSummary() {
    var header = document.querySelector(".h-zone-data-header");
    if (!header) return;

    injectStyles();

    var missing = total(".h-qualite-missing", "sum");
    var outlier = total(".h-qualite-outlier", "sum");
    var duplicate = total(".h-qualite-duplicate", "max");

    var summary = document.getElementById("hygie-quality-summary");
    if (!summary) {
      summary = document.createElement("div");
      summary.id = "hygie-quality-summary";
      summary.className = "h-quality-summary";
      header.parentNode.insertBefore(summary, header.nextSibling);
    }

    if (!missing && !outlier && !duplicate) {
      summary.innerHTML = `
        <div class="h-quality-summary-title">Qualité des données</div>
        <div class="h-quality-summary-card ok">
          <span class="h-quality-summary-number">OK</span>
          <span class="h-quality-summary-label">aucun problème détecté</span>
        </div>`;
      return;
    }

    summary.innerHTML = `
      <div class="h-quality-summary-title">Qualité des données</div>
      <div class="h-quality-summary-card missing">
        <span class="h-quality-summary-number">${missing}</span>
        <span class="h-quality-summary-label">valeur(s) manquante(s)</span>
      </div>
      <div class="h-quality-summary-card outlier">
        <span class="h-quality-summary-number">${outlier}</span>
        <span class="h-quality-summary-label">valeur(s) aberrante(s)</span>
      </div>
      <div class="h-quality-summary-card duplicate">
        <span class="h-quality-summary-number">${duplicate}</span>
        <span class="h-quality-summary-label">ligne(s) dupliquée(s)</span>
      </div>`;
  }

  function schedule() {
    window.clearTimeout(window.__hygieQualitySummaryTimer);
    window.__hygieQualitySummaryTimer = window.setTimeout(renderSummary, 80);
  }

  document.addEventListener("DOMContentLoaded", function () {
    var root = document.querySelector(".h-app") || document.body;
    observer = new MutationObserver(schedule);
    observer.observe(root, { childList:true, subtree:true });
    schedule();
  });
})();

// ---------------------------------------------------------------------
// TYPES DES COLONNES — icône + menu de changement rapide
// ---------------------------------------------------------------------
(function () {
  var TYPE_DEFS = {
    numerique:    { icon: "123", label: "Numérique", cls: "numeric" },
    texte:        { icon: "Aa",  label: "Texte", cls: "text" },
    categorielle: { icon: "Ab",  label: "Catégorielle", cls: "factor" },
    logique:      { icon: "✓",   label: "Logique", cls: "logical" },
    date:         { icon: "▣",   label: "Date", cls: "date" }
  };

  var MENU_ID = "hygie-type-menu";
  var timer = null;

  function injectTypeStyles() {
    if (document.getElementById("hygie-type-style")) return;
    var style = document.createElement("style");
    style.id = "hygie-type-style";
    style.textContent = `
      .hygie-type-wrap { display:inline-flex; align-items:center; position:relative; margin-right:5px; }
      .hygie-type-icon { display:inline-flex; align-items:center; justify-content:center; width:20px; height:20px;
        border:1px solid #D0D5DD; border-radius:4px; background:#F9FAFB; color:#344054;
        font:600 9px/1 'Noto Sans',sans-serif; cursor:pointer; vertical-align:middle; }
      .hygie-type-icon:hover { background:#F2F4F7; border-color:#98A2B3; }
      .hygie-type-icon.date { font-size:13px; }
      .hygie-type-menu { position:fixed; z-index:99999; min-width:170px; padding:4px;
        background:#fff; border:1px solid #D0D5DD; border-radius:5px; box-shadow:0 8px 20px rgba(16,24,40,.14);
        font-family:'Noto Sans',sans-serif; }
      .hygie-type-option { width:100%; display:flex; align-items:center; gap:9px; padding:7px 9px;
        border:0; background:#fff; color:#344054; cursor:pointer; text-align:left; font-size:11.5px; border-radius:3px; }
      .hygie-type-option:hover { background:#F2F4F7; }
      .hygie-type-option .mini { width:22px; text-align:center; font-weight:700; }
    `;
    document.head.appendChild(style);
  }

  function normalizeName(s) {
    return (s || "").replace(/\s+/g, " ").trim();
  }

  function guessType(values) {
    var vals = values.map(function (v) { return normalizeName(v); }).filter(function (v) { return v !== ""; });
    if (!vals.length) return "texte";
    if (vals.every(function (v) { return /^(TRUE|FALSE|T|F|VRAI|FAUX)$/i.test(v); })) return "logique";
    if (vals.every(function (v) { return /^[-+]?\d+(?:[.,]\d+)?$/.test(v.replace(/\s/g, "")); })) return "numerique";
    if (vals.every(function (v) { return /^(?:\d{4}[-/.]\d{1,2}[-/.]\d{1,2}|\d{1,2}[-/.]\d{1,2}[-/.]\d{2,4})/.test(v); })) return "date";
    var uniques = [];
    vals.forEach(function(v){ if (uniques.indexOf(v) === -1) uniques.push(v); });
    return uniques.length <= Math.min(20, Math.max(5, vals.length * 0.2)) ? "categorielle" : "texte";
  }

  function getColumnCells(th) {
    var table = th.closest(".rt-table") || th.closest("table") || document;
    var idx = Array.prototype.indexOf.call(th.parentNode.children, th);
    var cells = table.querySelectorAll(".rt-tbody .rt-td");
    var values = [];
    Array.prototype.forEach.call(cells, function(cell, i) {
      var cellIdx = Array.prototype.indexOf.call(cell.parentNode.children, cell);
      if (cellIdx === idx && values.length < 30) values.push(cell.textContent || "");
    });
    return values;
  }

  function closeMenu() {
    var m = document.getElementById(MENU_ID);
    if (m) m.remove();
  }

  function applyType(col, type) {
    closeMenu();
    // Réutilise le mécanisme de type déjà présent dans Hygie :
    // aucune nouvelle logique de conversion n'est dupliquée côté JS.
    Shiny.setInputValue("type_col", col, {priority:"event"});
    Shiny.setInputValue("type_nouveau", type, {priority:"event"});
    Shiny.setInputValue("type_fmt", "%Y-%m-%d", {priority:"event"});
    // Le serveur écoute déjà type_ok. Le court délai laisse à Shiny le
    // temps d'enregistrer type_col/type_nouveau avant de déclencher l'étape.
    window.setTimeout(function () {
      Shiny.setInputValue("type_ok", Date.now() + Math.random(), {priority:"event"});
    }, 40);
  }

  function openMenu(icon, col, current) {
    closeMenu();
    var menu = document.createElement("div");
    menu.id = MENU_ID;
    menu.className = "hygie-type-menu";
    var rect = icon.getBoundingClientRect();
    menu.style.left = Math.max(5, Math.min(window.innerWidth - 180, rect.left)) + "px";
    menu.style.top = Math.min(window.innerHeight - 10, rect.bottom + 4) + "px";

    Object.keys(TYPE_DEFS).forEach(function(type) {
      var def = TYPE_DEFS[type];
      var option = document.createElement("button");
      option.type = "button";
      option.className = "hygie-type-option";
      option.innerHTML = '<span class="mini">' + def.icon + '</span><span>' + def.label + '</span>';
      if (type === current) option.style.fontWeight = "700";
      option.addEventListener("click", function(e) {
        e.stopPropagation();
        applyType(col, type);
      });
      menu.appendChild(option);
    });
    document.body.appendChild(menu);
  }

  function enhanceHeaders() {
    injectTypeStyles();
    var headers = document.querySelectorAll(".rt-thead .rt-th");
    if (!headers.length) return;

    Array.prototype.forEach.call(headers, function(th) {
      if (th.querySelector(".hygie-type-icon")) return;
      var rawName = th.getAttribute("data-column-id") || "";
      var labelNode = th.querySelector(".rt-th-content") || th;
      var col = normalizeName(rawName || labelNode.textContent);
      if (!col || col === "" || col.toLowerCase() === "group") return;

      var current = guessType(getColumnCells(th));
      var wrap = document.createElement("span");
      wrap.className = "hygie-type-wrap";
      var icon = document.createElement("button");
      icon.type = "button";
      icon.className = "hygie-type-icon " + TYPE_DEFS[current].cls;
      icon.title = "Type : " + TYPE_DEFS[current].label + " — cliquer pour modifier";
      icon.textContent = TYPE_DEFS[current].icon;
      icon.addEventListener("click", function(e) {
        e.preventDefault();
        e.stopPropagation();
        openMenu(icon, col, current);
      });
      wrap.appendChild(icon);
      labelNode.insertBefore(wrap, labelNode.firstChild);
    });
  }

  function schedule() {
    window.clearTimeout(timer);
    timer = window.setTimeout(enhanceHeaders, 150);
  }

  document.addEventListener("DOMContentLoaded", function() {
    var root = document.querySelector(".h-app") || document.body;
    var observer = new MutationObserver(schedule);
    observer.observe(root, {childList:true, subtree:true});
    schedule();
  });

  document.addEventListener("click", function(e) {
    if (!e.target.closest(".hygie-type-icon") && !e.target.closest("#" + MENU_ID)) closeMenu();
  });

  window.addEventListener("scroll", closeMenu, true);
})();

// ---------------------------------------------------------------------
// IMPORTANT : le rendu/masquage des tableaux DT est volontairement
// absent de ce fichier. Shiny crée maintenant les DT uniquement quand
// les données existent. Cela évite d'initialiser DataTables dans un
// conteneur display:none et élimine les courses entre Shiny, DT et JS.
// ---------------------------------------------------------------------