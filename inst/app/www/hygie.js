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

// ── Types de variables dans les en-têtes Reactable ────────────────
// Chaque colonne reçoit une petite icône correspondant au type R.
// Un clic sur l'icône ouvre un menu permettant de changer directement
// le type. Le changement réutilise les événements Shiny déjà présents
// dans le module "Corriger le type d'une variable".
(function () {
  var TYPE_INFO = {
    numeric:    { icon: "fa-hashtag", label: "Numérique", value: "numerique" },
    integer:    { icon: "fa-hashtag", label: "Numérique entier", value: "numerique" },
    character:  { icon: "fa-font", label: "Texte", value: "texte" },
    factor:     { icon: "fa-tags", label: "Catégorielle", value: "categorielle" },
    ordered:    { icon: "fa-list-ol", label: "Catégorielle ordonnée", value: "categorielle" },
    logical:    { icon: "fa-toggle-on", label: "Logique", value: "logique" },
    Date:       { icon: "fa-calendar-alt", label: "Date", value: "date" },
    POSIXct:    { icon: "fa-clock", label: "Date-heure", value: "date" },
    POSIXlt:    { icon: "fa-clock", label: "Date-heure", value: "date" },
    default:    { icon: "fa-question", label: "Autre type", value: "texte" }
  };

  var TYPE_CHOICES = [
    { value: "numerique",    icon: "fa-hashtag",      label: "Numérique" },
    { value: "texte",        icon: "fa-font",         label: "Texte" },
    { value: "categorielle", icon: "fa-tags",         label: "Catégorielle" },
    { value: "logique",      icon: "fa-toggle-on",    label: "Logique" },
    { value: "date",         icon: "fa-calendar-alt", label: "Date" }
  ];

  function getTableMetadata() {
    var scripts = Array.prototype.slice.call(
      document.querySelectorAll('script[type="application/json"][data-for]')
    );
    for (var i = 0; i < scripts.length; i++) {
      try {
        var parsed = JSON.parse(scripts[i].textContent || "{}");
        var root = parsed && parsed[Object.keys(parsed)[0]];
        var data = root && root.tag && root.tag.attribs && root.tag.attribs.data;
        if (data && Array.isArray(data.columns)) return data.columns;
      } catch (err) {}
    }
    return [];
  }

  function infoFor(type) {
    if (!type) return TYPE_INFO.default;
    if (TYPE_INFO[type]) return TYPE_INFO[type];
    if (type.indexOf("POSIX") === 0) return TYPE_INFO.POSIXct;
    return TYPE_INFO.default;
  }

  function closeAllTypeMenus(except) {
    document.querySelectorAll(".h-type-menu-ouvert").forEach(function (el) {
      if (el !== except) el.classList.remove("h-type-menu-ouvert");
    });
  }

  function changeType(colName, typeValue) {
    // Date : passer par le convertisseur automatique multi-format de Hygie.
    // Cela évite de retomber sur un unique format %Y-%m-%d.
    if (typeValue === "date") {
      Shiny.setInputValue("cd_col", colName, { priority: "event" });
      Shiny.setInputValue("cd_fmt", "__auto__", { priority: "event" });
      window.setTimeout(function () {
        Shiny.setInputValue("cd_ok", Date.now() + Math.random(), { priority: "event" });
      }, 30);
      return;
    }

    Shiny.setInputValue("type_col", colName, { priority: "event" });
    Shiny.setInputValue("type_nouveau", typeValue, { priority: "event" });
    window.setTimeout(function () {
      Shiny.setInputValue("type_ok", Date.now() + Math.random(), { priority: "event" });
    }, 30);
  }

  function buildMenu(colName, currentType) {
    var info = infoFor(currentType);
    var wrapper = document.createElement("span");
    wrapper.className = "h-type-control";
    wrapper.title = "Type : " + info.label + " — cliquer pour modifier";

    var button = document.createElement("button");
    button.type = "button";
    button.className = "h-type-icon";
    button.setAttribute("aria-label", "Modifier le type de " + colName);
    button.innerHTML = '<i class="fas ' + info.icon + '" aria-hidden="true"></i>';

    var menu = document.createElement("span");
    menu.className = "h-type-menu";
    menu.setAttribute("role", "menu");

    TYPE_CHOICES.forEach(function (choice) {
      var item = document.createElement("button");
      item.type = "button";
      item.className = "h-type-menu-item";
      item.setAttribute("role", "menuitem");
      item.innerHTML = '<i class="fas ' + choice.icon + '" aria-hidden="true"></i><span>' + choice.label + '</span>';
      item.addEventListener("click", function (e) {
        e.preventDefault();
        e.stopPropagation();
        closeAllTypeMenus();
        changeType(colName, choice.value);
      });
      menu.appendChild(item);
    });

    button.addEventListener("click", function (e) {
      e.preventDefault();
      e.stopPropagation();
      var wasOpen = wrapper.classList.contains("h-type-menu-ouvert");
      closeAllTypeMenus(wrapper);
      wrapper.classList.toggle("h-type-menu-ouvert", !wasOpen);
    });

    wrapper.appendChild(button);
    wrapper.appendChild(menu);
    return wrapper;
  }

  function decorateHeaders() {
    var headers = document.querySelectorAll(".Reactable .rt-thead .rt-th");
    if (!headers.length) return;

    var columns = getTableMetadata().filter(function (c) {
      return c && c.id !== ".details" && c.show !== false;
    });
    if (!columns.length) return;

    headers.forEach(function (header, index) {
      if (header.querySelector(".h-type-control")) return;
      var col = columns[index];
      if (!col || !col.id) return;

      var text = header.querySelector(".rt-text-content");
      if (!text) return;

      var name = text.textContent || col.name || col.id;
      var info = infoFor(col.type);
      var control = buildMenu(col.id, col.type);

      var container = document.createElement("span");
      container.className = "h-type-header-content";
      container.appendChild(control);
      var label = document.createElement("span");
      label.className = "h-type-column-name";
      label.textContent = name;
      container.appendChild(label);
      text.textContent = "";
      text.appendChild(container);
    });
  }

  function injectTypeStyles() {
    if (document.getElementById("hygie-type-control-style")) return;
    var style = document.createElement("style");
    style.id = "hygie-type-control-style";
    style.textContent = `
      .h-type-header-content {
        display:inline-flex; align-items:center; gap:6px;
        max-width:100%; vertical-align:middle;
      }
      .h-type-column-name {
        overflow:hidden; text-overflow:ellipsis; white-space:nowrap;
        vertical-align:middle;
      }
      .h-type-control { position:relative; display:inline-flex; flex:0 0 auto; }
      .h-type-icon {
        width:20px; height:20px; padding:0; margin:0;
        border:1px solid transparent; border-radius:3px;
        background:transparent; color:#667085; cursor:pointer;
        display:inline-flex; align-items:center; justify-content:center;
        font-size:11px; line-height:1;
      }
      .h-type-icon:hover, .h-type-menu-ouvert .h-type-icon {
        background:#EBF2FF; border-color:#B3CFFB; color:#1A56C4;
      }
      .h-type-menu {
        display:none; position:absolute; z-index:10000;
        top:24px; left:0; min-width:165px; padding:4px;
        background:#fff; border:1px solid #CDD2DB; border-radius:4px;
        box-shadow:0 5px 16px rgba(16,24,40,.16);
      }
      .h-type-menu-ouvert .h-type-menu { display:block; }
      .h-type-menu-item {
        width:100%; border:0; background:transparent; padding:6px 8px;
        display:flex; align-items:center; gap:8px; text-align:left;
        font:11.5px 'Noto Sans',sans-serif; color:#344054; cursor:pointer;
        border-radius:3px;
      }
      .h-type-menu-item:hover { background:#F2F4F7; color:#1A56C4; }
      .h-type-menu-item i { width:15px; text-align:center; color:#667085; }
      .h-type-menu-item:hover i { color:#1A56C4; }
      .rt-th { overflow:visible !important; }
      .rt-th-inner, .rt-sort-header, .rt-text-content { overflow:visible !important; }
    `;
    document.head.appendChild(style);
  }

  function schedule() {
    window.clearTimeout(window.__hygieTypeControlTimer);
    window.__hygieTypeControlTimer = window.setTimeout(function () {
      injectTypeStyles();
      decorateHeaders();
    }, 120);
  }

  document.addEventListener("DOMContentLoaded", function () {
    var root = document.querySelector(".h-app") || document.body;
    var observer = new MutationObserver(schedule);
    observer.observe(root, { childList:true, subtree:true });
    schedule();

    document.addEventListener("click", function (e) {
      if (!e.target.closest(".h-type-control")) closeAllTypeMenus();
    });
  });
})();

// ---------------------------------------------------------------------
// IMPORTANT : le rendu/masquage des tableaux DT est volontairement
// absent de ce fichier. Shiny crée maintenant les DT uniquement quand
// les données existent. Cela évite d'initialiser DataTables dans un
// conteneur display:none et élimine les courses entre Shiny, DT et JS.
// ---------------------------------------------------------------------