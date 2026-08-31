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
// IMPORTANT : le rendu/masquage des tableaux DT est volontairement
// absent de ce fichier. Shiny crée maintenant les DT uniquement quand
// les données existent. Cela évite d'initialiser DataTables dans un
// conteneur display:none et élimine les courses entre Shiny, DT et JS.
// ---------------------------------------------------------------------