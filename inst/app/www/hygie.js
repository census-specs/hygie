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

// ---------------------------------------------------------------------
// IMPORTANT : le rendu/masquage des tableaux DT est volontairement
// absent de ce fichier. Shiny crée maintenant les DT uniquement quand
// les données existent. Cela évite d'initialiser DataTables dans un
// conteneur display:none et élimine les courses entre Shiny, DT et JS.
// ---------------------------------------------------------------------