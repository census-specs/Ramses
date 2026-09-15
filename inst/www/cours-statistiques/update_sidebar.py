#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
RAMSES 1.0 — Script de mise à jour automatique de la sidebar (v3)
Ajoute les 7 nouveaux modules : 02b, 05b, 08b, 09b, 10b, 12, 13.
Gère les ancres avec class="nav-link" ET class="nav-link active".
"""

import os
import shutil
from pathlib import Path

# ============================================================================
# CONFIGURATION — Les 7 nouveaux liens à insérer
# ============================================================================

NEW_LINKS = [
    # --- Module 02b : après 02-comprendre-mes-variables.html ---
    {
        "target": "02b-preparer-et-nettoyer-mes-donnees.html",
        "anchor_file": "02-comprendre-mes-variables.html",
        "block": """
      <a href="{prefix}02b-preparer-et-nettoyer-mes-donnees.html" class="nav-link">
        <i class="fa-solid fa-broom"></i> 02b. Préparer et nettoyer mes données <span class="nav-badge" style="background:#2563EB; color:#FFFFFF;">Nouveau</span>
      </a>""",
    },
    # --- Module 05b : après 05-visualiser-mes-donnees.html ---
    {
        "target": "05b-intervalles-de-confiance.html",
        "anchor_file": "05-visualiser-mes-donnees.html",
        "block": """
      <a href="{prefix}05b-intervalles-de-confiance.html" class="nav-link">
        <i class="fa-solid fa-arrows-left-right-to-line"></i> 05b. Les intervalles de confiance <span class="nav-badge" style="background:#2563EB; color:#FFFFFF;">Nouveau</span>
      </a>""",
    },
    # --- Module 08b : après 08-comparer-groupes-quel-test.html ---
    {
        "target": "08b-verifier-les-conditions-dun-test.html",
        "anchor_file": "08-comparer-groupes-quel-test.html",
        "block": """
      <a href="{prefix}08b-verifier-les-conditions-dun-test.html" class="nav-link">
        <i class="fa-solid fa-clipboard-check"></i> 08b. Vérifier les conditions d'un test <span class="nav-badge" style="background:#2563EB; color:#FFFFFF;">Nouveau</span>
      </a>""",
    },
    # --- Module 09b : après 09-comparer-3-groupes-ou-plus.html ---
    {
        "target": "09b-anova-deux-facteurs.html",
        "anchor_file": "09-comparer-3-groupes-ou-plus.html",
        "block": """
      <a href="{prefix}09b-anova-deux-facteurs.html" class="nav-link">
        <i class="fa-solid fa-table-cells-large"></i> 09b. ANOVA à deux facteurs <span class="nav-badge" style="background:#2563EB; color:#FFFFFF;">Nouveau</span>
      </a>""",
    },
    # --- Module 10b : après 10-etudier-relation-deux-variables.html ---
    {
        "target": "10b-regression-lineaire-simple.html",
        "anchor_file": "10-etudier-relation-deux-variables.html",
        "block": """
      <a href="{prefix}10b-regression-lineaire-simple.html" class="nav-link">
        <i class="fa-solid fa-chart-line"></i> 10b. Régression linéaire simple <span class="nav-badge" style="background:#2563EB; color:#FFFFFF;">Nouveau</span>
      </a>""",
    },
    # --- Module 12 : après 11-exploiter-resultats-rediger-article.html ---
    {
        "target": "12-puissance-et-taille-echantillon.html",
        "anchor_file": "11-exploiter-resultats-rediger-article.html",
        "block": """
      <a href="{prefix}12-puissance-et-taille-echantillon.html" class="nav-link">
        <i class="fa-solid fa-users-gear"></i> 12. Puissance &amp; taille d'échantillon <span class="nav-badge" style="background:#2563EB; color:#FFFFFF;">Nouveau</span>
      </a>""",
    },
    # --- Module 13 : après 11-exploiter-resultats-rediger-article.html (inséré après le 12) ---
    {
        "target": "13-limites-et-biais-statistiques.html",
        "anchor_file": "12-puissance-et-taille-echantillon.html",
        "block": """
      <a href="{prefix}13-limites-et-biais-statistiques.html" class="nav-link">
        <i class="fa-solid fa-shield-halved"></i> 13. Limites et biais <span class="nav-badge" style="background:#2563EB; color:#FFFFFF;">Nouveau</span>
      </a>""",
    },
    # --- Module 14 : Glossaire (inséré après 13) ---
    {
        "target": "14-glossaire.html",
        "anchor_file": "13-limites-et-biais-statistiques.html",
        "block": """
      <a href="{prefix}14-glossaire.html" class="nav-link">
        <i class="fa-solid fa-book"></i> 14. Glossaire des symboles <span class="nav-badge" style="background:#2563EB; color:#FFFFFF;">Nouveau</span>
      </a>""",
    },
]

# ============================================================================
# COULEURS POUR LE TERMINAL
# ============================================================================

class C:
    OK = "\033[92m"
    WARN = "\033[93m"
    FAIL = "\033[91m"
    BOLD = "\033[1m"
    END = "\033[0m"
    BLUE = "\033[94m"
    GREY = "\033[90m"

# ============================================================================
# FONCTIONS UTILITAIRES
# ============================================================================

def find_html_files(root_dir):
    """Trouve tous les fichiers HTML du projet (hors dossiers techniques)."""
    html_files = []
    ignored_dirs = {".git", "node_modules", "katex", "__pycache__", "assets", ".vscode"}
    for dirpath, dirnames, filenames in os.walk(root_dir):
        dirnames[:] = [d for d in dirnames if d not in ignored_dirs]
        for filename in filenames:
            if filename.endswith(".html") and not filename.endswith(".bak.html"):
                html_files.append(Path(dirpath) / filename)
    return sorted(html_files)


def backup_file(filepath):
    """Crée une sauvegarde .bak du fichier (si elle n'existe pas déjà)."""
    backup_path = filepath.with_suffix(filepath.suffix + ".bak")
    if not backup_path.exists():
        shutil.copy2(filepath, backup_path)
        return backup_path
    return None


def detect_prefix(content, filepath):
    """
    Détecte le préfixe utilisé dans le fichier :
    - "chapitres/" si le fichier est à la racine (index.html)
    - "" si le fichier est dans chapitres/
    """
    if 'href="chapitres/' in content or "href='chapitres/" in content:
        return "chapitres/"
    if 'href="../index.html"' in content or "href='../index.html'" in content:
        return ""
    if 'href="index.html"' in content or "href='index.html'" in content:
        return "chapitres/"
    if filepath.parent.name == "chapitres":
        return ""
    return "chapitres/"


def find_anchor_position(content, anchor_filename, prefix):
    """
    Trouve la position juste après la balise </a> du lien correspondant à anchor_filename.
    Gère les classes 'nav-link' ET 'nav-link active'.
    Retourne (position, bool_trouvé).
    """
    # Cas 1 : class="nav-link" (lien classique)
    marker_simple = f'href="{prefix}{anchor_filename}" class="nav-link">'
    idx = content.find(marker_simple)

    # Cas 2 : class="nav-link active" (lien actif)
    if idx == -1:
        marker_active = f'href="{prefix}{anchor_filename}" class="nav-link active">'
        idx = content.find(marker_active)

    # Cas 3 : fallback — cherche juste le href et trouve le </a> qui suit
    if idx == -1:
        marker_href = f'href="{prefix}{anchor_filename}"'
        idx = content.find(marker_href)

    if idx == -1:
        return -1, False

    end_a = content.find("</a>", idx)
    if end_a == -1:
        return -1, False

    return end_a + len("</a>"), True


def link_already_exists(content, href):
    """Vérifie si le lien existe déjà dans le fichier (idempotence)."""
    return f'href="{href}"' in content or f"href='{href}'" in content


def process_file(filepath):
    """Traite un fichier HTML. Retourne un dict avec le statut."""
    result = {
        "file": str(filepath),
        "inserted": [],
        "skipped": [],
        "errors": [],
        "backup": None,
    }

    try:
        with open(filepath, "r", encoding="utf-8") as f:
            content = f.read()
    except UnicodeDecodeError:
        result["errors"].append("Fichier non lisible en UTF-8")
        return result

    if "sidebar-nav" not in content:
        result["skipped"].append("Pas de sidebar détectée")
        return result

    prefix = detect_prefix(content, filepath)

    backup_path = backup_file(filepath)
    if backup_path:
        result["backup"] = str(backup_path)

    modified = False

    for link in NEW_LINKS:
        full_target_href = f"{prefix}{link['target']}"

        # Vérifier si déjà présent
        if link_already_exists(content, full_target_href):
            result["skipped"].append(f"{link['target']} (déjà présent)")
            continue

        # Trouver la position d'insertion
        anchor_filename = link["anchor_file"]
        pos, found = find_anchor_position(content, anchor_filename, prefix)

        if not found:
            result["errors"].append(f"Ancre introuvable pour {link['target']} (cherchait {anchor_filename})")
            continue

        # Insérer le bloc
        block = link["block"].format(prefix=prefix)
        content = content[:pos] + block + content[pos:]
        modified = True
        result["inserted"].append(link["target"])

    if modified:
        try:
            with open(filepath, "w", encoding="utf-8") as f:
                f.write(content)
        except Exception as e:
            result["errors"].append(f"Erreur d'écriture : {e}")

    return result


# ============================================================================
# PROGRAMME PRINCIPAL
# ============================================================================

def main():
    print(f"\n{C.BOLD}{C.BLUE}╔══════════════════════════════════════════════════════════════╗{C.END}")
    print(f"{C.BOLD}{C.BLUE}║  RAMSES 1.0 — Mise à jour de la sidebar (v3)                ║{C.END}")
    print(f"{C.BOLD}{C.BLUE}║  Ajout des modules 02b, 05b, 08b, 09b, 10b, 12, 13          ║{C.END}")
    print(f"{C.BOLD}{C.BLUE}╚══════════════════════════════════════════════════════════════╝{C.END}\n")

    root_dir = Path(__file__).resolve().parent
    print(f"{C.GREY}Dossier analysé : {root_dir}{C.END}\n")

    if not (root_dir / "index.html").exists():
        print(f"{C.FAIL}✘ Erreur : index.html introuvable à la racine.{C.END}")
        print(f"{C.GREY}Assure-toi que ce script est bien placé au même niveau que index.html.{C.END}")
        return 1

    html_files = find_html_files(root_dir)
    print(f"{C.BOLD}{len(html_files)} fichier(s) HTML trouvé(s).{C.END}\n")

    total_inserted = 0
    total_skipped = 0
    total_errors = 0
    files_modified = 0

    print(f"{C.BOLD}Résultats :{C.END}\n")

    for filepath in html_files:
        result = process_file(filepath)
        rel = filepath.relative_to(root_dir)

        if result["errors"]:
            print(f"{C.FAIL}✘ {rel}{C.END}")
            for err in result["errors"]:
                print(f"    {C.FAIL}→ {err}{C.END}")
            total_errors += len(result["errors"])
            continue

        if result["inserted"]:
            files_modified += 1
            print(f"{C.OK}✔ {rel}{C.END} — {len(result['inserted'])} lien(s) ajouté(s)")
            for link in result["inserted"]:
                print(f"    {C.OK}→ {link}{C.END}")
            total_inserted += len(result["inserted"])
        elif result["skipped"]:
            print(f"{C.GREY}○ {rel}{C.END} — {len(result['skipped'])} lien(s) déjà présent(s)")
            total_skipped += len(result["skipped"])
        else:
            print(f"{C.GREY}○ {rel}{C.END} — aucun changement")

    print(f"\n{C.BOLD}{'─' * 64}{C.END}")
    print(f"{C.BOLD}Résumé :{C.END}")
    print(f"  Fichiers modifiés   : {C.OK}{files_modified}{C.END}")
    print(f"  Liens ajoutés       : {C.OK}{total_inserted}{C.END}")
    print(f"  Liens déjà présents : {C.GREY}{total_skipped}{C.END}")

    if total_errors:
        print(f"  {C.FAIL}Erreurs             : {total_errors}{C.END}")
    else:
        print(f"  Erreurs             : {C.OK}0{C.END}")

    print(f"{C.BOLD}{'─' * 64}{C.END}\n")

    if total_inserted > 0:
        print(f"{C.OK}{C.BOLD}🎉 Terminé ! {files_modified} fichier(s) mis à jour.{C.END}")
        print(f"{C.GREY}Les sauvegardes .bak ont été créées pour chaque fichier modifié.{C.END}\n")
    else:
        print(f"{C.OK}✅ Tout est déjà en ordre !{C.END}\n")

    return 0 if total_errors == 0 else 1


if __name__ == "__main__":
    exit(main())