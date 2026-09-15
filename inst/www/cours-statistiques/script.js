/**
 * RAMSES 1.0 — COURS DE MÉTHODES STATISTIQUES
 * Script interactif v2 : recherche rapide, navigation, aide à la décision, quiz
 *
 * Architecture :
 *   - initSearch()        → recherche dans la sidebar et les cartes
 *   - initBackToRamses()  → bouton "Retour à Ramses"
 *   - initCopyCode()      → double-clic pour copier un bloc de code
 *   - initChartToggles()  → bascule histogramme/points, effectifs/pourcentages
 *   - bindQuiz()          → moteur générique de quiz (utilisé partout)
 *   - initAllQuizzes()    → instancie tous les quiz du cours
 *   - renderAllMath()     → rendu KaTeX global
 */

'use strict';

document.addEventListener('DOMContentLoaded', () => {
  initSearch();
  initBackToRamses();
  initCopyCode();
  initChartToggles();
  initAllQuizzes();
  renderAllMath();
});

/* ==========================================================================
   1. RECHERCHE DANS LA SIDEBAR ET LES CARTES
   ========================================================================== */
function initSearch() {
  const searchInput = document.getElementById('sidebar-search-input');
  if (!searchInput) return;

  const navLinks = document.querySelectorAll('.sidebar-nav .nav-link');
  const navGroups = document.querySelectorAll('.sidebar-nav .nav-group-title');
  const cards = document.querySelectorAll('.chapter-card');

  searchInput.addEventListener('input', (e) => {
    const q = e.target.value.toLowerCase().trim();

    // Filtrer les liens de navigation
    navLinks.forEach((link) => {
      const match = link.textContent.toLowerCase().includes(q);
      link.style.display = match ? 'flex' : 'none';
    });

    // Masquer les titres de groupe qui n'ont plus aucun lien visible
    navGroups.forEach((group) => {
      let next = group.nextElementSibling;
      let hasVisible = false;
      while (next && !next.classList.contains('nav-group-title')) {
        if (next.classList.contains('nav-link') && next.style.display !== 'none') {
          hasVisible = true;
          break;
        }
        next = next.nextElementSibling;
      }
      group.style.display = hasVisible || q === '' ? '' : 'none';
    });

    // Filtrer les cartes de chapitres
    cards.forEach((card) => {
      const title = card.querySelector('.card-title')?.textContent.toLowerCase() || '';
      const desc = card.querySelector('.card-desc')?.textContent.toLowerCase() || '';
      const match = title.includes(q) || desc.includes(q);
      card.style.display = match ? 'flex' : 'none';
    });
  });
}

/* ==========================================================================
   2. BOUTON "RETOUR À RAMSES"
   ========================================================================== */
function initBackToRamses() {
  const backBtns = document.querySelectorAll('.btn-back-ramses, .btn-return-ramses');
  if (!backBtns.length) return;

  backBtns.forEach((btn) => {
    btn.addEventListener('click', (e) => {
      // Cas 1 : intégré dans une iframe Ramses
      if (window.parent && window.parent !== window) {
        try {
          const parentBackBtn =
            window.parent.document.getElementById('btn_cours_back') ||
            window.parent.document.getElementById('btn_about_back');
          if (parentBackBtn) {
            e.preventDefault();
            parentBackBtn.click();
            return;
          }
        } catch (err) {
          // Cross-origin : on ignore silencieusement
        }
      }

      // Cas 2 : si un href est défini, on laisse le navigateur suivre le lien
      const href = btn.getAttribute('href');
      if (href && href !== '#') return;

      // Cas 3 : sinon, retour dans l'historique
      e.preventDefault();
      if (window.history.length > 1) {
        window.history.back();
      }
    });
  });
}

/* ==========================================================================
   3. COPIE DE CODE AU DOUBLE-CLIC
   ========================================================================== */
function initCopyCode() {
  document.querySelectorAll('pre, .formula-box').forEach((block) => {
    block.style.position = 'relative';

    block.addEventListener('dblclick', () => {
      const text = block.innerText;
      if (!navigator.clipboard) return;

      navigator.clipboard.writeText(text).then(() => {
        const note = document.createElement('span');
        note.textContent = 'Copié !';
        note.setAttribute('role', 'status');
        note.style.cssText = `
          position: absolute; top: 6px; right: 8px;
          background: #1F2937; color: #FFFFFF;
          font-size: 11px; padding: 2px 6px; border-radius: 4px;
          font-family: var(--font-sans);
        `;
        block.appendChild(note);
        setTimeout(() => note.remove(), 1500);
      }).catch(() => {
        // Silencieux si la copie échoue
      });
    });
  });
}

/* ==========================================================================
   4. BASCULES DE GRAPHIQUES (histogramme/points, effectifs/pourcentages)
   ========================================================================== */
function initChartToggles() {
  // Module 1 : histogramme vs points
  bindTogglePair({
    btnA: 'btn-show-histogram',
    btnB: 'btn-show-points',
    viewA: 'chart-view-histogram',
    viewB: 'chart-view-points',
  });

  // Module 3 : effectifs vs pourcentages
  bindTogglePair({
    btnA: 'btn-show-counts',
    btnB: 'btn-show-pct',
    viewA: 'chart-view-counts',
    viewB: 'chart-view-pct',
  });
}

function bindTogglePair({ btnA, btnB, viewA, viewB }) {
  const a = document.getElementById(btnA);
  const b = document.getElementById(btnB);
  const vA = document.getElementById(viewA);
  const vB = document.getElementById(viewB);

  if (!a || !b || !vA || !vB) return;

  a.addEventListener('click', () => {
    a.classList.add('active');
    b.classList.remove('active');
    vA.style.display = 'block';
    vB.style.display = 'none';
  });

  b.addEventListener('click', () => {
    b.classList.add('active');
    a.classList.remove('active');
    vA.style.display = 'none';
    vB.style.display = 'block';
  });
}

/* ==========================================================================
   5. MOTEUR GÉNÉRIQUE DE QUIZ
   ========================================================================== */
/**
 * Branche un quiz sur un ensemble de boutons.
 * @param {Object} config
 * @param {string} config.selector      - Sélecteur CSS des boutons du quiz
 * @param {string} config.feedbackId    - ID de l'élément de feedback
 * @param {Function} config.isCorrect   - (btn) => boolean : détermine si la réponse est correcte
 * @param {Function} config.render      - (isCorrect) => { bg, border, color, html }
 */
function bindQuiz({ selector, feedbackId, isCorrect, render }) {
  const btns = document.querySelectorAll(selector);
  const feedback = document.getElementById(feedbackId);
  if (!btns.length || !feedback) return;

  btns.forEach((btn) => {
    btn.addEventListener('click', () => {
      const ok = isCorrect(btn);

      // Marquer tous les boutons
      btns.forEach((b) => {
        b.classList.remove('correct', 'wrong');
        b.disabled = true;
        if (isCorrect(b)) b.classList.add('correct');
      });
      if (!ok) btn.classList.add('wrong');

      // Afficher le feedback
      const { bg, border, color, html } = render(ok);
      feedback.style.display = 'block';
      feedback.style.background = bg;
      feedback.style.border = border;
      feedback.style.color = color;
      feedback.innerHTML = html;
      feedback.setAttribute('role', 'alert');

      // Rendu KaTeX dans le feedback si nécessaire
      renderMathIn(feedback);
    });
  });
}

/** Utilitaire : rendu KaTeX local sur un élément */
function renderMathIn(el) {
  if (typeof renderMathInElement === 'function') {
    try {
      renderMathInElement(el, {
        delimiters: [
          { left: '$$', right: '$$', display: true },
          { left: '$', right: '$', display: false },
        ],
        throwOnError: false,
      });
    } catch (err) {
      console.warn('KaTeX render warning:', err);
    }
  }
}

/** Utilitaire : styles de feedback standardisés */
const FB = {
  ok: { bg: '#ECFDF5', border: '1px solid #A7F3D0', color: '#065F46' },
  ko: { bg: '#FEF2F2', border: '1px solid #FECACA', color: '#991B1B' },
  neutral: { bg: '#F9FAFB', border: '1px solid #E5E7EB', color: '#374151' },
};

/** Utilitaire : réponse correcte par data-answer="correct" */
const isCorrectByAnswer = (btn) => btn.getAttribute('data-answer') === 'correct';

/* ==========================================================================
   6. INSTANCIATION DE TOUS LES QUIZ DU COURS
   ========================================================================== */
function initAllQuizzes() {
  // --- Module 3 : Quiz pourcentage ---
  bindQuiz({
    selector: '.quiz-option-btn',
    feedbackId: 'quiz-feedback-box',
    isCorrect: (b) => b.getAttribute('data-correct') === 'true',
    render: (ok) => ok
      ? { ...FB.ok, html: '<strong><i class="fa-solid fa-circle-check"></i> Bravo, c\'est la bonne réponse !</strong><br>12 producteurs sur 20 représentent : <code>(12 / 20) × 100 = 60 %</code>.' }
      : { ...FB.ko, html: '<strong><i class="fa-solid fa-circle-xmark"></i> Pas tout à fait !</strong><br>La bonne réponse est <strong>60 %</strong>.<br>Calcul : <code>(12 / 20) × 100 = 0,6 × 100 = 60 %</code>.' },
  });

  // --- Module 4 : Quiz moyenne ---
  bindQuiz({
    selector: '.quiz-module4-btn',
    feedbackId: 'quiz-module4-feedback',
    isCorrect: (b) => b.getAttribute('data-answer') === 'above',
    render: (ok) => ok
      ? { ...FB.ok, html: '<strong><i class="fa-solid fa-circle-check"></i> Exactement, c\'est au-dessus !</strong><br>Avec 3,2 t/ha, cette parcelle dépasse la moyenne générale de 2,72 t/ha (écart de <code>+0,48 t/ha</code>).' }
      : { ...FB.ko, html: '<strong><i class="fa-solid fa-circle-xmark"></i> Pas tout à fait !</strong><br>La bonne réponse est <strong>Supérieur à la moyenne</strong> car <code>3,2 &gt; 2,72</code>.' },
  });

  // --- Module 4 : Quiz écart-type ---
  bindQuiz({
    selector: '.quiz-module4-q2-btn',
    feedbackId: 'quiz-module4-q2-feedback',
    isCorrect: isCorrectByAnswer,
    render: (ok) => ok
      ? { ...FB.ok, html: '<strong><i class="fa-solid fa-circle-check"></i> Bravo !</strong><br>Un écart-type faible indique des rendements homogènes et resserrés autour de la moyenne.' }
      : { ...FB.ko, html: '<strong><i class="fa-solid fa-circle-xmark"></i> Pas tout à fait !</strong><br>Un écart-type <em>faible</em> signifie que les valeurs sont <strong>peu dispersées</strong> autour de la moyenne.' },
  });

  // --- Module 5 : Quiz 3 situations graphiques ---
  const chartExplanations = {
    1: {
      ok: '<strong><i class="fa-solid fa-circle-check"></i> Exactement, le diagramme en barres !</strong><br>Le village est une variable <em>qualitative</em>. <code>barplot()</code> permet de comparer immédiatement les effectifs.',
      ko: '<strong><i class="fa-solid fa-circle-xmark"></i> Pas tout à fait !</strong><br>La bonne réponse est le <strong>Diagramme en barres</strong> car <code>village</code> est une variable qualitative.',
    },
    2: {
      ok: '<strong><i class="fa-solid fa-circle-check"></i> Bravo, l\'histogramme !</strong><br>L\'âge est une variable <em>quantitative continue</em>. <code>hist()</code> découpe les âges en classes régulières.',
      ko: '<strong><i class="fa-solid fa-circle-xmark"></i> Pas tout à fait !</strong><br>La bonne réponse est l\'<strong>Histogramme</strong> car l\'âge est une variable quantitative continue.',
    },
    3: {
      ok: '<strong><i class="fa-solid fa-circle-check"></i> Parfait, le nuage de points !</strong><br>Superficie et rendement sont deux variables <em>quantitatives</em> : <code>plot()</code> permet de déceler une tendance.',
      ko: '<strong><i class="fa-solid fa-circle-xmark"></i> Pas tout à fait !</strong><br>La bonne réponse est le <strong>Nuage de points</strong> car on observe la relation entre deux variables quantitatives.',
    },
  };

  [1, 2, 3].forEach((id) => {
    bindQuiz({
      selector: `.quiz-challenge-btn[data-challenge="${id}"]`,
      feedbackId: `quiz-challenge-${id}-feedback`,
      isCorrect: isCorrectByAnswer,
      render: (ok) => ok
        ? { ...FB.ok, html: chartExplanations[id].ok }
        : { ...FB.ko, html: chartExplanations[id].ko },
    });
  });

  // --- Module 6 : Population vs Échantillon ---
  bindQuiz({
    selector: '.quiz-module6-btn',
    feedbackId: 'quiz-module6-feedback',
    isCorrect: isCorrectByAnswer,
    render: (ok) => ok
      ? { ...FB.ok, html: '<strong><i class="fa-solid fa-circle-check"></i> C\'est la bonne réponse !</strong><br>Étudier toute la population est presque toujours impossible (temps, coût, logistique). On sélectionne donc un échantillon représentatif.' }
      : { ...FB.ko, html: '<strong><i class="fa-solid fa-circle-xmark"></i> Pas tout à fait !</strong><br>La bonne réponse est : « Parce qu\'étudier toute la population peut être difficile, coûteux ou impossible. » Un échantillon n\'élimine pas l\'incertitude.' },
  });

  // --- Module 7 : Exercice 1 (p = 0,03) ---
  bindQuiz({
    selector: '.quiz-module7-ex1-btn',
    feedbackId: 'quiz-module7-ex1-feedback',
    isCorrect: isCorrectByAnswer,
    render: (ok) => ok
      ? { ...FB.ok, html: '<strong><i class="fa-solid fa-circle-check"></i> Exactement, on rejette H₀ !</strong><br>Puisque <code>0,03 &lt; 0,05</code> ($p < \\alpha$), les données sont peu probables sous $H_0$. On rejette donc $H_0$.' }
      : { ...FB.ko, html: '<strong><i class="fa-solid fa-circle-xmark"></i> Pas tout à fait !</strong><br>La bonne réponse est <strong>« On rejette H₀ »</strong> car $0{,}03 < 0{,}05$.' },
  });

  // --- Module 7 : Exercice 2 (p = 0,42) ---
  bindQuiz({
    selector: '.quiz-module7-ex2-btn',
    feedbackId: 'quiz-module7-ex2-feedback',
    isCorrect: isCorrectByAnswer,
    render: (ok) => ok
      ? { ...FB.ok, html: '<strong><i class="fa-solid fa-circle-check"></i> Parfait, on ne rejette pas H₀ !</strong><br>Puisque <code>0,42 ≥ 0,05</code>, les données ne fournissent pas de preuves suffisantes. Attention : cela ne prouve pas que les variétés sont identiques.' }
      : { ...FB.ko, html: '<strong><i class="fa-solid fa-circle-xmark"></i> Pas tout à fait !</strong><br>La bonne réponse est <strong>« On ne rejette pas H₀ »</strong> car $0{,}42 \\ge 0{,}05$.' },
  });

  // --- Module 8 : 3 défis de classification ---
  const mod8 = {
    a: {
      ok: '<strong><i class="fa-solid fa-circle-check"></i> C\'est la bonne réponse !</strong><br>Les <em>mêmes producteurs</em> interrogés à deux moments : les observations sont <strong>appariées</strong>.',
      ko: '<strong><i class="fa-solid fa-circle-xmark"></i> Pas tout à fait !</strong><br>La bonne réponse est <strong>« Deux groupes appariés »</strong> car les mesures sont répétées sur les mêmes producteurs.',
    },
    b: {
      ok: '<strong><i class="fa-solid fa-circle-check"></i> C\'est la bonne réponse !</strong><br>Les producteurs de chaque zone sont totalement distincts : les échantillons sont <strong>indépendants</strong>.',
      ko: '<strong><i class="fa-solid fa-circle-xmark"></i> Pas tout à fait !</strong><br>La bonne réponse est <strong>« Deux groupes indépendants »</strong> car il s\'agit d\'individus différents.',
    },
    c: {
      ok: '<strong><i class="fa-solid fa-circle-check"></i> Exactement !</strong><br>Un seul groupe confronté à une norme fixe : <strong>un seul groupe</strong>.',
      ko: '<strong><i class="fa-solid fa-circle-xmark"></i> Pas tout à fait !</strong><br>La bonne réponse est <strong>« Un seul groupe »</strong> car la référence est un chiffre fixe.',
    },
  };

  ['a', 'b', 'c'].forEach((id) => {
    bindQuiz({
      selector: `.quiz-module8-${id}-btn`,
      feedbackId: `quiz-module8-${id}-feedback`,
      isCorrect: isCorrectByAnswer,
      render: (ok) => ok
        ? { ...FB.ok, html: mod8[id].ok }
        : { ...FB.ko, html: mod8[id].ko },
    });
  });

  // --- Module 9 : 2 questions ---
  bindQuiz({
    selector: '.quiz-module9-q1-btn',
    feedbackId: 'quiz-module9-q1-feedback',
    isCorrect: isCorrectByAnswer,
    render: (ok) => ok
      ? { ...FB.ok, html: '<strong><i class="fa-solid fa-circle-check"></i> C\'est la bonne réponse !</strong><br>Pour comparer 3 groupes ou plus, on utilise une <strong>ANOVA à un facteur</strong> pour tester globalement l\'existence d\'une différence sans gonfler le risque d\'erreur.' }
      : { ...FB.ko, html: '<strong><i class="fa-solid fa-circle-xmark"></i> Pas tout à fait !</strong><br>La bonne méthode pour débuter est <strong>« Une ANOVA à un facteur »</strong>. Multiplier les tests t gonflerait le risque d\'erreur.' },
  });

  bindQuiz({
    selector: '.quiz-module9-q2-btn',
    feedbackId: 'quiz-module9-q2-feedback',
    isCorrect: isCorrectByAnswer,
    render: (ok) => ok
      ? { ...FB.ok, html: '<strong><i class="fa-solid fa-circle-check"></i> Exactement !</strong><br>Une ANOVA significative ($p < 0{,}05$) indique qu\'<strong>au moins deux moyennes diffèrent</strong>. Des tests post-hoc permettront de préciser lesquelles.' }
      : { ...FB.ko, html: '<strong><i class="fa-solid fa-circle-xmark"></i> Attention au piège classique !</strong><br>Une ANOVA significative permet seulement d\'affirmer qu\'<strong>au moins deux groupes diffèrent</strong>, pas que tous diffèrent entre eux.' },
  });

  // --- Module 10 : 4 questions ---
  const mod10 = {
    q1: {
      ok: '<strong><i class="fa-solid fa-circle-check"></i> Exactement !</strong><br>Âge et rendement sont deux variables <strong>quantitatives</strong> : la <strong>corrélation</strong> est adaptée.',
      ko: '<strong><i class="fa-solid fa-circle-xmark"></i> Pas tout à fait !</strong><br>Ce sont deux variables quantitatives : la méthode appropriée est la <strong>corrélation</strong>.',
    },
    q2: {
      ok: '<strong><i class="fa-solid fa-circle-check"></i> Parfait !</strong><br>Village et engrais sont deux variables <strong>qualitatives</strong> : le <strong>test du χ² d\'indépendance</strong> est adapté.',
      ko: '<strong><i class="fa-solid fa-circle-xmark"></i> Attention !</strong><br>Ce sont deux variables qualitatives : on utilise un <strong>test du χ² d\'indépendance</strong>.',
    },
    q3: {
      ok: '<strong><i class="fa-solid fa-circle-check"></i> Très bien !</strong><br>Superficie et rendement sont deux <strong>quantitatives</strong> continues : la <strong>corrélation</strong> mesure leur relation linéaire.',
      ko: '<strong><i class="fa-solid fa-circle-xmark"></i> Pas tout à fait !</strong><br>Deux variables quantitatives se mettent en relation avec une <strong>corrélation</strong>.',
    },
    q4: {
      ok: '<strong><i class="fa-solid fa-circle-check"></i> Bravo !</strong><br>Sexe (H/F) et équipement (Oui/Non) sont deux variables <strong>qualitatives</strong> : le <strong>test du χ² d\'indépendance</strong> est adapté.',
      ko: '<strong><i class="fa-solid fa-circle-xmark"></i> Erreur de méthode !</strong><br>Ce sont deux variables qualitatives catégorielles : l\'analyse appropriée est le <strong>test du χ² d\'indépendance</strong>.',
    },
  };

  ['q1', 'q2', 'q3', 'q4'].forEach((id) => {
    bindQuiz({
      selector: `.quiz-module10-${id}-btn`,
      feedbackId: `quiz-module10-${id}-feedback`,
      isCorrect: isCorrectByAnswer,
      render: (ok) => ok
        ? { ...FB.ok, html: mod10[id].ok }
        : { ...FB.ko, html: mod10[id].ko },
    });
  });
  // --- Module 02b : Quiz nettoyage de données ---
  bindQuiz({
    selector: '.quiz-module02b-btn',
    feedbackId: 'quiz-module02b-feedback',
    isCorrect: isCorrectByAnswer,
    render: (ok) => ok
      ? { ...FB.ok, html: '<strong><i class="fa-solid fa-circle-check"></i> Excellente réponse !</strong><br>Avant de corriger ou supprimer, il faut <strong>vérifier la donnée à la source</strong>. Une valeur aberrante est souvent une faute de saisie (32,0 au lieu de 3,0) — mais elle peut aussi être réelle. Ne jamais modifier une donnée sans certitude.' }
      : { ...FB.ko, html: '<strong><i class="fa-solid fa-circle-xmark"></i> Attention au réflexe !</strong><br>Le premier réflexe est de <strong>vérifier la source</strong>. On ne supprime jamais une donnée suspecte sans comprendre son origine.' },
  });

  // --- Module 05b : Quiz intervalle de confiance ---
  bindQuiz({
    selector: '.quiz-module05b-btn',
    feedbackId: 'quiz-module05b-feedback',
    isCorrect: isCorrectByAnswer,
    render: (ok) => ok
      ? { ...FB.ok, html: '<strong><i class="fa-solid fa-circle-check"></i> Très bien !</strong><br>Comme 3,0 est <strong>en dehors</strong> de l\'intervalle [3,10 ; 4,20], on peut conclure à une différence statistiquement significative avec la référence. C\'est équivalent à un test t bilatéral significatif à 5 %.' }
      : { ...FB.ko, html: '<strong><i class="fa-solid fa-circle-xmark"></i> Pas tout à fait !</strong><br>La bonne réponse est : la valeur 3,0 est <strong>hors IC</strong>, donc on rejette l\'hypothèse d\'égalité à la référence.' },
  });

  // --- Module 08b : Quiz conditions d'un test ---
  bindQuiz({
    selector: '.quiz-module08b-btn',
    feedbackId: 'quiz-module08b-feedback',
    isCorrect: isCorrectByAnswer,
    render: (ok) => ok
      ? { ...FB.ok, html: '<strong><i class="fa-solid fa-circle-check"></i> Exactement !</strong><br>Un <em>p</em> &lt; 0,05 au test de Shapiro-Wilk signifie que la normalité est <strong>rejetée</strong>. Il faut alors envisager une alternative non paramétrique comme le test de <strong>Mann-Whitney</strong> (ou Wilcoxon selon la situation).' }
      : { ...FB.ko, html: '<strong><i class="fa-solid fa-circle-xmark"></i> Attention au piège !</strong><br>Le test de Shapiro-Wilk teste la <strong>normalité</strong>, pas une différence entre groupes. Un p &lt; 0,05 signifie : <em>la normalité est rejetée</em>.' },
  });

  // --- Module 09b : Quiz ANOVA 2 facteurs ---
  bindQuiz({
    selector: '.quiz-module09b-btn',
    feedbackId: 'quiz-module09b-feedback',
    isCorrect: isCorrectByAnswer,
    render: (ok) => ok
      ? { ...FB.ok, html: '<strong><i class="fa-solid fa-circle-check"></i> Exactement !</strong><br>Un p = 0,32 (> 0,05) signifie que l\'<strong>interaction n\'est pas significative</strong>. On peut alors interpréter les effets principaux de chaque facteur séparément, ce qui simplifie la présentation.' }
      : { ...FB.ko, html: '<strong><i class="fa-solid fa-circle-xmark"></i> Attention au piège !</strong><br>Quand l\'interaction n\'est pas significative, on peut interpréter les <strong>effets principaux séparément</strong>. Ce n\'est pas un échec du modèle, au contraire : cela simplifie l\'interprétation.' },
  });

  // --- Module 10b : Quiz régression linéaire ---
  bindQuiz({
    selector: '.quiz-module10b-btn',
    feedbackId: 'quiz-module10b-feedback',
    isCorrect: isCorrectByAnswer,
    render: (ok) => ok
      ? { ...FB.ok, html: '<strong><i class="fa-solid fa-circle-check"></i> Très bien !</strong><br>Un p = 0,42 (> 0,05) signifie qu\'on <strong>ne peut pas rejeter H₀</strong> (pente = 0). Le modèle n\'apporte pas de preuve statistique d\'une relation linéaire, même si la pente estimée est positive par hasard.' }
      : { ...FB.ko, html: '<strong><i class="fa-solid fa-circle-xmark"></i> Piège classique !</strong><br>La valeur de la pente seule ne suffit pas. C\'est la <strong>p-value</strong> qui détermine si la pente est significativement différente de 0. Ici, p = 0,42 → pas de relation significative.' },
  });

  // --- Module 12 : Quiz puissance ---
  bindQuiz({
    selector: '.quiz-module12-btn',
    feedbackId: 'quiz-module12-feedback',
    isCorrect: isCorrectByAnswer,
    render: (ok) => ok
      ? { ...FB.ok, html: '<strong><i class="fa-solid fa-circle-check"></i> Exactement !</strong><br>Avec seulement 5 parcelles par groupe, la <strong>puissance est très faible</strong>. Un p > 0,05 ne signifie pas « pas d\'effet » : cela peut simplement dire que l\'étude n\'a pas la capacité de le détecter. C\'est la notion d\'<strong>erreur de type II</strong>.' }
      : { ...FB.ko, html: '<strong><i class="fa-solid fa-circle-xmark"></i> Attention au piège !</strong><br>Un résultat non significatif sur un <strong>petit échantillon</strong> ne prouve pas l\'absence d\'effet. Il faut vérifier la puissance de l\'étude.' },
  });

  // --- Module 13 : Quiz biais ---
  bindQuiz({
    selector: '.quiz-module13-btn',
    feedbackId: 'quiz-module13-feedback',
    isCorrect: isCorrectByAnswer,
    render: (ok) => ok
      ? { ...FB.ok, html: '<strong><i class="fa-solid fa-circle-check"></i> Exactement !</strong><br>C\'est un cas classique de <strong>p-hacking</strong> : en multipliant les tests, on augmente mécaniquement le risque de trouver un résultat significatif par hasard (inflation du risque d\'erreur de type I). Les bonnes pratiques : <strong>corriger pour multiplicité</strong> (Bonferroni, FDR) ou <strong>pré-enregistrer</strong> une seule hypothèse.' }
      : { ...FB.ko, html: '<strong><i class="fa-solid fa-circle-xmark"></i> Attention !</strong><br>Multiplier les tests puis ne rapporter que le significatif est une forme de <strong>p-hacking</strong>. Le seuil de 0,05 n\'est valable que pour UN test planifié à l\'avance.' },
  });
}

/* ==========================================================================
   7. RENDU KATEX GLOBAL
   ========================================================================== */
function renderAllMath() {
  if (typeof renderMathInElement === 'function') {
    try {
      renderMathInElement(document.body, {
        delimiters: [
          { left: '$$', right: '$$', display: true },
          { left: '\\[', right: '\\]', display: true },
          { left: '$', right: '$', display: false },
          { left: '\\(', right: '\\)', display: false },
        ],
        ignoredTags: ['script', 'noscript', 'style', 'textarea', 'pre', 'code', 'option'],
        throwOnError: false,
      });
    } catch (err) {
      console.warn('KaTeX render warning:', err);
    }
  }
}

// Exposer la fonction pour un appel manuel si nécessaire
window.renderAllMath = renderAllMath;
window.addEventListener('load', renderAllMath);