# ============================================================================
# Génération du jeu de données factoriel : 40 parcelles (2 variétés × 2 fert.)
# Utilisé dans les modules 09b et 12
# ============================================================================

set.seed(42)  # Reproductibilité

# Plan factoriel complet 2 × 2, 10 répétitions par combinaison
donnees <- expand.grid(
  variete       = c("A", "B"),
  fertilisation = c("Sans", "Avec"),
  rep           = 1:10
)

# Moyennes par combinaison (définies dans le module 09b)
moyennes <- c(
  "A.Sans" = 2.55,
  "A.Avec" = 3.10,
  "B.Sans" = 2.70,
  "B.Avec" = 3.55
)

# Écart-type commun (constante pour tous les groupes)
sd_commun <- 0.22

# Générer les rendements
donnees$rendement <- round(
  mapply(
    function(v, f) rnorm(1, mean = moyennes[paste(v, f, sep = ".")], sd = sd_commun),
    donnees$variete, donnees$fertilisation
  ),
  2
)

# Ordonner par variété, fertilisation, répétition
donnees <- donnees[order(donnees$variete, donnees$fertilisation, donnees$rep), ]

# Créer un identifiant de parcelle
donnees$parcelle <- sprintf("P%03d", 1:nrow(donnees))

# Réorganiser les colonnes
donnees <- donnees[, c("parcelle", "variete", "fertilisation", "rendement")]

# Écrire en CSV
write.csv(donnees, "../40-parcelles-factoriel.csv", row.names = FALSE, fileEncoding = "UTF-8")

cat("✅ Fichier '40-parcelles-factoriel.csv' généré.\n")
cat("Moyennes par groupe :\n")
print(tapply(donnees$rendement, list(donnees$variete, donnees$fertilisation), mean))