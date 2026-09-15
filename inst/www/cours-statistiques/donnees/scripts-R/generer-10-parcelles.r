# ============================================================================
# Génération du jeu de données principal : 10 parcelles de maïs
# Utilisé dans les modules 01 à 11
# ============================================================================

donnees <- data.frame(
  parcelle   = c("P01","P02","P03","P04","P05","P06","P07","P08","P09","P10"),
  village    = c("Bonabéri","Bonabéri","Bonabéri","Bonabéri","Bonabéri",
                 "Bépanda","Bépanda","Bépanda","Bépanda","Bépanda"),
  superficie = c(1.2, 0.8, 1.5, 1.0, 2.0, 0.7, 1.1, 1.4, 0.9, 1.8),
  rendement  = c(2.8, 2.4, 3.1, 2.7, 3.4, 2.1, 2.5, 2.9, 2.3, 3.0)
)

# Écrire en CSV (UTF-8, sans row.names)
write.csv(donnees, "../10-parcelles-mais.csv", row.names = FALSE, fileEncoding = "UTF-8")

cat("✅ Fichier '10-parcelles-mais.csv' généré.\n")