# =========================================================
# Oscarovi: Predviđanje pobjednika (1928–2020)
# =========================================================
# Pitanje: Možemo li pomoću dostupnih značajki pouzdano 
#          razlikovati pobjednike od ostalih?
# =========================================================

# --- 0. Učitavanje paketa i podataka ---
library(tidyr)
library(dplyr)
library(ggplot2)

data <- read.csv("oscars_dataset.csv", sep=";")

# ---------------------------------------------------------
# 1. Priprema podataka za logističku regresiju
# ---------------------------------------------------------

# Kreiranje binarnog ishoda (Winner = 1, Nominee = 0)
data$IsWinner <- ifelse(data$Award == "Winner", 1, 0)

# Čišćenje i pretvaranje varijabli
data$Rating <- as.numeric(data$IMDB.Rating)
data$Votes <- as.numeric(gsub(",", "", data$IMDB.Votes))
data$Runtime <- as.numeric(data$Movie.Time)
data$Year <- as.numeric(data$Year.of.Release)

# Grupiranje rijetkih žanrova
common_genres <- c("Action", "Adventure", "Biography", "Comedy", "Crime", "Drama")
data$GenreGroup <- ifelse(data$Main.Genre %in% common_genres,
                          data$Main.Genre,
                          "Other")
data$GenreGroup <- factor(data$GenreGroup, 
                          levels = c("Drama", "Action", "Adventure", 
                                     "Biography", "Comedy", "Crime", "Other"))

# Uklanjanje redaka s nedostajućim podacima
model_data <- data %>%
  dplyr::select(IsWinner, Rating, Votes, Runtime, Year, GenreGroup) %>%
  na.omit()

cat("Broj filmova u analizi:", nrow(model_data), "\n")
cat("Broj pobjednika:", sum(model_data$IsWinner), "\n")
cat("Broj nominiranih (bez pobjede):", sum(model_data$IsWinner == 0), "\n")

# ---------------------------------------------------------
# 2. Eksplorativna analiza - usporedba pobjednika i nominiranih
# ---------------------------------------------------------

# 2.1 Usporedba IMDB ocjena (t-test iz Poglavlja 5)
cat("\n=== IMDB Rating ===\n")
cat("Pobjednici - Mean:", mean(model_data$Rating[model_data$IsWinner == 1]), "\n")
cat("Nominirani - Mean:", mean(model_data$Rating[model_data$IsWinner == 0]), "\n")

t_rating <- t.test(Rating ~ IsWinner, data = model_data)
print(t_rating)

# Boxplot - Rating
ggplot(model_data, aes(x = factor(IsWinner, labels = c("Nominee", "Winner")), 
                       y = Rating, fill = factor(IsWinner))) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = c("0" = "#a6cee3", "1" = "#1f78b4")) +
  theme_minimal(base_size = 14) +
  labs(title = "IMDB ocjena: Pobjednici vs Nominirani",
       x = "", y = "IMDB Rating") +
  theme(legend.position = "none")

# 2.2 Usporedba broja glasova (Wilcoxonov test - Poglavlje 12.3)
cat("\n=== IMDB Votes (neparametarski test) ===\n")
cat("Pobjednici - Median votes:", median(model_data$Votes[model_data$IsWinner == 1]), "\n")
cat("Nominirani - Median votes:", median(model_data$Votes[model_data$IsWinner == 0]), "\n")

wilcox_votes <- wilcox.test(Votes ~ IsWinner, data = model_data)
print(wilcox_votes)

# Boxplot - Votes (log scale)
ggplot(model_data, aes(x = factor(IsWinner, labels = c("Nominee", "Winner")), 
                       y = Votes, fill = factor(IsWinner))) +
  geom_boxplot(alpha = 0.7) +
  scale_y_log10(labels = scales::comma) +
  scale_fill_manual(values = c("0" = "#a6cee3", "1" = "#1f78b4")) +
  theme_minimal(base_size = 14) +
  labs(title = "Broj glasova (log skala): Pobjednici vs Nominirani",
       x = "", y = "IMDB Votes (log10)") +
  theme(legend.position = "none")

# 2.3 Usporedba duljine filma
cat("\n=== Runtime ===\n")
cat("Pobjednici - Mean runtime:", mean(model_data$Runtime[model_data$IsWinner == 1]), "\n")
cat("Nominirani - Mean runtime:", mean(model_data$Runtime[model_data$IsWinner == 0]), "\n")

t_runtime <- t.test(Runtime ~ IsWinner, data = model_data)
print(t_runtime)

# 2.4 Distribucija po žanrovima (Chi-squared test - Poglavlje 6.3)
genre_winner_table <- table(model_data$GenreGroup, model_data$IsWinner)
cat("\n=== Distribucija po žanrovima ===\n")
print(addmargins(genre_winner_table))

# Chi-squared test nezavisnosti
chisq_genre <- chisq.test(genre_winner_table)
print(chisq_genre)

# Vizualizacija žanrova
genre_plot_data <- model_data %>%
  group_by(GenreGroup, IsWinner) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(Outcome = ifelse(IsWinner == 1, "Winner", "Nominee"))

ggplot(genre_plot_data, aes(x = GenreGroup, y = Count, fill = Outcome)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  scale_fill_manual(values = c("Winner" = "#1f78b4", "Nominee" = "#a6cee3")) +
  theme_minimal(base_size = 14) +
  labs(title = "Distribucija pobjednika i nominiranih po žanrovima",
       x = "Žanr", y = "Broj filmova", fill = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ---------------------------------------------------------
# 3. Logistička regresija - MODEL 1 (samo Rating)
# ---------------------------------------------------------

cat("\n\n========================================\n")
cat("MODEL 1: IsWinner ~ Rating\n")
cat("========================================\n")

model1 <- glm(IsWinner ~ Rating, 
              data = model_data, 
              family = binomial(link = "logit"))

summary(model1)

# McFadden's Pseudo R² (str. 86 skripte)
null_model <- glm(IsWinner ~ 1, data = model_data, family = binomial)
ll_null <- logLik(null_model)
ll_model1 <- logLik(model1)
mcfadden_r2_m1 <- 1 - (ll_model1 / ll_null)
cat("\nMcFadden's Pseudo R²:", as.numeric(mcfadden_r2_m1), "\n")

# ---------------------------------------------------------
# 4. Logistička regresija - MODEL 2 (svi prediktori)
# ---------------------------------------------------------

cat("\n\n========================================\n")
cat("MODEL 2: IsWinner ~ Rating + log(Votes) + Runtime + Year + Genre\n")
cat("========================================\n")

model2 <- glm(IsWinner ~ Rating + log10(Votes) + Runtime + Year + GenreGroup, 
              data = model_data, 
              family = binomial(link = "logit"))

summary(model2)

# McFadden's Pseudo R²
ll_model2 <- logLik(model2)
mcfadden_r2_m2 <- 1 - (ll_model2 / ll_null)
cat("\nMcFadden's Pseudo R²:", as.numeric(mcfadden_r2_m2), "\n")

# ---------------------------------------------------------
# 5. Test omjera vjerodostojnosti (LR test)
# ---------------------------------------------------------

cat("\n\n========================================\n")
cat("TEST OMJERA VJERODOSTOJNOSTI (LR test)\n")
cat("========================================\n")

# LR test: Model 2 vs Null model
LR_full <- -2 * (as.numeric(ll_null) - as.numeric(ll_model2))
df_full <- length(coef(model2)) - 1
p_value_full <- pchisq(LR_full, df = df_full, lower.tail = FALSE)

cat("\nH0: Svi regresori su redundantni\n")
cat("H1: Barem jedan regresor nije redundantan\n\n")
cat("LR statistika:", LR_full, "\n")
cat("Stupnjevi slobode:", df_full, "\n")
cat("p-vrijednost:", p_value_full, "\n")

if(p_value_full < 0.05) {
  cat("\nZaključak: Odbacujemo H0 (p < 0.05)\n")
  cat("Model 2 značajno objašnjava podatke.\n")
} else {
  cat("\nZaključak: Ne možemo odbaciti H0 (p ≥ 0.05)\n")
}

# LR test: Model 2 vs Model 1
LR_comparison <- -2 * (as.numeric(ll_model1) - as.numeric(ll_model2))
df_comparison <- length(coef(model2)) - length(coef(model1))
p_value_comparison <- pchisq(LR_comparison, df = df_comparison, lower.tail = FALSE)

cat("\n--- Usporedba Model 2 vs Model 1 ---\n")
cat("LR statistika:", LR_comparison, "\n")
cat("Stupnjevi slobode:", df_comparison, "\n")
cat("p-vrijednost:", p_value_comparison, "\n")

if(p_value_comparison < 0.05) {
  cat("\nZaključak: Model 2 je statistički bolji od Model 1 (p < 0.05)\n")
} else {
  cat("\nZaključak: Model 2 nije značajno bolji od Model 1 (p ≥ 0.05)\n")
}

# ---------------------------------------------------------
# 6. Odds Ratios - Tumačenje koeficijenata
# ---------------------------------------------------------

cat("\n\n========================================\n")
cat("ODDS RATIOS - Tumačenje koeficijenata\n")
cat("========================================\n")

# Izračun Odds Ratios za Model 2
odds_ratios <- exp(coef(model2))
conf_int <- exp(confint(model2))

odds_table <- data.frame(
  Predictor = names(odds_ratios),
  OddsRatio = odds_ratios,
  CI_Lower = conf_int[, 1],
  CI_Upper = conf_int[, 2]
)
rownames(odds_table) <- NULL

cat("\nOdds Ratios s 95% intervalima pouzdanosti:\n")
print(odds_table, digits = 3)

cat("\n--- Interpretacija Odds Ratios ---\n")
cat("OR > 1: Povećanje regresora povećava šanse za pobjedu\n")
cat("OR < 1: Povećanje regresora smanjuje šanse za pobjedu\n")
cat("OR = 1: Regresor nema utjecaj na šanse za pobjedu\n")

# Vizualizacija Odds Ratios (bez intercepta)
odds_plot_data <- odds_table[-1, ]

ggplot(odds_plot_data, aes(x = OddsRatio, y = reorder(Predictor, OddsRatio))) +
  geom_point(size = 4, color = "#1f78b4") +
  geom_errorbarh(aes(xmin = CI_Lower, xmax = CI_Upper), height = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
  theme_minimal(base_size = 14) +
  labs(title = "Odds Ratios za Model 2",
       subtitle = "Crvena linija = neutralan efekt (OR = 1)",
       x = "Odds Ratio (log skala)", 
       y = "Prediktor") +
  scale_x_log10()

# ---------------------------------------------------------
# 7. Predikcija i Confusion Matrix
# ---------------------------------------------------------

cat("\n\n========================================\n")
cat("PREDIKCIJA I CONFUSION MATRIX\n")
cat("========================================\n")

# Predviđene vjerojatnosti
predicted_probs <- predict(model2, type = "response")

# Odabir threshold-a (standardno 0.5, ali možemo prilagoditi)
# Za neuravnotežene podatke (16.5% pobjednika), koristimo omjer
threshold <- sum(model_data$IsWinner) / nrow(model_data)  # ≈ 0.165
cat("\nOdabrani threshold:", round(threshold, 3), "\n")
cat("(Threshold postavljen na proporciju pobjednika u uzorku)\n")

# Klasifikacija
predicted_class <- ifelse(predicted_probs >= threshold, 1, 0)

# Confusion matrix (ručno)
conf_matrix <- table(Predicted = predicted_class, Actual = model_data$IsWinner)
cat("\n=== Confusion Matrix ===\n")
print(conf_matrix)

# Izračun metrika
TP <- conf_matrix[2, 2]  # True Positive
TN <- conf_matrix[1, 1]  # True Negative
FP <- conf_matrix[2, 1]  # False Positive
FN <- conf_matrix[1, 2]  # False Negative

accuracy <- (TP + TN) / sum(conf_matrix)
precision <- TP / (TP + FP)
recall <- TP / (TP + FN)
f1 <- 2 * (precision * recall) / (precision + recall)

cat("\n=== Metrike performansi ===\n")
cat("Accuracy:  ", round(accuracy, 4), 
    " - postotak točnih predikcija\n")
cat("Precision: ", round(precision, 4), 
    " - od predviđenih pobjednika, koliko je stvarno pobijedilo\n")
cat("Recall:    ", round(recall, 4), 
    " - koliko stvarnih pobjednika uspijemo detektirati\n")
cat("F1 Score:  ", round(f1, 4), 
    " - harmonijska sredina precision i recall\n")

# Vizualizacija confusion matrix
conf_df <- as.data.frame(conf_matrix)
colnames(conf_df) <- c("Predicted", "Actual", "Freq")

ggplot(conf_df, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = Freq), size = 10, color = "white") +
  scale_fill_gradient(low = "#a6cee3", high = "#1f78b4") +
  theme_minimal(base_size = 14) +
  labs(title = "Confusion Matrix (Model 2)",
       x = "Stvarni ishod (0=Nominee, 1=Winner)", 
       y = "Predviđeni ishod") +
  theme(legend.position = "none")

# ---------------------------------------------------------
# 8. Vizualizacija predviđenih vjerojatnosti
# ---------------------------------------------------------

pred_df <- data.frame(
  Probability = predicted_probs,
  Actual = factor(model_data$IsWinner, 
                  levels = c(0, 1), 
                  labels = c("Nominee", "Winner"))
)

ggplot(pred_df, aes(x = Probability, fill = Actual)) +
  geom_histogram(position = "identity", alpha = 0.6, bins = 30, color = "black") +
  geom_vline(xintercept = threshold, linetype = "dashed", 
             color = "red", size = 1) +
  scale_fill_manual(values = c("Nominee" = "#a6cee3", "Winner" = "#1f78b4")) +
  theme_minimal(base_size = 14) +
  labs(title = "Distribucija predviđenih vjerojatnosti",
       subtitle = paste0("Crvena linija = threshold (", 
                         round(threshold, 3), ")"),
       x = "Predviđena vjerojatnost pobjede", 
       y = "Broj filmova",
       fill = "Stvarni ishod")

# ---------------------------------------------------------
# 9. ZAKLJUČAK
# ---------------------------------------------------------

cat("1. Model 2 (logistička regresija s više prediktora):\n")
cat("   - McFadden R²:", round(as.numeric(mcfadden_r2_m2), 3), "\n")
cat("   - Accuracy:", round(accuracy, 3), "\n")
cat("   - Precision:", round(precision, 3), "\n")
cat("   - Recall:", round(recall, 3), "\n")
cat("   - F1 Score:", round(f1, 3), "\n\n")

cat("2. Najvažniji prediktori (prema Odds Ratios):\n")
significant_predictors <- odds_plot_data[abs(log(odds_plot_data$OddsRatio)) > 0.2, ]
significant_predictors <- significant_predictors[order(abs(log(significant_predictors$OddsRatio)), 
                                                       decreasing = TRUE), ]
for(i in 1:min(3, nrow(significant_predictors))) {
  cat("   -", significant_predictors$Predictor[i], 
      "(OR =", round(significant_predictors$OddsRatio[i], 2), ")\n")
}

cat("\n3. Test omjera vjerodostojnosti (LR test):\n")
cat("   - Model 2 vs Null: LR =", round(LR_full, 2), 
    ", p <", ifelse(p_value_full < 0.001, "0.001", round(p_value_full, 3)), "\n")
cat("   - Zaključak: Model statistički značajno objašnjava podatke\n")

cat("\n4. Možemo li pouzdano razlikovati pobjednike?\n")
cat("   - McFadden R² =", round(as.numeric(mcfadden_r2_m2), 3), 
    "(Model objašnjava", round(as.numeric(mcfadden_r2_m2) * 100, 1), 
    "% varijance)\n")

if(mcfadden_r2_m2 >= 0.08) {
  cat("   ✓ Model ima prihvatljivu prediktivnu moć\n")
  cat("   ✓ Značajke kao broj glasova, ocjena i godina doprinose predikciji\n")
} else {
  cat("   ✗ Model ima slabu prediktivnu moć\n")
}

cat("\n5. Ograničenja:\n")
cat("   - Model objašnjava samo dio varijabilnosti\n")
cat("   - Precision =", round(precision, 3), 
    "→ dosta lažnih pozitiva\n")
cat("   - Faktori kao režija, gluma, kampanja nisu uključeni\n")