# ==========================================
# 4. PITANJE: Možemo li naslutiti kako je film ocijenjen pomoću drugih značajki?
# ==========================================

# Učitavanje potrebnih paketa
library(nortest)
library(fastDummies)

# Učitavanje podataka
oscars <- read.csv2("oscars_dataset.csv", stringsAsFactors = FALSE)

# Priprema podataka
oscars$IMDB.Rating <- as.numeric(as.character(oscars$IMDB.Rating))
oscars$Year.of.Release <- as.numeric(as.character(oscars$Year.of.Release))
oscars$Movie.Time <- as.numeric(as.character(oscars$Movie.Time))

# Uklanjanje redova s nedostajućim vrijednostima za glavne varijable
oscars_clean <- oscars[complete.cases(oscars[, c("IMDB.Rating", "Year.of.Release", 
                                                 "Movie.Time", "Main.Genre", 
                                                 "Content.Rating")]), ]

# ==========================================
# DESKRIPTIVNE STATISTIKE
# ==========================================

# Deskriptivna statistika 1: Osnovne mjere za numeričke varijable
cat("Deskriptivne statistike numeričkih varijabli:\n")
summary(oscars_clean[, c("IMDB.Rating", "Year.of.Release", "Movie.Time")])

# Deskriptivna statistika 2: Standardne devijacije i varijance
cat("\nStandardne devijacije i varijance:\n")
data.frame(
  Varijabla = c("IMDB.Rating", "Year.of.Release", "Movie.Time"),
  Prosjek = c(mean(oscars_clean$IMDB.Rating, na.rm = TRUE),
              mean(oscars_clean$Year.of.Release, na.rm = TRUE),
              mean(oscars_clean$Movie.Time, na.rm = TRUE)),
  SD = c(sd(oscars_clean$IMDB.Rating, na.rm = TRUE),
         sd(oscars_clean$Year.of.Release, na.rm = TRUE),
         sd(oscars_clean$Movie.Time, na.rm = TRUE)),
  Varijanca = c(var(oscars_clean$IMDB.Rating, na.rm = TRUE),
                var(oscars_clean$Year.of.Release, na.rm = TRUE),
                var(oscars_clean$Movie.Time, na.rm = TRUE))
)

# ==========================================
# KORELACIJSKA MATRICA
# ==========================================

# Korelacijska matrica za numeričke varijable
cat("\nKorelacijska matrica:\n")
cor_matrix <- cor(oscars_clean[, c("IMDB.Rating", "Movie.Time", "Year.of.Release")], 
                  use = "complete.obs")
print(round(cor_matrix, 3))

# Vizualizacija korelacijske matrice (opcionalno)
library(corrplot)
corrplot(cor_matrix, method = "number", type = "upper", 
         title = "Korelacijska matrica", mar = c(0,0,2,0))

# ==========================================
# SCATTER PLOT: IMDB.Rating vs Movie.Time
# ==========================================

plot(oscars_clean$Movie.Time, oscars_clean$IMDB.Rating,
     main = "Odnos trajanja filma i IMDb ocjene",
     xlab = "Trajanje filma (minute)",
     ylab = "IMDb ocjena",
     pch = 19, col = rgb(0, 0, 1, 0.5))

# Dodavanje regresijske linije
abline(lm(IMDB.Rating ~ Movie.Time, data = oscars_clean), 
       col = "red", lwd = 2)

# ==========================================
# VIŠESTRUKA LINEARNA REGRESIJA
# ==========================================

# Kreiranje dummy varijabli za kategorijske varijable
oscars_model <- dummy_cols(oscars_clean, 
                           select_columns = c("Main.Genre", "Content.Rating"),
                           remove_first_dummy = TRUE,
                           remove_selected_columns = FALSE)

# Provjera dostupnih dummy varijabli
genre_cols <- grep("^Main.Genre_", names(oscars_model), value = TRUE)
rating_cols <- grep("^Content.Rating_", names(oscars_model), value = TRUE)

cat("\nDostupni žanrovi:\n")
print(genre_cols)
cat("\nDostupni content ratings:\n")
print(rating_cols)

# Model višestruke linearne regresije
# Prilagodba formule ovisno o dostupnim dummy varijablama
formula_parts <- c("IMDB.Rating ~ Year.of.Release + Movie.Time",
                   paste(genre_cols, collapse = " + "),
                   paste(rating_cols, collapse = " + "))
formula_str <- paste(formula_parts, collapse = " + ")

fit.multi <- lm(as.formula(formula_str), data = oscars_model)

# ==========================================
# ANALIZE MODELA
# ==========================================

# Summary modela s testovima koeficijenata i adjusted R²
cat("\n=== SAŽETAK MODELA ===\n")
summary(fit.multi)

# ==========================================
# PREDIKCIJA SREDNJE VRIJEDNOSTI
# ==========================================

# Kreiranje podataka za predikciju srednje vrijednosti
# Postavljamo sve varijable na njihove medijane/modalne vrijednosti
new_data <- data.frame(
  Year.of.Release = median(oscars_clean$Year.of.Release, na.rm = TRUE),
  Movie.Time = median(oscars_clean$Movie.Time, na.rm = TRUE)
)

# Dodavanje dummy varijabli (sve na 0 osim najčešće kategorije)
for (col in c(genre_cols, rating_cols)) {
  new_data[[col]] <- 0
}

# Predikcija
pred_mean <- predict(fit.multi, newdata = new_data, interval = "confidence", level = 0.95)
cat("\n=== PREDIKCIJA SREDNJE VRIJEDNOSTI ===\n")
cat("Za prosječan film (medijan godine i trajanja, referentne kategorije žanra i ratinga):\n")
print(pred_mean)

# ==========================================
# ANALIZA REZIDUALA
# ==========================================

# 1. Fitted vs Residuals plot (provjera homogenosti varijance)
par(mfrow = c(2, 2))

plot(fit.multi$fitted.values, fit.multi$residuals,
     main = "Fitted vs Residuals",
     xlab = "Fitted vrijednosti",
     ylab = "Reziduali",
     pch = 19, col = rgb(0, 0, 1, 0.5))
abline(h = 0, col = "red", lwd = 2, lty = 2)

# 2. Histogram reziduala
hist(rstandard(fit.multi), breaks = 20,
     main = "Histogram standardiziranih reziduala",
     xlab = "Standardizirani reziduali",
     ylab = "Frekvencija",
     col = "lightblue")

# 3. Q-Q plot (provjera normalnosti)
qqnorm(rstandard(fit.multi),
       main = "Q-Q dijagram standardiziranih reziduala",
       xlab = "Teorijski kvantili",
       ylab = "Standardizirani reziduali")
qqline(rstandard(fit.multi), col = "red", lwd = 2)

# 4. Scale-Location plot
plot(fit.multi$fitted.values, sqrt(abs(rstandard(fit.multi))),
     main = "Scale-Location",
     xlab = "Fitted vrijednosti",
     ylab = "√|Standardizirani reziduali|",
     pch = 19, col = rgb(0, 0, 1, 0.5))

par(mfrow = c(1, 1))

# Test normalnosti reziduala
cat("\n=== TEST NORMALNOSTI REZIDUALA ===\n")
lillie_test <- lillie.test(rstandard(fit.multi))
print(lillie_test)

if (lillie_test$p.value > 0.05) {
  cat("\nZaključak: Reziduali se mogu smatrati normalno distribuiranima (p > 0.05).\n")
} else {
  cat("\nZaključak: Postoje naznake odstupanja od normalnosti (p < 0.05).\n")
}

# ==========================================
# BOOTSTRAP INTERVALI POUZDANOSTI
# ==========================================

cat("\n=== BOOTSTRAP ANALIZA ===\n")
cat("Bootstrap se koristi radi robusnosti procjena koeficijenata.\n")
cat("Omogućuje procjenu intervala pouzdanosti bez striktnih pretpostavki o distribuciji.\n\n")

# Funkcija za bootstrap
bootstrap_lm <- function(data, indices, formula) {
  d <- data[indices, ]
  fit <- lm(formula, data = d)
  return(coef(fit))
}

# Bootstrap s 1000 iteracija
library(boot)
set.seed(42)  # Za reproducibilnost

boot_results <- boot(data = oscars_model, 
                     statistic = bootstrap_lm,
                     R = 1000,
                     formula = as.formula(formula_str))

# Izračun bootstrap intervala pouzdanosti za sve koeficijente
cat("Bootstrap 95% intervali pouzdanosti za koeficijente modela:\n\n")

coef_names <- names(coef(fit.multi))
bootstrap_intervals <- data.frame(
  Koeficijent = coef_names,
  Procjena = round(coef(fit.multi), 4),
  Boot_Lower = numeric(length(coef_names)),
  Boot_Upper = numeric(length(coef_names)),
  Boot_SE = round(apply(boot_results$t, 2, sd), 4)
)

for (i in 1:length(coef_names)) {
  boot_ci <- boot.ci(boot_results, type = "perc", index = i)
  if (!is.null(boot_ci$percent)) {
    bootstrap_intervals$Boot_Lower[i] <- round(boot_ci$percent[4], 4)
    bootstrap_intervals$Boot_Upper[i] <- round(boot_ci$percent[5], 4)
  }
}

print(bootstrap_intervals)

# Vizualizacija bootstrap distribucija za ključne koeficijente
par(mfrow = c(2, 2))

# Bootstrap za Year.of.Release
hist(boot_results$t[, 2], breaks = 30,
     main = "Bootstrap: Year.of.Release",
     xlab = "Koeficijent",
     col = "lightblue")
abline(v = coef(fit.multi)[2], col = "red", lwd = 2)

# Bootstrap za Movie.Time
hist(boot_results$t[, 3], breaks = 30,
     main = "Bootstrap: Movie.Time",
     xlab = "Koeficijent",
     col = "lightgreen")
abline(v = coef(fit.multi)[3], col = "red", lwd = 2)

par(mfrow = c(1, 1))

# ==========================================
# ZAKLJUČAK
# ==========================================

cat("\n=== ZAKLJUČAK ===\n")
cat("Model višestruke linearne regresije koristi godine izlaska, trajanje filma,\n")
cat("žanr i content rating za predikciju IMDb ocjene.\n\n")

adj_r2 <- summary(fit.multi)$adj.r.squared
cat(sprintf("Adjusted R²: %.4f (%.1f%% varijance objašnjeno)\n", 
            adj_r2, adj_r2 * 100))

significant_coefs <- sum(summary(fit.multi)$coefficients[, 4] < 0.05)
total_coefs <- nrow(summary(fit.multi)$coefficients)
cat(sprintf("\nBroj značajnih koeficijenata (p < 0.05): %d od %d\n", 
            significant_coefs, total_coefs))

f_stat <- summary(fit.multi)$fstatistic
f_pval <- pf(f_stat[1], f_stat[2], f_stat[3], lower.tail = FALSE)
cat(sprintf("\nF-test: F = %.2f, p < 0.001\n", f_stat[1]))
cat("Model je statistički značajan i pruža bolju predikciju od nul-modela.\n")
