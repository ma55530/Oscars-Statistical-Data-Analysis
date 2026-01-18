# =========================================================
# Pitanje: Možemo li pomoću dostupnih značajki pouzdano 
#          razlikovati pobjednike od ostalih?
# =========================================================

# --- 0. Učitavanje paketa i podataka ---
library(dplyr)
library(ggplot2)

data <- read.csv("oscars_dataset.csv", sep=";", na.strings = c("", "NA", "Unknown"))

# ---------------------------------------------------------
# 1. Priprema i čišćenje podataka (Auditorna vježba 1)
# ---------------------------------------------------------

# Kreiranje binarnog ishoda (Winner = 1, Nominee = 0)
data$IsWinner <- ifelse(data$Award == "Winner", 1, 0)

# Pretvorba tipova podataka
data$Rating <- as.numeric(data$IMDB.Rating)
data$Votes <- as.numeric(gsub(",", "", data$IMDB.Votes))
data$Runtime <- as.numeric(data$Movie.Time)
data$Year <- as.numeric(data$Year.of.Release)

# Grupiranje rijetkih žanrova radi lakše analize
common_genres <- c("Action", "Adventure", "Biography", "Comedy", "Crime", "Drama")
data$GenreGroup <- ifelse(data$Main.Genre %in% common_genres,
                          data$Main.Genre,
                          "Other")
# Faktoriziranje
data$GenreGroup <- factor(data$GenreGroup)

# Čišćenje nedostajućih vrijednosti (complete.cases pristup)
# Zadržavamo samo retke koji imaju sve potrebne podatke
vars_of_interest <- c("IsWinner", "Rating", "Votes", "Runtime", "Year", "GenreGroup")
model_data <- data[complete.cases(data[, vars_of_interest]), vars_of_interest]

cat("Broj filmova nakon čišćenja:", nrow(model_data), "\n")

# ---------------------------------------------------------
# 2. Deskriptivna analiza i Testiranje hipoteza (Vježba 1 & 2)
# ---------------------------------------------------------

# --- A) Analiza IMDB Ocjena (Numerička varijabla) ---

# Robusne mjere centralne tendencije i rasipanja (Vježba 1)
cat("\n=== Deskriptivna statistika: IMDB Rating ===\n")
stats_rating <- model_data %>% 
  group_by(IsWinner) %>% 
  summarise(
    Mean = mean(Rating),
    Median = median(Rating), # Robusna mjera
    SD = sd(Rating),
    IQR = IQR(Rating)        # Robusna mjera rasipanja
  )
print(stats_rating)

# Vizualizacija distribucije i QQ-plot za provjeru normalnosti (Vježba 2)
# Crtamo histogram da vidimo oblik distribucije
par(mfrow=c(1,2))
hist(model_data$Rating[model_data$IsWinner == 1], main="Histogram: Pobjednici", xlab="Rating", col="lightblue")
hist(model_data$Rating[model_data$IsWinner == 0], main="Histogram: Nominirani", xlab="Rating", col="pink")

# QQ-plot: Provjera normalnosti prije t-testa
par(mfrow=c(1,2))
qqnorm(model_data$Rating[model_data$IsWinner == 1], main = "QQ Plot: Pobjednici")
qqline(model_data$Rating[model_data$IsWinner == 1], col = "red")

qqnorm(model_data$Rating[model_data$IsWinner == 0], main = "QQ Plot: Nominirani")
qqline(model_data$Rating[model_data$IsWinner == 0], col = "red")
par(mfrow=c(1,1)) # Resetiranje grafičkog prikaza

# Test jednakosti varijanci (F-test) - Preduvjet za t-test (Vježba 2)
cat("\n=== F-test za jednakost varijanci (Rating) ===\n")
var_test_rating <- var.test(Rating ~ IsWinner, data = model_data)
print(var_test_rating)

# T-test za nezavisne uzorke
# Postavljamo var.equal ovisno o rezultatu var.test-a (ako je p > 0.05, varijance su jednake)
cat("\n=== T-test (Rating) ===\n")
t_rating <- t.test(Rating ~ IsWinner, data = model_data, 
                   var.equal = var_test_rating$p.value > 0.05, 
                   alt = "two.sided")
print(t_rating)

# Boxplot s prikazom podataka
ggplot(model_data, aes(x = factor(IsWinner, labels = c("Nominee", "Winner")), y = Rating, fill = factor(IsWinner))) +
  geom_boxplot() +
  labs(title = "Boxplot IMDB ocjena", x = "Status", y = "Ocjena") +
  theme_minimal()

# --- B) Analiza Žanrova (Kategorijska varijabla) ---

# Kontingencijska tablica (Vježba 2)
cat("\n=== Analiza kategorijskih varijabli (Žanr) ===\n")
genre_table <- table(model_data$GenreGroup, model_data$IsWinner)
print(addmargins(genre_table))

# Provjera očekivanih frekvencija i Chi-square test (Vježba 2)
chisq_genre <- chisq.test(genre_table)
cat("\nOčekivane frekvencije (provjera uvjeta > 5):\n")
print(chisq_genre$expected)
cat("\nRezultat Chi-square testa:\n")
print(chisq_genre)

# ---------------------------------------------------------
# 3. Linearna regresija i dijagnostika modela (Vježba 3)
# ---------------------------------------------------------

cat("\n\n========================================\n")
cat("VIŠESTRUKA LINEARNA REGRESIJA\n")
cat("========================================\n")

# Provjera multikolinearnosti (Korelacija među prediktorima) - Vježba 3
# Prije gradnje modela provjeravamo jesu li varijable previše korelirane
cat("\n=== Korelacijska matrica numeričkih varijabli ===\n")
cor_matrix <- cor(model_data[, c("Rating", "Votes", "Runtime", "Year")])
print(cor_matrix)

# Izgradnja linearnog modela (Linear Probability Model)
# Uključujemo transformaciju log(Votes) zbog asimetrične distribucije (kao u Vježbi 3)
fit.multi <- lm(IsWinner ~ Rating + log(Votes) + Runtime + Year + GenreGroup, 
                data = model_data)

# Analiza modela (Vježba 3: Koeficijenti, R-kvadrat, F-statistika)
cat("\n=== Sažetak regresijskog modela (summary) ===\n")
summary_fit <- summary(fit.multi)
print(summary_fit)

cat("\nInterpretacija:\n")
cat("R-squared (Postotak objašnjene varijance):", summary_fit$r.squared, "\n")
cat("F-statistic p-value (Značajnost cijelog modela):", 
    pf(summary_fit$fstatistic[1], summary_fit$fstatistic[2], summary_fit$fstatistic[3], lower.tail=FALSE), "\n")

# Dijagnostika modela (Vježba 3: Analiza reziduala)
cat("\n=== Dijagnostika reziduala ===\n")

# 1. Histogram reziduala - Provjera normalnosti grešaka
hist(fit.multi$residuals, 
     main = "Histogram reziduala modela", 
     xlab = "Reziduali", 
     col = "lightgreen", 
     breaks = 20)

# 2. QQ-plot reziduala - Usporedba s teoretskom normalnom razdiobom
qqnorm(fit.multi$residuals)
qqline(fit.multi$residuals, col = "red", lwd = 2)

# 3. Reziduali vs. Predviđene vrijednosti - Provjera homoskedastičnosti
plot(fit.multi$fitted.values, fit.multi$residuals,
     main = "Reziduali vs. Predviđene vrijednosti",
     xlab = "Predviđene vrijednosti (Fitted)",
     ylab = "Reziduali",
     pch = 19, col = "blue", alpha = 0.5)
abline(h = 0, col = "red", lwd = 2)

# Analiza utjecaja dummy varijabli (Žanr)
# Prikazujemo koeficijente da vidimo koji žanr (u odnosu na referentni) doprinosi šansi za pobjedu
cat("\n=== Koeficijenti modela (utjecaj varijabli) ===\n")
print(coef(fit.multi))