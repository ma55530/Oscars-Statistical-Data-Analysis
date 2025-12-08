# =========================================================
# Oscarovi: Analiza žanrova (1928–2020)
# =========================================================

# --- 0. Učitavanje podataka i provjera varijabli ---
data <- read.csv("oscars_dataset.csv", sep=";")
var_types <- sapply(data, class)
cat(paste(names(var_types), ": ", var_types, sep = "", collapse = "\n"))

# ---------------------------------------------------------
# 1. Jesu li među pobjednicima neki žanrovi češći?
# ---------------------------------------------------------

# Spajanje rijetkih žanrova u "Other"
common_genres <- c("Action", "Adventure", "Biography", "Comedy", "Crime", "Drama")
data$GenreGroup <- ifelse(data$Main.Genre %in% common_genres,
                          data$Main.Genre,
                          "Other")

# Skupovi podataka
winners <- subset(data, Award == "Winner")
nominees <- subset(data, Award %in% c("Nominee", "Winner"))

# Broj po žanru
winner_counts <- as.data.frame(table(winners$GenreGroup))
nominee_counts <- as.data.frame(table(nominees$GenreGroup))

# Merge u jednu tablicu
df <- merge(nominee_counts, winner_counts, by="Var1", all.x=TRUE)
colnames(df) <- c("Genre", "Nominations", "Winners")
df$Winners[is.na(df$Winners)] <- 0  
df$NotWinner <- df$Nominations - df$Winners

df  # pregled tablice

# Kontingencijska tablica: žanr × (Pobjednik / Nije pobjednik)
contingency <- df[, c("Winners", "NotWinner")]
row.names(contingency) <- df$Genre
contingency

# Chi-squared test
chisq <- chisq.test(contingency)
chisq

# Standardizirani reziduali (pokazuju koji žanrovi odstupaju)
chisq$stdres

# Fisher Exact Test (za provjeru)
fisher.test(contingency, simulate.p.value = TRUE, B = 1e6)

df_melt <- melt(df[, c("Genre", "Winners", "NotWinner")], id.vars = "Genre")
colnames(df_melt) <- c("Genre", "Outcome", "Count")

ggplot(df_melt, aes(x = Genre, y = Count, fill = Outcome)) +
  geom_bar(stat = "identity", position = "stack", color = "black") +
  scale_fill_manual(values = c("Winners" = "#1f78b4", "NotWinner" = "#a6cee3")) +
  theme_minimal(base_size = 14) +
  labs(title = "Oscari po žanru: Pobjede vs Nominacije",
       x = "Žanr", y = "Broj filmova", fill = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Pretvori kontingenciju u table
cont_table <- as.table(as.matrix(contingency))

# Mosaic plot sa shadingom prema standardiziranim rezidualima
mosaic(
  cont_table,
  shade = TRUE,
  legend = TRUE,
  main = "Mosaic plot: Genre × Oscar Win",
  xlab = "Je li film pobjednik?",
  ylab = "Žanr"
)



