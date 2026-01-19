library(dplyr)
library(ggplot2)
library(pROC)

oscars <- read.csv("oscars_dataset.csv", sep = ";", na.strings = c("", "NA", "Unknown"))

# Pretvorbe tipova, uređivanje dataseta
oscars <- oscars %>%
  mutate(
    IMDB.Rating = as.numeric(IMDB.Rating),
    Year.of.Release = as.numeric(Year.of.Release),
    Movie.Time = as.numeric(Movie.Time),
    # Kreiranje binarne varijable (1 = Winner, 0 = Nominee)
    IsWinner = ifelse(Award == "Winner", 1, 0),
    # Grupiranje žanrova radi stabilnosti modela
    Main.Genre = ifelse(Main.Genre %in% c("Drama", "Comedy", "Biography", "Adventure", "Action", "Crime"), 
                        Main.Genre, "Other"),
    Main.Genre = factor(Main.Genre),
    Award = factor(Award, levels = c("Nominee", "Winner"))
  ) %>%
  filter(complete.cases(IMDB.Rating, Main.Genre, Year.of.Release, Movie.Time, IsWinner))

# 1. Box plot (IMDB.Rating ~ Winner)
ggplot(oscars, aes(x = Award, y = IMDB.Rating, fill = Award)) +
  geom_boxplot() +
  labs(
    x = "Ishod",
    y = "IMDb ocjena",
    title = "Distribucija IMDb ocjena s obzirom na pobjedu"
  )

# 2. Histogram IMDB.Rating (pobjednici vs ostali)
ggplot(oscars, aes(x = IMDB.Rating, fill = Award)) +
  geom_histogram(position = "identity", alpha = 0.6, bins = 20) +
  labs(
    x = "IMDb ocjena",
    y = "Frekvencija",
    title = "Histogram ocjena: Pobjednici vs Nominirani"
  )

# 3. Stupčasti dijagram udjela pobjednika po žanru
genre_prop <- oscars %>%
  group_by(Main.Genre) %>%
  summarise(
    Total = n(),
    Winners = sum(IsWinner),
    PropWinner = Winners / Total
  )

ggplot(genre_prop, aes(x = Main.Genre, y = PropWinner)) +
  geom_col(fill = "steelblue") +
  labs(
    x = "Žanr",
    y = "Udio pobjednika",
    title = "Udio pobjednika po žanru"
  )

# t-test (IMDB.Rating: pobjednici vs ostali)
t.test(IMDB.Rating ~ Award, data = oscars)

# Logistička regresija
model <- glm(
  IsWinner ~ IMDB.Rating + Main.Genre + Year.of.Release + Movie.Time,
  data = oscars,
  family = binomial(link = "logit")
)

summary(model)

# Evaluacija - Accuracy
predicted_probs <- predict(model, type = "response")
predicted_class <- ifelse(predicted_probs > 0.5, 1, 0)
accuracy <- mean(predicted_class == oscars$IsWinner)
print(paste("Accuracy:", round(accuracy, 4)))

# Evaluacija - ROC krivulja
roc_obj <- roc(oscars$IsWinner, predicted_probs)
plot(roc_obj, main = "ROC Krivulja", col = "blue")

# Evaluacija - AUC
auc_value <- auc(roc_obj)
print(paste("AUC:", round(auc_value, 4)))

# Evaluacija - Pseudo-R^2 (McFadden)
null_model <- glm(IsWinner ~ 1, data = oscars, family = binomial)
pseudo_r2 <- 1 - (logLik(model) / logLik(null_model))
print(paste("McFadden's Pseudo-R^2:", round(pseudo_r2, 4)))