library(ggplot2)

# Scatter plot: Movie.Time vs IMDB.Rating
ggplot(data, aes(x = Movie.Time, y = IMDB.Rating)) +
  geom_point(alpha = 0.4, color = "#1f78b4") +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  theme_minimal(base_size = 14) +
  labs(title = "Duljina filma vs IMDB ocjena",
       x = "Duljina filma (minute)",
       y = "IMDB ocjena")

# Korelacija
cor.test(data$Movie.Time, data$IMDB.Rating, method = "pearson")


data$LengthGroup <- ifelse(data$Movie.Time < 120, "Short", "Long")

t.test(IMDB.Rating ~ LengthGroup, data = data)
boxplot(IMDB.Rating ~ LengthGroup, data = data,
        main = "IMDB ocjena po duljini filma",
        xlab = "Duljina filma",
        ylab = "IMDB ocjena",
        col = c("#a6cee3", "#1f78b4"))