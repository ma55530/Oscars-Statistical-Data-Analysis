library(ggplot2)
library(dplyr)

oscars <- read.csv("oscars_dataset.csv", sep=";")
oscars$rating <- as.numeric(oscars$IMDB.Rating) #IMDb rating
oscars$votes <- as.numeric(gsub(",", "", oscars$IMDB.Votes)) #broj glasova na IMDb
oscars$runtime <- as.numeric(oscars$Movie.Time) # trajanje filma
oscars$year <- as.numeric(oscars$Year.of.Release) # godina izlaska filma
oscars$genre <- oscars$Main.Genre # žanr filma

oscars <- oscars %>%
  select(rating, votes, runtime, year, genre) %>%
  na.omit()

summary(oscars)

#analiza parametara

#1. Možemo li sa brojem glasova na IMDb-u predvidjeti ocjenu?
#   H0: ne možemo, H1: možemo

ggplot(oscars, aes(x = votes, y = rating)) +
  geom_point(alpha = 0.4) +
  scale_x_log10() + 
  labs(x = "Broj glasova na IMDb-u (log)", y = "Ocjena filma na IMDb-u")

# Spearmanov test korelacije

cor.test(
  oscars$rating,
  oscars$votes,
  method = "spearman"
)

# p value je izrazito mali, odbacujemo hipotezu H0 (glasovi i ocjena nisu korelirane)

#2. Možemo li s obzirom na žanr predvidjeti kako je ocijenjen film?
# H0: ne, H1: da

ggplot(oscars, aes(x = genre, y = rating)) +
  geom_boxplot() +
  coord_flip()


# linearni model kada uzmemo logaritam broja glasova
m1 <- lm(rating ~ log10(votes), data = oscars)
summary(m1)



m2 <- lm(
  rating ~ log10(votes) + runtime + year + genre,
  data = oscars
)

summary(m2)







