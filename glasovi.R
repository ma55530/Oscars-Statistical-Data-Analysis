library(ggplot2)

oscars <- read.csv("oscars_dataset.csv", sep = ";") 
oscars$win <- ifelse(oscars$Award == "Winner", 1, 0)
oscars$votes <- as.numeric(gsub(",", "", oscars$IMDB.Votes))
summary(oscars$votes)

model <- glm(
  win ~ votes,
  data = oscars,
  family = binomial
)

summary(model)
# za inkrementalan glas publike log odds za dobivanje nagrade
# ce biti veci za 1.155e-06

# za inkrementalnig 100 tis glasova
oscars$votes100k <- oscars$votes / 100000

model100k <- glm(win ~ votes100k, family = binomial, data = oscars)
summary(model100k)
# z je 4.186, dovoljno velika za odbacivanje hipoteze
# vjerojatnost da će film osvojiti Oscar raste s vecim brojem glasova

# vizualizacija
newframe <- data.frame(
  votes100k = seq(
    min(oscars$votes100k),
    max(oscars$votes100k),
    length.out = 100
  )
)
newframe$prob <- predict(model100k, newframe, type = "response") # vjerojatnosti
ggplot(newframe, aes(x = votes100k, y = prob)) +
  geom_line() +
  labs(x = "IMDb votes (log scale)", y = "Predicted probability of winning", 
       title = "Audience votes and Oscar-winning probability"
  )

