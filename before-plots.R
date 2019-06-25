library(tidyverse)
library(ggiraph)
library(ggiraphExtra)

###Different intercepts, different slopes
ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width, col = Species)) + 
    geom_point() +
    geom_smooth(method = lm, se = FALSE)

model2 <- lm(Sepal.Width ~ Sepal.Length * Species, data = iris)
ggPredict(model2)

###Same slope, different intercepts
model1 <- lm(Sepal.Width ~ Sepal.Length + Species, data = iris)
m <- model1$coefficients[2] #slope of model
b <- model1$coefficients[1] #setosa intercept
ggplot() + 
    geom_point(data = iris, aes(x = Sepal.Length, y = Sepal.Width, col = Species)) +
    geom_abline(aes(slope = m,
                intercept = b, 
                color = 'setosa')) +
    geom_abline(aes(slope = m, 
                intercept = b + model$coefficients[3], 
                color = 'versicolor')) +
    geom_abline(aes(slope = m, 
                intercept = b + model$coefficients[4], 
                color = 'virginica'))

ggPredict(model1)
