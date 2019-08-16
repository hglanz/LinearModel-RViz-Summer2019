## ----setup, echo = FALSE, message = FALSE, warning = FALSE---------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(tidyverse)
library(ggLinearModel)

## ------------------------------------------------------------------------
ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width, col = Species)) + 
    geom_point() +
    geom_smooth(method = lm, se = FALSE)
ggLinearModel(iris, Sepal.Length, Sepal.Width, Species)

## ------------------------------------------------------------------------
model <- lm(Sepal.Width ~ Sepal.Length + Species, data = iris)
m <- model$coefficients[2] #slope of model
b <- model$coefficients[1] #setosa intercept
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
ggLinearModel(iris, Sepal.Length, Sepal.Width, Species, same_slope = TRUE)

## ------------------------------------------------------------------------
model <- lm(Sepal.Width ~ Sepal.Length + Sepal.Length:Species, data = iris)
b <- model$coefficients[1] #intercept of model
m <- model$coefficients[2] #slope
ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width, col = Species)) + 
    geom_point() +
    geom_abline(aes(intercept = b,
                    slope = m,
                    color = 'setosa')) +
    geom_abline(aes(slope = m + model$coefficients[3], 
                    intercept = b,
                    color = 'versicolor')) +
    geom_abline(aes(slope = m + model$coefficients[4], 
                    intercept = b,
                    col = 'virginica'))
ggLinearModel(iris, Sepal.Length, Sepal.Width, Species, same_intercept = TRUE)

## ------------------------------------------------------------------------
mpg$year <- as.factor(as.character(mpg$year))
model <- lm(hwy ~ displ + I(displ^2) + displ:year + I(displ^2):year, data = mpg)
ggplot(data = mpg, aes(x = displ, y = hwy, col = year)) +
    geom_point() +
    stat_function(fun=function(x) 
        model$coefficients[1] + model$coefficients[2]*x + 
            model$coefficients[3]*x^2, 
        aes(col = '1999')) + 
    stat_function(fun=function(x) 
        model$coefficients[1] + model$coefficients[2]*x + 
            I(x^2)*model$coefficients[5]+model$coefficients[3]*x^2 + model$coefficients[4]*x, 
        aes(col = '2008'))
ggLinearModel(mpg, displ, hwy, year, poly = 2)

## ------------------------------------------------------------------------
model <- lm(hwy ~ displ + I(displ^2) + displ:year, data = mpg)
ggplot(data = mpg, aes(x = displ, y = hwy, col = year)) +
    geom_point() +
    stat_function(fun=function(x) 
        model$coefficients[1] + model$coefficients[2]*x + 
            model$coefficients[3]*x^2, 
        aes(col = '1999')) + 
    stat_function(fun=function(x) 
        model$coefficients[1] + model$coefficients[2]*x + 
            I(x^2)*model$coefficients[3] + model$coefficients[4]*x, 
        aes(col = '2008'))
ggLinearModel(mpg, displ, hwy, year, poly = 2, interactions = 1)

## ------------------------------------------------------------------------
ggLinearModel(CO2, conc, uptake, Type, ci = T)
ggLinearModel(CO2, conc, uptake, Type, pi = T)
ggLinearModel(CO2, conc, uptake, Type, pi = T, level = .5)

## ------------------------------------------------------------------------
ggLinearModel(mtcars, wt, mpg, cyl, 
              title = "The Affect of a car's weight on the MPG", xlabel = 'Weight (1,000 
lbs)', 
              ylabel = 'Miles per Gallon', legendTitle = 'Number of Cylinders in engine')

