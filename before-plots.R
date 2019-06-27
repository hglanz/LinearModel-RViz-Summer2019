library(tidyverse)
library(ggiraph)
library(ggiraphExtra)
library(plotly)

###Different intercepts, different slopes
ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width, col = Species)) + 
    geom_point() +
    geom_smooth(method = lm, se = FALSE)

model2 <- lm(Sepal.Width ~ Sepal.Length * Species, data = iris)
ggPredict(model2)

ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width, col = Species)) + 
    geom_point()

ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width, col = Species)) + 
    geom_point() +
    geom_smooth(method = 'lm', se = FALSE)

plot_ly(iris, x = ~Sepal.Length, y = ~Sepal.Width, color = ~Species, type = 'scatter', showlegend = TRUE) %>%
    add_lines(x = ~Sepal.Length, y = fitted(model2))

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
                intercept = b + model1$coefficients[3], 
                color = 'versicolor')) +
    geom_abline(aes(slope = m, 
                intercept = b + model1$coefficients[4], 
                color = 'virginica'))

ggPredict(model1)

plot_ly(iris, x = ~Sepal.Length, y = ~Sepal.Width, color = ~Species, type = 'scatter', showlegend = TRUE) %>%
    add_lines(x = ~Sepal.Length, y = fitted(model1))

###Same intercept, different slopes
model3 <- lm(Sepal.Width ~ Sepal.Length + Sepal.Length:Species, data = iris)
b <- model3$coefficients[1] #intercept of model
m <- model3$coefficients[2] #slope
ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width, col = Species)) + 
    geom_point() +
    geom_abline(aes(intercept = b,
                    slope = m,
                    color = 'setosa')) +
    geom_abline(aes(slope = m + model3$coefficients[3], 
                    intercept = b,
                    color = 'versicolor')) +
    geom_abline(aes(slope = m + model3$coefficients[4], 
                    intercept = b,
                    col = 'virginica'))

ggPredict(model3)

plot_ly(iris, x = ~Sepal.Length, y = ~Sepal.Width, color = ~Species, type = 'scatter', showlegend = TRUE) %>%
    add_lines(x = ~Sepal.Length, y = fitted(model3))

library(readxl)
polydata <- read_excel("C:/Users/jackl_000/Desktop/polydata.xlsx")

###Quadratic with same slopes diff intercepts
ggplot(data = polydata, aes(x = x, y = y, col = cat)) +
    geom_point() +
    #geom_smooth(method = 'lm', se = FALSE, formula = y ~ x + I(x^2))
    stat_smooth(method = 'lm', se = FALSE, formula = y ~ x + I(x^2))# also works

model4 <- lm(y ~ x + I(x^2) + cat, data = polydata)

ggplot(data = polydata, aes(x = x, y = y, col = cat)) +
    geom_point() +
    stat_function(fun=function(x) 
        model4$coefficients[1] + model4$coefficients[2]*x + model4$coefficients[3]*x^2, 
        aes(col = 'A')) + 
    stat_function(fun=function(x) 
        model4$coefficients[1] + model4$coefficients[2]*x + model4$coefficients[3]*x^2 + model4$coefficients[4], 
        aes(col = 'B'))

plot_ly(polydata, x = ~x, y = ~y, color = ~cat, type = 'scatter', showlegend = TRUE) %>%
    add_lines(x = ~x, y = predict(model4))

###Quadratic with same intercept different slopes
model5 <- lm(y ~ x + I(x^2) + x:cat, data = polydata)

ggplot(data = polydata, aes(x = x, y = y, col = cat)) +
    geom_point() +
    stat_function(fun=function(x) 
        model5$coefficients[1] + model5$coefficients[2]*x + 
            model5$coefficients[3]*x^2, 
        aes(col = 'A')) + 
    stat_function(fun=function(x) 
        model5$coefficients[1] + model5$coefficients[2]*x + 
            model5$coefficients[3]*x^2 + model5$coefficients[4]*x, 
        aes(col = 'B')) 

plot_ly(polydata, x = ~x, y = ~y, color = ~cat, type = 'scatter', showlegend = TRUE) %>%
    add_lines(x=~x, y=~fitted(model5), type="scatter", data = polydata)

#plotly would be smoother with more points and tighter but this shows the problems for using plotly with quadratic

model6 <- lm(y ~ x + I(x^2) + x:cat + I(x^2):cat, data = polydata)

ggplot(data = polydata, aes(x = x, y = y, col = cat)) +
    geom_point() +
    stat_function(fun=function(x) 
        model6$coefficients[1] + model6$coefficients[2]*x + 
            model6$coefficients[3]*x^2, 
        aes(col = 'A')) + 
    stat_function(fun=function(x) 
        model6$coefficients[1] + model6$coefficients[2]*x + 
            I(x^2)*model6$coefficients[5]+model6$coefficients[3]*x^2 + model6$coefficients[4]*x, 
        aes(col = 'B')) 

plot_ly(polydata, x = ~x, y = ~y, color = ~cat, type = 'scatter', showlegend = TRUE) %>%
    add_lines(x=~x, y=~fitted(model5), type="scatter", data = polydata)



