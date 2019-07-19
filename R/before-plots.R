library(tidyverse)
library(ggiraph)
library(ggiraphExtra)
library(plotly)

###Different intercepts, different slopes
ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width, col = Species)) + 
    geom_point() +
    geom_smooth(method = lm, se = TRUE) #geom_smooth only does full model

model2 <- lm(Sepal.Width ~ Sepal.Length * Species, data = iris)
ggPredict(model2, se = T)

plot_ly(iris, x = ~Sepal.Length, y = ~Sepal.Width, color = ~Species, type = 'scatter', showlegend = TRUE) %>%
    add_lines(y = fitted(model2),  tooltip = paste(~Species, '= Species'))

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

ggplot() + 
    geom_point(data = iris, aes(x = Sepal.Length, y = Sepal.Width, col = Species)) +
    geom_segment(aes(x = min(iris[iris$Species == 'setosa',]$Sepal.Length),
                     xend = max(iris[iris$Species == 'setosa',]$Sepal.Length),
                     y = min(iris[iris$Species == 'setosa',]$Sepal.Length)*m+b,
                     yend = max(iris[iris$Species == 'setosa',]$Sepal.Length)*m+b,
                     color = 'setosa')) +
    geom_segment(aes(x = min(iris[iris$Species == 'versicolor',]$Sepal.Length),
                     xend = max(iris[iris$Species == 'versicolor',]$Sepal.Length),
                     y = min(iris[iris$Species == 'versicolor',]$Sepal.Length)*m+b+model1$coefficients[3],
                     yend = max(iris[iris$Species == 'versicolor',]$Sepal.Length)*m+b+model1$coefficients[3],
                     color = 'versicolor')) +
    geom_segment(aes(x = min(iris[iris$Species == 'virginica',]$Sepal.Length),
                     xend = max(iris[iris$Species == 'virginica',]$Sepal.Length),
                     y = min(iris[iris$Species == 'virginica',]$Sepal.Length)*m+b+model1$coefficients[4],
                     yend = max(iris[iris$Species == 'virginica',]$Sepal.Length)*m+b+model1$coefficients[4], 
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
polydata <- read_excel("polydata.xlsx")


###Quadratic with same slopes diff intercepts
ggplot(data = polydata, aes(x = x, y = y, col = cat)) +
    geom_point() +
    #geom_smooth(method = 'lm', se = FALSE, formula = y ~ x + I(x^2))
    stat_smooth(method  = 'lm', se = FALSE, formula = y ~ x + I(x^2))# also works

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
        aes(col = 'B'))# + interactive_polyline_grob(x = xvec, y = yvec, tooltip = 'BOINKL')

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
#visreg doesn't work with lm made models and cat variables and interactions


#interactive
model1 <- lm(Sepal.Width ~ Sepal.Length + Species, data = iris)
ggPredict(model1)
ggPredict(model1, se = TRUE)
ggPredict(model1, interactive = TRUE)
ggPredict(model1, interactive = TRUE, se = TRUE)

m <- model1$coefficients[2] #slope of model
b <- model1$coefficients[1] #setosa intercept

x <- 'Sepal.Length'
y <- 'Sepal.Width'

data <- model1$model
data$data_id=rownames(data)
iris$tooltip=paste0(data$data_id,"\n",x," = ",data[[x]],"\n",y," = ",data[[y]])

tooltip_css <- "background-color:white;font-style:italic;padding:10px;border-radius:10px 20px 10px 20px;"
hover_css="r:4px;cursor:pointer;stroke:red;stroke-width:2px;"
selected_css = "fill:#FF3333;stroke:black;"

setosaTT <- paste('Setosa \n', y, '=', sprintf("%.3f",m), '*', x, '+', sprintf("%.3f",b), sep = ' ')
versicolorTT <- paste('Versicolor \n', y, '=', sprintf("%.3f",m), '*', x, '+', sprintf("%.3f",b+model1$coefficients[3]), sep = ' ')
virginicaTT <- paste('Virginica \n', y, '=', sprintf("%.3f",m), '*', x, '+', sprintf("%.3f",b+model1$coefficients[4]), sep = ' ')

p <- ggplot(data = iris, aes(x = Sepal.Length,
                             y = Sepal.Width, 
                             col = Species, 
                             fill = Species,
                             group = Species)) + 
    geom_segment_interactive(aes(x = min(iris[iris$Species == 'setosa',]$Sepal.Length),
                     xend = max(iris[iris$Species == 'setosa',]$Sepal.Length),
                     y = min(iris[iris$Species == 'setosa',]$Sepal.Length)*m+b,
                     yend = max(iris[iris$Species == 'setosa',]$Sepal.Length)*m+b,
                     color = 'setosa',
                     tooltip = setosaTT)) +
    geom_segment_interactive(aes(x = min(iris[iris$Species == 'versicolor',]$Sepal.Length),
                     xend = max(iris[iris$Species == 'versicolor',]$Sepal.Length),
                     y = min(iris[iris$Species == 'versicolor',]$Sepal.Length)*m+b+model1$coefficients[3],
                     yend = max(iris[iris$Species == 'versicolor',]$Sepal.Length)*m+b+model1$coefficients[3],
                     color = 'versicolor',
                     tooltip = versicolorTT)) +
    geom_segment_interactive(aes(x = min(iris[iris$Species == 'virginica',]$Sepal.Length),
                     xend = max(iris[iris$Species == 'virginica',]$Sepal.Length),
                     y = min(iris[iris$Species == 'virginica',]$Sepal.Length)*m+b+model1$coefficients[4],
                     yend = max(iris[iris$Species == 'virginica',]$Sepal.Length)*m+b+model1$coefficients[4], 
                     color = 'virginica',
                     tooltip = virginicaTT)) +
    geom_point_interactive(aes(tooltip = tooltip, data_id = data$data_id))

ggiraph(code=print(p),
             tooltip_extra_css=tooltip_css,
             tooltip_opacity=.75,
             zoom_max=10,
             hover_css=hover_css,
             selected_css=selected_css)

xvec <- polydata$x
yvec <- polydata$y
interactive_polyline_grob(x = xvec, y = yvec, tooltip = 'I hope this works')