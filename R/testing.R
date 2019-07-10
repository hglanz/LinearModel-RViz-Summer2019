#poly, interactions, slope, intercept, se, plotly
data <- iris
x <- 'Sepal.Length'
y <- 'Sepal.Width'
cat <- 'Species'

#full model, no se, no plotly
ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width, col = Species)) + 
    geom_point() +
    geom_smooth(method = lm, se = FALSE)
rl(data, x, y, cat)
rl_linear(data, x, y, cat)
rl_full_model(data, x, y, cat)

#full model, se, no plotly
ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width, col = Species)) + 
    geom_point() +
    geom_smooth(method = lm, se = TRUE)
rl(data, x, y, cat, se = TRUE)
rl_linear(data, x, y, cat, se = TRUE)
rl_full_model(data, x, y, cat, se = TRUE)

#full model, no se, plotly
ggplotly(ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width, col = Species)) + 
    geom_point() +
    geom_smooth(method = lm, se = FALSE))
rl(data, x, y, cat, plotly = TRUE)
rl_linear(data, x, y, cat, plotly = TRUE)
rl_full_model(data, x, y, cat, plotly = TRUE)

#full model, se, plotly
ggplotly(ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width, col = Species)) + 
    geom_point() +
    geom_smooth(method = lm, se = TRUE))
rl(data, x, y, cat, se = TRUE, plotly = TRUE)
rl_linear(data, x, y, cat, se = TRUE, plotly = TRUE)
rl_full_model(data, x, y, cat, se = TRUE, plotly = TRUE)

#same slope, no se, no plotly
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
rl(data, x, y, cat, same_slope = TRUE)
rl_linear(data, x, y, cat, same_slope = TRUE)
rl_same_slope(data, x, y, cat)

#same slope, se, no plotly
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
rl(data, x, y, cat, se = TRUE, same_slope = TRUE)
rl_linear(data, x, y, cat, se = TRUE, same_slope = TRUE)
rl_same_slope(data, x, y, cat, se = TRUE)

#same slope, no se, plotly
model1 <- lm(Sepal.Width ~ Sepal.Length + Species, data = iris)
m <- model1$coefficients[2] #slope of model
b <- model1$coefficients[1] #setosa intercept
ggplotly(ggplot() + 
    geom_point(data = iris, aes(x = Sepal.Length, y = Sepal.Width, col = Species)) +
    geom_abline(aes(slope = m,
                    intercept = b, 
                    color = 'setosa')) +
    geom_abline(aes(slope = m, 
                    intercept = b + model1$coefficients[3], 
                    color = 'versicolor')) +
    geom_abline(aes(slope = m, 
                    intercept = b + model1$coefficients[4], 
                    color = 'virginica')))
rl(data, x, y, cat, same_slope = TRUE, plotly = TRUE)
rl_linear(data, x, y, cat, same_slope = TRUE, plotly = TRUE)
rl_same_slope(data, x, y, cat, plotly = TRUE)

#same slope, se, plotly
model1 <- lm(Sepal.Width ~ Sepal.Length + Species, data = iris)
m <- model1$coefficients[2] #slope of model
b <- model1$coefficients[1] #setosa intercept
ggplotly(ggplot() + 
             geom_point(data = iris, aes(x = Sepal.Length, y = Sepal.Width, col = Species)) +
             geom_abline(aes(slope = m,
                             intercept = b, 
                             color = 'setosa')) +
             geom_abline(aes(slope = m, 
                             intercept = b + model1$coefficients[3], 
                             color = 'versicolor')) +
             geom_abline(aes(slope = m, 
                             intercept = b + model1$coefficients[4], 
                             color = 'virginica')))
rl(data, x, y, cat, se = TRUE, same_slope = TRUE, plotly = TRUE)
rl_linear(data, x, y, cat, se = TRUE, same_slope = TRUE, plotly = TRUE)
rl_same_slope(data, x, y, cat, se = TRUE, plotly = TRUE)