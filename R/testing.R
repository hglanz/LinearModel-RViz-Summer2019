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