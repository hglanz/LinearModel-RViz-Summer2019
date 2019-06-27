library(ggiraphExtra)

fullModel <- lm(Sepal.Width ~ Sepal.Length * Species, data = iris)
ggPredict(fullModel)

sameSlopeModel <- lm(Sepal.Width ~ Sepal.Length + Species, data = iris)
ggPredict(sameSlopeModel)

sameInterceptModel <- lm(Sepal.Width ~ Sepal.Length + Sepal.Length:Species, data = iris)
ggPredict(sameInterceptModel)

singleLineModel <- lm(Sepal.Width ~ Sepal.Length, data = iris)
ggPredict(singleLineModel)