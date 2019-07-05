library(tidyverse)
library(plotly)

#data is the data frame. tibbles currently don't work
#x is string of x variable name
#y is string of y variable name
#cat is string of categorical variable name
#plotly is boolean if they want the graph in plotly. TRUE returns plotly graph, FALSE returns ggplot. Set to FALSE
#same_slope is boolean if they want the regression lines to have the same slope. Set to FALSE
#same_intercept is boolean if they want the regression lines to have the same intercept Set to FALSE
#poly is a integer value to declare how many polynomial variables the use wants. Set to 1, i.e. no polynomial 
#interactions is a integer value for if the user wants to include interactions in the polynomial model. Note only matters when poly > 1 and inteactions <= poly. Set to 0

#TODO List
#make model a input -> would streamline the code
#interactive/stickylabels functions



rl <- function(data, x, y, cat, plotly = FALSE, same_slope = FALSE, same_intercept = FALSE, poly = 1, interactions = 0, se = FALSE)
{
    if (is_tibble(data))
    {
        data <- as.data.frame(data)
    }
    if (poly > 1)
    {
        plot <- rl_polynomial(data, x, y, cat, plotly, same_slope, same_intercept, poly, interactions, se)
    } else if (poly == 1)
    {
        plot <- rl_linear(data, x, y, cat, plotly, same_slope, same_intercept, se)
    } else
    {
        stop('Please enter valid parameters')
    }
    plot
}

rl_linear <- function(data, x, y, cat, plotly = FALSE, same_slope = FALSE, same_intercept = FALSE, se = FALSE)
{
    if (is_tibble(data))
    {
        data <- as.data.frame(data)
    }
    if (same_slope == FALSE)
    {
        if (same_intercept == FALSE)
        {
            plot <- rl_full_model(data, x, y, cat, plotly, se)
        } else 
        {
            plot <- rl_same_intercept(data, x, y, cat, plotly, se)
        }
    } else
    {
        if (same_intercept == FALSE)
        {
            plot <- rl_same_slope(data, x, y, cat, plotly, se)
        } else 
        {
            plot <- rl_same_line(data, x, y, cat, plotly, se)
        }
    }
    plot
}

rl_polynomial <- function(data, x, y, cat, plotly = FALSE, same_slope = FALSE, same_intercept = FALSE, poly = 2, interactions = 1, se = FALSE)
{    
    if (is_tibble(data))
    {
        data <- as.data.frame(data)
    }
    if (same_slope == FALSE)
    {
        if (same_intercept == FALSE)
        {
            plot <- rl_poly_full_model(data, x, y, cat, poly, interactions, plotly, se)
        } else 
        {
            plot <- rl_poly_same_intercept(data, x, y, cat, poly, interactions, plotly, se)
        }
    } else
    {
        if (same_intercept == FALSE)
        {
            plot <- rl_poly_same_slope(data, x, y, cat, poly, plotly, se)
        } else 
        {
            plot <- rl_poly_same_line(data, x, y, cat, poly, plotly, se)
        }
    }
    plot
}

rl_poly_full_model <- function(data, x, y, cat, poly, interactions = 1, plotly = FALSE, se = FALSE)
{
    if (is_tibble(data))
    {
        data <- as.data.frame(data)
    }
    newx <- data[,x]
    newy <- data[,y]
    newcat <- as.factor(as.character(data[,cat]))
    if (interactions > 0)
    {
        if (length(newx) == length(newy) && length(newy) == length(newcat))
        {
            interactionsString <- paste('newy ~ poly(newx, degrees = poly, raw = TRUE) + newcat +')
            for (i in 1:interactions)
            {
                interactionsString <- paste(interactionsString, 'newcat:I(newx^', i, ')')
                if (i != interactions)
                {
                    interactionsString <- paste(interactionsString, '+')
                }
            }
            model <- lm(eval(parse(text = interactionsString)), data = data)
            intercept <- model$coefficients[1]
            
            slopeString <- ''
            for (i in 1:poly)
            {
                coef <- paste("'poly(newx, degrees = poly, raw = TRUE)", i, "'", sep = '')
                newSlopeString <- paste('model$coefficients[', coef, "]*x^", i, sep = '')
                slopeString <- paste(slopeString, newSlopeString, sep = ' + ')
            }
            
            catEff <- model$coefficients[paste('newcat', levels(newcat)[length(levels(newcat))], sep = '')]
            plot <- ggplot(data = data, aes(x = newx, y = newy, col = newcat)) +
                geom_point() +
                stat_function(fun = function(x) intercept + catEff + eval(parse(text = slopeString)),
                              aes(col = levels(newcat)[length(levels(newcat))]))
            
            statFunctions <- list()
            catEffects <- list()
            for (i in 1:(length(levels(newcat))-1))
            {
                if (i != 1)
                {
                    catEffects[[i]] <- model$coefficients[paste('newcat', levels(newcat)[i], sep = '')]
                } else
                {
                    catEffects[[i]] <- 0
                }
                statFunctions[[i]] <- paste('stat_function(aes(color = levels(newcat)[', 
                                            i, ']), fun = function(x) intercept + catEffects[[', i, ']] + eval(parse(text = slopeString))', sep = '')
                for (j in 1:interactions)
                {
                    statFunctions[[i]] <- paste(statFunctions[[i]], 
                                                " + model$coefficients['newcat", 
                                                levels(newcat)[i], ":I(newx^", 
                                                j, ")']*x^", j, sep = '')
                }
                statFunctions[[i]] <- paste(statFunctions[[i]], ')', sep = '')
            }
            
            #unknown symbol error, what is wrong here?
            for (i in 1:(length(levels(newcat))-1))
            {
                plot <- plot + eval(parse(text = statFunctions[[i]]))
            }
            
        } else
        {
            stop('Please enter valid parameters')
        }
        if (se == TRUE)
        {
            plot <- add_se(plot = plot, data = data, newcat = newcat, model = model)
        }
        if (plotly == TRUE)
        {
            plot <- ggplotly(plot)
        }
    } else 
    {
        plot <- rl_poly_same_line(data, x, y, cat, poly, plotly)
    }
    plot
}

rl_poly_same_intercept <- function(data, x, y, cat, poly, interactions = poly, plotly = FALSE, se = FALSE)
{
    if (is_tibble(data))
    {
        data <- as.data.frame(data)
    }
    newx <- data[,x]
    newy <- data[,y]
    newcat <- as.factor(as.character(data[,cat]))
    if (interactions > 0)
    {
        if (length(newx) == length(newy) && length(newy) == length(newcat))
        {
            interactionsString <- paste('newy ~ poly(newx, degrees = poly, raw = TRUE)+')
            for (i in 1:interactions)
            {
                interactionsString <- paste(interactionsString, 'newcat:I(newx^', i, ')')
                if (i != interactions)
                {
                    interactionsString <- paste(interactionsString, '+')
                }
            }
            model <- lm(eval(parse(text = interactionsString)), data = data)
            intercept <- model$coefficients[1]
            
            slopeString <- ''
            for (i in 1:poly)
            {
                coef <- paste("'poly(newx, degrees = poly, raw = TRUE)", i, "'", sep = '')
                newSlopeString <- paste('model$coefficients[', coef, "]*x^", i, sep = '')
                slopeString <- paste(slopeString, newSlopeString, sep = ' + ')
            }
            
            plot <- ggplot(data = data, aes(x = newx, y = newy, col = newcat)) +
                geom_point() +
                stat_function(fun = function(x) intercept + eval(parse(text = slopeString)),
                              aes(col = levels(newcat)[length(levels(newcat))]))
            
            statFunctions <- list()
            for (i in 1:(length(levels(newcat))-1))
            {
                statFunctions[[i]] <- paste('stat_function(aes(color = levels(newcat)[', 
                    i, ']), fun = function(x) intercept + eval(parse(text = slopeString))')
                for (j in 1:interactions)
                {
                    statFunctions[[i]] <- paste(statFunctions[[i]], 
                        " + model$coefficients['newcat", 
                        levels(newcat)[i], ":I(newx^", 
                        j, ")']*x^", j, sep = '')
                }
                statFunctions[[i]] <- paste(statFunctions[[i]], ')', sep = '')
            }
            
            #unknown symbol error, what is wrong here?
            for (i in 1:(length(levels(newcat))-1))
            {
                plot <- plot + eval(parse(text = statFunctions[[i]]))
            }
            
        } else
        {
            stop('Please enter valid parameters')
        }        
        if (se == TRUE)
        {
            plot <- add_se(plot = plot, data = data, newcat = newcat, model = model)
        }
        if (plotly == TRUE)
        {
            plot <- ggplotly(plot)
        }
    } else 
    {
        plot <- rl_poly_same_line(data, x, y, cat, poly, plotly)
    }
    plot
}

rl_poly_same_slope <- function(data, x, y, cat, poly, plotly = FALSE, se = FALSE)
{
    if (is_tibble(data))
    {
        data <- as.data.frame(data)
    }
    newx <- data[,x]
    newy <- data[,y]
    newcat <- as.factor(as.character(data[,cat]))
    if (length(newx) == length(newy) && length(newy) == length(newcat))
    {
        model <- lm(newy ~ poly(newx, degrees = poly, raw = TRUE) + newcat, data = data)
        intercept <- model$coefficients[1]
        
        slopeString <- ''
        for (i in 1:poly)
        {
            coef <- paste("'poly(newx, degrees = poly, raw = TRUE)", i, "'", sep = '')
            newSlopeString <- paste('model$coefficients[', coef, "]*x^", i, sep = '')
            slopeString <- paste(slopeString, newSlopeString, sep = ' + ')
        }
        
        plot <- ggplot(data = data, aes(x = newx, y = newy, col = newcat)) +
            geom_point() +
            stat_function(fun = function(x) intercept + eval(parse(text = slopeString)),
                          aes(col = levels(newcat)[1]))
        
        statFunctions <- list()
        for (i in 1:(length(levels(newcat))-1))
        {
            statFunctions[i] <- paste("stat_function(fun = function(x) intercept + eval(parse(text = slopeString)) + model$coefficients[", i, "+1+", poly, "], aes(col = (levels(newcat)[", i, "+1])))")
        }
        
        for (i in 1:(length(levels(newcat))-1))
        {
            plot <- plot + eval(parse(text = statFunctions[i]))
        }

    } else
    {
        stop('Please enter valid parameters')
    }
    if (se == TRUE)
    {
        plot <- add_se(plot = plot, data = data, newcat = newcat, model = model)
    }
    if (plotly == TRUE)
    {
        plot <- ggplotly(plot)
    }
    plot
} 

rl_poly_same_line <- function(data, x, y, cat, poly, plotly = FALSE, se = FALSE)
{
    if (is_tibble(data))
    {
        data <- as.data.frame(data)
    }
    newx <- data[,x]
    newy <- data[,y]
    newcat <- as.factor(as.character(data[,cat]))
    if (length(newx) == length(newy) && length(newy) == length(newcat))
    {
        model <- lm(newy ~ poly(newx, degrees = poly, raw = TRUE), data = data)
        intercept <- model$coefficients[1]
        
        slopeString <- ''
        for (i in 1:poly)
        {
            coef <- paste("'poly(newx, degrees = poly, raw = TRUE)", i, "'", sep = '')
            newSlopeString <- paste('model$coefficients[', coef, "]*x^", i, sep = '')
            slopeString <- paste(slopeString, newSlopeString, sep = ' + ')
        }
        
        plot <- ggplot(data = data, aes(x = newx, y = newy, col = newcat)) +
            geom_point() +
            stat_function(color = 'black', fun = function(x) intercept + eval(parse(text = slopeString)))
        
    } else
    {
        stop('Please enter valid parameters')
    }
    if (se == TRUE)
    {
        plot <- add_se(plot = plot, data = data, newcat = newcat, model = model, one_line = TRUE)
    }
    if (plotly == TRUE)
    {
        plot <- ggplotly(plot)
    }
    plot
}

rl_full_model <- function(data, x, y, cat, plotly = FALSE, se = FALSE)
{
    newx <- data[,x]
    newy <- data[,y]
    newcat <- as.factor(as.character(data[,cat]))
    if (length(newx) == length(newy) && length(newy) == length(newcat))
    {
        model <- lm(newy ~ newx * newcat, data = data)
        plot <- ggplot(data = data, aes(x = newx, y = newy, col = newcat)) + 
                    geom_point() + 
                    geom_abline(aes(slope = model$coefficients[2],
                                   intercept = model$coefficients[1],
                                    color = levels(newcat)[1]))
        for (i in 1:(length(levels(newcat))-1))
        {
            plot <- plot + geom_abline(aes_string(slope = model$coefficients[2] + model$coefficients[i+length(levels(newcat))+1],
                                           intercept = model$coefficients[1] + model$coefficients[i+2],
                                           color = shQuote(levels(newcat)[i+1])))
        }
    } else 
    {
        stop('Please enter valid parameters')
    }
    if (se == TRUE)
    {
        plot <- add_se(plot = plot, data = data, newcat = newcat, model = model)
    }
    if (plotly == TRUE)
    {
        plot <- ggplotly(plot)
    }
    plot
}

rl_same_intercept <- function(data, x, y, cat, plotly = FALSE, se = FALSE)
{
    newx <- data[,x]
    newy <- data[,y]
    newcat <- as.factor(as.character(data[,cat]))
    if (length(newx) == length(newy) && length(newy) == length(newcat))
    {
        model <- lm(newy ~ newx + newx:newcat, data = data)
        intercept <- model$coefficients[1]
        slope <- model$coefficients[2]
        
        plot <- ggplot(data = data, aes(x = newx, y = newy, col = newcat)) +
                    geom_point() +
                    geom_abline(aes(intercept = intercept,
                                    slope = slope,
                                    color = levels(newcat)[1]))
        
        for (i in 1:(length(levels(newcat))-1))
        {
            plot <- plot + geom_abline(aes_string(intercept = intercept, 
                                                  slope = slope + model$coefficients[i+2],
                                                  color = shQuote(levels(newcat)[i+1])))
        }
    } else 
    {
        stop('Please enter valid parameters')
    }
    if (se == TRUE)
    {
        plot <- add_se(plot = plot, data = data, newcat = newcat, model = model)
    }
    if (plotly == TRUE)
    {
        plot <- ggplotly(plot)
    }
    plot
}

rl_same_slope <- function(data, x, y, cat, plotly = FALSE, se = FALSE)
{
    newx <- data[,x]
    newy <- data[,y]
    newcat <- as.factor(as.character(data[,cat]))
    if (length(newx) == length(newy) && length(newy) == length(newcat))
    {
        model <- lm(newy ~ newx + newcat, data = data)
        plot <- ggplot(data = data, aes(x = newx, y = newy, col = newcat)) + 
            geom_point() + 
            geom_abline(aes(slope = model$coefficients[2],
                            intercept = model$coefficients[1],
                            color = levels(newcat)[1]))
        for (i in 1:(length(levels(newcat))-1))
        {
            plot <- plot + geom_abline(aes_string(slope = model$coefficients[2],
                                                  intercept = model$coefficients[1] + model$coefficients[i+2],
                                                  color = shQuote(levels(newcat)[i+1])))
        }
    } else 
    {
        stop('Please enter valid parameters')
    }
    if (se == TRUE)
    {
        plot <- add_se(plot = plot, data = data, newcat = newcat, model = model)
    }
    if (plotly == TRUE)
    {
        plot <- ggplotly(plot)
    }
    plot
}

rl_same_line <- function(data, x, y, cat, plotly = FALSE, se = FALSE)
{
    newx <- data[,x]
    newy <- data[,y]
    newcat <- as.factor(as.character(data[,cat]))
    if (length(newx) == length(newy) && length(newy) == length(newcat))
    {
        model <- lm(newy ~ newx, data = data)
        plot <- ggplot(data = data, aes(x = newx, y = newy, col = newcat)) + 
            geom_point() + 
            geom_abline(aes(slope = model$coefficients[2],
                            intercept = model$coefficients[1]))
    } else
    {
        stop('Please enter valid parameters')
    }
    if (se == TRUE)
    {
        plot <- add_se(plot = plot, data = data, newcat = newcat, model = model, one_line = TRUE)
    }
    if (plotly == TRUE)
    {
        plot <- ggplotly(plot)
    }
    plot
}

rl_plotly <- function(data, x, y, cat, plotly = TRUE, same_slope = FALSE, same_intercept = FALSE, se = FALSE)
{
    rl(data, x, y, cat, plotly = plotly, same_slope = same_slope, same_intercept = same_intercept)
}

add_se <- function(plot, data, model, newcat, one_line = TRUE)
{
    if (!one_line)
    {
        result <- predict(model, newdata = data, type = "response", se.fit=TRUE)
        plot <- plot + geom_ribbon(aes(ymin = result$fit - result$se.fit, 
                                       ymax = result$fit + result$se.fit, 
                                       fill = newcat), colour = NA, alpha = .35)
    } else
    {
        result <- predict(model, newdata = data, type = "response", se.fit=TRUE)
        plot <- plot + geom_ribbon(aes(ymin = result$fit - result$se.fit, 
                                       ymax = result$fit + result$se.fit), 
                                   colour = NA, alpha = .35)
    }
    plot
}
