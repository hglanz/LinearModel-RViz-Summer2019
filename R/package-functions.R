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
#Figure out correct move with interactions, include them?, just one?, all of them?
#make model a input -> would streamline the code
#Add a SE ribbon



rl <- function(data, x, y, cat, plotly = FALSE, same_slope = FALSE, same_intercept = FALSE, poly = 1, interactions = 0) 
{
    if (is_tibble(data))
    {
        data <- as.data.frame(data)
    }
    if (poly >= 1)
    {
        plot <- rl_polynomial(data, x, y, cat, plotly, same_slope, same_intercept, poly, interactions)
    } else if (poly = 1)
    {
        plot <- rl_linear(data, x, y, cat, plotly, same_slope, same_intercept)
    } else
    {
        stop('Please enter valid parameters')
    }
    plot
}

rl_linear <- function(data, x, y, cat, plotly = FALSE, same_slope = FALSE, same_intercept = FALSE)
{
    if (is_tibble(data))
    {
        data <- as.data.frame(data)
    }
    if (same_slope == FALSE)
    {
        if (same_intercept == FALSE)
        {
            plot <- rl_full_model(data, x, y, cat)
        } else 
        {
            plot <- rl_same_intercept(data, x, y, cat)
        }
    } else
    {
        if (same_intercept == FALSE)
        {
            plot <- rl_same_slope(data, x, y, cat)
        } else 
        {
            plot <- rl_same_line(data, x, y, cat)
        }
    }
    plot
}

rl_polynomial <- function(data, x, y, cat, plotly = FALSE, same_slope = FALSE, same_intercept = FALSE, poly = 2, interactions = 0)
{    
    if (is_tibble(data))
    {
        data <- as.data.frame(data)
    }
    if (same_slope == FALSE)
    {
        if (same_intercept == FALSE)
        {
            plot <- rl_poly_full_model(data, x, y, cat, poly)
        } else 
        {
            plot <- rl_poly_same_intercept(data, x, y, cat, poly)
        }
    } else
    {
        if (same_intercept == FALSE)
        {
            plot <- rl_poly_same_slope(data, x, y, cat, poly)
        } else 
        {
            plot <- rl_poly_same_line(data, x, y, cat, poly)
        }
    }
    plot
}

rl_poly_full_model <- function(data, x, y, cat, poly, interactiosn)
{
    #TODO
    
    if (is_tibble(data))
    {
        data <- as.data.frame(data)
    }
    newx <- data[,x]
    newy <- data[,y]
    newcat <- as.factor(as.character(data[,cat]))
    if (length(newx) == length(newy) && length(newy) == length(newcat))
    {
        
    } else
    {
        stop('Please enter valid parameters')
    }
    if (plotly == TRUE)
    {
        plot <- ggplotly(plot)
    }
    plot
}

rl_poly_same_intercept <- function(data, x, y, cat, poly, interactions = 0)
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
        interactionsString <- paste('newy ~ poly(newx, degrees = interactions, raw = TRUE)+')
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
            coef <- paste("'poly(newx, degrees = interactions, raw = TRUE)", i, "'", sep = '')
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
        }
        
        #unknown symbol error, what is wrong here?
        for (i in 1:(length(levels(newcat))-1))
        {
            print(i)
            plot <- plot + eval(parse(text = statFunctions[[i]]))
            print(i)
        }
        
    } else
    {
        stop('Please enter valid parameters')
    }
    if (plotly == TRUE)
    {
        plot <- ggplotly(plot)
    }
    plot
}

rl_poly_same_slope <- function(data, x, y, cat, poly, plotly = FALSE)
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
    if (plotly == TRUE)
    {
        plot <- ggplotly(plot)
    }
    plot
} 

rl_poly_same_line <- function(data, x, y, cat, poly, plotly = FALSE)
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
    if (plotly == TRUE)
    {
        plot <- ggplotly(plot)
    }
    plot
}

rl_full_model <- function(data, x, y, cat, plotly = FALSE)
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
    if (plotly == TRUE)
    {
        plot <- ggplotly(plot)
    }
    plot
}

rl_same_intercept <- function(data, x, y, cat, plotly = FALSE)
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
    if (plotly == TRUE)
    {
        plot <- ggplotly(plot)
    }
    plot
}

rl_same_slope <- function(data, x, y, cat, plotly = FALSE)
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
    if (plotly == TRUE)
    {
        plot <- ggplotly(plot)
    }
    plot
}

rl_same_line <- function(data, x, y, cat, plotly = FALSE)
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
    if (plotly == TRUE)
    {
        plot <- ggplotly(plot)
    }
    plot
}

rl_plotly <- function(data, x, y, cat, plotly = TRUE, same_slope = FALSE, same_intercept = FALSE)
{
    rl(data, x, y, cat, plotly = plotly, same_slope = same_slope, same_intercept = same_intercept)
}