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

#figure out how to create slope in polynomial functions


rl <- function(data, x, y, cat, plotly = FALSE, same_slope = FALSE, same_intercept = FALSE, poly = 1, interactions = 0) {
    if (poly != 1)
    {
        plot <- rl_polynomial(data, x, y, cat, plotly, same_slope, same_intercept, poly, interactions)
    } else 
    {
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
    }
    if (plotly == TRUE)
    {
        plot <- ggplotly(plot)
    }
    plot
}

rl_polynomial <- function(data, x, y, cat, plotly = FALSE, same_slope = FALSE, same_intercept = FALSE, poly = 2, interactions = 0)
{
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

rl_poly_full_model <- function(data, x, y, cat, poly)
{
    #TODO
}

rl_poly_same_intercept <- function(data, x, y, cat, poly)
{
    #TODO
}

rl_poly_same_slope <- function(data, x, y, cat, poly)
{
    newx <- data[,x]
    newy <- data[,y]
    newcat <- as.factor(as.character(data[,cat]))
    if (length(newx) == length(newy) && length(newy) == length(newcat))
    {
        model <- lm(newy ~ poly(newx, degrees = poly, raw = TRUE) + newcat, data = data) #howtodo with raised to 3, 4, 5 6?
        intercept <- model$coefficients[1]
        
        #unsure how to go about this.
        slopes <- matrix(model$coefficients[2])
        for (i in 3:(poly+1))
        {
            slopes <- rbind(slopes, model$coefficients[i])
        }
        
        plot <- ggplot(data = data, aes(x = newx, y = newy, col = newcat)) +
            geom_point() +
            stat_function(fun = function(x) intercept +
                              t(slope)%*%t(matrix(poly(newx, poly))), #must add additional slopes. this doesn't work
                          aes(col = levels(newcat)[1]))
        for (i in 1:(length(levels(newcat))-1))
        {
            plot <- plot + stat_function(fun = function(x) intercept + slope + model$coefficients[i+1+poly], aes_string(col = shQuote(levels(newcat)[i+1]))) #may convert to aes_string with shQuote
        }
        
    } else
    {
        stop('Please enter valid parameters')
    }
    plot
}

rl_poly_same_line <- function(data, x, y, cat, poly)
{
    #TODO
}


rl_full_model <- function(data, x, y, cat)
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
    plot
}

rl_same_intercept <- function(data, x, y, cat)
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
    plot
}

rl_same_slope <- function(data, x, y, cat)
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
    plot
}

rl_same_line <- function(data, x, y, cat)
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
    plot
}


rl_plotly <- function(data, x, y, cat, plotly = TRUE, same_slope = FALSE, same_intercept = FALSE)
{
    rl(data, x, y, cat, plotly = plotly, same_slope = same_slope, same_intercept = same_intercept)
}