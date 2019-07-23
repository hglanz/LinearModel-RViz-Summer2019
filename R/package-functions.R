library(tidyverse)
library(plotly)
library(ggiraph)

#data is the data frame. tibbles will work
#x is string of x variable name
#y is string of y variable name
#cat is string of categorical variable name
#plotly is boolean if they want the graph in plotly. TRUE returns plotly graph, FALSE returns ggplot. Set to FALSE
#same_slope is boolean if they want the regression lines to have the same slope. Set to FALSE
#same_intercept is boolean if they want the regression lines to have the same intercept Set to FALSE
#poly is a integer value to declare how many polynomial variables the use wants. Set to 1, i.e. no polynomial 
#interactions is a integer value for if the user wants to include interactions in the polynomial model. Note only matters when poly > 1 and inteactions <= poly. Set to 0
#interactive is boolean to make interactive, defaults to false
#title is a string to change the title of the graph, defaults to x name vs y name
#xlabel is the label of the x axis, defaults to x name
#ylabel is the label of the y axis, defaults to y name
#legendTitle is the title of a legned, defaults to category name
#level is the percent level to make a CI/PI, defaults to 95%


rl <- function(data, x, y, cat, plotly = FALSE, same_slope = FALSE, same_intercept = FALSE, 
               poly = 1, interactions = poly, ci = FALSE, pi = FALSE, interactive = FALSE, 
               title = paste(x, 'vs.', y), xlabel = x, ylabel = y, legendTitle = cat, level = .95)
{
    if (is_tibble(data))
    {
        data <- as.data.frame(data)
    }
    if (poly > 1)
    {
        plot <- rl_polynomial(data, x, y, cat, plotly = plotly, same_slope = same_slope, same_intercept = same_intercept, poly = poly, interactions = interactions, ci = ci, pi = pi, interactive = interactive, title = title, xlabel = xlabel, ylabel = ylabel, legendTitle = legendTitle, level = level)
    } else
    {
        plot <- rl_linear(data, x, y, cat, plotly = plotly, same_slope = same_slope, same_intercept = same_intercept, ci = ci, pi = pi, interactive = interactive, title = title, xlabel = xlabel, ylabel = ylabel, legendTitle = legendTitle, level = level)
    }
    plot
}

rl_linear <- function(data, x, y, cat, plotly = FALSE, same_slope = FALSE, same_intercept = FALSE, 
                      ci = FALSE, pi = FALSE, interactive = FALSE, title = paste(x, 'vs.', y), 
                      xlabel = x, ylabel = y, legendTitle = cat, level = .95)
{
    if (is_tibble(data))
    {
        data <- as.data.frame(data)
    }
    if (same_slope == FALSE)
    {
        if (same_intercept == FALSE)
        {
            plot <- rl_full_model(data, x, y, cat, plotly = plotly, ci = ci, pi = pi, interactive = interactive, title = title, xlabel = xlabel, ylabel = ylabel, legendTitle = legendTitle, level = level)
        } else 
        {
            plot <- rl_same_intercept(data, x, y, cat, plotly = plotly, ci = ci, pi = pi, interactive = interactive, title = title, xlabel = xlabel, ylabel = ylabel, legendTitle = legendTitle, level = level)
        }
    } else
    {
        if (same_intercept == FALSE)
        {
            plot <- rl_same_slope(data, x, y, cat, plotly = plotly, ci = ci, pi = pi, interactive = interactive, title = title, xlabel = xlabel, ylabel = ylabel, legendTitle = legendTitle, level = level)
        } else 
        {
            plot <- rl_same_line(data, x, y, cat, plotly = plotly, ci = ci, pi = pi, interactive = interactive, title = title, xlabel = xlabel, ylabel = ylabel, legendTitle = legendTitle, level = level)
        }
    }
    plot
}

rl_polynomial <- function(data, x, y, cat, poly, interactions = poly, plotly = FALSE, same_slope = FALSE,
                          same_intercept = FALSE, ci = FALSE, pi = FALSE, interactive = FALSE,
                          title = paste(x, 'vs.', y), xlabel = x, ylabel = y, legendTitle = cat, level = .95)
{    
    if (is_tibble(data))
    {
        data <- as.data.frame(data)
    }
    if (same_slope == FALSE)
    {
        if (same_intercept == FALSE)
        {
            plot <- rl_poly_full_model(data, x, y, cat, plotly = plotly, poly = poly, interactions = interactions, ci = ci, pi = pi, interactive = interactive, title = title, xlabel = xlabel, ylabel = ylabel, legendTitle = legendTitle, level = level)
        } else 
        {
            plot <- rl_poly_same_intercept(data, x, y, cat, plotly = plotly, poly = poly, interactions = interactions, ci = ci, pi = pi, interactive = interactive, title = title, xlabel = xlabel, ylabel = ylabel, legendTitle = legendTitle, level = level)
        }
    } else
    {
        if (same_intercept == FALSE)
        {
            plot <- rl_poly_same_slope(data, x, y, cat, plotly = plotly, poly = poly, ci = ci, pi = pi, interactive = interactive, title = title, xlabel = xlabel, ylabel = ylabel, legendTitle = legendTitle, level = level)
        } else 
        {
            plot <- rl_poly_same_line(data, x, y, cat, plotly = plotly, poly = poly, ci = ci, pi = pi, interactive = interactive, title = title, xlabel = xlabel, ylabel = ylabel, legendTitle = legendTitle, level = level)
       }
    }

    plot
}

rl_plotly <- function(data, x, y, cat, plotly = FALSE, same_slope = FALSE, same_intercept = FALSE,
                      poly = 1, interactions = poly, ci = FALSE, pi = FALSE, interactive = FALSE,
                      title = paste(x, 'vs.', y), xlabel = x, ylabel = y, legendTitle = cat, level = .95)
{
    rl(data, x, y, cat, plotly = FALSE, same_slope = FALSE, same_intercept = FALSE, poly = 1, interactions = poly, ci = FALSE, pi = FALSE, interactive = FALSE, title = NULL, xlabel = NULL, ylabel = NULL, legendTitle = NULL, level = .95)
}

rl_poly_full_model <- function(data, x, y, cat, poly, interactions = poly, plotly = FALSE, ci = FALSE,
                               pi = FALSE, interactive = FALSE, title = NULL, xlabel = NULL, ylabel = NULL,
                               legendTitle = NULL, level = .95)
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
                              aes(col = levels(newcat)[length(levels(newcat))]),
                              xlim = c(min(data[data[,cat] == levels(newcat)[length(levels(newcat))],][,x]),
                                       max(data[data[,cat] == levels(newcat)[length(levels(newcat))],][,x])))
            
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
                                            i, ']), fun = function(x) intercept + catEffects[[',
                                            i, ']] + eval(parse(text = slopeString))', sep = '')
                for (j in 1:interactions)
                {
                    statFunctions[[i]] <- paste(statFunctions[[i]], 
                                                " + model$coefficients['newcat", 
                                                levels(newcat)[i], ":I(newx^", 
                                                j, ")']*x^", j, sep = '')
                }
                statFunctions[[i]] <- paste(statFunctions[[i]], 
                                            ', xlim = c(min(data[data[,cat] == levels(newcat)[', i,
                                            '],][,x]),max(data[data[,cat] == levels(newcat)[', i, 
                                            '],][,x])))', sep = '')
            }
            
            for (i in 1:(length(levels(newcat))-1))
            {
                plot <- plot + eval(parse(text = statFunctions[[i]]))
            }
            
        } else
        {
            stop('Please enter valid parameters')
        }
        if (ci == TRUE)
        {
            plot <- add_ci(plot, data,  model, level = level)
        }
        if (pi == TRUE)
        {
            plot <- add_pi(plot, data,  model, level = level)
        }
        if (is.null(title))
        {
            plot <- plot + ggtitle(paste(x, 'vs.', y))
        } else
        {
            plot <- plot + ggtitle(title)
        }
        if (is.null(xlabel))
        {
            plot <- plot + xlab(x)
        } else
        {
            plot <- plot + xlab(xlabel)
        }
        if (is.null(ylabel))
        {
            plot <- plot + ylab(y)
        } else
        {
            plot <- plot + ylab(ylabel)
        }
        if (is.null(legendTitle))
        {
            plot <- plot + labs(color = cat, fill = cat)
        } else
        {
            plot <- plot + labs(color = legendTitle, fill = legendTitle)
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

rl_poly_same_intercept <- function(data, x, y, cat, poly, interactions = poly, plotly = FALSE, ci = FALSE,
                                   pi = FALSE, interactive = FALSE, title = NULL, xlabel = NULL, 
                                   ylabel = NULL, legendTitle = NULL, level = .95)
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
                              aes(col = levels(newcat)[length(levels(newcat))]),
                              xlim = c(min(data[data[,cat] == levels(newcat)[length(levels(newcat))],][,x]),
                                       max(data[data[,cat] == levels(newcat)[length(levels(newcat))],][,x])))
            
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
                statFunctions[[i]] <- paste(statFunctions[[i]], 
                                            ', xlim = c(min(data[data[,cat] == levels(newcat)[', i,
                                            '],][,x]),max(data[data[,cat] == levels(newcat)[', i, 
                                            '],][,x])))', sep = '')            
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
        if (ci == TRUE)
        {
            plot <- add_ci(plot, data,  model, level = level)
        }
        if (pi == TRUE)
        {
            plot <- add_pi(plot, data,  model, level = level)
        }
        if (is.null(title))
        {
            plot <- plot + ggtitle(paste(x, 'vs.', y))
        } else
        {
            plot <- plot + ggtitle(title)
        }
        if (is.null(xlabel))
        {
            plot <- plot + xlab(x)
        } else
        {
            plot <- plot + xlab(xlabel)
        }
        if (is.null(ylabel))
        {
            plot <- plot + ylab(y)
        } else
        {
            plot <- plot + ylab(ylabel)
        }
        if (is.null(legendTitle))
        {
            plot <- plot + labs(color = cat, fill = cat)
        } else
        {
            plot <- plot + labs(color = legendTitle, fill = legendTitle)
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

rl_poly_same_slope <- function(data, x, y, cat, poly, plotly = FALSE, ci = FALSE, pi = FALSE, 
                               interactive = FALSE, title = NULL, xlabel = NULL, ylabel = NULL, 
                               legendTitle = NULL, level = .95)
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
                          aes(col = levels(newcat)[1]),
                          xlim = c(min(data[data[,cat] == levels(newcat)[1],][,x]),
                                   max(data[data[,cat] == levels(newcat)[1],][,x])))
        
        statFunctions <- list()
        for (i in 1:(length(levels(newcat))-1))
        {
            statFunctions[i] <- paste("stat_function(fun = function(x) intercept + eval(parse(text = slopeString)) + model$coefficients[", i, "+1+", poly, "], aes(col = (levels(newcat)[", i, "+1]))")
            statFunctions[i] <- paste(statFunctions[i], 
                                        ', xlim = c(min(data[data[,cat] == levels(newcat)[1+', i,
                                        '],][,x]),max(data[data[,cat] == levels(newcat)[1+', i, 
                                        '],][,x])))', sep = '')
        }
        
        for (i in 1:(length(levels(newcat))-1))
        {
            plot <- plot + eval(parse(text = statFunctions[i]))
        }

    } else
    {
        stop('Please enter valid parameters')
    }
    if (ci == TRUE)
    {
        plot <- add_ci(plot, data,  model, level = level)
    }
    if (pi == TRUE)
    {
        plot <- add_pi(plot, data,  model, level = level)
    }
    if (is.null(title))
    {
        plot <- plot + ggtitle(paste(x, 'vs.', y))
    } else
    {
        plot <- plot + ggtitle(title)
    }
    if (is.null(xlabel))
    {
        plot <- plot + xlab(x)
    } else
    {
        plot <- plot + xlab(xlabel)
    }
    if (is.null(ylabel))
    {
        plot <- plot + ylab(y)
    } else
    {
        plot <- plot + ylab(ylabel)
    }
    if (is.null(legendTitle))
    {
        plot <- plot + labs(color = cat, fill = cat)
    } else
    {
        plot <- plot + labs(color = legendTitle, fill = legendTitle)
    }
    if (plotly == TRUE)
    {
        plot <- ggplotly(plot)
    }

    plot
} 

rl_poly_same_line <- function(data, x, y, cat, poly, plotly = FALSE, ci = FALSE, pi = FALSE, 
                              interactive = FALSE, title = NULL, xlabel = NULL, ylabel = NULL, 
                              legendTitle = NULL, level = .95)
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
    if (ci == TRUE)
    {
        plot <- add_ci(plot, data,  model, level = level, one_line = TRUE)
    }
    if (pi == TRUE)
    {
        plot <- add_pi(plot, data,  model, level = level, one_line = TRUE)
    }
    if (is.null(title))
    {
        plot <- plot + ggtitle(paste(x, 'vs.', y))
    } else
    {
        plot <- plot + ggtitle(title)
    }
    if (is.null(xlabel))
    {
        plot <- plot + xlab(x)
    } else
    {
        plot <- plot + xlab(xlabel)
    }
    if (is.null(ylabel))
    {
        plot <- plot + ylab(y)
    } else
    {
        plot <- plot + ylab(ylabel)
    }
    if (is.null(legendTitle))
    {
        plot <- plot + labs(color = cat, fill = cat)
    } else
    {
        plot <- plot + labs(color = legendTitle, fill = legendTitle)
    }
    if (plotly == TRUE)
    {
        plot <- ggplotly(plot)
    }

    plot
}

rl_full_model <- function(data, x, y, cat, plotly = FALSE, ci = FALSE, pi = FALSE, interactive = FALSE, 
                          title = paste(x, 'vs.', y), xlabel = x, ylabel = y, legendTitle = cat, level = .95)
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
        model <- lm(newy ~ newx * newcat, data = data)
        b <- model$coefficients[1]
        m <- model$coefficients[2]
        fit <- model$model
        fit$data_id <- rownames(fit)
        data$tooltip <- paste0(fit$data_id, "\n", x, " = ", fit[['newx']], "\n", y, " = ", fit[['newy']], "\n", cat, " = ", fit[['newcat']])
        
        plot <- ggplot(data = data, aes(x = newx, y = newy, col = newcat))
        
        if (ci == TRUE)
        {
            plot <- add_ci(plot, data,  model, level = level)
        }
        if (pi == TRUE)
        {
            plot <- add_pi(plot, data,  model, level = level)
        }
        
        if (!plotly)
        {
            plot <- plot + geom_point_interactive(aes(tooltip = tooltip))  +
                geom_segment_interactive(aes(x = min(data[newcat == levels(newcat)[1],][,x]),
                                             xend = max(data[newcat == levels(newcat)[1],][,x]),
                                             y = min(data[newcat == levels(newcat)[1],][,x])*m+b,
                                             yend = max(data[newcat == levels(newcat)[1],][,x])*m+b,
                                             color = levels(newcat)[1],
                                             tooltip = paste(levels(newcat)[1], '\n ', y, '=', sprintf('%.3f',m), 
                                                             '*', x, '+', sprintf('%.3f',b), sep = ' ')))
        
            tooltips <- list()
            for (i in 2:length(levels(newcat)))
            {
                tooltips[[i]] <-  paste(levels(newcat)[i], '\n', y, '=', sprintf("%.3f",m+model$coefficients[i+length(levels(newcat))]), '*', x, '+', sprintf("%.3f",b+model$coefficients[i+1]), sep = ' ')
            }    

            for (i in 1:(length(levels(newcat))-1))
            {
                plot <- plot + geom_segment_interactive(aes_string(x = min(data[newcat == levels(newcat)[i+1],][,x]),
                                                                   xend = max(data[newcat == levels(newcat)[i+1],][,x]),
                                                                   y = min(data[newcat == levels(newcat)[i+1],][,x])*(m+model$coefficients[i+length(levels(newcat))+1])+b+model$coefficients[i+2],
                                                                   yend = max(data[newcat == levels(newcat)[i+1],][,x])*(m+model$coefficients[i+length(levels(newcat))+1])+b+model$coefficients[i+2],
                                                                   color = shQuote(levels(newcat)[i+1])),
                                                        tooltip = tooltips[i+1])
            }
            
        } else
        {
            plot <- plot + geom_point()  +
                geom_segment(aes(x = min(data[newcat == levels(newcat)[1],][,x]),
                                 xend = max(data[newcat == levels(newcat)[1],][,x]),
                                 y = min(data[newcat == levels(newcat)[1],][,x])*m+b,
                                 yend = max(data[newcat == levels(newcat)[1],][,x])*m+b,
                                 color = levels(newcat)[1]))
            
            for (i in 1:(length(levels(newcat))-1))
            {
                plot <- plot + geom_segment(aes_string(x = min(data[newcat == levels(newcat)[i+1],][,x]),
                                                                   xend = max(data[newcat == levels(newcat)[i+1],][,x]),
                                                                   y = min(data[newcat == levels(newcat)[i+1],][,x])*(m+model$coefficients[i+length(levels(newcat))+1])+b+model$coefficients[i+2],
                                                                   yend = max(data[newcat == levels(newcat)[i+1],][,x])*(m+model$coefficients[i+length(levels(newcat))+1])+b+model$coefficients[i+2],
                                                                   color = shQuote(levels(newcat)[i+1])))   
            }
        }
        
        plot <- plot + ggtitle(title) + xlab(xlabel) + ylab(ylabel) + labs(color = legendTitle, fill = legendTitle)
        
        if (interactive == TRUE)
        {
            tooltip_css <- "background-color:white;padding:10px;border-radius:10px 20px 10px 20px;"
            hover_css="r:4px;cursor:pointer;stroke:black;stroke-width:2px;"
            selected_css = "fill:#FF3333;stroke:black;"
            
            
            plot <- ggiraph(code=print(plot),
                            tooltip_extra_css=tooltip_css,
                            tooltip_opacity=.75,
                            zoom_max=10,
                            hover_css=hover_css,
                            selected_css=selected_css)
        } else if (plotly == TRUE)
        {
            plot <- ggplotly(plot, names = Species)
        }
        plot
        
    } else 
    {
        stop('Please enter valid parameters')
    }
}

rl_same_intercept <- function(data, x, y, cat, plotly = FALSE, ci = FALSE, pi = FALSE, interactive = FALSE, 
                              title = paste(x, 'vs.', y), xlabel = x, ylabel = y, legendTitle = cat, 
                              level = .95)
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
        model <- lm(newy ~ newx + newx:newcat, data = data)
        b <- model$coefficients[1]
        m <- model$coefficients[2]
        fit <- model$model
        fit$data_id <- rownames(fit)
        data$tooltip <- paste0(fit$data_id, "\n", x, " = ", fit[['newx']], "\n", y, " = ", fit[['newy']], "\n", cat, " = ", fit[['newcat']])
        
        plot <- ggplot(data = data, aes(x = newx, y = newy, col = newcat))
        
        if (ci == TRUE)
        {
            plot <- add_ci(plot, data,  model, level = level)
        }
        if (pi == TRUE)
        {
            plot <- add_pi(plot, data,  model, level = level)
        }
        
        if (!plotly)
        {
            plot <- plot + geom_point_interactive(aes(tooltip = tooltip)) + 
                geom_segment_interactive(aes(x = min(data[newcat == levels(newcat)[1],][,x]),
                                             xend = max(data[newcat == levels(newcat)[1],][,x]),
                                             y = b+m*min(data[newcat == levels(newcat)[1],][,x]),
                                             yend = b+m*max(data[newcat == levels(newcat)[1],][,x]),
                                             color = levels(newcat)[1],
                                             tooltip = paste(levels(newcat)[1], '\n ', y, '=', sprintf('%.3f',m), '*', x, '+', sprintf('%.3f',b), sep = ' ')))
        
            tooltips <- list()
            for (i in 2:length(levels(newcat)))
            {
                tooltips[[i]] <-  paste(levels(newcat)[i], "\n", y, '=', sprintf("%.3f",m+model$coefficients[i+1]), '*', x, '+', sprintf("%.3f",b), sep = ' ')
            } 
        
        
            for (i in 1:(length(levels(newcat))-1))
            {
                plot <- plot + geom_segment_interactive(aes_string(x = min(data[newcat == levels(newcat)[i+1],][,x]),
                                                                   xend = max(data[newcat == levels(newcat)[i+1],][,x]),
                                                                   y = b+(model$coefficients[i+2]+m)*min(data[newcat == levels(newcat)[i+1],][,x]),
                                                                   yend = b+(model$coefficients[i+2]+m)*max(data[newcat == levels(newcat)[i+1],][,x]),
                                                                   color = shQuote(levels(newcat)[1+i])),
                                                        tooltip = tooltips[[i+1]])
            }
        } else
        {
            plot <- plot + geom_point() + 
                geom_segment(aes(x = min(data[newcat == levels(newcat)[1],][,x]),
                                             xend = max(data[newcat == levels(newcat)[1],][,x]),
                                             y = b+m*min(data[newcat == levels(newcat)[1],][,x]),
                                             yend = b+m*max(data[newcat == levels(newcat)[1],][,x]),
                                             color = levels(newcat)[1]))
            
            for (i in 1:(length(levels(newcat))-1))
            {
                plot <- plot + geom_segment(aes_string(x = min(data[newcat == levels(newcat)[i+1],][,x]),
                                                                   xend = max(data[newcat == levels(newcat)[i+1],][,x]),
                                                                   y = b+(model$coefficients[i+2]+m)*min(data[newcat == levels(newcat)[i+1],][,x]),
                                                                   yend = b+(model$coefficients[i+2]+m)*max(data[newcat == levels(newcat)[i+1],][,x]),
                                                                   color = shQuote(levels(newcat)[1+i])))
            }
        }
        
        plot <- plot + ggtitle(title) + xlab(xlabel) + ylab(ylabel) + labs(color = legendTitle, fill = legendTitle)
        
        if (interactive == TRUE)
        {
            tooltip_css <- "background-color:white;padding:10px;border-radius:10px 20px 10px 20px;"
            hover_css="r:4px;cursor:pointer;stroke:black;stroke-width:2px;"
            selected_css = "fill:#FF3333;stroke:black;"
            
            
            plot <- ggiraph(code=print(plot),
                            tooltip_extra_css=tooltip_css,
                            tooltip_opacity=.75,
                            zoom_max=10,
                            hover_css=hover_css,
                            selected_css=selected_css)
            
        } else if (plotly == TRUE)
        {
            plot <- ggplotly(plot)
        }
        plot
        
    } else 
    {
        stop('Please enter valid parameters')
    }
}

rl_same_slope <- function(data, x, y, cat, plotly = FALSE, ci = FALSE, pi = FALSE, interactive = FALSE, 
                          title = paste(x, 'vs.', y), xlabel = x, ylabel = y, legendTitle = cat, level = .95)
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
        model <- lm(newy ~ newx + newcat, data = data)
        b <- model$coefficients[1]
        m <- model$coefficients[2]
        fit <- model$model
        fit$data_id <- rownames(fit)
        data$tooltip <- paste0(fit$data_id, "\n", x, " = ", fit[['newx']], "\n", y, " = ", fit[['newy']], "\n", cat, " = ", fit[['newcat']])
        
        plot <- ggplot(data = data, aes(x = newx, y = newy, col = newcat))
        
        if (ci == TRUE)
        {
            plot <- add_ci(plot, data,  model, level = level)
        }
        if (pi == TRUE)
        {
            plot <- add_pi(plot, data,  model, level = level)
        }
        
        if (!plotly)
        {
            plot <- plot + geom_point_interactive(aes(tooltip = tooltip)) + 
                           geom_segment_interactive(aes(x = min(data[newcat == levels(newcat)[1],][,x]),
                                                        xend = max(data[newcat == levels(newcat)[1],][,x]),
                                                        y = min(data[newcat == levels(newcat)[1],][,x])*m+b,
                                                        yend = max(data[newcat == levels(newcat)[1],][,x])*m+b,
                                                        color = levels(newcat)[1]),
                                                    tooltip = paste(levels(newcat)[1], '\n ', y, '=', sprintf('%.3f',m), '*', x, '+', sprintf('%.3f',b), sep = ' '))
            tooltips <- list()
            for (i in 2:length(levels(newcat)))
            {
                tooltips[[i]] <-  paste(levels(newcat)[i], "\n", y, '=', sprintf("%.3f",m), '*', x, '+', sprintf("%.3f",b+model$coefficients[i+1]), sep = ' ')
            }    
        
            for (i in 1:(length(levels(newcat))-1))
            {
                plot <- plot + geom_segment_interactive(aes_string(x = min(data[newcat == levels(newcat)[1+i],][,x]),
                                                                   xend = max(data[newcat == levels(newcat)[1+i],][,x]),
                                                                   y = min(data[newcat == levels(newcat)[1+i],][,x])*m+model$coefficients[2+i]+b,
                                                                   yend = max(data[newcat == levels(newcat)[1+i],][,x])*m+model$coefficients[2+i]+b,
                                                                   color = shQuote(levels(newcat)[1+i])),
                                                        tooltip = tooltips[[i+1]])
            }
        } else
        {
            {
                plot <- plot + geom_point() + 
                    geom_segment(aes(x = min(data[newcat == levels(newcat)[1],][,x]),
                                                 xend = max(data[newcat == levels(newcat)[1],][,x]),
                                                 y = min(data[newcat == levels(newcat)[1],][,x])*m+b,
                                                 yend = max(data[newcat == levels(newcat)[1],][,x])*m+b,
                                                 color = levels(newcat)[1]))
                
                for (i in 1:(length(levels(newcat))-1))
                {
                    plot <- plot + geom_segment(aes_string(x = min(data[newcat == levels(newcat)[1+i],][,x]),
                                                                       xend = max(data[newcat == levels(newcat)[1+i],][,x]),
                                                                       y = min(data[newcat == levels(newcat)[1+i],][,x])*m+model$coefficients[2+i]+b,
                                                                       yend = max(data[newcat == levels(newcat)[1+i],][,x])*m+model$coefficients[2+i]+b,
                                                                       color = shQuote(levels(newcat)[1+i])))
                }
            }
        }
        
        
        plot <- plot + ggtitle(title) + xlab(xlabel) + ylab(ylabel) + labs(color = legendTitle, fill = legendTitle)
        
        
        if (interactive == TRUE)
        {
            tooltip_css <- "background-color:white;padding:10px;border-radius:10px 20px 10px 20px;"
            hover_css="r:4px;cursor:pointer;stroke:black;stroke-width:2px;"
            selected_css = "fill:#FF3333;stroke:black;"
            
            
            plot <- ggiraph(code=print(plot),
                            tooltip_extra_css=tooltip_css,
                            tooltip_opacity=.75,
                            zoom_max=10,
                            hover_css=hover_css,
                            selected_css=selected_css)
        } else if (plotly == TRUE)
        {
            plot <- ggplotly(plot)
        }
    } else 
    {
        stop('Please enter valid parameters')
    }
    
    
    
    plot
}

rl_same_line <- function(data, x, y, cat, plotly = FALSE, ci = FALSE, pi = FALSE, interactive = FALSE, 
                         title = paste(x, 'vs.', y), xlabel = x, ylabel = y, legendTitle = cat, level = .95)
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
        model <- lm(newy ~ newx, data = data)
        fit <- model$model
        fit$data_id <- rownames(fit)
        data$tooltip <- paste0(fit$data_id, "\n", 
                               x, " = ", fit[['newx']], "\n", 
                               y, " = ", fit[['newy']])
        
        plot <- ggplot(data = data, aes(x = newx, y = newy))
        
        if (ci == TRUE)
        {
            plot <- add_ci(plot, data,  model, level = level, one_line = TRUE)
        }
        if (pi == TRUE)
        {
            plot <- add_pi(plot, data,  model, level = level, one_line = TRUE)
        }
        
        if (!plotly)
        {
            plot <- plot + geom_point_interactive(data = data, aes(col = newcat, tooltip = tooltip)) + 
                           geom_segment_interactive(aes(x = min(newx),
                                                        xend = max(newx),
                                                        y = min(newx)*model$coefficients[2] + model$coefficients[1], 
                                                        yend = max(newx)*model$coefficients[2] + model$coefficients[1],
                                                        tooltip = paste(y, '=', x, '*', sprintf("%.3f",model$coefficients[2]), '+', sprintf("%.3f",model$coefficients[1]))))
        } else {
            plot <- plot + geom_point(data = data, aes(col = newcat)) + 
                           geom_segment(aes(x = min(newx),
                                            xend = max(newx),
                                            y = min(newx)*model$coefficients[2] + model$coefficients[1], 
                                            yend = max(newx)*model$coefficients[2] + model$coefficients[1]))
            
        }
        plot <- plot + ggtitle(title) + xlab(xlabel) + ylab(ylabel) + labs(color = legendTitle, fill = legendTitle)
        
        if (interactive == TRUE)
        {
            tooltip_css <- "background-color:white;padding:10px;border-radius:10px 20px 10px 20px;"
            hover_css="r:4px;cursor:pointer;stroke:black;stroke-width:2px;"
            selected_css = "fill:#FF3333;stroke:black;"
            
            
            plot <- ggiraph(code=print(plot),
                            tooltip_extra_css=tooltip_css,
                            tooltip_opacity=.75,
                            zoom_max=10,
                            hover_css=hover_css,
                            selected_css=selected_css)
        } else if (plotly == TRUE)
        {
            plot <- ggplotly(plot)
        }
        
    } else
    {
        stop('Please enter valid parameters')
    }
    
    plot
}



add_ci <- function(plot, data, model, level = .95, one_line = FALSE)
{
    if (!one_line)
    {
        result <- predict(model, newdata = data, type = "response", se.fit=TRUE,
                          interval = 'confidence', level = level)
        plot <- plot + geom_ribbon(aes(ymin = result$fit[,'lwr'], 
                                       ymax = result$fit[,'upr']),
                                   alpha = .35)
    } else
    {
        result <- predict(model, newdata = data, type = "response", se.fit=TRUE, 
                          interval = 'confidence', level = level)
        plot <- plot + geom_ribbon(aes(ymin = result$fit[,'lwr'], 
                                       ymax = result$fit[,'upr']), 
                                   col = NA, alpha = .35)
    }
    plot
}

add_pi <- function(plot, data, model, level = .95, one_line = FALSE)
{
    if (!one_line)
    {
        result <- predict(model, newdata = data, type = "response", se.fit=TRUE, 
                          interval = 'prediction', level = level)
        plot <- plot + geom_ribbon(aes(ymin = result$fit[,'lwr'], 
                                       ymax = result$fit[,'upr']), 
                                   alpha = .35)
    } else
    {
        result <- predict(model, newdata = data, type = "response", se.fit=TRUE,
                          interval = 'prediction', level = level)
        plot <- plot + geom_ribbon(aes(ymin = result$fit[,'lwr'], 
                                       ymax = result$fit[,'upr']), 
                                   col = NA, alpha = .35)
    }
    plot
}
