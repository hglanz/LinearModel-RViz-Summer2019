#' ggLinearModel
#'
#' \pkg{ggLinearModel()} creates a ggplot (or plotly) object. It is designed to create visualizations of various types of linear models. Inserting a datatable, x variable, y variable, and categorical variable will produce a graph with a different least squares line for each level in the categorical variable. You can modify the graph by changing the values for various parameters.
#'
#' @param data a R data frame. tibbles will work
#' @param x variable name of the x (predicting) variable, must be a column name in the data table
#' @param y variable name of the y (response) variable, must be a column name in the data table
#' @param cat variable name of the categorical variable, must be a column name in the data table. Set to NULL
#' @param plotly boolean if they want the graph in plotly. TRUE returns plotly graph, FALSE returns ggplot. Set to FALSE
#' @param same_slope boolean if they want the regression lines to have the same slope. Set to FALSE
#' @param same_intercept boolean if they want the regression lines to have the same intercept Set to FALSE
#' @param poly  a integer value to declare how many polynomial variables the use wants. Set to 1, i.e. no polynomial
#' @param interactions a integer value for if the user wants to include interactions in the polynomial model. Note: only matters when poly > 1 and interactions <= poly. Set to 0
#' @param ci boolean to add a confidence interval to the graph. Set to FALSE
#' @param pi boolean to add a prediction interval to the graph. Set to FALSE
#' @param interactive boolean to make interactive. Set to FALSE
#' @param title a string to change the title of the graph. Set to x name vs y name
#' @param xlabel a string for the label of the x axis. Set to the x variable name
#' @param ylabel a string for the label of the y axis. Set to the y variable name
#' @param legendTitle a string for the label of the legend. Set to the categorical variable name
#' @param level a number between 0-1 for the interval percentage of the confidence and prediction intervals. Set to .95
#' @param coefficients a boolean to print the coefficients of the model. Set to FALSE
#' @param model a boolean to return the model used to create the LSRL. Set to FALSE
#'
#' @return a ggplot or plotly object, and a model object if requested
#' @export
#'
#' @examples
#' ggLinearModel(iris, Sepal.Length, Sepal.Width, Species)
ggLinearModel <- function(data, x, y, cat = NULL, plotly = FALSE, same_slope = FALSE,
                          same_intercept = FALSE, poly = 1, interactions = poly, ci = FALSE, pi = FALSE,
                          interactive = FALSE, title = NULL, xlabel = NULL, ylabel = NULL, legendTitle = NULL,
                          level = .95, coefficients = FALSE, model = FALSE)
{
    if (is_tibble(data))
    {
        data <- as.data.frame(data)
    }
    if (is.null(title))
    {
        title <- paste(as_label(enquo(x)), 'vs.', as_label(enquo(y)))
    }
    if (is.null(xlabel))
    {
        xlabel <- as_label(enquo(x))
    }
    if (is.null(ylabel))
    {
        ylabel <- as_label(enquo(y))
    }
    if (is.null(legendTitle))
    {
        legendTitle <- as_label(enquo(cat))
    }
    if (missing(cat))
    {
        if (poly > 1)
        {
            returned <- rl_poly_no_cat(data, {{x}}, {{y}}, plotly = plotly, poly = poly, ci = ci, pi = pi, interactive = interactive, title = title, xlabel = xlabel, ylabel = ylabel, level = level, coefficients = coefficients, model = model)
        } else
        {
            returned <- rl_no_cat(data, {{x}}, {{y}}, plotly = plotly, ci = ci, pi = pi, interactive = interactive, title = title, xlabel = xlabel, ylabel = ylabel, level = level, coefficients = coefficients, model = model)
        }
    } else if (poly > 1)
    {
        if (same_slope == FALSE)
        {
            if (same_intercept == FALSE)
            {
                returned <- rl_poly_full_model(data, {{x}}, {{y}}, {{cat}}, plotly = plotly, poly = poly, interactions = interactions, ci = ci, pi = pi, interactive = interactive, title = title, xlabel = xlabel, ylabel = ylabel, legendTitle = legendTitle, level = level, coefficients = coefficients, model = model)
            } else
            {
                returned <- rl_poly_same_intercept(data, {{x}}, {{y}}, {{cat}}, plotly = plotly, poly = poly, interactions = interactions, ci = ci, pi = pi, interactive = interactive, title = title, xlabel = xlabel, ylabel = ylabel, legendTitle = legendTitle, level = level, coefficients = coefficients, model = model)
            }
        } else
        {
            if (same_intercept == FALSE)
            {
                returned <- rl_poly_same_slope(data, {{x}}, {{y}}, {{cat}}, plotly = plotly, poly = poly, ci = ci, pi = pi, interactive = interactive, title = title, xlabel = xlabel, ylabel = ylabel, legendTitle = legendTitle, level = level, coefficients = coefficients, model = model)
            } else
            {
                returned <- rl_poly_same_line(data, {{x}}, {{y}}, {{cat}}, plotly = plotly, poly = poly, ci = ci, pi = pi, interactive = interactive, title = title, xlabel = xlabel, ylabel = ylabel, legendTitle = legendTitle, level = level, coefficients = coefficients, model = model)
            }
        }
    } else
    {
        if (same_slope == FALSE)
        {
            if (same_intercept == FALSE)
            {
                returned <- rl_full_model(data, {{x}}, {{y}}, {{cat}}, plotly = plotly, ci = ci, pi = pi, interactive = interactive, title = title, xlabel = xlabel, ylabel = ylabel, legendTitle = legendTitle, level = level, coefficients = coefficients, model = model)
            } else
            {
                returned <- rl_same_intercept(data, {{x}}, {{y}}, {{cat}}, plotly = plotly, ci = ci, pi = pi, interactive = interactive, title = title, xlabel = xlabel, ylabel = ylabel, legendTitle = legendTitle, level = level, coefficients = coefficients, model = model)
            }
        } else
        {
            if (same_intercept == FALSE)
            {
                returned <- rl_same_slope(data, {{x}}, {{y}}, {{cat}}, plotly = plotly, ci = ci, pi = pi, interactive = interactive, title = title, xlabel = xlabel, ylabel = ylabel, legendTitle = legendTitle, level = level, coefficients = coefficients, model = model)
            } else
            {
                returned <- rl_same_line(data, {{x}}, {{y}}, {{cat}}, plotly = plotly, ci = ci, pi = pi, interactive = interactive, title = title, xlabel = xlabel, ylabel = ylabel, legendTitle = legendTitle, level = level, coefficients = coefficients, model = model)
            }
        }
    }
    return(returned)
}

rl_poly_full_model <- function(data, x, y, cat, poly, interactions = poly, plotly = FALSE, ci = FALSE,
                               pi = FALSE, interactive = FALSE,
                               title = paste(x, 'vs.', y), xlabel = x, ylabel = y, legendTitle = cat,
                               level = .95, coefficients = FALSE, model = FALSE)
{
    if (is_tibble(data))
    {
        data <- as.data.frame(data)
    }
    xvar <- data %>% pull({{x}})
    yvar <- data%>% pull({{y}})
    catvar <- as.factor(as.character(data %>% pull({{cat}})))
    x <- as_label(enquo(x))
    y <- as_label(enquo(y))
    cat <- as_label(enquo(cat))
    if (interactions > 0)
    {
        if (length(xvar) == length(yvar) && length(yvar) == length(catvar))
        {
            modString <- paste('yvar ~ poly(xvar, degrees = poly, raw = TRUE) + catvar +')
            for (i in 1:interactions)
            {
                modString <- paste(modString, 'catvar:I(xvar^', i, ')')
                if (i != interactions)
                {
                    modString <- paste(modString, '+')
                }
            }
            mod <- lm(eval(parse(text = modString)), data = data)
            b <- mod$coefficients[1]
            fit <- mod$model
            fit$data_id <- rownames(fit)
            data$tooltip <- paste0(fit$data_id, "\n",
                                   x, " = ", fit[['I(xvar^1)']], "\n",
                                   y, " = ", fit[['yvar']], "\n",
                                   cat, " = ", fit[['catvar']])

            for (i in 1:(length(mod$coefficients)))
            {
                if (is.na(mod$coefficients[i]))
                {
                    if (i >= (poly+length(levels(catvar))))
                    {
                        j <- i - poly - length(levels(catvar))
                        if (j/(length(levels(catvar))) != round(j/(length(levels(catvar)))))
                        {
                            mod$coefficients[i] <- 0
                            print('Some aspects of this model may be incorrect. The model is missing a few coefficients')
                        }
                    } else
                    {
                        mod$coefficients[i] <- 0
                        print('Some aspects of this model may be incorrect. The model is missing a few coefficients')

                    }
                }
            }
            plot <- ggplot()

            if (ci == TRUE)
            {
                plot <- add_ci_poly(plot, data, {{x}}, catvar, mod, level = level)
            }
            if (pi == TRUE)
            {
                plot <- add_pi_poly(plot, data, {{x}}, catvar, mod, level = level)
            }

            if (!plotly)
            {
                xvecs <- list()
                yvecs <- list()

                xvecs[[length(levels(catvar))]] <- seq(from = min(data[catvar == levels(catvar)[length(levels(catvar))],]%>% pull({{x}})),
                                                       to = max(data[catvar == levels(catvar)[length(levels(catvar))],]%>% pull({{x}})),
                                                       by = ((max(data[catvar == levels(catvar)[length(levels(catvar))],]%>% pull({{x}}))-
                                                                  min(data[catvar == levels(catvar)[length(levels(catvar))],]%>% pull({{x}})))/1001))

                slopeString <- ''
                for (i in 1:poly)
                {
                    coef <- paste("'poly(xvar, degrees = poly, raw = TRUE)", i, "'", sep = '')
                    newSlopeString <- paste('mod$coefficients[', coef, "]*xvecs[[length(levels(catvar))]]^", i, sep = '')
                    slopeString <- paste(slopeString, newSlopeString, sep = ' + ')
                }
                catEff <- mod$coefficients[paste('catvar', levels(catvar)[length(levels(catvar))], sep = '')]
                yvecs[[length(levels(catvar))]] <- b + catEff + eval(parse(text = slopeString))

                tooltips <- list()
                for (i in 1:length(levels(catvar)))
                {
                    tooltips[[i]] <- paste(levels(catvar)[i], '\n',
                                           y, '=', sep = ' ')
                    if (i != 1)
                    {
                        tooltips[[i]] <- paste(tooltips[[i]],
                                               sprintf('%.3f', b + mod$coef[poly + i]), sep = ' ')
                    } else
                    {
                        tooltips[[i]] <- paste(tooltips[[i]],
                                               sprintf('%.3f', b), sep = ' ')
                    }
                    for (j in 1:poly)
                    {
                        coef <- paste('catvar', levels(catvar)[i], ':I(xvar^', j, ')', sep = '')

                        if (j != 1)
                        {
                            if (j > interactions || i == length(levels(catvar)))
                            {
                                tooltips[[i]] <- paste(tooltips[[i]], ' + ',
                                                       sprintf("%.3f", mod$coefficients[1+j]),
                                                       '*', x, '^', j, sep = '')
                            } else
                            {
                                tooltips[[i]] <- paste(tooltips[[i]], ' + ',
                                                       sprintf("%.3f", mod$coefficients[1+j]+mod$coefficients[coef]),
                                                       '*', x, '^', j, sep = '')
                            }
                        } else
                        {
                            if (i != length(levels(catvar)))
                            {
                                tooltips[[i]] <- paste(tooltips[[i]], ' + ',
                                                       sprintf("%.3f", mod$coefficients[2]+mod$coefficients[coef]),
                                                       '*', x, sep = '')
                            } else
                            {
                                tooltips[[i]] <- paste(tooltips[[i]], ' + ',
                                                       sprintf("%.3f", mod$coefficients[2]),
                                                       '*', x, sep = '')
                            }
                        }
                    }
                }

                plot <- plot + geom_point_interactive(data = data,
                                                      aes(x = xvar, y = yvar,
                                                          col = catvar, tooltip = tooltip)) +
                    geom_path_interactive(aes(x = xvecs[[length(levels(catvar))]],
                                              y = yvecs[[length(levels(catvar))]],
                                              color = levels(catvar)[length(levels(catvar))]),
                                          tooltip = tooltips[[length(levels(catvar))]])

                slopeString <- ''
                for (i in 1:poly)
                {
                    coef <- paste("'poly(xvar, degrees = poly, raw = TRUE)", i, "'", sep = '')
                    newSlopeString <- paste('mod$coefficients[', coef, "]*xvecs[[i]]^", i, sep = '')
                    slopeString <- paste(slopeString, newSlopeString, sep = ' + ')
                }

                for (i in 1:(length(levels(catvar))-1))
                {
                    xvecs[[i]] <- seq(from = min(data[catvar == levels(catvar)[i],]%>% pull({{x}})),
                                      to = max(data[catvar == levels(catvar)[i],]%>% pull({{x}})),
                                      by = (max(data[catvar == levels(catvar)[i],]%>% pull({{x}}))-
                                                min(data[catvar == levels(catvar)[i],]%>% pull({{x}})))/1001)

                    interactionsString <- ''
                    for (j in 1:interactions)
                    {
                        coef <- poly + length(levels(catvar))*j + i
                        newInteractionsString <- paste('mod$coefficients[', coef, "]*(xvecs[[", i,"]]^", j, ')', sep = '')
                        interactionsString <- paste(interactionsString, newInteractionsString, sep = ' + ')
                    }

                    if (i == 1)
                    {
                        yvecs[[i]] <- b + eval(parse(text = slopeString)) + eval(parse(text = interactionsString))
                        plot <- plot + geom_path_interactive(aes_string(x = xvecs[[i]],
                                                                        y = yvecs[[i]],
                                                                        color = shQuote(levels(catvar)[i])),
                                                             tooltip = tooltips[[i]])

                    } else
                    {
                        yvecs[[i]] <- b + mod$coefficients[paste('catvar', levels(catvar)[i], sep = '')] + eval(parse(text = slopeString)) + eval(parse(text = interactionsString))
                        plot <- plot + geom_path_interactive(aes_string(x = xvecs[[i]],
                                                                        y = yvecs[[i]],
                                                                        color = shQuote(levels(catvar)[i])),
                                                             tooltip = tooltips[[i]])
                    }
                }
            } else
            {
                slopeString <- ''
                for (i in 1:poly)
                {
                    coef <- paste("'poly(xvar, degrees = poly, raw = TRUE)", i, "'", sep = '')
                    newSlopeString <- paste('mod$coefficients[', coef, "]*x^", i, sep = '')
                    slopeString <- paste(slopeString, newSlopeString, sep = ' + ')
                }

                catEff <- mod$coefficients[paste('catvar', levels(catvar)[length(levels(catvar))], sep = '')]
                plot <- plot +
                    geom_point(data = data, aes(x = xvar, y = yvar, col = catvar)) +
                    stat_function(fun = function(x) b + catEff + eval(parse(text = slopeString)),
                                  aes(col = levels(catvar)[length(levels(catvar))]),
                                  xlim = c(min(data[data %>% pull({{cat}}) == levels(catvar)[length(levels(catvar))],]%>% pull({{x}})),
                                           max(data[data %>% pull({{cat}}) == levels(catvar)[length(levels(catvar))],]%>% pull({{x}}))))

                statFunctions <- list()
                catEffects <- list()
                for (i in 1:(length(levels(catvar))-1))
                {
                    if (i != 1)
                    {
                        catEffects[[i]] <- mod$coefficients[paste('catvar', levels(catvar)[i], sep = '')]
                    } else
                    {
                        catEffects[[i]] <- 0
                    }
                    statFunctions[[i]] <- paste('stat_function(aes(color = levels(catvar)[',
                                                i, ']), fun = function(x) b + catEffects[[',
                                                i, ']] + eval(parse(text = slopeString))', sep = '')
                    for (j in 1:interactions)
                    {
                        statFunctions[[i]] <- paste(statFunctions[[i]],
                                                    " + mod$coefficients['catvar",
                                                    levels(catvar)[i], ":I(xvar^",
                                                    j, ")']*x^", j, sep = '')
                    }
                    statFunctions[[i]] <- paste(statFunctions[[i]],
                                                ', xlim = c(min(data[data %>% pull({{cat}}) == levels(catvar)[', i,
                                                '],]%>% pull({{x}})),max(data[data %>% pull({{cat}}) == levels(catvar)[', i,
                                                '],]%>% pull({{x}}))))', sep = '')
                }

                for (i in 1:(length(levels(catvar))-1))
                {
                    plot <- plot + eval(parse(text = statFunctions[[i]]))
                }
            }

        } else
        {
            stop('Please enter valid parameters')
        }

        plot <- plot + ggtitle(title) + xlab(xlabel) + ylab(ylabel) + labs(color = legendTitle, fill = legendTitle)

        if ((model == TRUE) || (coefficients == TRUE))
        {
            nameList <- list()
            nameList[[1]] <- '(Intercept)'
            nameList[[2]] <- x
            j <- 2
            polys <- list()
            polys[[1]] <- x
            for (i in 3:(poly+1))
            {
                polys[[i-1]] <- paste(x, '^', j, sep = '')
                nameList[[i]] <- polys[[i-1]]
                j <- j + 1
            }
            levels <- list()
            for (i in 1:length(levels(catvar)))
            {
                levels[[i]] <- paste(cat, levels(catvar)[i], sep = '')
            }
            for (i in (poly+2):(poly+length(levels(catvar))))
            {
                nameList[[i]] <- levels[[i-poly]]
            }
            j <- 1
            k <- 1
            for (i in (1+poly+length(levels(catvar))):length(mod$coef))
            {
                coef <- paste(levels[[j]], ':', polys[[k]],sep = '')
                nameList[[i]] <- coef
                j <- j + 1
                if (j > length(levels(catvar)))
                {
                    j <- 1
                    k <- k + 1
                }
            }

            names(mod$coefficients) <- nameList
            if (model == FALSE)
            {
                print(mod$coefficients)
            }
        }

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
    } else
    {
        plot <- rl_poly_same_line(data, x, y, cat, poly, plotly = plotly, ci = ci, pi = pi,
                                  interactive = interactive, title = title, xlabel = xlabel, ylabel = ylabel,
                                  legendTitle = legendTitle, level = level, coefficients = coefficients)
    }
    if (model == TRUE)
    {
        return(list(model = mod, plot = plot))
    } else
    {
        plot
    }
}

rl_poly_same_intercept <- function(data, x, y, cat, poly, interactions = poly, plotly = FALSE, ci = FALSE,
                                   pi = FALSE, interactive = FALSE,
                                   title = paste(x, 'vs.', y), xlabel = x, ylabel = y, legendTitle = cat,
                                   level = .95, coefficients = FALSE, model = FALSE)
{
    if (is_tibble(data))
    {
        data <- as.data.frame(data)
    }
    xvar <- data %>% pull({{x}})
    yvar <- data %>% pull({{y}})
    catvar <- as.factor(as.character(data %>% pull({{cat}})))
    x <- as_label(enquo(x))
    y <- as_label(enquo(y))
    cat <- as_label(enquo(cat))
    if (interactions > 0)
    {
        if (length(xvar) == length(yvar) && length(yvar) == length(catvar))
        {
            modString <- paste('yvar ~ poly(xvar, degrees = poly, raw = TRUE)+')
            for (i in 1:interactions)
            {
                modString <- paste(modString, 'catvar:I(xvar^', i, ')')
                if (i != interactions)
                {
                    modString <- paste(modString, '+')
                }
            }
            mod <- lm(eval(parse(text = modString)), data = data)
            b <- mod$coefficients[1]
            fit <- mod$model
            fit$data_id <- rownames(fit)
            data$tooltip <- paste0(fit$data_id, "\n", x, " = ", fit[['I(xvar^1)']], "\n", y, " = ", fit[['yvar']], "\n", cat, " = ", fit[['catvar']])

            for (i in 1:(length(mod$coefficients)))
            {
                if (is.na(mod$coefficients[i]))
                {
                    if (i >= poly)
                    {
                        j <- i - poly -1
                        if (j/(length(levels(catvar))) != round(j/(length(levels(catvar)))))
                        {
                            mod$coefficients[i] <- 0
                            print('Some aspects of this model may be incorrect. The model is missing a few coefficients')
                        }
                    } else
                    {
                        mod$coefficients[i] <- 0
                        print('Some aspects of this model may be incorrect. The model is missing a few coefficients')

                    }
                }
            }

            plot <- ggplot()

            if (ci == TRUE)
            {
                plot <- add_ci_poly(plot, data, {{x}}, catvar, mod, level = level)
            }
            if (pi == TRUE)
            {
                plot <- add_pi_poly(plot, data, {{x}}, catvar, mod, level = level)
            }

            if (!plotly)
            {
                xvecs <- list()
                yvecs <- list()

                xvecs[[length(levels(catvar))]] <- seq(from = min(data[catvar == levels(catvar)[length(levels(catvar))],]%>% pull({{x}})),
                                                       to = max(data[catvar == levels(catvar)[length(levels(catvar))],]%>% pull({{x}})),
                                                       by = ((max(data[catvar == levels(catvar)[length(levels(catvar))],]%>% pull({{x}}))-
                                                                  min(data[catvar == levels(catvar)[length(levels(catvar))],]%>% pull({{x}})))/1001))

                slopeString <- ''
                for (i in 1:poly)
                {
                    coef <- paste("'poly(xvar, degrees = poly, raw = TRUE)", i, "'", sep = '')
                    newSlopeString <- paste('mod$coefficients[', coef, "]*xvecs[[length(levels(catvar))]]^", i, sep = '')
                    slopeString <- paste(slopeString, newSlopeString, sep = ' + ')
                }

                yvecs[[length(levels(catvar))]] <- b + eval(parse(text = slopeString))

                tooltips <- list()
                for (i in 1:length(levels(catvar)))
                {
                    tooltips[[i]] <- paste(levels(catvar)[i], '\n',
                                           y, '=', sprintf('%.3f', b), sep = ' ')
                    for (j in 1:poly)
                    {
                        coef <- paste('catvar', levels(catvar)[i], ':I(xvar^', j, ')', sep = '')

                        if (j != 1)
                        {
                            if (j > interactions || i == length(levels(catvar)))
                            {
                                tooltips[[i]] <- paste(tooltips[[i]], ' + ',
                                                       sprintf("%.3f", mod$coefficients[1+j]),
                                                       '*', x, '^', j, sep = '')
                            } else
                            {
                                tooltips[[i]] <- paste(tooltips[[i]], ' + ',
                                                       sprintf("%.3f", mod$coefficients[1+j]+mod$coefficients[coef]),
                                                       '*', x, '^', j, sep = '')
                            }
                        } else
                        {
                            if (i != length(levels(catvar)))
                            {
                                tooltips[[i]] <- paste(tooltips[[i]], ' + ',
                                                       sprintf("%.3f", mod$coefficients[2]+mod$coefficients[coef]),
                                                       '*', x, sep = '')
                            } else
                            {
                                tooltips[[i]] <- paste(tooltips[[i]], ' + ',
                                                       sprintf("%.3f", mod$coefficients[2]),
                                                       '*', x, sep = '')
                            }
                        }
                    }
                }

                plot <- plot + geom_point_interactive(data = data,
                                                      aes(x = xvar, y = yvar,
                                                          col = catvar, tooltip = tooltip)) +
                    geom_path_interactive(aes(x = xvecs[[length(levels(catvar))]],
                                              y = yvecs[[length(levels(catvar))]],
                                              color = levels(catvar)[length(levels(catvar))]),
                                          tooltip = tooltips[[length(levels(catvar))]])

                slopeString <- ''
                for (i in 1:poly)
                {
                    coef <- paste("'poly(xvar, degrees = poly, raw = TRUE)", i, "'", sep = '')
                    newSlopeString <- paste('mod$coefficients[', coef, "]*xvecs[[i]]^", i, sep = '')
                    slopeString <- paste(slopeString, newSlopeString, sep = ' + ')
                }

                for (i in 1:(length(levels(catvar))-1))
                {
                    xvecs[[i]] <- seq(from = min(data[catvar == levels(catvar)[i],]%>% pull({{x}})),
                                      to = max(data[catvar == levels(catvar)[i],]%>% pull({{x}})),
                                      by = (max(data[catvar == levels(catvar)[i],]%>% pull({{x}}))-
                                                min(data[catvar == levels(catvar)[i],]%>% pull({{x}})))/1001)

                    interactionsString <- ''
                    for (j in 1:interactions)
                    {
                        coef <- paste("'catvar", levels(catvar)[i], ":I(xvar^", j, ")'", sep = '')
                        newInteractionsString <- paste('mod$coefficients[', coef, "]*(xvecs[[", i,"]]^", j, ')', sep = '')
                        interactionsString <- paste(interactionsString, newInteractionsString, sep = ' + ')
                    }

                    yvecs[[i]] <- b + eval(parse(text = slopeString)) + eval(parse(text = interactionsString))
                    plot <- plot + geom_path_interactive(aes_string(x = xvecs[[i]],
                                                                    y = yvecs[[i]],
                                                                    color = shQuote(levels(catvar)[i])),
                                                         tooltip = tooltips[[i]])
                }
            } else
            {
                slopeString <- ''
                for (i in 1:poly)
                {
                    coef <- paste("'poly(xvar, degrees = poly, raw = TRUE)", i, "'", sep = '')
                    newSlopeString <- paste('mod$coefficients[', coef, "]*x^", i, sep = '')
                    slopeString <- paste(slopeString, newSlopeString, sep = ' + ')
                }

                plot <- ggplot(data = data, aes(x = xvar, y = yvar, col = catvar)) +
                    geom_point() +
                    stat_function(fun = function(x) b + eval(parse(text = slopeString)),
                                  aes(col = levels(catvar)[length(levels(catvar))]),
                                  xlim = c(min(data[data %>% pull({{cat}}) == levels(catvar)[length(levels(catvar))],]%>% pull({{x}})),
                                           max(data[data %>% pull({{cat}}) == levels(catvar)[length(levels(catvar))],]%>% pull({{x}}))))

                statFunctions <- list()
                for (i in 1:(length(levels(catvar))-1))
                {
                    statFunctions[[i]] <- paste('stat_function(aes(color = levels(catvar)[',
                                                i, ']), fun = function(x) b + eval(parse(text = slopeString))')
                    for (j in 1:interactions)
                    {
                        statFunctions[[i]] <- paste(statFunctions[[i]],
                                                    " + mod$coefficients['catvar",
                                                    levels(catvar)[i], ":I(xvar^",
                                                    j, ")']*x^", j, sep = '')
                    }
                    statFunctions[[i]] <- paste(statFunctions[[i]],
                                                ', xlim = c(min(data[data %>% pull({{cat}}) == levels(catvar)[', i,
                                                '],]%>% pull({{x}})),max(data[data %>% pull({{cat}}) == levels(catvar)[', i,
                                                '],]%>% pull({{x}}))))', sep = '')
                }

                for (i in 1:(length(levels(catvar))-1))
                {
                    plot <- plot + eval(parse(text = statFunctions[[i]]))
                }
            }
        } else
        {
            stop('Please enter valid parameters')
        }

        plot <- plot + ggtitle(title) + xlab(xlabel) + ylab(ylabel) + labs(color = legendTitle, fill = legendTitle)

        if ((model == TRUE) || (coefficients == TRUE))
        {
            nameList <- list()
            nameList[[1]] <- '(Intercept)'
            nameList[[2]] <- x
            j <- 2
            polys <- list()
            polys[[1]] <- x
            for (i in 3:(poly+1))
            {
                polys[[i-1]] <- paste(x, '^', j, sep = '')
                nameList[[i]] <- polys[[i-1]]
                j <- j + 1
            }
            levels <- list()
            for (i in 1:length(levels(catvar)))
            {
                levels[[i]] <- paste(cat, levels(catvar)[i], sep = '')
            }
            j <- 1
            k <- 1
            for (i in (poly+2):length(mod$coef))
            {
                coef <- paste(levels[[j]], ':', polys[[k]],sep = '')
                nameList[[i]] <- coef
                j <- j + 1
                if (j > length(levels(catvar)))
                {
                    j <- 1
                    k <- k + 1
                }
            }

            names(mod$coefficients) <- nameList
            if (model == FALSE)
            {
                print(mod$coefficients)
            }
        }

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
    } else
    {
        plot <- rl_poly_same_line(data, x, y, cat, poly, plotly)
    }
    if (model == TRUE)
    {
        return(list(model = mod, plot = plot))
    } else
    {
        plot
    }
}

rl_poly_same_slope <- function(data, x, y, cat, poly, plotly = FALSE, ci = FALSE,
                               pi = FALSE, interactive = FALSE,
                               title = paste(x, 'vs.', y), xlabel = x, ylabel = y, legendTitle = cat,
                               level = .95, coefficients = FALSE, model = FALSE)
{
    if (is_tibble(data))
    {
        data <- as.data.frame(data)
    }
    xvar <- data %>% pull({{x}})
    yvar <- data %>% pull({{y}})
    catvar <- as.factor(as.character(data %>% pull({{cat}})))
    x <- as_label(enquo(x))
    y <- as_label(enquo(y))
    cat <- as_label(enquo(cat))
    if (length(xvar) == length(yvar) && length(yvar) == length(catvar))
    {
        plot <- ggplot()

        mod <- lm(yvar ~ poly(xvar, degrees = poly, raw = TRUE) + catvar, data = data)
        b <- mod$coefficients[1]
        fit <- mod$model
        fit$data_id <- rownames(fit)
        data$tooltip <- paste0(fit$data_id, "\n",
                               x, " = ", fit[,2][,1], "\n",
                               y, " = ", fit[['yvar']], "\n",
                               cat, " = ", fit[['catvar']])

        for (i in 1:(length(mod$coefficients)))
        {
            if (is.na(mod$coefficients[i]))
            {
                if (i >= (poly+length(levels(catvar))))
                {
                    j <- i - poly - length(levels(catvar))
                    if (j/(length(levels(catvar))) != round(j/(length(levels(catvar)))))
                    {
                        mod$coefficients[i] <- 0
                        print('Some aspects of this model may be incorrect. The model is missing a few coefficients')
                    }
                } else
                {
                    mod$coefficients[i] <- 0
                    print('Some aspects of this model may be incorrect. The model is missing a few coefficients')

                }
            }
        }

        if (ci == TRUE)
        {
            plot <- add_ci_poly(plot, data, {{x}}, catvar, mod, level = level)
        }
        if (pi == TRUE)
        {
            plot <- add_pi_poly(plot, data, {{x}}, catvar, mod, level = level)
        }

        if (!plotly)
        {
            xvecs <- list()
            yvecs <- list()

            xvecs[[length(levels(catvar))]] <- seq(from = min(data[catvar == levels(catvar)[length(levels(catvar))],]%>% pull({{x}})),
                                                   to = max(data[catvar == levels(catvar)[length(levels(catvar))],]%>% pull({{x}})),
                                                   by = ((max(data[catvar == levels(catvar)[length(levels(catvar))],]%>% pull({{x}}))-
                                                              min(data[catvar == levels(catvar)[length(levels(catvar))],]%>% pull({{x}})))/1001))

            slopeString <- ''
            for (i in 1:poly)
            {
                coef <- paste("'poly(xvar, degrees = poly, raw = TRUE)", i, "'", sep = '')
                newSlopeString <- paste('mod$coefficients[', coef, "]*xvecs[[length(levels(catvar))]]^", i, sep = '')
                slopeString <- paste(slopeString, newSlopeString, sep = ' + ')
            }

            catEff <- mod$coefficients[paste('catvar', levels(catvar)[length(levels(catvar))], sep = '')]
            yvecs[[length(levels(catvar))]] <- b + catEff + eval(parse(text = slopeString))

            tooltips <- list()
            for (i in 1:length(levels(catvar)))
            {
                tooltips[[i]] <- paste(levels(catvar)[i], '\n',
                                       y, '=', sep = ' ')
                if (i != 1)
                {
                    tooltips[[i]] <- paste(tooltips[[i]],
                                           sprintf('%.3f', b + mod$coef[poly + i]), sep = ' ')
                } else
                {
                    tooltips[[i]] <- paste(tooltips[[i]],
                                           sprintf('%.3f', b), sep = ' ')
                }
                tooltips[[i]] <- paste(tooltips[[i]], ' + ',
                                       sprintf("%.3f", mod$coefficients[2]),
                                       '*', x, sep = '')
                for (j in 2:poly)
                {
                    tooltips[[i]] <- paste(tooltips[[i]], ' + ',
                                           sprintf("%.3f", mod$coefficients[1+j]),
                                           '*', x, '^', j, sep = '')
                }
            }

            plot <- plot + geom_point_interactive(data = data,
                                                  aes(x = xvar, y = yvar,
                                                      col = catvar, tooltip = tooltip)) +
                geom_path_interactive(aes(x = xvecs[[length(levels(catvar))]],
                                          y = yvecs[[length(levels(catvar))]],
                                          color = levels(catvar)[length(levels(catvar))]),
                                      tooltip = tooltips[[length(levels(catvar))]])

            slopeString <- ''
            for (i in 1:poly)
            {
                coef <- paste("'poly(xvar, degrees = poly, raw = TRUE)", i, "'", sep = '')
                newSlopeString <- paste('mod$coefficients[', coef, "]*xvecs[[i]]^", i, sep = '')
                slopeString <- paste(slopeString, newSlopeString, sep = ' + ')
            }

            for (i in 1:(length(levels(catvar))-1))
            {
                xvecs[[i]] <- seq(from = min(data[catvar == levels(catvar)[i],]%>% pull({{x}})),
                                  to = max(data[catvar == levels(catvar)[i],]%>% pull({{x}})),
                                  by = (max(data[catvar == levels(catvar)[i],]%>% pull({{x}}))-
                                            min(data[catvar == levels(catvar)[i],]%>% pull({{x}})))/1001)

                if (i == 1)
                {
                    yvecs[[i]] <- b + eval(parse(text = slopeString))
                    plot <- plot + geom_path_interactive(aes_string(x = xvecs[[i]],
                                                                    y = yvecs[[i]],
                                                                    color = shQuote(levels(catvar)[i])),
                                                         tooltip = tooltips[[i]])

                } else
                {
                    yvecs[[i]] <- b + mod$coefficients[paste('catvar', levels(catvar)[i], sep = '')] + eval(parse(text = slopeString))
                    plot <- plot + geom_path_interactive(aes_string(x = xvecs[[i]],
                                                                    y = yvecs[[i]],
                                                                    color = shQuote(levels(catvar)[i])),
                                                         tooltip = tooltips[[i]])
                }
            }
        } else
        {
            slopeString <- ''
            for (i in 1:poly)
            {
                coef <- paste("'poly(xvar, degrees = poly, raw = TRUE)", i, "'", sep = '')
                newSlopeString <- paste('mod$coefficients[', coef, "]*x^", i, sep = '')
                slopeString <- paste(slopeString, newSlopeString, sep = ' + ')
            }

            plot <- geom_point(data = data, aes(x = xvar, y = yvar, col = catvar)) +
                stat_function(fun = function(x) b + eval(parse(text = slopeString)),
                              aes(col = levels(catvar)[1]),
                              xlim = c(min(data[data %>% pull({{cat}}) == levels(catvar)[1],]%>% pull({{x}})),
                                       max(data[data %>% pull({{cat}}) == levels(catvar)[1],]%>% pull({{x}}))))

            statFunctions <- list()
            for (i in 1:(length(levels(catvar))-1))
            {
                statFunctions[i] <- paste("stat_function(fun = function(x) b + eval(parse(text = slopeString)) + mod$coefficients[", i, "+1+", poly, "], aes(col = (levels(catvar)[", i, "+1]))")
                statFunctions[i] <- paste(statFunctions[i],
                                          ', xlim = c(min(data[data %>% pull({{cat}}) == levels(catvar)[1+', i,
                                          '],]%>% pull({{x}})),max(data[data %>% pull({{cat}}) == levels(catvar)[1+', i,
                                          '],]%>% pull({{x}}))))', sep = '')
            }

            for (i in 1:(length(levels(catvar))-1))
            {
                plot <- plot + eval(parse(text = statFunctions[i]))
            }
        }


        plot <- plot + ggtitle(title) + xlab(xlabel) + ylab(ylabel) + labs(color = legendTitle, fill = legendTitle)

        if ((model == TRUE) || (coefficients == TRUE))
        {
            nameList <- list()
            nameList[[1]] <- '(Intercept)'
            nameList[[2]] <- x
            j <- 2
            polys <- list()
            polys[[1]] <- x
            for (i in 3:(poly+1))
            {
                polys[[i-1]] <- paste(x, '^', j, sep = '')
                nameList[[i]] <- polys[[i-1]]
                j <- j + 1
            }
            levels <- list()
            for (i in 1:length(levels(catvar)))
            {
                levels[[i]] <- paste(cat, levels(catvar)[i], sep = '')
            }
            for (i in (poly+2):(poly+length(levels(catvar))))
            {
                nameList[[i]] <- levels[[i-poly]]
            }

            names(mod$coefficients) <- nameList

            if (model == FALSE)
            {
                print(mod$coefficients)
            }
        }

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

    } else
    {
        stop('Please enter valid parameters')
    }

    if (model == TRUE)
    {
        return(list(model = mod, plot = plot))
    } else
    {
        plot
    }
}

rl_poly_same_line <- function(data, x, y, cat, poly, plotly = FALSE, ci = FALSE,
                              pi = FALSE, interactive = FALSE,
                              title = paste(x, 'vs.', y), xlabel = x, ylabel = y, legendTitle = cat,
                              level = .95, coefficients = FALSE, model = FALSE)
{
    if (is_tibble(data))
    {
        data <- as.data.frame(data)
    }
    xvar <- data %>% pull({{x}})
    yvar <- data %>% pull({{y}})
    catvar <- as.factor(as.character(data %>% pull({{cat}})))
    cat <- as_label(enquo(cat))
    x <- as_label(enquo(x))
    y <- as_label(enquo(y))
    if (length(xvar) == length(yvar))
    {
        plot <- ggplot()

        mod <- lm(yvar ~ poly(xvar, degrees = poly, raw = TRUE), data = data)

        if (ci == TRUE)
        {
            plot <- add_ci_poly(plot, data, {{x}}, catvar, mod, level = level, one_line = TRUE)
        }
        if (pi == TRUE)
        {
            plot <- add_pi_poly(plot, data, {{x}}, catvar, mod, level = level, one_line = TRUE)
        }

        b <- mod$coefficients[1]
        fit <- mod$model
        fit$data_id <- rownames(fit)
        data$tooltip <- paste0(fit$data_id, "\n",
                               x, " = ", fit[,2][,1], "\n",
                               y, " = ", fit[['yvar']])


        for (i in 1:(length(mod$coefficients)))
        {
            if (is.na(mod$coefficients[i]))
            {
                if (i >= (poly+length(levels(catvar))))
                {
                    j <- i - poly - length(levels(catvar))
                    if (j/(length(levels(catvar))) != round(j/(length(levels(catvar)))))
                    {
                        mod$coefficients[i] <- 0
                        print('Some aspects of this model may be incorrect. The model is missing a few coefficients')
                    }
                } else
                {
                    mod$coefficients[i] <- 0
                    print('Some aspects of this model may be incorrect. The model is missing a few coefficients')

                }
            }
        }

        if (!plotly)
        {
            xvec <- seq(from = min(xvar),
                        to = max(xvar),
                        by = (max(xvar)-min(xvar))/1001)

            yvec <- 0
            tooltip <- paste(y, '=', sprintf("%.3f",b))
            for (i in 1:poly)
            {
                if (i != 1)
                {
                    tooltip <- paste(tooltip, ' + ', sprintf("%.3f",mod$coefficients[1+i]), ' * ', x, '^', i, sep = '')
                } else
                {
                    tooltip <- paste(tooltip, '+', sprintf("%.3f",mod$coefficients[1+i]), '*', x, sep = ' ')                }
                yvec <- yvec + mod$coefficients[1+i]*(xvec^i)
            }
            yvec <- yvec + b

            plot <- plot + geom_point_interactive(data = data,
                                                  aes(x = xvar, y = yvar,
                                                      col = catvar, tooltip = tooltip)) +
                geom_path_interactive(aes(x = xvec,
                                          y = yvec),
                                      color = 'black',
                                      tooltip = tooltip)
        } else
        {
            slopeString <- ''
            for (i in 1:poly)
            {
                coef <- paste("'poly(xvar, degrees = poly, raw = TRUE)", i, "'", sep = '')
                newSlopeString <- paste('mod$coefficients[', coef, "]*x^", i, sep = '')
                slopeString <- paste(slopeString, newSlopeString, sep = ' + ')
            }

            plot <- ggplot(data = data, aes(x = xvar, y = yvar, col = catvar)) +
                geom_point() +
                stat_function(color = 'black', fun = function(x) b + eval(parse(text = slopeString)))
        }
    } else
    {
        stop('Please enter valid parameters')
    }

    plot <- plot + ggtitle(title) + xlab(xlabel) + ylab(ylabel) + labs(color = legendTitle, fill = legendTitle)

    if ((model == TRUE) || (coefficients == TRUE))
    {
        nameList <- list()
        nameList[[1]] <- '(Intercept)'
        nameList[[2]] <- x
        j <- 2
        polys <- list()
        polys[[1]] <- x
        for (i in 3:(poly+1))
        {
            polys[[i-1]] <- paste(x, '^', j, sep = '')
            nameList[[i]] <- polys[[i-1]]
            j <- j + 1
        }

        names(mod$coefficients) <- nameList
        if (model == FALSE)
        {
            print(mod$coefficients)
        }
    }
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

    if (model == TRUE)
    {
        return(list(model = mod, plot = plot))
    } else
    {
        plot
    }
}

rl_poly_no_cat <- function(data, x, y, poly, plotly = FALSE, ci = FALSE,
                           pi = FALSE, interactive = FALSE,
                           title = paste(x, 'vs.', y), xlabel = x, ylabel = y,
                           level = .95, coefficients = FALSE, model = FALSE)
{
    if (is_tibble(data))
    {
        data <- as.data.frame(data)
    }
    xvar <- data %>% pull({{x}})
    yvar <- data %>% pull({{y}})
    x <- as_label(enquo(x))
    y <- as_label(enquo(y))
    if (length(xvar) == length(yvar))
    {
        plot <- ggplot()

        mod <- lm(yvar ~ poly(xvar, degrees = poly, raw = TRUE), data = data)

        if (ci == TRUE)
        {
            xmin <- min(data %>% pull({{x}}))
            xmax <- max(data %>% pull({{x}}))
            xval <- seq(from = xmin, to = xmax, by = (xmax-xmin)/999)
            newdata <- cbind.data.frame(xvar = xval)

            result <- predict(mod, newdata, interval = 'confidence', level = level,
                              type = "response", se.fit = TRUE)

            ymin <- result$fit[,'lwr']
            ymax <- result$fit[,'upr']

            plot <- plot + geom_ribbon(aes(x = xval,
                                           ymin = ymin,
                                           ymax = ymax),
                                       col = NA, alpha = .2)
        }
        if (pi == TRUE)
        {
            xmin <- min(data %>% pull({{x}}))
            xmax <- max(data %>% pull({{x}}))
            xval <- seq(from = xmin, to = xmax, by = (xmax-xmin)/999)
            newdata <- cbind.data.frame(xvar = xval)

            result <- predict(mod, newdata, interval = 'prediction', level = level,
                              type = "response", se.fit = TRUE)

            ymin <- result$fit[,'lwr']
            ymax <- result$fit[,'upr']

            plot <- plot + geom_ribbon(aes(x = xval,
                                           ymin = ymin,
                                           ymax = ymax),
                                       col = NA, alpha = .2)    }

        b <- mod$coefficients[1]
        fit <- mod$model
        fit$data_id <- rownames(fit)
        data$tooltip <- paste0(fit$data_id, "\n",
                               x, " = ", fit[,2][,1], "\n",
                               y, " = ", fit[['yvar']])


        for (i in 1:(length(mod$coefficients)))
        {
            if (is.na(mod$coefficients[i]))
            {
                mod$coefficients[i] <- 0
                print('Some aspects of this model may be incorrect. The model is missing a few coefficients')
            }
        }

        if (!plotly)
        {
            xvec <- seq(from = min(xvar),
                        to = max(xvar),
                        by = (max(xvar)-min(xvar))/1001)

            yvec <- 0
            tooltip <- paste(y, '=', sprintf("%.3f",b))
            for (i in 1:poly)
            {
                if (i != 1)
                {
                    tooltip <- paste(tooltip, ' + ', sprintf("%.3f",mod$coefficients[1+i]), ' * ', x, '^', i, sep = '')
                } else
                {
                    tooltip <- paste(tooltip, '+', sprintf("%.3f",mod$coefficients[1+i]), '*', x, sep = ' ')                }
                yvec <- yvec + mod$coefficients[1+i]*(xvec^i)
            }
            yvec <- yvec + b

            plot <- plot + geom_point_interactive(data = data,
                                                  aes(x = xvar, y = yvar,
                                                      tooltip = tooltip)) +
                geom_path_interactive(aes(x = xvec,
                                          y = yvec),
                                      color = 'black',
                                      tooltip = tooltip)
        } else
        {
            slopeString <- ''
            for (i in 1:poly)
            {
                coef <- paste("'poly(xvar, degrees = poly, raw = TRUE)", i, "'", sep = '')
                newSlopeString <- paste('mod$coefficients[', coef, "]*x^", i, sep = '')
                slopeString <- paste(slopeString, newSlopeString, sep = ' + ')
            }

            plot <- ggplot(data = data, aes(x = xvar, y = yvar)) +
                geom_point() +
                stat_function(color = 'black', fun = function(x) b + eval(parse(text = slopeString)))
        }
    } else
    {
        stop('Please enter valid parameters')
    }

    plot <- plot + ggtitle(title) + xlab(xlabel) + ylab(ylabel)

    if ((model == TRUE) || (coefficients == TRUE))
    {
        nameList <- list()
        nameList[[1]] <- '(Intercept)'
        nameList[[2]] <- x
        j <- 2
        polys <- list()
        polys[[1]] <- x
        for (i in 3:(poly+1))
        {
            polys[[i-1]] <- paste(x, '^', j, sep = '')
            nameList[[i]] <- polys[[i-1]]
            j <- j + 1
        }

        names(mod$coefficients) <- nameList
        if (model == FALSE)
        {
            print(mod$coefficients)
        }
    }
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

    if (model == TRUE)
    {
        return(list(model = mod, plot = plot))
    } else
    {
        plot
    }
}

rl_full_model <- function(data, x, y, cat, plotly = FALSE, ci = FALSE, pi = FALSE, interactive = FALSE,
                          title = paste(x, 'vs.', y), xlabel = x, ylabel = y, legendTitle = cat,
                          level = .95, coefficients = FALSE, model = FALSE)
{
    if (is_tibble(data))
    {
        data <- as.data.frame(data)
    }
    xvar <- data %>% pull({{x}})
    yvar <- data %>% pull({{y}})
    catvar <- as.factor(as.character(data %>% pull({{cat}})))
    x <- as_label(enquo(x))
    y <- as_label(enquo(y))
    cat <- as_label(enquo(cat))
    if (length(xvar) == length(yvar) && length(yvar) == length(catvar))
    {
        mod <- lm(yvar ~ xvar * catvar, data = data)
        b <- mod$coefficients[1]
        m <- mod$coefficients[2]
        fit <- mod$model
        fit$data_id <- rownames(fit)
        data$tooltip <- paste0(fit$data_id, "\n", x, " = ", fit[['xvar']], "\n", y, " = ", fit[['yvar']], "\n", cat, " = ", fit[['catvar']])

        plot <- ggplot()

        for (i in 1:(length(mod$coefficients)))
        {
            if (is.na(mod$coefficients[i]))
            {
                mod$coefficients[i] <- 0
                print('Some aspects of this model may be incorrect. The model is missing a few coefficients')
            }
        }

        if (ci == TRUE)
        {
            for (i in 1:(length(levels(catvar))))
            {
                xmin <- min(data[catvar == levels(catvar)[i],] %>% pull({{x}}))
                xmax <- max(data[catvar == levels(catvar)[i],] %>% pull({{x}}))
                xval <- seq(from = xmin, to = xmax, by = (xmax-xmin)/999)
                newdata <- cbind.data.frame(xvar = xval, catvar = levels(catvar)[i])

                result <- predict(mod, newdata, interval = 'confidence', level = level,
                                  type = "response", se.fit = TRUE)

                plot <- plot + geom_ribbon(aes_string(x = xval,
                                                      ymin = result$fit[,'lwr'],
                                                      ymax = result$fit[,'upr']),
                                           alpha = .2)
            }
        }
        if (pi == TRUE)
        {
            for (i in 1:(length(levels(catvar))))
            {
                xmin <- min(data[catvar == levels(catvar)[i],]%>% pull({{x}}))
                xmax <- max(data[catvar == levels(catvar)[i],]%>% pull({{x}}))
                xval <- seq(from = xmin, to = xmax, by = (xmax-xmin)/999)
                newdata <- cbind.data.frame(xvar = xval, catvar = levels(catvar)[i])

                result <- predict(mod, newdata, interval = 'prediction', level = level,
                                  type = "response", se.fit = TRUE)

                plot <- plot + geom_ribbon(aes_string(x = xval,
                                                      ymin = result$fit[,'lwr'],
                                                      ymax = result$fit[,'upr']),
                                           alpha = .2)
            }
        }

        if (!plotly)
        {
            plot <- plot + geom_point_interactive(data = data,
                                                  aes(x = xvar, y = yvar, col = catvar, tooltip = tooltip))  +
                geom_segment_interactive(aes(x = min(data[catvar == levels(catvar)[1],]%>% pull({{x}})),
                                             xend = max(data[catvar == levels(catvar)[1],]%>% pull({{x}})),
                                             y = min(data[catvar == levels(catvar)[1],]%>% pull({{x}}))*m+b,
                                             yend = max(data[catvar == levels(catvar)[1],]%>% pull({{x}}))*m+b,
                                             color = levels(catvar)[1],
                                             tooltip = paste(levels(catvar)[1], '\n ', y, '=', sprintf('%.3f',m),
                                                             '*', x, '+', sprintf('%.3f',b), sep = ' ')))

            tooltips <- list()
            for (i in 2:length(levels(catvar)))
            {
                tooltips[[i]] <-  paste(levels(catvar)[i], '\n', y, '=', sprintf("%.3f",m+mod$coefficients[i+length(levels(catvar))]), '*', x, '+', sprintf("%.3f",b+mod$coefficients[i+1]), sep = ' ')
            }

            for (i in 1:(length(levels(catvar))-1))
            {
                plot <- plot + geom_segment_interactive(aes_string(x = min(data[catvar == levels(catvar)[i+1],]%>% pull({{x}})),
                                                                   xend = max(data[catvar == levels(catvar)[i+1],]%>% pull({{x}})),
                                                                   y = min(data[catvar == levels(catvar)[i+1],]%>% pull({{x}}))*(m+mod$coefficients[i+length(levels(catvar))+1])+b+mod$coefficients[i+2],
                                                                   yend = max(data[catvar == levels(catvar)[i+1],]%>% pull({{x}}))*(m+mod$coefficients[i+length(levels(catvar))+1])+b+mod$coefficients[i+2],
                                                                   color = shQuote(levels(catvar)[i+1])),
                                                        tooltip = tooltips[i+1])
            }

        } else
        {
            plot <- plot + geom_point(data = data,
                                      aes(x = xvar, y = yvar, col = catvar))  +
                geom_segment(aes(x = min(data[catvar == levels(catvar)[1],]%>% pull({{x}})),
                                 xend = max(data[catvar == levels(catvar)[1],]%>% pull({{x}})),
                                 y = min(data[catvar == levels(catvar)[1],]%>% pull({{x}}))*m+b,
                                 yend = max(data[catvar == levels(catvar)[1],]%>% pull({{x}}))*m+b,
                                 color = levels(catvar)[1]))

            for (i in 1:(length(levels(catvar))-1))
            {
                plot <- plot + geom_segment(aes_string(x = min(data[catvar == levels(catvar)[i+1],]%>% pull({{x}})),
                                                       xend = max(data[catvar == levels(catvar)[i+1],]%>% pull({{x}})),
                                                       y = min(data[catvar == levels(catvar)[i+1],]%>% pull({{x}}))*(m+mod$coefficients[i+length(levels(catvar))+1])+b+mod$coefficients[i+2],
                                                       yend = max(data[catvar == levels(catvar)[i+1],]%>% pull({{x}}))*(m+mod$coefficients[i+length(levels(catvar))+1])+b+mod$coefficients[i+2],
                                                       color = shQuote(levels(catvar)[i+1])))
            }
        }

        plot <- plot + ggtitle(title) + xlab(xlabel) + ylab(ylabel) + labs(color = legendTitle, fill = legendTitle)

        if ((model == TRUE) || (coefficients == TRUE))
        {
            nameList <- list()
            nameList[[1]] <- '(Intercept)'
            nameList[[2]] <- x
            levels <- list()
            for (i in 3:(1+length(levels(catvar))))
            {
                levels[[i-1]] <- paste(cat, levels(catvar)[i-1], sep = '')
                nameList[[i]] <- levels[[i-1]]
            }
            for (i in (2+length(levels(catvar))):(3+length(levels(catvar))))
            {
                nameList[[i]] <- paste(x, ':', levels[[i-3]], sep = '')
            }

            names(mod$coefficients) <- nameList
            if (model == FALSE)
            {
                print(mod$coefficients)
            }
        }

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

    } else
    {
        stop('Please enter valid parameters')
    }
    if (model == TRUE)
    {
        return(list(model = mod, plot = plot))
    } else
    {
        plot
    }
}

rl_same_intercept <- function(data, x, y, cat, plotly = FALSE, ci = FALSE, pi = FALSE, interactive = FALSE,
                              title = paste(x, 'vs.', y), xlabel = x, ylabel = y, legendTitle = cat,
                              level = .95, coefficients = FALSE, model = FALSE)
{
    if (is_tibble(data))
    {
        data <- as.data.frame(data)
    }
    xvar <- data %>% pull({{x}})
    yvar <- data %>% pull({{y}})
    catvar <- as.factor(as.character(data %>% pull({{cat}})))
    x <- as_label(enquo(x))
    y <- as_label(enquo(y))
    cat <- as_label(enquo(cat))
    if (length(xvar) == length(yvar) && length(yvar) == length(catvar))
    {
        mod <- lm(yvar ~ xvar + xvar:catvar, data = data)
        b <- mod$coefficients[1]
        m <- mod$coefficients[2]
        fit <- mod$model
        fit$data_id <- rownames(fit)
        data$tooltip <- paste0(fit$data_id, "\n", x, " = ", fit[['xvar']], "\n", y, " = ", fit[['yvar']], "\n", cat, " = ", fit[['catvar']])

        plot <- ggplot()

        for (i in 1:(length(mod$coefficients)))
        {
            if (is.na(mod$coefficients[i]))
            {
                mod$coefficients[i] <- 0
                print('Some aspects of this model may be incorrect. The model is missing a few coefficients')
            }
        }


        if (ci == TRUE)
        {
            for (i in 1:(length(levels(catvar))))
            {
                xmin <- min(data[catvar == levels(catvar)[i],]%>% pull({{x}}))
                xmax <- max(data[catvar == levels(catvar)[i],]%>% pull({{x}}))
                xval <- seq(from = xmin, to = xmax, by = (xmax-xmin)/999)
                newdata <- cbind.data.frame(xvar = xval, catvar = levels(catvar)[i])

                result <- predict(mod, newdata, interval = 'confidence', level = level,
                                  type = "response", se.fit = TRUE)

                plot <- plot + geom_ribbon(aes_string(x = xval,
                                                      ymin = result$fit[,'lwr'],
                                                      ymax = result$fit[,'upr']),
                                           alpha = .2)
            }
        }
        if (pi == TRUE)
        {
            for (i in 1:(length(levels(catvar))))
            {
                xmin <- min(data[catvar == levels(catvar)[i],]%>% pull({{x}}))
                xmax <- max(data[catvar == levels(catvar)[i],]%>% pull({{x}}))
                xval <- seq(from = xmin, to = xmax, by = (xmax-xmin)/999)
                newdata <- cbind.data.frame(xvar = xval, catvar = levels(catvar)[i])

                result <- predict(mod, newdata, interval = 'prediction', level = level,
                                  type = "response", se.fit = TRUE)

                plot <- plot + geom_ribbon(aes_string(x = xval,
                                                      ymin = result$fit[,'lwr'],
                                                      ymax = result$fit[,'upr']),
                                           alpha = .2)
            }
        }

        if (!plotly)
        {
            plot <- plot + geom_point_interactive(data = data,
                                                  aes(x = xvar, y = yvar, col = catvar, tooltip = tooltip)) +
                geom_segment_interactive(aes(x = min(data[catvar == levels(catvar)[1],]%>% pull({{x}})),
                                             xend = max(data[catvar == levels(catvar)[1],]%>% pull({{x}})),
                                             y = b+m*min(data[catvar == levels(catvar)[1],]%>% pull({{x}})),
                                             yend = b+m*max(data[catvar == levels(catvar)[1],]%>% pull({{x}})),
                                             color = levels(catvar)[1],
                                             tooltip = paste(levels(catvar)[1], '\n ', y, '=', sprintf('%.3f',m), '*', x, '+', sprintf('%.3f',b), sep = ' ')))

            tooltips <- list()
            for (i in 2:length(levels(catvar)))
            {
                tooltips[[i]] <-  paste(levels(catvar)[i], "\n", y, '=', sprintf("%.3f",m+mod$coefficients[i+1]), '*', x, '+', sprintf("%.3f",b), sep = ' ')
            }


            for (i in 1:(length(levels(catvar))-1))
            {
                plot <- plot + geom_segment_interactive(aes_string(x = min(data[catvar == levels(catvar)[i+1],]%>% pull({{x}})),
                                                                   xend = max(data[catvar == levels(catvar)[i+1],]%>% pull({{x}})),
                                                                   y = b+(mod$coefficients[i+2]+m)*min(data[catvar == levels(catvar)[i+1],]%>% pull({{x}})),
                                                                   yend = b+(mod$coefficients[i+2]+m)*max(data[catvar == levels(catvar)[i+1],]%>% pull({{x}})),
                                                                   color = shQuote(levels(catvar)[1+i])),
                                                        tooltip = tooltips[[i+1]])
            }
        } else
        {
            plot <- plot + geom_point(data = data,
                                      aes(x = xvar, y = yvar, col = catvar)) +
                geom_segment(aes(x = min(data[catvar == levels(catvar)[1],]%>% pull({{x}})),
                                 xend = max(data[catvar == levels(catvar)[1],]%>% pull({{x}})),
                                 y = b+m*min(data[catvar == levels(catvar)[1],]%>% pull({{x}})),
                                 yend = b+m*max(data[catvar == levels(catvar)[1],]%>% pull({{x}})),
                                 color = levels(catvar)[1]))

            for (i in 1:(length(levels(catvar))-1))
            {
                plot <- plot + geom_segment(aes_string(x = min(data[catvar == levels(catvar)[i+1],]%>% pull({{x}})),
                                                       xend = max(data[catvar == levels(catvar)[i+1],]%>% pull({{x}})),
                                                       y = b+(mod$coefficients[i+2]+m)*min(data[catvar == levels(catvar)[i+1],]%>% pull({{x}})),
                                                       yend = b+(mod$coefficients[i+2]+m)*max(data[catvar == levels(catvar)[i+1],]%>% pull({{x}})),
                                                       color = shQuote(levels(catvar)[1+i])))
            }
        }

        plot <- plot + ggtitle(title) + xlab(xlabel) + ylab(ylabel) + labs(color = legendTitle, fill = legendTitle)

        if ((model == TRUE) || (coefficients == TRUE))
        {
            nameList <- list()
            nameList[[1]] <- '(Intercept)'
            nameList[[2]] <- x
            levels <- list()
            for (i in 3:(1+length(levels(catvar))))
            {
                levels[[i-1]] <- paste(cat, levels(catvar)[i-1], sep = '')
                nameList[[i]] <- paste(x, ':', levels[[i-1]], sep = '')
            }

            names(mod$coefficients) <- nameList

            if (model == FALSE)
            {
                print(mod$coefficients)
            }
        }

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
    if (model == TRUE)
    {
        return(list(model = mod, plot = plot))
    } else
    {
        plot
    }
}

rl_same_slope <- function(data, x, y, cat, plotly = FALSE, ci = FALSE, pi = FALSE, interactive = FALSE,
                          title = paste(x, 'vs.', y), xlabel = x, ylabel = y, legendTitle = cat,
                          level = .95, coefficients = FALSE, model = FALSE)
{
    if (is_tibble(data))
    {
        data <- as.data.frame(data)
    }
    xvar <- data %>% pull({{x}})
    yvar <- data %>% pull({{y}})
    catvar <- as.factor(as.character(data %>% pull({{cat}})))
    x <- as_label(enquo(x))
    y <- as_label(enquo(y))
    cat <- as_label(enquo(cat))

    if (length(xvar) == length(yvar) && length(yvar) == length(catvar))
    {
        mod <- lm(yvar ~ xvar + catvar, data = data)
        b <- mod$coefficients[1]
        m <- mod$coefficients[2]
        fit <- mod$model
        fit$data_id <- rownames(fit)
        data$tooltip <- paste0(fit$data_id, "\n", x, " = ", fit[['xvar']], "\n", y, " = ", fit[['yvar']], "\n", cat, " = ", fit[['catvar']])

        plot <- ggplot()
        for (i in 1:(length(mod$coefficients)))
        {
            if (is.na(mod$coefficients[i]))
            {
                mod$coefficients[i] <- 0
                print('Some aspects of this model may be incorrect. The model is missing a few coefficients')
            }
        }

        if (ci == TRUE)
        {
            for (i in 1:(length(levels(catvar))))
            {
                xmin <- min(data[catvar == levels(catvar)[i],]%>% pull({{x}}))
                xmax <- max(data[catvar == levels(catvar)[i],]%>% pull({{x}}))
                xval <- seq(from = xmin, to = xmax, by = (xmax-xmin)/999)
                newdata <- cbind.data.frame(xvar = xval, catvar = levels(catvar)[i])

                result <- predict(mod, newdata, interval = 'confidence', level = level,
                                  type = "response", se.fit = TRUE)

                plot <- plot + geom_ribbon(aes_string(x = xval,
                                                      ymin = result$fit[,'lwr'],
                                                      ymax = result$fit[,'upr']),
                                           alpha = .2)
            }
        }
        if (pi == TRUE)
        {
            for (i in 1:(length(levels(catvar))))
            {
                xmin <- min(data[catvar == levels(catvar)[i],]%>% pull({{x}}))
                xmax <- max(data[catvar == levels(catvar)[i],]%>% pull({{x}}))
                xval <- seq(from = xmin, to = xmax, by = (xmax-xmin)/999)
                newdata <- cbind.data.frame(xvar = xval, catvar = levels(catvar)[i])

                result <- predict(mod, newdata, interval = 'prediction', level = level,
                                  type = "response", se.fit = TRUE)

                plot <- plot + geom_ribbon(aes_string(x = xval,
                                                      ymin = result$fit[,'lwr'],
                                                      ymax = result$fit[,'upr']),
                                           alpha = .2)
            }
        }

        if (!plotly)
        {
            plot <- plot + geom_point_interactive(data = data,
                                                  aes(x = xvar, y = yvar, col = catvar, tooltip = tooltip)) +
                geom_segment_interactive(aes(x = min(data[catvar == levels(catvar)[1],]%>% pull({{x}})),
                                             xend = max(data[catvar == levels(catvar)[1],]%>% pull({{x}})),
                                             y = min(data[catvar == levels(catvar)[1],]%>% pull({{x}}))*m+b,
                                             yend = max(data[catvar == levels(catvar)[1],]%>% pull({{x}}))*m+b,
                                             color = levels(catvar)[1]),
                                         tooltip = paste(levels(catvar)[1], '\n ', y, '=', sprintf('%.3f',m), '*', x, '+', sprintf('%.3f',b), sep = ' '))
            tooltips <- list()
            for (i in 2:length(levels(catvar)))
            {
                tooltips[[i]] <-  paste(levels(catvar)[i], "\n", y, '=', sprintf("%.3f",m), '*', x, '+', sprintf("%.3f",b+mod$coefficients[i+1]), sep = ' ')
            }

            for (i in 1:(length(levels(catvar))-1))
            {
                plot <- plot + geom_segment_interactive(aes_string(x = min(data[catvar == levels(catvar)[1+i],]%>% pull({{x}})),
                                                                   xend = max(data[catvar == levels(catvar)[1+i],]%>% pull({{x}})),
                                                                   y = min(data[catvar == levels(catvar)[1+i],]%>% pull({{x}}))*m+mod$coefficients[2+i]+b,
                                                                   yend = max(data[catvar == levels(catvar)[1+i],]%>% pull({{x}}))*m+mod$coefficients[2+i]+b,
                                                                   color = shQuote(levels(catvar)[1+i])),
                                                        tooltip = tooltips[[i+1]])
            }
        } else
        {
            {
                plot <- plot + geom_point(data = data,
                                          aes(x = xvar, y = yvar, col = catvar)) +
                    geom_segment(aes(x = min(data[catvar == levels(catvar)[1],]%>% pull({{x}})),
                                     xend = max(data[catvar == levels(catvar)[1],]%>% pull({{x}})),
                                     y = min(data[catvar == levels(catvar)[1],]%>% pull({{x}}))*m+b,
                                     yend = max(data[catvar == levels(catvar)[1],]%>% pull({{x}}))*m+b,
                                     color = levels(catvar)[1]))

                for (i in 1:(length(levels(catvar))-1))
                {
                    plot <- plot + geom_segment(aes_string(x = min(data[catvar == levels(catvar)[1+i],]%>% pull({{x}})),
                                                           xend = max(data[catvar == levels(catvar)[1+i],]%>% pull({{x}})),
                                                           y = min(data[catvar == levels(catvar)[1+i],]%>% pull({{x}}))*m+mod$coefficients[2+i]+b,
                                                           yend = max(data[catvar == levels(catvar)[1+i],]%>% pull({{x}}))*m+mod$coefficients[2+i]+b,
                                                           color = shQuote(levels(catvar)[1+i])))
                }
            }
        }


        plot <- plot + ggtitle(title) + xlab(xlabel) + ylab(ylabel) + labs(color = legendTitle, fill = legendTitle)

        if ((model == TRUE) || (coefficients == TRUE))
        {
            nameList <- list()
            nameList[[1]] <- '(Intercept)'
            nameList[[2]] <- x
            levels <- list()
            for (i in 3:(1+length(levels(catvar))))
            {
                levels[[i-1]] <- paste(cat, levels(catvar)[i-1], sep = '')
                nameList[[i]] <- levels[[i-1]]
            }

            names(mod$coefficients) <- nameList
            if (model == FALSE)
            {
                print(mod$coefficients)
            }
        }

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

    if (model == TRUE)
    {
        return(list(model = mod, plot = plot))
    } else
    {
        plot
    }
}

rl_same_line <- function(data, x, y, cat, plotly = FALSE, ci = FALSE, pi = FALSE, interactive = FALSE,
                         title = paste(x, 'vs.', y), xlabel = x, ylabel = y, legendTitle = cat,
                         level = .95, coefficients = FALSE, model = FALSE)
{
    if (is_tibble(data))
    {
        data <- as.data.frame(data)
    }
    xvar <- data %>% pull({{x}})
    yvar <- data %>% pull({{y}})
    catvar <- as.factor(as.character(data %>% pull({{cat}})))
    cat <- as_label(enquo(cat))
    x <- as_label(enquo(x))
    y <- as_label(enquo(y))
    if (length(xvar) == length(yvar))
    {
        mod <- lm(yvar ~ xvar, data = data)
        fit <- mod$model
        fit$data_id <- rownames(fit)
        data$tooltip <- paste0(fit$data_id, "\n",
                               x, " = ", fit[['xvar']], "\n",
                               y, " = ", fit[['yvar']])

        plot <- ggplot()

        for (i in 1:(length(mod$coefficients)))
        {
            if (is.na(mod$coefficients[i]))
            {
                mod$coefficients[i] <- 0
                print('Some aspects of this model may be incorrect. The model is missing a few coefficients')
            }
        }


        if (ci == TRUE)
        {
            xmin <- min(data %>% pull({{x}}))
            xmax <- max(data %>% pull({{x}}))
            xval <- seq(from = xmin, to = xmax, by = (xmax-xmin)/999)
            newdata <- cbind.data.frame(xvar = xval)

            result <- predict(mod, newdata, interval = 'confidence', level = level,
                              type = "response", se.fit = TRUE)

            plot <- plot + geom_ribbon(aes_string(x = xval,
                                                  ymin = result$fit[,'lwr'],
                                                  ymax = result$fit[,'upr']),
                                       alpha = .2)
        }
        if (pi == TRUE)
        {
            xmin <- min(data %>% pull({{x}}))
            xmax <- max(data %>% pull({{x}}))
            xval <- seq(from = xmin, to = xmax, by = (xmax-xmin)/999)
            newdata <- cbind.data.frame(xvar = xval)

            result <- predict(mod, newdata, interval = 'prediction', level = level,
                              type = "response", se.fit = TRUE)

            plot <- plot + geom_ribbon(aes_string(x = xval,
                                                  ymin = result$fit[,'lwr'],
                                                  ymax = result$fit[,'upr']),
                                       alpha = .2)
        }

        if (!plotly)
        {
            plot <- plot + geom_point_interactive(data = data,
                                                  aes(x = xvar, y = yvar, col = catvar, tooltip = tooltip)) +
                geom_segment_interactive(aes(x = min(xvar),
                                             xend = max(xvar),
                                             y = min(xvar)*mod$coefficients[2] + mod$coefficients[1],
                                             yend = max(xvar)*mod$coefficients[2] + mod$coefficients[1],
                                             tooltip = paste(y, '=', x, '*', sprintf("%.3f",mod$coefficients[2]), '+', sprintf("%.3f",mod$coefficients[1]))))
        } else {
            plot <- plot + geom_point(data = data, aes(x = xvar, y = yvar, col = catvar)) +
                geom_segment(aes(x = min(xvar),
                                 xend = max(xvar),
                                 y = min(xvar)*mod$coefficients[2] + mod$coefficients[1],
                                 yend = max(xvar)*mod$coefficients[2] + mod$coefficients[1]))

        }
        plot <- plot + ggtitle(title) + xlab(xlabel) + ylab(ylabel) + labs(color = legendTitle, fill = legendTitle)

        if ((model == TRUE) || (coefficients == TRUE))
        {
            nameList <- list()
            nameList[[1]] <- '(Intercept)'
            nameList[[2]] <- x

            names(mod$coefficients) <- nameList
            if (model == FALSE)
            {
                print(mod$coefficients)
            }
        }

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

    if (model == TRUE)
    {
        return(list(model = mod, plot = plot))
    } else
    {
        plot
    }
}

rl_no_cat <- function(data, x, y, plotly = FALSE, ci = FALSE, pi = FALSE, interactive = FALSE,
                      title = paste(x, 'vs.', y), xlabel = x, ylabel = y,
                      level = .95, coefficients = FALSE, model = FALSE)
{
    if (is_tibble(data))
    {
        data <- as.data.frame(data)
    }
    xvar <- data %>% pull({{x}})
    yvar <- data %>% pull({{y}})
    x <- as_label(enquo(x))
    y <- as_label(enquo(y))
    if (length(xvar) == length(yvar))
    {
        mod <- lm(yvar ~ xvar, data = data)
        fit <- mod$model
        fit$data_id <- rownames(fit)
        data$tooltip <- paste0(fit$data_id, "\n",
                               x, " = ", fit[['xvar']], "\n",
                               y, " = ", fit[['yvar']])

        plot <- ggplot()

        for (i in 1:(length(mod$coefficients)))
        {
            if (is.na(mod$coefficients[i]))
            {
                mod$coefficients[i] <- 0
                print('Some aspects of this model may be incorrect. The model is missing a few coefficients')
            }
        }


        if (ci == TRUE)
        {
            xmin <- min(data %>% pull({{x}}))
            xmax <- max(data %>% pull({{x}}))
            xval <- seq(from = xmin, to = xmax, by = (xmax-xmin)/999)
            newdata <- cbind.data.frame(xvar = xval)

            result <- predict(mod, newdata, interval = 'confidence', level = level,
                              type = "response", se.fit = TRUE)

            plot <- plot + geom_ribbon(aes_string(x = xval,
                                                  ymin = result$fit[,'lwr'],
                                                  ymax = result$fit[,'upr']),
                                       alpha = .2)
        }
        if (pi == TRUE)
        {
            xmin <- min(data %>% pull({{x}}))
            xmax <- max(data %>% pull({{x}}))
            xval <- seq(from = xmin, to = xmax, by = (xmax-xmin)/999)
            newdata <- cbind.data.frame(xvar = xval)

            result <- predict(mod, newdata, interval = 'prediction', level = level,
                              type = "response", se.fit = TRUE)

            plot <- plot + geom_ribbon(aes_string(x = xval,
                                                  ymin = result$fit[,'lwr'],
                                                  ymax = result$fit[,'upr']),
                                       alpha = .2)
        }

        if (!plotly)
        {
            plot <- plot + geom_point_interactive(data = data,
                                                  aes(x = xvar, y = yvar, tooltip = tooltip)) +
                geom_segment_interactive(aes(x = min(xvar),
                                             xend = max(xvar),
                                             y = min(xvar)*mod$coefficients[2] + mod$coefficients[1],
                                             yend = max(xvar)*mod$coefficients[2] + mod$coefficients[1],
                                             tooltip = paste(y, '=', x, '*', sprintf("%.3f",mod$coefficients[2]), '+', sprintf("%.3f",mod$coefficients[1]))))
        } else {
            plot <- plot + geom_point(data = data, aes(x = xvar, y = yvar)) +
                geom_segment(aes(x = min(xvar),
                                 xend = max(xvar),
                                 y = min(xvar)*mod$coefficients[2] + mod$coefficients[1],
                                 yend = max(xvar)*mod$coefficients[2] + mod$coefficients[1]))

        }
        plot <- plot + ggtitle(title) + xlab(xlabel) + ylab(ylabel)

        if ((model == TRUE) || (coefficients == TRUE))
        {
            nameList <- list()
            nameList[[1]] <- '(Intercept)'
            nameList[[2]] <- x

            names(mod$coefficients) <- nameList
            if (model == FALSE)
            {
                print(mod$coefficients)
            }
        }

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

    if (model == TRUE)
    {
        return(list(model = mod, plot = plot))
    } else
    {
        plot
    }
}

add_ci_poly <- function(plot, data, x, catvar, mod, level = .95, one_line = FALSE)
{
    if (!one_line)
    {
        for (i in 1:(length(levels(catvar))))
        {
            xmin <- min(data[catvar == levels(catvar)[i],]%>% pull({{x}}))
            xmax <- max(data[catvar == levels(catvar)[i],]%>% pull({{x}}))
            xval <- seq(from = xmin, to = xmax, by = (xmax-xmin)/999)
            newdata <- data.frame(xvar = xval, catvar = levels(catvar)[i])

            result <- predict(mod, newdata, interval = 'confidence', level = level,
                              type = "response", se.fit = TRUE)

            ymin <- result$fit[,'lwr']
            ymax <- result$fit[,'upr']

            plot <- plot + geom_ribbon(aes_string(x = xval,
                                                  ymin = ymin,
                                                  ymax = ymax),
                                       alpha = .2)
        }
    } else
    {
        xmin <- min(data %>% pull({{x}}))
        xmax <- max(data %>% pull({{x}}))
        xval <- seq(from = xmin, to = xmax, by = (xmax-xmin)/999)
        newdata <- cbind.data.frame(xvar = xval)

        result <- predict(mod, newdata, interval = 'confidence', level = level,
                          type = "response", se.fit = TRUE)

        ymin <- result$fit[,'lwr']
        ymax <- result$fit[,'upr']

        plot <- plot + geom_ribbon(aes(x = xval,
                                       ymin = ymin,
                                       ymax = ymax),
                                   col = NA, alpha = .2)
    }
    plot
}

add_pi_poly <- function(plot, data, x, catvar, mod, level = .95, one_line = FALSE)
{
    if (!one_line)
    {
        for (i in 1:(length(levels(catvar))))
        {
            xmin <- min(data[catvar == levels(catvar)[i],] %>% pull({{x}}))
            xmax <- max(data[catvar == levels(catvar)[i],] %>% pull({{x}}))
            xval <- seq(from = xmin, to = xmax, by = (xmax-xmin)/999)
            newdata <- data.frame(xvar = xval, catvar = levels(catvar)[i])

            result <- predict(mod, newdata, interval = 'prediction', level = level,
                              type = "response", se.fit = TRUE)

            ymin <- result$fit[,'lwr']
            ymax <- result$fit[,'upr']

            plot <- plot + geom_ribbon(aes_string(x = xval,
                                                  ymin = ymin,
                                                  ymax = ymax),
                                       alpha = .2)
        }
    } else
    {
        xmin <- min(data %>% pull({{x}}))
        xmax <- max(data %>% pull({{x}}))
        xval <- seq(from = xmin, to = xmax, by = (xmax-xmin)/999)
        newdata <- cbind.data.frame(xvar = xval)

        result <- predict(mod, newdata, interval = 'prediction', level = level,
                          type = "response", se.fit = TRUE)

        ymin <- result$fit[,'lwr']
        ymax <- result$fit[,'upr']

        plot <- plot + geom_ribbon(aes(x = xval,
                                       ymin = ymin,
                                       ymax = ymax),
                                   col = NA, alpha = .2)
    }
    plot
}
