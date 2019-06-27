rl <- function(data, x, y, cat, plotly = FALSE, same_slope = FALSE, same_intercept = FALSE) {
    if (same_slope == FALSE)
    {
        if (same_intercept == FALSE)
        {
            plot <- rl_fullmodel(data, x, y, cat)
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
    if (plotly == TRUE)
    {
        plot <- ggplotly(plot)
    }
    plot
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
    }
    plot
}

rl_same_intercept <- function(data, x, y, cat)
{
    #TODO
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
    }
    plot
}


rl_plotly <- function(data, x, y, cat, plotly = plotly, same_slope = FALSE, same_intercept = FALSE)
{
    rl(data, x, y, cat, plotly = plotly, same_slope = same_slope, same_intercept = same_intercept)
}