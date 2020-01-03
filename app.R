#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

my_colors <- list(blue = "#4a5F70",
                  beige = "#7f825f",
                  mauve = "#c2ae95",
                  red = "#824e4e",
                  grey = "#66777d")

binom_obj <- function(in_x, in_size, in_p) {
    x <- 0:in_size
    y <- dbinom(x = 0:in_size, size = in_size, prob = in_p)
    is_x <- if_else(x <= in_x, "x", "other")
    obj <- list(df = data.frame(x, y, is_x), 
                title = "Binomial Distribution", 
                subtitle = paste0("P(X<=", in_x, ") successes",
                                  " in ", in_size, " trials",
                                  " when p = ", in_p,
                                  " is ", round(pbinom(in_x, in_size, in_p), 4), "."), 
                xlab = "successes (x)", 
                ylab = "probability (dbinom)",
                type = "discrete")
    obj
}

chisq_obj <- function(in_x, in_df) {
    x <- 0:(2*in_df)
    y <- dchisq(x = x, df = in_df)
    is_x <- if_else(x <= in_x, "x", "other")
    obj <- list(df = data.frame(x, y, is_x), 
                title = "Chi-Square Distribution", 
                subtitle = paste0("P(ChiSq<=", in_x, ") ",
                                  " when df ", in_df,
                                  " is ", round(pchisq(in_x, in_df), 4), "."), 
                xlab = "chisq (x)", 
                ylab = "density (dchisq)",
                type = "continuous")
    obj
}

exp_obj <- function(in_x, in_rate) {
    x <- 0:(2*in_x)
    y <- dexp(x = x, rate = in_rate)
    is_x <- if_else(x <= in_x, "x", "other")
    obj <- list(df = data.frame(x, y, is_x), 
                title = "Exponential Distribution", 
                subtitle = paste0("P(X<=", in_x, ") periods",
                                  " when events occur at a rate of ", in_rate, " per period",
                                  " is ", round(pexp(in_x, in_rate), 4), "."), 
                xlab = "x", 
                ylab = "density (dexp)",
                type = "continuous")
    obj
}

f_obj <- function(in_x, in_df1, in_df2) {
    x <- 0:max(in_df1, in_df2)
    y <- df(x = x, df1 = in_df1, df2 = in_df2)
    is_x <- if_else(x <= in_x, "x", "other")
    obj <- list(df = data.frame(x, y, is_x), 
                title = "F Distribution", 
                subtitle = paste0("P(X<=", in_x, ") ",
                                  " when df1 is ", in_df1,
                                  " and df2 is ", in_df2,
                                  " is ", round(pf(in_x, in_df1, in_df2), 4), "."), 
                xlab = "x", 
                ylab = "density (df)",
                type = "continuous")
    obj
}

gamma_obj <- function(in_x, in_shape, in_rate) {
    x <- 0:(2*in_shape)
    y <- dgamma(x = x, shape = in_shape, rate = in_rate)
    is_x <- if_else(x <= in_x, "x", "other")
    obj <- list(df = data.frame(x, y, is_x), 
                title = "Gamma Distribution", 
                subtitle = paste0("P(X<=", in_x, ") events",
                                  " in period ", in_shape,
                                  " when rate is ", in_rate,
                                  " is ", round(pgamma(in_x, in_shape, in_rate), 4), "."), 
                xlab = "x", 
                ylab = "density (dgamma)",
                type = "continuous")
    obj
}

geom_obj <- function(in_x, in_p) {
    x <- 0:(2*in_x)
    y <- dgeom(x = 0:(2*in_x), prob = in_p)
    is_x <- if_else(x <= in_x, "x", "other")
    obj <- list(df = data.frame(x, y, is_x), 
                title = "Geometric Distribution", 
                subtitle = paste0("P(X<=", in_x, ") failures",
                                  " prior to first success ",
                                  " when p = ", in_p,
                                  " is ", round(pgeom(in_x, in_p), 4), "."), 
                xlab = "failures (x)", 
                ylab = "probability (dgeom)",
                type = "discrete")
    obj
}

hyper_obj <- function(in_x, in_m, in_n, in_k) {
    x <- 0:(in_m + in_n)
    y <- dhyper(x = 0:(in_m + in_n), in_m, in_n, in_k)
    is_x <- if_else(x <= in_x, "x", "other")
    obj <- list(df = data.frame(x, y, is_x), 
                title = "Hypergeometric Distribution", 
                subtitle = paste0("P(X<=", in_x, ") successes",
                                  " in ", in_m,
                                  " and n = ", in_n,
                                  " is ", round(phyper(in_x, in_m, in_n, in_k), 4), "."), 
                xlab = "m (x)", 
                ylab = "probability (dhyper)",
                type = "discrete")
    obj
}

nbinom_obj <- function(in_x, in_size, in_p) {
    x <- 0:(2*in_size)
    y <- dnbinom(x = 0:(2*in_size), size = in_size, prob = in_p)
    is_x <- if_else(x <= in_x, "x", "other")
    obj <- list(df = data.frame(x, y, is_x), 
                title = "Negative Binomial Distribution", 
                subtitle = paste0("P(X<=", in_x, ") failures",
                                  " prior to success ", in_size, 
                                  " when p = ", in_p,
                                  " is ", round(pnbinom(in_x, in_size, in_p), 4), "."), 
                xlab = "failures (x)", 
                ylab = "probability (dnbinom)",
                type = "discrete")
    obj
}

norm_obj <- function(in_x, in_mean, in_sd) {
    x <- 0:(2*in_mean)
    y <- dnorm(x = x, mean = in_mean, sd = in_sd)
    is_x <- if_else(x <= in_x, "x", "other")
    obj <- list(df = data.frame(x, y, is_x), 
                title = "Normal Distribution", 
                subtitle = paste0("P(X<=x) where x = ", in_x, 
                                  " when mean(X) = ", in_mean, 
                                  " and sd(X) = ", in_sd,
                                  " is ", round(pnorm(in_x, in_mean, in_sd), 4)), 
                xlab = "x", 
                ylab = "density (norm)",
                type = "continuous")
    obj
}

pois_obj <- function(in_x, in_lambda) {
    x <- 0:(2*in_lambda)
    y <- dpois(x = x, lambda = in_lambda)
    is_x <- if_else(x <= in_x, "x", "other")
    obj <- list(df = data.frame(x, y, is_x), 
                title = "Poisson Distribution", 
                subtitle = paste0("Prob of X<=", in_x, 
                                  " when lambda = ", in_lambda, 
                                  " = ", round(ppois(in_x, in_lambda), 4)), 
                xlab = "x", 
                ylab = "density (pois)",
                type = "continuous")
    obj
}

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Distribution Viewer"),
    
    sidebarPanel(
            selectInput(inputId = "dist", 
                        label = "Distribution", 
                        choices = list("binom(x, size, prob)" = "binom", 
                                       "chisq(x, df1)" = "chisq",
                                       "exp(x, rate)" = "exp", 
                                       "f(x, df1, df2)" = "f", 
                                       "gamma(x, shape, rate)" = "gamma",
                                       "geom(x, prob)" = "geom",
                                       "hyper(x, m, n, k)" = "hyper",
                                       "nbinom(x, size, prob)" = "nbinom",
                                       "norm(x, mean, sd)" = "norm",
                                       "pois(x, lambda)" = "pois"), 
                        selected = 1),
            numericInput(inputId = "x", label = "x", value = 5),
            conditionalPanel(
                condition = "input.dist == 'binom' | input.dist == 'nbinom'",
                numericInput(inputId = "size", label = "size", value = 20)
                ),
            conditionalPanel(
                condition = "input.dist == 'binom' | input.dist == 'geom' | input.dist == 'nbinom'",
                sliderInput(inputId = "p", label = "p", min = 0, max = 1, value = 0.30)
            ),
            conditionalPanel(
                condition = "input.dist == 'chisq' | input.dist == 'f'",
                numericInput(inputId = "df1", label = "df1", value = 20)
            ),
            conditionalPanel(
                condition = "input.dist == 'exp'",
                numericInput(inputId = "rate", label = "rate", value = 1)
            ),
            conditionalPanel(
                condition = "input.dist == 'f'",
                numericInput(inputId = "df2", label = "df2", value = 1)
            ),
            conditionalPanel(
                condition = "input.dist == 'gamma'",
                numericInput(inputId = "shape", label = "shape", value = 1)
            ),
            conditionalPanel(
                condition = "input.dist == 'gamma'",
                numericInput(inputId = "rate", label = "rate", value = 1)
            ),
            conditionalPanel(
                condition = "input.dist == 'hyper'",
                numericInput(inputId = "m", label = "m", value = 1)
            ),
            conditionalPanel(
                condition = "input.dist == 'hyper'",
                numericInput(inputId = "n", label = "n", value = 1)
            ),
            conditionalPanel(
                condition = "input.dist == 'hyper'",
                numericInput(inputId = "k", label = "k", value = 1)
            ),
            conditionalPanel(
                condition = "input.dist == 'norm' | input.dist == 'pois'",
                numericInput(inputId = "mean", label = "mean", value = 10)
            ),
            conditionalPanel(
                condition = "input.dist == 'norm'",
                numericInput(inputId = "sd", label = "sd", value = 3)
            )
        ),
        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$distPlot <- renderPlot({
        dist_obj <- as.list(NA)
        if(input$dist == "binom") {
            dist_obj <- binom_obj(input$x, input$size, input$p)
        } else if(input$dist == "chisq") {
            dist_obj <- chisq_obj(input$x, input$df1)
        } else if(input$dist == "exp") {
            dist_obj <- exp_obj(input$x, input$rate)
        } else if(input$dist == "f") {
            dist_obj <- f_obj(input$x, input$df1, input$df2)
        } else if(input$dist == "gamma") {
            dist_obj <- gamma_obj(input$x, input$shape, input$rate)
        } else if(input$dist == "geom") {
            dist_obj <- geom_obj(input$x, input$p)
        } else if(input$dist == "hyper") {
            dist_obj <- hyper_obj(input$x, input$m, input$n, input$k)
        } else if(input$dist == "nbinom") {
            dist_obj <- nbinom_obj(input$x, input$size, input$p)
        } else if(input$dist == "norm") {
            dist_obj <- norm_obj(input$x, input$mean, input$sd)
        } else if(input$dist == "pois") {
            dist_obj <- pois_obj(input$x, input$mean)
        }
        p <- ggplot(dist_obj$df, aes(x = x, y = y)) +
            labs(title = dist_obj$title,
                 subtitle = dist_obj$subtitle,
                 x = dist_obj$xlab,
                 y = dist_obj$ylab)
        if(dist_obj$type == "discrete") {
            p <- p + geom_col(aes(fill = is_x))
            p <- p + scale_fill_manual(values = c(my_colors$blue, my_colors$red))
            p <- p + theme(legend.position = "none") 
            p <- p + geom_label(aes(label = round(y, 2)), size = 3, alpha = .6)
        } else {
            p <- p + geom_line()
            p <- p + geom_area(aes(fill = is_x))
            p <- p + scale_fill_manual(values = c(NA, my_colors$blue))
            p <- p + theme(legend.position = "none")
        }
        p
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
