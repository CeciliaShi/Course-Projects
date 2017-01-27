#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

fluidPage(
  titlePanel("Socks-ABC"),
  sidebarPanel(
    numericInput("n_sims","Number of simulations to run:", value=10000, min=1000, max=1e6),
    hr(),
    h4("Options:"),
    checkboxInput("showplot", "Plot Details", TRUE),
    checkboxInput("showtext", "Text", TRUE),
    checkboxInput("showtrue", "True Value", FALSE),
    hr(),
    h4("Data:"),
    sliderInput("data_n_pairs","Number of Socks Pairs:", value=0, min=0, max=50),
    sliderInput("data_n_sin","Number of Singleton Socks:", value=11, min=1, max=100),
    hr(),
    h4("Prior:"),
    h4("Hyper Parameters for Number of Socks:"),
    sliderInput("mu", "Mean:", value= 30, min=0, max=100),  # mean number of socks with default 30
    sliderInput("sd", "Standard Deviation:", value= 15, min=0, max=50), # standard deviation of number of socks
    h4("Hyper Parameters for Proportion of Paired Socks:"),
    sliderInput("beta_alpha", "Number of Pairs:", value= 15, min=0, max=50),  # first parameter for the beta distribution
    sliderInput("beta_beta", "Number of Singletons:", value= 2, min=0, max=50)  # # second parameter for the beta distribution
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("Number of Socks",
               plotOutput("prior_plot"),
               verbatimTextOutput('prior_summary'),
               plotOutput("posterior_plot"),
               verbatimTextOutput('posterior_summary'),
               verbatimTextOutput("true_value")),
      tabPanel("Number of Pairs",
               plotOutput("prior_plot2"),
               verbatimTextOutput('prior_summary2'),
               plotOutput("posterior_plot2"),
               verbatimTextOutput('posterior_summary2'),
               verbatimTextOutput("true_value2"))
    ))
)