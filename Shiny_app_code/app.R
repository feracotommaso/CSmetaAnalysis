#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(readxl)
library(metafor)
library(ggplot2)
library(htmltools)
library(shiny)
library(dplyr)
library(pacman)
library(flextable)
load("results/strengths.R")
d <- readRDS("results/resList.RDS")
# Define UI for application that draws a histogram
ui <- navbarPage(
  title = "Character strengths meta analysis",
  id = "pages",
  
  #### PAGE 1: INTRODUCTION  
  tabPanel(
    title = "Introduction",
    br(),  # Line break
    h1("Introduction to the Shiny app", align = "left"),  # Title
    br(),
    p("Welcome to our Shiny app for visualizing the results of our meta-analysis. 
    This tool allows you to explore and plot findings related to the predictors 
    of interest, specifically the 24 character strengths of the VIA framework, across various outcomes
    (i.e., mental health and well-being, or overall effects)
    and moderator analyses (i.e., well-being vs mental health, VIA short vs long, 
    clinal vs not-clinical populations). Here's what you can do in this app:"),
    tags$ul(
      tags$li(tags$b("Forest (caterpillar) plots and moderator analyses:"), 
              " These plots display the individual effect sizes included in the meta-analysis along 
            with their confidence intervals and the main meta-analytical effects. 
            Moderator analyses allow you to investigate how  the moderators influence 
              the relationship between predictors and outcomes."),
      tags$li(tags$b("Funnel plots:"), 
              " Funnel plots are a diagnostic tool used to assess the presence of publication bias in 
            the meta-analysis. They help you visualize the distribution of effect sizes and identify 
            potential asymmetries that may indicate bias.")
    ),
    sidebarPanel(
      actionButton(inputId = "p1p2", label = "Forest (caterpillar) plots and moderators"),
      br(),br(),
      actionButton(inputId = "p1p3", label = "Funnel plots")
    )
  ), #END PAGE 1
  
  #### PAGE 2A: CATERPILLAR PLOTS
  tabPanel(
    title="Plot",
    br(),  # a capo
    h1("Results plotting",align="left"), # titoletto di primo livello
    br(),
    sidebarPanel(h3("Select"),
                 p("Select the desired plots
                 "),
                 selectInput("strength", "Strength:",
                             c(strengths24)),
                 selectInput("outcome", "Desired analysis:",
                             c(
                               "Overall" = "overall",
                               "WB vs MH moderations" =  "mainMod",
                               "MH, Clinical vs not clinical" = "mhPop",
                               "MH, VIA short vs long" = "mhVia",
                               "MH, specific outcomes" = "mhmh",
                               "WB, Clinical vs not clinical" = "wbPop",
                               "WB, VIA short vs long" = "wbVia",
                               "WB, specific outcomes" = "wbwb"
                             )),
                 selectInput("predictions", "Prediction intervals",
                             c(FALSE,TRUE))
    ),
    mainPanel(
      #h3("Results"),
      fluidRow(
        column(6, plotOutput("yourplot")),  # First plot in half the width
        column(6, plotOutput("yourplot2")) # Second plot in half the width
      ),
      br(),
      uiOutput("yourtable")
    )
  ), #END PAGE 2
  
  #### PAGE 3: FUNNEL PLOTS
  tabPanel(
    title="Funnel",
    br(),  # a capo
    h1("Funnel plots",align="left"), # titoletto di primo livello
    br(),
    sidebarPanel(h3("Select"),
                 p("Select the desired plots
                 "),
                 selectInput("strength2", "Strength:",
                             c(strengths24)),
                 selectInput("outcome2", "Desired analysis:",
                             c(
                               "Overall" = "overall",
                               "WB vs MH moderations" =  "mainMod",
                               "MH, Clinical vs not clinical" = "mhPop",
                               "MH, VIA short vs long" = "mhVia",
                               "MH, specific outcomes" = "mhmh",
                               "WB, Clinical vs not clinical" = "wbPop",
                               "WB, VIA short vs long" = "wbVia",
                               "WB, specific outcomes" = "wbwb"
                             ))
    ),
    mainPanel(
      uiOutput("yourtable2"),
      plotOutput("yourfunnel")
    )# Second plot in half the width
  ) #END PAGE 3
) #END

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  # Load shiny data
  # Specify the directory where the R scripts are located
  folder_path <- "R"  # Adjust this if the folder is located elsewhere
  
  # List all R script files in the folder (you can filter for specific types of R files)
  r_files <- list.files(folder_path, full.names = TRUE, pattern = "\\.R$")
  
  # Loop through each file and source it
  for (r_file in r_files) {
    source(r_file)  # This will run the R code in the specified script file
  }
  
  #### PAGE 1
  observeEvent(input$p1p2, {
    updateNavbarPage(session = session, inputId = "pages", selected = "Plot")
  })
  observeEvent(input$p1p3, {
    updateNavbarPage(session = session, inputId = "pages", selected = "Funnel")
  })
  ### PAGE 2
  output$yourplot <- renderPlot({
    S = input$strength
    S2 = which(strengths24 == S)
    O = input$outcome
    M = ifelse(O == "overall", "1",
               ifelse(O == "mainMod", "Outcome",
                      ifelse(O %in% c("mhPop", "wbPop"), "Population", 
                             ifelse(O %in% c("mhVia","wbVia"), "CS_measure",
                                    "spec_out"))))
    P = input$predictions
    if(M == "spec_out"){caterpillars2(d[[O]][[S2]],
                                      mod = M,
                                      group = "sample",
                                      xlab = "Pearson r",
                                      pred = P, 
                                      # strng = tolower(gsub("_", " ", S)),
                                      # colerrorbar = "blue",
                                      # colpoint = "red",
                                      # colpoly = "green",
                                      k = TRUE, # Show number of effects
                                      g = TRUE # Number of grouping levels
    )}else{
      caterpillars(d[[O]][[S2]],
                   mod = M,
                   group = "sample",
                   xlab = "Pearson r",
                   pred = P, 
                   strng = tolower(gsub("_", " ", S)),
                   # colerrorbar = "blue",
                   # colpoint = "red",
                   # colpoly = "green",
                   k = TRUE, # Show number of effects
                   g = TRUE # Number of grouping levels
      )}  
  })
  output$yourplot2 <- renderPlot({
    S = input$strength
    S2 = which(strengths24 == S)
    O = input$outcome
    M = ifelse(O == "overall", "1",
               ifelse(O == "mainMod", "Outcome",
                      ifelse(O %in% c("mhPop", "wbPop"), "Population", 
                             ifelse(O %in% c("mhVia","wbVia"), "CS_measure",
                                    "spec_out"))))
    P = input$predictions
    orchard_plot(d[[O]][[S2]], 
                 mod = M, 
                 group = "sample", 
                 xlab = "Pearson r",
                 transfm = "none", 
                 pred = P,
                 strng = tolower(gsub("_", " ", S)),
                 trunk.size = 1,
                 branch.size = 1,
                 twig.size = .3,
                 angle = 0,
                 k = TRUE, # Show number of effects
                 g = TRUE, # Number of grouping levels
                 cb = TRUE # Color-blind plot
    )
  })
  output$yourtable <-
    renderUI({ #renderTable
      # Convert flextable to HTML and render it
      S = input$strength
      S2 = which(strengths24 == S)
      O = input$outcome
      M = ifelse(O == "overall", "1",
                 ifelse(O == "mainMod", "Outcome",
                        ifelse(O %in% c("mhPop", "wbPop"), "Population", 
                               ifelse(O %in% c("mhVia","wbVia"), "CS_measure",
                                      "spec_out"))))
      flextable::flextable(mod_results(d[[O]][[S2]],
                                       group = "sample",
                                       mod = M
      )$mod_table) %>%
        htmltools_value()
    })
  ### PAGE 3
  output$yourfunnel <- renderPlot({
    S = input$strength2
    S2 = which(strengths24 == S)
    O = input$outcome2
    M = ifelse(O == "overall", "1",
               ifelse(O == "mainMod", "Outcome",
                      ifelse(O %in% c("mhPop", "wbPop"), "Population", 
                             ifelse(O %in% c("mhVia","wbVia"), "CS_measure",
                                    "spec_out"))))
    funnel(d[[O]][[S2]], 
           level=c(90, 95, 99), 
           shade=c("white", "gray55", "gray75"), 
           refline=0, 
           legend=TRUE)
  })
  output$yourtable2 <-
    renderUI({ #renderTable
      # Convert flextable to HTML and render it
      S = input$strength2
      S2 = which(strengths24 == S)
      O = input$outcome2
      M = ifelse(O == "overall", "1",
                 ifelse(O == "mainMod", "Outcome",
                        ifelse(O %in% c("mhPop", "wbPop"), "Population", 
                               ifelse(O %in% c("mhVia","wbVia"), "CS_measure",
                                      "spec_out"))))
      flextable::flextable(mod_results(d[[O]][[S2]],
                                       group = "sample",
                                       mod = M
      )$mod_table) %>%
        htmltools_value()
    })
  
}

# Run the application 
shinyApp(ui = ui, server = server)