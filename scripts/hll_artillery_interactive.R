# set ----
library(shiny)
library(shinythemes)
library(plotly)
library(DT)
library(igraph)
library(tidyverse)

# load ----
data_arty <- list()
# american artillery
data_arty$usa <-
  data.frame(m = c(1600,1500,1400,1300,1200,1100,1000,900,800,700,600,
                   500,400,300,200,100),
             mil = c(622,646,670,693,717,741,764,788,812,836,859,883,
                     907,930,954,978))

# UI ---------------------------------------------------------------------------
ui <- 
  fluidPage(theme = shinytheme("lumen"),
            title = "Artillery",
            sidebarLayout(
              sidebarPanel(
                selectInput('artillery', 'Artillery', c("USA")),
                numericInput("range","Range (m)", value = 1000)
              ),
              mainPanel(
                plotOutput("arty_plot")
              )
            )
)

# server -----------------------------------------------------------------------
server <-
  function(input, output, session){
    
    reactive({
      fit <- 
        fit <- lm(mil ~
                    m,
                  data = data_arty[[input$artillery]])
      
      # estimate range accuracy
      acc <- mean(sqrt(fit$residuals^2))
      
      # predict
      arty <-
        arty %>% 
        mutate(prediction = predict(fit),
               upper      = mil+acc,
               lower      = mil-acc)
      
      # calculate
      new <- data.frame(m=c(input$range))
      })
    
    
    
    output$mil <- predict(fit, newdata = new)
      
    output$arty_plot <- renderPlot({
      fit <- 
        fit <- lm(mil ~
                    m,
                  data = data_arty[[input$artillery]])
      
      # estimate range accuracy
      acc <- mean(sqrt(fit$residuals^2))
      
      # predict
      arty <-
        arty %>% 
        mutate(prediction = predict(fit),
               upper      = mil+acc,
               lower      = mil-acc)
      
      ggplot(arty, aes(x = m)) +
        geom_point(aes(y = mil), size = 3) +
        geom_line(aes(y = prediction), linewidth = 1) +
        theme_classic()
      
    })
    
  }

# run app ----------------------------------------------------------------------
shinyApp(ui = ui, server = server)


























