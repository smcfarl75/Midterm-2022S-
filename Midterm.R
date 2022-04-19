library(fpp3)
library(seasonal)
library(shinydashboard)
library(fpp3)
library(readr)
library(shinyWidgets)
library(shiny)



recent_production <- aus_production %>%
  filter(year(Quarter) >= 1992)
fit_beer <- recent_production %>%
  model(TSLM(Beer ~ trend() + season()))
fc_beer <- forecast(fit_beer)

server <- shinyServer(function(input, output, session) {
  
  your_plot <- reactive({
    if(input$PlotChoice == "Decomposition") {
      plot(aus_production %>%
             model(
               classical_decomposition(Beer, type = "multiplicative")
             ) %>%
             components() %>%
             autoplot())
    }
    else if (input$PlotChoice == "Seasonal"){
      plot(aus_production %>%
             model(
               STL(Beer ~ trend(window = 7) +
                     season(window = "periodic"),
                   robust = TRUE)) %>%
             components() %>%
             autoplot())
    }
    else if (input$PlotChoice == "Autocorrelation"){
      plot(aus_production %>%
             ACF(Beer, lag_max = 48) %>%
             autoplot())
    }
    else if (input$PlotChoice == "Regular"){
      plot(aus_production %>%
             autoplot(Beer))
    }
    else if (input$PlotChoice == "Forecast"){
      plot(fc_beer %>%
             autoplot(recent_production))
    }
  })
  
  output$SelectedPlot <- renderPlot({ 
    your_plot()
  })
  
  output$text <- renderText({
    paste("The plot above shows the total beer production of Australia from 1956 
    until 2010.  Australia's beer production steadily increased until 
    roughtly  1975.  From 1975 until 1992, the production of beer plateaus
    and neither increases nor decreases, on average.  Beer production
    later decreases from 1992 until the end of the dataset (2010).
    
You can view various timeseries features of the data by selecting a 
          feature from the options to the left of the graph.
          
Regular is the normal time series plot.

Autocorrelation shows the relationship between lagged variables 
  in a time series.  Because many of the bars in this graph are
  above the blue dashed line, these lag variables are 
  significantly present.

Decomposition used in this graph is multiplicative (compared
  to additive).Here, assuming the seasonal variable is 
  constant from year to year, we can see that the majority 
  of the variation in the decomposition comes from random variation

Forecast shows what the projected beer production in Australia will be
  in the future.  In this case, it appears to project a 
  continuation of the downward trend.
  
Seasonal shows how beer production has changed over the course 
  of a year, organized by the season.  The big y-axis bar shows
  that Beer production is very affected by this")
  })
  
})


ui <-  shinyUI(fluidPage(
  navbarPage(title="Midterm Project",
             tabPanel("Australian Beer Production",
                      sidebarLayout(
                        sidebarPanel(
                          radioButtons("PlotChoice", "Displayed plot:", 
                                       choices = c("Regular", "Autocorrelation","Decomposition","Forecast","Seasonal"))),
                        mainPanel(plotOutput("SelectedPlot"),
                                  verbatimTextOutput("text")))))
  ,  fluid=TRUE))

verbatimTextOutput("summary")

shinyApp(ui=ui, server=server)