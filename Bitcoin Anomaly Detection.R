#install.packages("shinydashboard")
#install.packages('anomalize')
#install.packages('coindeskr')
#install.packages('devtools')
#install.packages('tidyverse')
library(shinydashboard)
library(shiny)

library(anomalize)
library(tidyverse)
library(coindeskr)
library(dplyr)
library(ggplot2)







ui <- dashboardPage(
  skin = "purple",
  dashboardHeader(title = "Innodynamics"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Widgets", tabName = "widgets", icon = icon("th")),
      menuItem("GeoMap", tabName = "geoMap", icon = icon("th"), badgeLabel = "new", badgeColor = "green"),
      menuItem("BTC Anomaly Detection", tabName = "anomalyDetection", icon = icon("th"), badgeLabel = "new", badgeColor = "green")
      
    )
  ),
  ## Body content
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              h2("Dashboard"),
              fluidRow(
                box(title = "Histogram", status = "primary", plotOutput("plot1", height = 250)),
                
                box(
                  title = "Controls", status = "warning",
                  sliderInput("slider", "Number of observations:", 1, 100, 50)
                )
              ),
              # infoBoxes with fill=FALSE
              fluidRow(
                # A static infoBox
                infoBox("New Orders", 10 * 2, icon = icon("credit-card")),
                # Dynamic infoBoxes
                infoBoxOutput("progressBox"),
                infoBoxOutput("approvalBox")
              ),
              
              # infoBoxes with fill=TRUE
              fluidRow(
                infoBox("New Orders", 10 * 2, icon = icon("credit-card"), fill = TRUE),
                infoBoxOutput("progressBox2"),
                infoBoxOutput("approvalBox2")
              ),
              
              fluidRow(
                # Clicking this will increment the progress amount
                box(width = 4, actionButton("count", "Increment progress"))
              )
              
      ),
      
      # Second tab content
      tabItem(tabName = "widgets",
              h2("Widgets tab content")
      ),
      
      # Third tab content
      tabItem(tabName = "geoMap"
             
              ),
      
      # Forth tab content
      tabItem(tabName = "anomalyDetection",
              h2("Bitcoin Anomaly Detection"),
              fluidRow(
                
                box(
                  width = "1000px",
                  plotOutput("plot4", height = 1000)
                )
              )
              
      )#end of forth tab content
    )
  )
)


server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
 
  
  output$plot4 <- renderPlot({
    btc <- get_historic_price(start = "2017-01-01")
    
    btc_ts <- btc %>% rownames_to_column() %>% as.tibble() %>% 
      mutate(date = as.Date(rowname)) %>% select(-one_of('rowname'))
    
    data = btc_ts %>% 
      time_decompose(Price, method = "stl", frequency = "auto", trend = "auto") %>%
      anomalize(remainder, method = "gesd", alpha = 0.05, max_anoms = 0.2)
    
    plot_anomaly_decomposition(data)
    
  })
    
  
  
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
  output$progressBox <- renderInfoBox({
    infoBox(
      "Progress", paste0(25 + input$count, "%"), icon = icon("list"),
      color = "purple"
    )
  })
  output$approvalBox <- renderInfoBox({
    infoBox(
      "Approval", "80%", icon = icon("thumbs-up", lib = "glyphicon"),
      color = "yellow"
    )
  })
  
  # Same as above, but with fill=TRUE
  output$progressBox2 <- renderInfoBox({
    infoBox(
      "Progress", paste0(25 + input$count, "%"), icon = icon("list"),
      color = "purple", fill = TRUE
    )
  })
  output$approvalBox2 <- renderInfoBox({
    infoBox(
      "Approval", "80%", icon = icon("thumbs-up", lib = "glyphicon"),
      color = "yellow", fill = TRUE
    )
  })
  
  
}

shinyApp(ui, server)