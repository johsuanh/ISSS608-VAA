pacman::p_load(tidyverse,urca,tseries,plotly,lubridate,knitr,shiny)


# Load dataset (replace 'your_data.csv' with actual file)
dataset <- readRDS("station_data.rds")

# Define UI
ui <- fluidPage(
  titlePanel("Interactive Cross-Correlation Function (CCF) Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("station", "Select Station:", choices = unique(dataset$station)),
      selectInput("var1", "Select First Variable:", choices = colnames(dataset)[5:12]),
      selectInput("var2", "Select Second Variable:", choices = colnames(dataset)[5:12]),
      dateRangeInput("date_range", "Select Date Range:", 
                     start = min(dataset$date), end = max(dataset$date)),
      checkboxInput("apply_diff", "Apply Differencing", value = FALSE),
      sliderInput("lag_max", "Max Lag:", min = 1, max = 60, value = 30)
    ),
    
    mainPanel(
      plotOutput("ccfPlot")
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  # Reactive dataset filtered by user input
  filtered_data <- reactive({
    dataset %>%
      filter(station == input$station, date >= input$date_range[1], date <= input$date_range[2])
  })
  
  # Reactive CCF Calculation & Plot
  output$ccfPlot <- renderPlot({
    data <- filtered_data()
    
    # Ensure enough data exists
    validate(
      need(nrow(data) > 2, "Not enough data for CCF analysis. Please select a larger range.")
    )
    
    # Convert selected columns to numeric time series
    ts1 <- as.numeric(data[[input$var1]])
    ts2 <- as.numeric(data[[input$var2]])
    
    # Handle missing values (replace NA with mean)
    ts1[is.na(ts1)] <- mean(ts1, na.rm = TRUE)
    ts2[is.na(ts2)] <- mean(ts2, na.rm = TRUE)
    
    # Apply differencing if checked
    if (input$apply_diff) {
      ts1 <- diff(ts1, differences = 1)
      ts2 <- diff(ts2, differences = 1)
    }
    
    # Compute CCF
    ccf_result <- ccf(ts1, ts2, lag.max = input$lag_max, plot = FALSE, na.action = na.pass)
    
    # Create CCF plot
    df_ccf <- data.frame(lag = ccf_result$lag, acf = ccf_result$acf)
    
    ggplot(df_ccf, aes(x = lag, y = acf)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      geom_hline(yintercept = c(-1.96 / sqrt(nrow(df_ccf)), 1.96 / sqrt(nrow(df_ccf))),
                 linetype = "dashed", color = "red") +
      theme_minimal() +
      labs(title = "Cross-Correlation Function", x = "Lag", y = "Correlation")
  })
}

# Run App
shinyApp(ui = ui, server = server)