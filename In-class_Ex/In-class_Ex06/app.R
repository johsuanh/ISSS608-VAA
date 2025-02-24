# load the packages
pacman::p_load(shiny,tidyverse)
#library(shiny)
#library(tidyverse)

# import the data
exam <- read_csv("data/Exam_data.csv")
# check the data
#print(exam)

# fluidPage need commas to seperate functions!!
ui <- fluidPage(
  titlePanel("Pupils Exam Results Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId="variable",
                 label = "Subject: ",
                 choices = c("English"="ENGLISH",
                             "Maths"="MATHS",
                             "Science"="SCIENCE"),
                 selected ="ENGLISH"),
    sliderInput(inputId = "bins",
                label="Number of Bins",
                min = 5,
                max =20,
                value =10)
    ),
    mainPanel(
      # name the plot, should be the same as server's output
      plotOutput("distPlot"
        
      )
    )
  )
)

server <- function(input, output) {
  # parenthesis is for Shiny's  function
  # curly bracket is for R code
  output$distPlot <- renderPlot({
    ggplot(exam, 
           # interactive input
           aes_string(input$variable))+ #unique ID
      geom_histogram(bins= input$bins, #unique ID
                     color='black',
                     fill="#C8CBE8")+
      theme_classic()
  })
}

shinyApp(ui = ui, server = server)
