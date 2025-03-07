pacman::p_load(shiny,tidyverse,ggplot2,ggiraph,patchwork)


top5_service_exp <- read.csv("data/top5_service_exp.csv")
list.files()
my_palette <- c("#D989AE", "#96C6D9", "#F2CD88", "#D97259", "#468C6C", "grey60","#B3A2C7", "#E1A77C", "#7CA08B", "#C77C8C")


# create dropdown list for service selection
ui <- fluidPage(
  
  titlePanel("Singapore's Services Exports: Top 5 Partners"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("selected_service", "Select a Service:", 
                  choices = unique(top5_service_exp$Service), 
                  selected = unique(top5_service_exp$Service)[1])
    ),
    
    mainPanel(
      girafeOutput("export_plot")
    )
  )
)

# filter data and update plots
server <- function(input, output) {
  output$export_plot <- renderGirafe({
    # filter dataset based on selected Service
    filtered_data <- top5_service_exp %>% filter(Service == input$selected_service)
    
    # reference line
    avg_export_by_service <- filtered_data %>%
      group_by(Service) %>%
      summarise(avg_export = mean(export, na.rm = TRUE), .groups = "drop")
    
    sum_export_by_country <- filtered_data %>%
      group_by(Country, year) %>%
      summarise(sum_export = sum(export, na.rm = TRUE), .groups = "drop")
    
    avg_ser_export <- sum_export_by_country %>%
      summarise(avg_export = mean(sum_export, na.rm = TRUE), .groups = "drop")
    
    # Main line plot by service type
    p1 <- ggplot(filtered_data, 
                 aes(x = year, y = export, color = Country, group = Country)) +
      geom_line_interactive(aes(data_id = Country), size = 1) +
      scale_color_manual(values = my_palette) +
      facet_wrap(~Service, scales = "free_y") +
      geom_hline(data = avg_export_by_service, aes(yintercept = avg_export, linetype = "Mean"), 
                 color = "grey30", size = 0.7) +
      scale_linetype_manual(name = "Reference", values = c("Mean"="dashed")) +
      labs(title = paste("Service Export Trends:", input$selected_service),
           x = "Year", y = "Export Value (S$ Bil)") +
      theme_minimal() +
      theme(
        legend.position = "top",
        plot.title = element_text(hjust = 0.5,size=16,face="bold"))
    
    # box plot by country
    p3 <- ggplot(sum_export_by_country, 
                 aes(x = Country, y = sum_export, fill = Country, group = Country)) +
      geom_boxplot_interactive(aes(data_id = Country), size = 1) +
      scale_fill_manual(values = my_palette) +
      geom_hline(data = avg_ser_export, aes(yintercept = avg_export), linetype = "dashed", 
                 color = "grey30", size = 0.7) +
      labs(y = "Export Value (S$ Bil)") +
      theme_minimal() +
      theme(legend.position = "none")
    
    patch <- p1 + p3 + plot_layout(widths = c(3, 1))
    
    girafe(ggobj = patch, width_svg = 12, height_svg = 6, 
           options = list(opts_hover(css = "stroke-width:3px; opacity: 1;"),
                          opts_hover_inv(css = "opacity: 0.1;")))
  })
}

shinyApp(ui, server)