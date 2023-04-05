library(shiny)
library(tidyverse)
library(ggplot2)
library(scales)
library(reshape2)
library(cowplot)
#library(googlesheets4)
#library(gargle)

#Google Sheets Import
#gs4_deauth()
#data <- read_sheet("https://docs.google.com/spreadsheets/d/--SHEETID--")#

#Import Synthetic Data
synthetic_data <- read.csv("synthetic-data.csv")

#DATA CLEANING
#Rename Columns
names(synthetic_data) <- c("timestamp", "overall", "status", "notes",
                           "symptom_A", "symptom_B", "symptom_C", "symptom_D", "symptom_E", "symptom_F", "symptom_G")

#Reformat entries
synthetic_data <- synthetic_data |> 
  #Make timestamp a datetime
  mutate(timestamp = sub("GMT+1", "", timestamp)) |> 
  mutate(timestamp = as.POSIXct(timestamp, format = "%Y/%m/%d %I:%M:%S %p", tz = "Europe/London")) |> 
  #Factor variables
  mutate(overall = ordered(overall, levels = c("Awful", "Bad", "Okay", "Good"))) |> 
  mutate(status = ordered(status, levels = c("Off", "On"))) |> 
  #Remove notes section
  select(-notes)

n = nrow(synthetic_data)

ui <- fluidPage(
  titlePanel("Patient Dashboard - How X Machine Affects Symptom Severity"),
  
  p("Dashboard to visualise how a medical machine X affects the severity of various symptoms in a patient."),
  
  fluidRow(
    column(4, plotOutput("overallPie")),
    column(4, plotOutput("overallPlot")),
    column(4, radioButtons("overallPres", "Presentation:",
                           choices = c("Absolute", "Percentage")))
  ),
  
  titlePanel("Severity of Symptoms"),
  
  fluidRow(
    column(3, checkboxGroupInput(
      "include_vars", "Select variables to include:",
      choices = c(
        "Symptom A" = "sym_a",
        "Symptom B" = "sym_b",
        "Symptom C" = "sym_c",
        "Symptom D" = "sym_d",
        "Symptom E" = "sym_e",
        "Symptom F" = "sym_f",
        "Symptom G" = "sym_g"
      ),
      selected = character(0)
    )),
    column(9, plotOutput("severityPlot"))
  ),
)

server <- function(input, output) {
  output$overallPie <- renderPlot({
    ggplot(synthetic_data, aes(x = factor(1), fill = overall)) +
      geom_bar(width = 1, stat = "count") +
      scale_fill_manual(values = c("#F8766D", "#FFB347", "#FDFB86", "lightgreen")) +
      coord_polar(theta = "y") +
      theme_void() +
      theme(plot.title = element_text(hjust = 0.5)) +
      labs(title = "Overall Assessment", fill = "")
  })
  
  output$overallPlot <- renderPlot({
    if (input$overallPres == "Absolute") {
      # render absolute plot
      ggplot(synthetic_data, aes(x = overall, fill = status)) + 
        geom_bar(position = "stack") +
        scale_fill_manual(values = c("#F8766D", "lightgreen"), 
                          labels = c("Off", "On"), 
                          name = "Machine") +
        scale_y_continuous(breaks = 0:(ceiling(max(table(synthetic_data$overall)))), expand = c(0, 0),
                           limits = c(0, ceiling(max(table(synthetic_data$overall))))) +
        theme_classic()+
        theme(panel.grid.major.y = element_line(color = "gray", linetype = "dashed") ) +
        labs(title = "Overall Condition By Machine Status",
             x = "Overall Condition", y = "", fill = "Machine")
    } else {
      # render percentage plot
      ggplot(synthetic_data, aes(x = overall, fill = status)) + 
        geom_bar(position = "fill") +
        scale_fill_manual(values = c("#F8766D", "lightgreen"), 
                          labels = c("Off", "On"), 
                          name = "Machine") +
        scale_y_continuous(expand = c(0, 0), limits = c(0, 1), labels = percent_format())+
        theme_classic()+
        theme(panel.grid.major.y = element_line(color = "gray", linetype = "dashed") ) +
        labs(title = "Overall Condition By Machine Status",
             x = "Overall Condition", y = "Percentage", fill = "Machine")
    }
  })
  
  observe({
    include_vars <- rep(FALSE, 7)
    if (!is.null(input$include_vars)) {
      include_vars[match(input$include_vars, c(
        "sym_a",
        "sym_b",
        "sym_c",
        "sym_d",
        "sym_e",
        "sym_f",
        "sym_g"
      ))] <- TRUE
    }
    legend_labels <- c("Symptom A", "Symptom B", "Symptom C", "Symptom D", "Symptom E", "Symptom F", "Symptom G")
    vars <- c("symptom_A", "symptom_B", "symptom_C", "symptom_D", "symptom_E", "symptom_F", "symptom_G")
    
    # Reshape and filter the data
    data_long <- melt(synthetic_data[,c("timestamp", vars)], id.vars = "timestamp", variable.name = "symptom")
    data_long_filtered <- data_long[data_long$symptom %in% vars[include_vars],]
    
    #Severity Line Graph Plot
    plot1 <- ggplot(data_long_filtered, aes(x = timestamp, y = value, color = symptom)) +
      geom_line() +
      xlab("Datetime") +
      ylab("Severity") +
      scale_y_continuous(breaks = 0:10, limits = c(0, 10), expand = c(0, 0), 
                         labels = function(x) ifelse(x == floor(x), format(x), "")) +
      scale_color_manual(values = c("red", "blue", "green", "orange", "magenta", "brown", "purple"), 
                         labels = legend_labels[include_vars]) +
      labs(color = "Symptom") +
      theme_bw()
    
    plot2 <- ggplot(synthetic_data, aes(x = timestamp, y = 0, color = status)) +
      geom_point(size=3) +
      scale_color_manual(values = c("blue", "red"), labels = c("On", "Off")) +
      labs(color = "Machine Status") +
      theme_void()
    
    output$severityPlot <- renderPlot({
      plot_grid(plot1, plot2, ncol = 1, align = "v", axis = "tb", rel_heights = c(6, 1))
    })
  })
  
  #index <- reactive({
  #  which(data$timestamp == input$timestamp_selection)
  #})
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
