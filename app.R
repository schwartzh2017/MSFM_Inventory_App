#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)
library(plotly)
library(ggplot2)

#-------------------------

# Call data
ds = read_csv("final_inventory.csv")

#-------------------------

# Define UI for application that draws a histogram
ui <- fluidPage(

  fluidPage(
    titlePanel("Inventory Datatable - Last Updated 9/9/24"),
    
    # Create a new Row in the UI for selectInputs
    fluidRow(
      column(2,
             selectInput("stock",
                         "Currently In Stock?",
                         c("See All Meds", "Yes", "No")
                         )
      ),
      column(2,
             selectInput("disp",
                         "Number of Times Dispensed",
                         c("See All Meds", "1-2 (First Quartile)", "3-5 (Second Quartile)", "6-22 (Third Quartile)", ">=23 (Fourth Quartile)")
             )
      ),
    ),
    # Create a new row for the table.
    DT::dataTableOutput("table")
  )
)

#-------------------------

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Filter data based on selections=
  output$table <- DT::renderDataTable(DT::datatable({
    data <- ds
    
    if (input$stock != "See All Meds") {
      if (input$stock == "Yes") {
      data <- data[data$total_in_stock >= 1,]
      } else {
        data <- data[data$total_in_stock == 0,]
      }
    }
    if (input$disp != "See All Meds") {
      if (input$disp == "1-2 (First Quartile)") {
        data <- data[data$no_times_dispensed <= 2,]
      } else if (input$disp == "3-5 (Second Quartile)") {
        data <- data[data$no_times_dispensed >= 3 & data$no_times_dispensed <= 5,]
      } else if (input$disp == "6-22 (Third Quartile)") {
        data <- data[data$no_times_dispensed >= 6 & data$no_times_dispensed <= 22,]
      } else if (input$disp == ">=23 (Fourth Quartile)") {
        data <- data[data$no_times_dispensed >= 23,]
      }
    }
    
    data
    
    #below not working to change column names
    # data = as.data.frame(data)
    # 
    # DT::datatable(data, colnames = c(
    #   "Medication Name" = "med_name",
    #   "Total in Stock" = "total_in_stock",
    #   "Number of Times Dispensed" = "no_times_dispensed"
    #               ))
  }))
  
}

#-------------------------

# Run the application 
shinyApp(ui = ui, server = server)
