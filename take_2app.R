library(shiny)

# Load the dataset
file_path <- "healthcare_dataset_Excel_workbook_shortened_data.csv"
data <- read.csv(file_path)

# Define UI
ui <- fluidPage(
  titlePanel("Healthcare Dataset Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Filters"),
      selectInput("condition", "Select Medical Condition:", 
                  choices = unique(data$Medical.Condition), 
                  selected = unique(data$Medical.Condition)[1]),
      sliderInput("age", "Age Range:", 
                  min = min(data$Age, na.rm = TRUE), 
                  max = max(data$Age, na.rm = TRUE), 
                  value = c(min(data$Age, na.rm = TRUE), max(data$Age, na.rm = TRUE))),
      selectInput("gender", "Gender:", 
                  choices = c("All", unique(data$Gender)), 
                  selected = "All")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Summary", tableOutput("summaryTable")),
        tabPanel("Visualization", plotOutput("billingPlot"))
      )
    )
  )
)

# Define Server
server <- function(input, output) {
  # Reactive data based on filters
  filteredData <- reactive({
    df <- data[data$Medical.Condition == input$condition, ]
    df <- df[df$Age >= input$age[1] & df$Age <= input$age[2], ]
    if (input$gender != "All") {
      df <- df[df$Gender == input$gender, ]
    }
    return(df)
  })
  
  # Summary table
  output$summaryTable <- renderTable({
    df <- filteredData()
    summary <- data.frame(
      Metric = c("Number of Patients", "Average Billing Amount", "Min Billing Amount", "Max Billing Amount"),
      Value = c(nrow(df), mean(df$Billing.Amount, na.rm = TRUE), 
                min(df$Billing.Amount, na.rm = TRUE), 
                max(df$Billing.Amount, na.rm = TRUE))
    )
    return(summary)
  })
  
  # Billing plot
  output$billingPlot <- renderPlot({
    df <- filteredData()
    hist(df$Billing.Amount, 
         main = "Distribution of Billing Amounts",
         xlab = "Billing Amount ($)",
         col = "lightblue",
         border = "black")
  })
}

# Run the app
shinyApp(ui = ui, server = server)