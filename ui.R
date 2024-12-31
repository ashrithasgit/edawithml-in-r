library(shiny)

ui <- fluidPage(
  titlePanel("EDA and ML"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV File", 
                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      checkboxInput("header", "Header", TRUE),
      
      selectInput("eda_graph", "Choose EDA Graph:",
                  choices = c("Histogram", "Boxplot", "Scatterplot")),
      
      conditionalPanel(
        condition = "input.eda_graph == 'Histogram' || input.eda_graph == 'Boxplot'",
        selectInput("eda_var", "Select Variable:", choices = NULL)
      ),
      
      conditionalPanel(
        condition = "input.eda_graph == 'Scatterplot'",
        selectInput("eda_xvar", "X-axis Variable:", choices = NULL),
        selectInput("eda_yvar", "Y-axis Variable:", choices = NULL)
      ),
      
      selectInput("ml_method", "Choose ML Algorithm:",
                  choices = c("Linear Regression", "Multilinear Regression", 
                              "K-Nearest Neighbors", 
                              "Decision Tree", "Random Forest", 
                              "K-Means Clustering")),
      
      conditionalPanel(
        condition = "input.ml_method != 'K-Means Clustering'",
        selectInput("target_var", "Select Target Variable:", choices = NULL)
      ),
      
      conditionalPanel(
        condition = "input.ml_method == 'K-Means Clustering'",
        numericInput("clusters", "Number of Clusters:", value = 3, min = 2)
      ),
      
      actionButton("run_ml", "Run Model")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Dataset", tableOutput("data")),
        tabPanel("Summary", verbatimTextOutput("summary")),
        tabPanel("Structure", verbatimTextOutput("structure")),
        tabPanel("EDA Graphs", plotOutput("eda_plot")),
        tabPanel("Results", verbatimTextOutput("ml_results"))
      )
    )
  )
)
