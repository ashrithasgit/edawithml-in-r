library(shiny)
library(caret)
library(e1071)
library(rpart)
library(randomForest)
library(cluster)
library(ggplot2)

server <- function(input, output, session) {
  dataset <- reactive({
    req(input$file)
    tryCatch(
      {
        read.csv(input$file$datapath, header = input$header)
      },
      error = function(e) {
        showNotification("Error reading file. Ensure it is a valid CSV.", type = "error")
        NULL
      }
    )
  })
  
  output$data <- renderTable({
    req(dataset())
    head(dataset())
  })
  
  output$summary <- renderPrint({
    req(dataset())
    summary(dataset())
  })
  
  output$structure <- renderPrint({
    req(dataset())
    str(dataset())
  })
  
  observe({
    req(dataset())
    updateSelectInput(session, "eda_var", choices = names(dataset()))
    updateSelectInput(session, "eda_xvar", choices = names(dataset()))
    updateSelectInput(session, "eda_yvar", choices = names(dataset()))
    updateSelectInput(session, "target_var", choices = names(dataset()))
  })
  
  output$eda_plot <- renderPlot({
    req(dataset(), input$eda_graph)
    df <- dataset()
    
    if (input$eda_graph == "Histogram") {
      req(input$eda_var)
      ggplot(df, aes_string(x = input$eda_var)) +
        geom_histogram(binwidth = 30, fill = "blue", color = "black") +
        labs(title = paste("Histogram of", input$eda_var), x = input$eda_var, y = "Frequency")
    } else if (input$eda_graph == "Boxplot") {
      req(input$eda_var)
      ggplot(df, aes_string(y = input$eda_var)) +
        geom_boxplot(fill = "orange", color = "black") +
        labs(title = paste("Boxplot of", input$eda_var), y = input$eda_var)
    } else if (input$eda_graph == "Scatterplot") {
      req(input$eda_xvar, input$eda_yvar)
      ggplot(df, aes_string(x = input$eda_xvar, y = input$eda_yvar)) +
        geom_point(color = "blue") +
        labs(title = paste("Scatterplot of", input$eda_xvar, "vs", input$eda_yvar),
             x = input$eda_xvar, y = input$eda_yvar)
    }
  })
  
  observeEvent(input$run_ml, {
    req(dataset())
    df <- dataset()
    method <- input$ml_method
    results <- NULL
    
    if (anyNA(df)) {
      output$ml_results <- renderPrint({
        "Dataset contains missing values. Please clean the dataset before proceeding."
      })
      return()
    }
    
    if (method == "K-Means Clustering") {
      numeric_data <- df[sapply(df, is.numeric)]
      if (ncol(numeric_data) < 2) {
        results <- "K-Means Clustering requires at least 2 numeric columns."
      } else {
        kmeans_model <- kmeans(scale(numeric_data), centers = input$clusters)
        results <- list(
          "Cluster Centers" = kmeans_model$centers,
          "Within Cluster Sum of Squares" = kmeans_model$tot.withinss,
          "Cluster Assignments" = table(kmeans_model$cluster)
        )
      }
    } else {
      req(input$target_var)
      target <- input$target_var
      predictors <- setdiff(names(df), target)
      
      if (method == "Linear Regression" || method == "Multilinear Regression") {
        lm_model <- lm(as.formula(paste(target, "~ .")), data = df)
        results <- summary(lm_model)
      } else if (method == "K-Nearest Neighbors") {
        trControl <- trainControl(method = "cv", number = 5)
        knn_model <- train(as.formula(paste(target, "~ .")), data = df, method = "knn", trControl = trControl)
        results <- knn_model
      } else if (method == "Decision Tree") {
        dt_model <- rpart(as.formula(paste(target, "~ .")), data = df, method = "class")
        results <- dt_model
      } else if (method == "Random Forest") {
        rf_model <- randomForest(as.formula(paste(target, "~ .")), data = df)
        results <- rf_model
      }
    }
    
    output$ml_results <- renderPrint({
      results
    })
  })
}
