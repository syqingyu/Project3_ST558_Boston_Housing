library(rstudioapi)
# the following line is for getting the path of your current open file
current_path <- getActiveDocumentContext()$path 
# The next line set the working directory to the relevant one:
setwd(dirname(current_path ))
library(tidyverse)
library(caret)
library(ggplot2)

BostonHousing <- read.csv('BostonHousing.csv')

function(input, output, session) {
    
    # Output histogram plot
    output$histPlot <- renderPlot({
        hist(BostonHousing[, input$variableName],
             main=paste("Histogram of", input$variableName),
             xlab=input$variableName)
    })
    
    savehistplot <- function(){
        hist(BostonHousing[, input$variableName],
             main=paste("Histogram of", input$variableName),
             xlab=input$variableName)
    }

    # Download histogram plot
    output$download_histplot <- downloadHandler(
        filename = "hist.png",
        content = function(file) {
            png(file)
            savehistplot()
            dev.off()
        }
    )
    
    
    # Output numerical summary
    output$summary <- renderPrint({
        summary(BostonHousing[, input$variableName])
    })
    
    # Output PCA math using MathJax
    output$PCAmath <- renderUI({
        withMathJax(
            helpText('Principal Components Analysis (PCA) is a dimension reduction technique.
                     If you have p variables, they contain some joint variability/correlation
                     PCA looks for linear combinations of those p variables that account for 
                     most of the variability. PCA is usually applied to the covariance matrix.
                     The covariance of two variables X and Y can be calculated using the following
                     formula: '),
            helpText('$$cov(X, Y) = \\frac{1}{n-1} \\sum_{i=1}^{n}(X_i-\\bar{x})(Y_i-\\bar{y})$$')
        )
    })
    
    # Output PCA information
    output$PCAinfo <- renderText({
        if (length(input$showVars) < 2){
            print("Please select at least two variables!")
        }
    })
    
    PCAplot <- function(){
        if (length(input$showVars) > 1){
            PCs <- prcomp(select(BostonHousing, input$showVars) , scale = TRUE)
            biplot(PCs, xlabs = rep(".", nrow(BostonHousing)), cex = 1.2, main="Biplot")
        }
    }
    
    # Output biplot for PCA
    output$biPlot <- renderPlot({
        if (length(input$showVars) > 1){
            PCs <- prcomp(select(BostonHousing, input$showVars) , scale = TRUE)
            biplot(PCs, xlabs = rep(".", nrow(BostonHousing)), cex = 1.2, main="Biplot")
        }
    })
    
    # Download PCA plot
    output$download_biplot <- downloadHandler(
        filename = "PCA.png",
        content = function(file) {
            png(file)
            PCAplot()
            dev.off()
            }
        )
    
    # Split data into training and testing
    trainIndex <- reactive({ 
        createDataPartition(BostonHousing$medv, 
                            p =input$selectPercent, 
                            list = FALSE, 
                            times = 1)
    })
    
    # Train model
    trainResult <- reactive({ 
        
        style <- isolate(input$style)
        
        withProgress(message = "Computing...", style = style, value = 0.1, {
            # Get training data
            BostonTrain <- BostonHousing[trainIndex(),]
            
            # Set training control
            control <- trainControl(method="repeatedcv", number=5, repeats=5)
            seed <- 7
            set.seed(seed)
            
            incProgress(0.8)
            # Train the model
            if (!is.null(input$inputVars)) {
                if (input$selectModel == "Linear Model") {
                    #train(medv~input$inputVars, data=BostonTrain, method="lm", trControl=control)
                    train(BostonTrain[, input$inputVars], BostonTrain[, "medv"], 
                          method="lm", trControl=control)
                    
                } else if (input$selectModel == "Random Forest") {
                    train(BostonTrain[, input$inputVars], BostonTrain[, "medv"], 
                          method="rf", trControl=control, ntree=input$treeNumber)
                }
            }
        })
    })
    
    # Test model
    testResult <- reactive({ 
        BostonTest  <- BostonHousing[-trainIndex(),]
        prediction <- predict(trainResult(), newdata = BostonTest)
        postResample(pred = prediction, obs = BostonTest$medv)
    })
    
    # Output train summary
    output$trainingSummary <- renderPrint({
        if (is.null(input$inputVars)) {
            return("Please select at least one predictor.")
        } else {
            trainResult()
        }
    })
    
    # Single zoomable plot
    ranges <- reactiveValues(x = NULL, y = NULL)
    
    # Output train plot
    output$trainPlot <- renderPlot({
        if (is.null(input$inputVars)) {
            return("Please select at least one predictor.")
        } else {
        tmp <- trainResult()
            BostonTrain <- BostonHousing[trainIndex(),]
            prediction <- predict(tmp, newdata = BostonTrain)
            observation <- BostonTrain$medv
            df <- tibble(prediction, observation)
            ggplot(df, aes(x=prediction, y=observation)) + 
                geom_point() + 
                geom_smooth(method=lm) +
                coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)
        }
    })
    
    # When a double-click happens, check if there's a brush on the plot.
    # If so, zoom to the brush bounds; if not, reset the zoom.
    observeEvent(input$trainplot_dblclick, {
        brush <- input$trainplot_brush
        if (!is.null(brush)) {
            ranges$x <- c(brush$xmin, brush$xmax)
            ranges$y <- c(brush$ymin, brush$ymax)
            
        } else {
            ranges$x <- NULL
            ranges$y <- NULL
        }
    })
    
    # For downloading train plot
    savetrainplot <- function() {
        if (is.null(input$inputVars)) {
            return("Please select at least one predictor.")
        } else {
            tmp <- trainResult()
            BostonTrain <- BostonHousing[trainIndex(),]
            prediction <- predict(tmp, newdata = BostonTrain)
            observation <- BostonTrain$medv
            df <- tibble(prediction, observation)
            ggplot(df, aes(x=prediction, y=observation)) + 
                geom_point() + 
                geom_smooth(method=lm) +
                coord_equal()
                
        }
    }
    
    
    
    # Download train plot
    output$download_trainplot <- downloadHandler(
        filename = "train.png",
        content = function(file) {
            ggsave(savetrainplot(), filename = file)
        })
    
    # Output test summary
    output$testingSummary <- renderPrint({
        if (is.null(input$inputVars)) {
            return("Please select at least one predictor.")
        } else {
            testResult()
        }
    })
    
    # Single zoomable plot
    ranges <- reactiveValues(x = NULL, y = NULL)
    
    # Output test plot
    output$testPlot <- renderPlot({
        if (is.null(input$inputVars)) {
            return("Please select at least one predictor.")
        } else {
            BostonTest  <- BostonHousing[-trainIndex(),]
            prediction <- predict(trainResult(), newdata = BostonTest)
            observation <- BostonTest$medv
            df <- tibble(prediction, observation)
            ggplot(df, aes(x=prediction, y=observation)) + 
                geom_point() + 
                geom_smooth(method=lm) +
                coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)
        }
    })

    # When a double-click happens, check if there's a brush on the plot.
    # If so, zoom to the brush bounds; if not, reset the zoom.
    observeEvent(input$testplot_dblclick, {
        brush <- input$testplot_brush
        if (!is.null(brush)) {
            ranges$x <- c(brush$xmin, brush$xmax)
            ranges$y <- c(brush$ymin, brush$ymax)
            
        } else {
            ranges$x <- NULL
            ranges$y <- NULL
        }
    })
    
    savetestplot <- function(){
        if (is.null(input$inputVars)) {
            return("Please select at least one predictor.")
        } else {
            BostonTest  <- BostonHousing[-trainIndex(),]
            prediction <- predict(trainResult(), newdata = BostonTest)
            observation <- BostonTest$medv
            df <- tibble(prediction, observation)
            ggplot(df, aes(x=prediction, y=observation)) + 
                geom_point() + 
                geom_smooth(method=lm) +
                coord_equal()
        }
    }
        
    # Download test plot
    output$download_testplot <- downloadHandler(
        filename = "test.png",
        content = function(file) {
            ggsave(savetestplot(), filename = file)
        })
    
    # Dynamic UI for user input hint
    output$ui <- renderUI({
        # Depending on input$inputVars, we'll generate a different
        # UI component and send it to the client.
        if (is.null(input$inputVars)) {
            includeMarkdown("variables_hint.md")
        } else {
            includeMarkdown("variables.md")
        }
    })
    
    userInput <- reactive({
        # Assign values to corresponding variables
        i <- 0
        tmp_values <- c()
        for (x in input$inputVars) {
            i <- i+1
            tmp_values[i] <- eval(parse(text=paste0("input$select", 
                                                    toupper(substr(x, 1, 1)), 
                                                    substr(x, 2, nchar(x))
            ))
            )
        }
       
        tmp_values
        
    })
    
    # Show the values in an HTML table ----
    output$userInputinfo <- renderTable({
        # Put them into dataframe
        data.frame(
            Name = input$inputVars,
            Value = as.character(userInput()),
            stringsAsFactors = FALSE)
    })
    
    # Output user input summary
    output$userInputresult <- renderPrint({
        df <- as.data.frame(t(userInput()))
        colnames(df) <- input$inputVars
        prediction <- predict(trainResult(), newdata = df)
        cat(prediction)
    })
    
    # Download data
    output$download_data <- downloadHandler(
        filename = "BostonHousing.csv",
        content = function(file) {
            write.csv(BostonHousing, file, row.names = FALSE)
        })
    
    # Output entire dataset
    output$dataTable <- DT::renderDataTable({
        DT::datatable(BostonHousing)
    })
}