library(shiny)
library(dplyr)
library(plotly)
library(caret)
library(tree)
library(randomForest)
library(GGally)
library(modelr)
library(rgl)
library(ggplot2)

vehicles <- read.csv("2018VehiclesDatasetrevised.csv", header = TRUE)

df <- data.frame(vehicles[5:8], vehicles[10], vehicles[12], vehicles[14:18])
# Define server logic required to draw a scatterplot
shinyServer(function(input, output, session) {
   
  #create link for the original data set
  output$html_link <- renderUI({
    a("To view the original data set for 2018 or see other data sets click here", href = "https://www.fueleconomy.gov/feg/download.shtml", target = "_blank")
  })
  
  #create text for information about data set
  output$info <- renderText({
    "The U.S. Department of Energy maintains a website called www.fueleconomy.gov. This is the official U.S. government source for fuel economy information. Each year vehicle testing is done at the Environmental Protection Agency's (EPA) National Vehicle and Fuel Emissions Laboratory in Ann Arbor, Michigan, and by vehicle manufacturers with oversight from the EPA. This data set compiles some of the variables that are collected by this website."
  })
  
  output$data <- renderText({
    "This data set is a collection of vehicles from the 2018 data set compiled by the EPA. There are 1,270 vehicles that were manufactured in 2018 in this data set. The table below describes the attribues present for each vehicle."
  })
  
  #creating a table of the attributes and description of each attribute
  Attribute <- c("Mfr Name", "Division", "Carline", "Hybrid", "# Cyl", "City FE", "HWY FE", "Comb FE", "Tailpipe CO2", "Transmission", "No-Gears", "Annual Fuel Cost", "Money Saved Over 5 Years", "DriveDesc", "Type", "FE Rating", "GHG Rating", "Smog Rating", "Emissions")
  #Description <- c(1:18)
  Description <- c("Manufacturer name", 
                   "Company name", 
                   "Model name of the vehicle", 
                   "Identifies whether the vehicle has a hybrid engine (Y, N) such that it utilizes more than one form of onboard energy to achieve propulsion. A hybrid will have a traditional engine and fuel tank, as well as one or more electric motors and a battery pack", "Number of cylinders in an engine", 
                   "Estimated miles per gallon in city driving", 
                   "Estimated miles per gallon in highway driving", 
                   "Estimated miles per gallon in a combination of city driving (55%) and highway driving (45%)", 
                   "Amount of grams of CO2 per mile the vehicle emits. The best emits 0 grams of CO2 per mile (tailpipe only). Producing and distributing fuel also creates emissions that aren't part of this calculation.", 
                   "Identifies vehicles as manual or automatic transmission", 
                   "Number of transmission gears", 
                   "Estimated annual fuel cost assuming 15,000 miles per year (55% city and 45% highway) and average fuel price", 
                   "Fuel cost savings or extra expenditure compared to an average new vehicle of the same model year (25 mpg for model year 2018)", 
                   "Drivetrain (2-wheel, 4-wheel, or all-wheel)", 
                   "Identifies the vehicle as car, SUV, van, minivan, or truck", 
                   "Compares fuel economy to those of other vehicles of the same model year on a  scale of 1 (worst) to 10 (best)", 
                   "Compares tailpipe greenhouse gas emissions to those of other vehicles of the same model year on a scale of 1 (worst) to 10 (best)", 
                   "Compared tailpipe emissions of smog-producing pollutants to those of other vehicles of the same model year on a scale of 1 (worst) to 10 (best)",
                   "This classifies a vehicle as having either High or Low emissions. A Tailpipe CO2 reading greater than 400 is classified as high, less than 400 is low.")
  
  datatable <- data.frame(Attribute, Description)
  output$table <- renderTable({datatable})
  
  output$CO2info <- renderText({
    "In this investigation we will take a careful look at the estimated rate at which vehicles release carbon dioxide (CO2) into the air, which EPA claims is about 404 grams per mile for passenger vehicles. Play with the data set below to see what other variables may affect the amount of CO2 emitted by a vehicle. Does the type of vehicle seem to have an effect on how high the CO2 emissions are?"
  })
  
  #Data Exploration Page
  #create plots for data exploration page
  
  plot1Input = function() {
    g <- ggplot(data = vehicles, aes_string(x = input$x_axis, y = input$y_axis))
    
    if(input$type){
      p <- g + geom_point(aes_string(col = input$qual_var)) + facet_wrap(~Type)
    }
    else{
      p <- g + geom_point(aes_string(col = input$qual_var))
    }
    print(p)
  }
  
  output$goo = downloadHandler(
    filename = 'EDA.png',
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = width, height = height,
                       res = 300, units = "in")
      }
      ggsave(file, plot = plot1Input(), device = device)
    })
  
  output$data_plot <- renderPlot({
      g <- ggplot(data = vehicles, aes_string(x = input$x_axis, y = input$y_axis))
      
      if(input$type){
        p <- g + geom_point(aes_string(col = input$qual_var)) + facet_wrap(~Type)
      }
      else{
        p <- g + geom_point(aes_string(col = input$qual_var))
      }
      print(p)
  })

  
  output$data_table <- renderDataTable({
    statistics <- subset.data.frame(vehicles, select = c(input$x_axis, input$y_axis, input$qual_var))
    summary(statistics)
  })
  
  output$table_info <- renderText({
    "The table below shows summary statistics for the variables you selected in the sidebar."
  })
  
  output$save <- downloadHandler(
    filename = "save.png" ,
    content = function(file) {
      ggsave(data_plot, file = file)
    })

  #Complete Data Set Page
  
  output$data_info <- renderText({
    "Use the table below to explore the data set by manufacturer, or use the Search bar to find a particular car of interest to you. Can you find the vehicle you drive? What sort of emissions does your car have?"
  })
  
  getData <- reactive({
    newData <- vehicles %>% filter(Mfr.Name == input$manufacturer)
  })
  
  output$complete_data <- renderDataTable({
    getData()
    if(input$hybrid){
      filtered_hybrid <- getData() %>% filter(Hybrid == "Y") 
    }
    else{
      getData()
    }
    
  })
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$manufacturer, ".csv", sep ="")
    },
    content = function(file) {
      write.csv(getData(), file, row.names = FALSE)
    }
  )
  
  #Unsupervised Learning Page
  
  #define a reactive data set based on Drive Type
  getData2 <- reactive({
    newData2 <- vehicles %>% filter(DriveDesc == input$drive)
    df <- data.frame(newData2[5:8], newData2[10], newData2[12], newData2[14:18])
  })
  
  output$pcatable <- renderTable({
      #if(input$PCA){
      pr.out = prcomp(getData2(), scale = TRUE, rownames = TRUE)
      pr.out$rotation
      #}
      #else{}
  })
  
  output$pcatext <- renderText({
    #if(input$PCA){
      "The table below shows the principal component loadings; each column contains the corresponding principal component loading vector. How many distinct components are there? Do the number of components changed based on your choice of Drive Type?"
    #}
  })
  
  output$biplottext <- renderText({
    #if(input$biplot){
    "The first two principal components for the data are given below. Which variables appear to have the most weight?"
    #}
  })
  
  output$biplot <- renderPlot({
    #if(input$biplot){
      pr.out <- prcomp(getData2(), scale = TRUE)
      biplot(pr.out, scale = 0, xlabs=rep("x", nrow(getData2())))
    #}
  })
  
  plot2Input = function() {
    pr.out <- prcomp(getData2(), scale = TRUE)
    biplot(pr.out, scale = 0, xlabs=rep("x", nrow(getData2())))
  }
  
  output$doo = downloadHandler(
    filename = 'biplot.png',
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = width, height = height,
                       res = 300, units = "in")
      }
      ggsave(file, plot = plot2Input(), device = device)
    })
  
  output$pvetext <- renderText({
    #if(input$PVE){
    "How much of the proportion of variance explained (PVE) is accounted for by the first principal component? Find the PVE for each PC in the graph below."
    #}
  })
  
  output$pveplot <- renderPlot({
    #if(input$PVE){
      pr.out <- prcomp(getData2(), scale = TRUE)
      pr.var = pr.out$sdev^2
      pve = pr.var/sum(pr.var)
      plot(pve, xlab = "Principal Component", ylab = "Proportion of Variance Explained", ylim = c(0,1), type = 'b')
    #}
  })
  
  plot3Input = function() {
    pr.out <- prcomp(getData2(), scale = TRUE)
    pr.var = pr.out$sdev^2
    pve = pr.var/sum(pr.var)
    plot(pve, xlab = "Principal Component", ylab = "Proportion of Variance Explained", ylim = c(0,1), type = 'b')
  }
  
  output$too = downloadHandler(
    filename = 'pve.png',
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = width, height = height,
                       res = 300, units = "in")
      }
      ggsave(file, plot = plot3Input(), device = device)
    })
  
  output$pvecumtext <- renderText({
    #if(input$PVEcum){
      "How much of the proportion of variance explained (PVE) is accounted for by the first two principal components? Find the cumulative PVE for the first two PCs in the graph below."
    #}
  })
  
  output$pvecumplot <- renderPlot({
    #if(input$PVEcum){
      pr.out <- prcomp(getData2(), scale = TRUE)
      pr.var = pr.out$sdev^2
      pve = pr.var/sum(pr.var)
      plot(cumsum(pve), xlab = "Principal Component", ylab = "Cumulative Proportion of Variance Explained", ylim = c(0,1), type = 'b')
    #}
  })
  
  plot4Input = function() {
    pr.out <- prcomp(getData2(), scale = TRUE)
    pr.var = pr.out$sdev^2
    pve = pr.var/sum(pr.var)
    plot(cumsum(pve), xlab = "Principal Component", ylab = "Cumulative Proportion of Variance Explained", ylim = c(0,1), type = 'b')
  }
  
  output$moo = downloadHandler(
    filename = 'cpve.png',
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = width, height = height,
                       res = 300, units = "in")
      }
      ggsave(file, plot = plot4Input(), device = device)
    })
  
  #Modeling Page
  
  #Ensemble Methods
  output$trainingtext <- renderText({
    "For supervised learning methods you can investigate classification and regression methods. For this section we will first look at classification methods. A class variable called Emissions was added to the dataset. This variable labels a vehicle as having high emissions if the Tailpipe CO2 is above 400 grams of CO2 per mile, and low otherwise. The vehicles data set was then split into an 80/20 training and testing set to test two possible ensemble fits, Classification Tree or Bagged Tree."
  })
  #define data set using Tailpipe CO2 as the response variable
  output$ensemble <- renderText({
    "Use the Input features on the sidebar to choose which classification method you will choose as well as how many k-folds you will use and repeats. The output below shows us the best model and the accuracy of that model on the training data."
  })
  modelset <- data.frame(vehicles[5:8], vehicles[10], vehicles[12], vehicles[14:17], vehicles[19])
  
  #set training sets
  set.seed(18)
  
  #indices to split on
  train <- sample(1:nrow(modelset), size = nrow(modelset)*0.8)
  test <- dplyr::setdiff(1:nrow(modelset), train)
  
  #subset
  vehiclesTrain <- modelset[train, ]
  vehiclesTest <- modelset[test, ]
  
  #create ensemble fit from ui input for ensemble type
  output$ensemblefit <- renderPrint({
    trctrl <- trainControl(method = "repeatedcv", number = input$folds, repeats = input$repeats)
    set.seed(3333)
    ensemble_fit <- train(Emissions ~ ., data = vehiclesTrain, method = input$method,
                          trControl = trctrl)
    ensemble_fit
  })
  
  #create confusion matrix for ensemble fit
  output$matrix <- renderText({
    "The confusion matrix below is produced using the training data. Note the accuracy of the classification model on the training set. Which is more accurate, classification tree or bagged tree?"
  })
  output$confusionmatrix <- renderPrint({
    trctrl <- trainControl(method = "repeatedcv", number = input$folds, repeats = input$repeats)
    set.seed(3333)
    ensemble_fit <- train(Emissions ~ ., data = vehiclesTrain, method = input$method,
                          trControl = trctrl)
    test_predict <- predict(ensemble_fit,newdata = vehiclesTest)
    confusionMatrix(test_predict, vehiclesTest$Emissions)
  })
  
  #make a prediction from selected set of parameters in side bar
  
  getData3 <- reactive({
    newData3 <- vehicles %>% filter(Carline == input$carline)
  })
  
  output$prediction <- renderTable({
    newData3 <- getData3()
    trctrl <- trainControl(method = "repeatedcv", number = input$folds, repeats = input$repeats)
    set.seed(3333)
    ensemble_fit <- train(Emissions ~ ., data = vehiclesTrain, method = input$method,
                          trControl = trctrl)
    prediction <- predict(ensemble_fit, getData3())
  })
  
  #Regression Page for Tailpipe CO2 emissions
  
  # Regression output
  output$title_panel <- renderUI({
    text <- paste0("Linear Regression Model for Tailpipe CO2 and ", input$indepvar)
    h3(text)
  })
  
  output$summary <- renderPrint({
    fit <- lm(vehicles[,"TailpipeCO2"] ~ vehicles[,input$indepvar])
    summary(fit)
  })
  
  
  # Scatterplot output
  plotInput = function() {
    plot(vehicles[,input$indepvar], vehicles[,"TailpipeCO2"], main="Scatterplot",
         xlab=input$indepvar, ylab="TailpipeCO2", pch=19)
    abline(lm(vehicles[,"TailpipeCO2"] ~ vehicles[,input$indepvar]), col="red")
  }
  output$scattermath <- renderUI({
    withMathJax(helpText("The scatterplot below graphs the explanatory variable on the x-axis and Tailpipe CO2 as the response variable on the y-axis. The red line represents the simple linear regression model $$Tailpipe CO_{2} = b_0 + b_1*input$$"))
  })
  output$scattertext <- renderText({
    "Click on the points above to see the coordinates."
  })
  output$click <- renderText({
    paste0("x=", input$plot_click$x, "\ny=", input$plot_click$y)
  })
  
  output$foo = downloadHandler(
    filename = 'scatterplot.png',
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = width, height = height,
                       res = 300, units = "in")
      }
      ggsave(file, plot = plotInput(), device = device)
    })
  
  output$scatterplot <- renderPlot({
    plot(vehicles[,input$indepvar], vehicles[,"TailpipeCO2"], main="Scatterplot",
         xlab=input$indepvar, ylab="TailpipeCO2", pch=19)
    abline(lm(vehicles[,"TailpipeCO2"] ~ vehicles[,input$indepvar]), col="red")
  }, height=400)
  
  
  # Histogram output var 1
  output$distribution1 <- renderPlot({
    hist(vehicles[,"TailpipeCO2"], main="", xlab="TailpipeCO2")
  }, height=300, width=300)
  
  # Histogram output var 2
  output$distribution2 <- renderPlot({
    hist(vehicles[,input$indepvar], main="", xlab=input$indepvar)
  }, height=300, width=300)
  
  getData4 <- reactive({
    newData4  <- vehicles %>% filter(Carline == input$choice)
  })
  

  
  


  

  
  

  
})
  



