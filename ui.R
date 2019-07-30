library(ggplot2)
library(shiny)
library(plotly)
library(shinyWidgets)

shinyUI(fluidPage(
  
  tabsetPanel(
    tabPanel("Description of ShinyApp", fluid = TRUE,
             titlePanel("Overview of Shiny App"),
             mainPanel(
    "This application allows the user to explore a data set containing 1,270 vehicles. A description of each tab follows.",
             br(),
             br(),
    h4("Data Set"),
    "This tab gives an overview of the data used throughout this app, including the data source and variables that are available.",
             br(),
             br(),
    h4("Data Exploration"),
    "This tab allows the user to explore relationships between different variables by choosing different variables for the x and y axis of a scatterplot as well as choosing an overlay variable. The user can also separate the graph by vehicle Type. A table is presented that shows summary statistics for each variable in the graph.",
            br(),
            br(),
    h4("Unsupervised Learning Models"),
    "This tab allows the user to perform Principal Components Analysis (PCA) on the data set. Users can specify which drive type they want to analyze for the PCA analysis. Plots are also available.",
            br(),
            br(),
    h4("Supervised Learning Models"),
    "This tab lets the user look at classification and regression methods for the data set. The classification method looks at classification tree and bagged tree models. The regression method looks at a simple linear regression model where the user can input the explanatory variable and compare it to the response variable of TailpipeCO2 emissions.",
            br(),
            br(),
    h4("Complete Data Set"),
    "This tab allows the user to explore the entire data set in a data table. The user can specify which car manufacturer they may be interested in or simply use the search bar to find a specific car. Users can also use this space download subsets of the data based on manufacturer."
             )

             ),
    tabPanel("Data Set", fluid = TRUE, 
             titlePanel("2018 Vehicles Data Set"),
             mainPanel(
               textOutput("info"),
               br(),
               htmlOutput("html_link"),
               br(),
               textOutput("data"),
               br(),
               tableOutput("table")
             )
             ),
    
    tabPanel("Data Exploration", fluid = TRUE, 
            titlePanel("Exploring the Data Set"),
            sidebarPanel(
                 # choose what goes on the x axis
                 selectInput(inputId = "x_axis",
                             label = "What would you like to see on the x axis?",
                             choices = colnames(vehicles),
                             selected = ""
                  ),
                 # choose what goes on the y axis
                 selectInput(inputId = "y_axis",
                             label = "What would you like to see on the y axis?",
                             choices = colnames(vehicles),
                             selected = ""
                  ),
                 #choose which groups you want to see
                 selectInput(inputId = "qual_var",
                              label = "Which Variables would you like to Overlay on the graph                                         ?",
                              choices = colnames(vehicles),
                              selected = ""
                  ),
                checkboxInput("type", h6("View Data by Type of Vehicle"))
            ),
                mainPanel(
                textOutput("CO2info"),
                plotOutput("data_plot"),
                downloadButton('goo'),
                textOutput("table_info"),
                dataTableOutput("data_table")
                )
    ),
    
    tabPanel("Unsupervised Learning Models", fluid = TRUE, 
             titlePanel("Principal Components Analysis"),
             sidebarPanel(
               selectInput(inputId = "drive", label = "Drive Description", 
                           selected = "All   Wheel Drive",
                           choices = levels(as.factor(vehicles$DriveDesc)))
             ),
             mainPanel(
             tabsetPanel(
               tabPanel("PCA Analysis", fluid = TRUE,
                textOutput("pcatext"),
                tableOutput("pcatable")
               ),
               tabPanel("Biplot", fluid = TRUE,
                textOutput("biplottext"),
                plotOutput("biplot"),
                downloadButton("doo")
               ),
               tabPanel("Proportion of Variance Explained", fluid = TRUE,
                textOutput("pvetext"),
                plotOutput("pveplot"),
                downloadButton("too")
               ),
               tabPanel("Cumulative Proportion of Variance Explained", fluid = TRUE,
                textOutput("pvecumtext"),
                plotOutput("pvecumplot"),
                downloadButton("moo")
               )
             )
             )
    ),
    
    tabPanel("Supervised Learning Models", fluid = TRUE, 
             
               tabsetPanel(
                 
                 tabPanel("Classification Model", fluid = TRUE,
                 
                 titlePanel("Models for Emissions Classification"),
                   sidebarPanel(
                   selectInput(inputId = "method",
                               label = "Choose which ensemble learning method to try:",
                               choices = c("Classification Tree" = "rpart",
                                           "Bagged Tree" = "treebag")),
                   sliderInput(inputId = "folds",
                               label = "Input Number of Folds for Repeated Cross Validation",
                               min = 1, max = 30, value = 10),
                   sliderInput(inputId = "repeats",
                               label = "Input Number of Complete Sets of Folds to compute", 
                               min = 1, max = 10, value = 3),
                   h4("Select a Car below to use the ensemble model to predict the Emissions                            Classificiation"),
                   selectInput(inputId = "carline",
                                           label = "Choose a Carline",
                                           choices = vehicles$Carline),
                   "Prediction",
                   tableOutput("prediction")
                 ),
                mainPanel(
                textOutput("trainingtext"),
                br(),
                textOutput("ensemble"),
                verbatimTextOutput("ensemblefit"),
                textOutput("matrix"),
                verbatimTextOutput("confusionmatrix")
                 )
                 ),
                
                 tabPanel("Regression Model", fluid = TRUE,
                    titlePanel(uiOutput("title_panel")),
                    sidebarLayout(
                    sidebarPanel(
                    selectInput("indepvar", label = h4("Choose an explanatory variable"),
                                choices = list("Cylinders" = "Cyl",
                                               "City Fuel Efficiency" = "CityFE",
                                               "Highway Fuel Efficiency" = "HWYFE",
                                               "Combined Fuel Efficiency" = "CombFE",
                                               "Gears" = "Gears",
                                               "Annual Fuel Cost" = "AnnualFuelCost",
                                               "Fuel Efficiency Rating" = "FERating",
                                               "Greenhouse Gas Rating" = "GHGRating",
                                               "Smog Rating" = "SmogRating")
                            )
                    ),
                            
                            mainPanel(
                              
                              tabsetPanel(type = "tabs",
                                          
                                tabPanel("Scatterplot", 
                                         uiOutput("scattermath"),
                                         plotOutput("scatterplot", click = "plot_click"),
                                         downloadButton('foo'),
                                         textOutput("scattertext"),
                                         verbatimTextOutput("click")),
                                tabPanel("Distribution", # Plots of distributions
                                    fluidRow(
                                      column(6, plotOutput("distribution1")),
                                      column(6, plotOutput("distribution2")))
                                          ),
                                #Regression output
                                tabPanel("Model Summary", verbatimTextOutput("summary"))
                                
                              )
                            )
                          
                 )
                 
                 )
               )
              ),
    
    #Complete Data Set Panel
    
    tabPanel("Complete Data Set", fluid = TRUE, 
             titlePanel("Complete Data Set"),
             sidebarPanel(
               selectInput(inputId = "manufacturer",
                           label = "Manufacturer",
                           choices = vehicles$Mfr.Name),
               checkboxInput("hybrid", "Choose Hybrids of this Manufacturer."),
               conditionalPanel(
                 condition = "input.hybrid == 1",
                 "Note: Not All Manufacturers have Hybrid Vehicles"
               )
             ),
             mainPanel(
               textOutput("data_info"),
               # Button
               downloadButton("downloadData", "Download"),
               dataTableOutput("complete_data")
             )
      )
  )
  
))



