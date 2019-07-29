library(markdown)

# Load data in ui.R to avoid "object not found error"
BostonHousing <- read.csv('BostonHousing.csv')

navbarPage("Boston Housing",
           tabPanel("Home",
                    img(class="img-kggle",
                        src="https://storage.googleapis.com/kaggle-competitions/kaggle/5315/logos/front_page.png"
                    ),
                    includeMarkdown("home.md")
           ),
           tabPanel("Data Exploration",
                    sidebarLayout(
                        sidebarPanel(
                            selectInput("variableName", "Variables:", selected = "crim",
                                        choices = colnames(BostonHousing)),
                            hr(),
                            includeMarkdown("variables.md")
                        ),
                        mainPanel(
                            h4("Summary"),
                            verbatimTextOutput("summary"),
                            h4("Historgram"),
                            plotOutput("histPlot"),
                            downloadButton("download_histplot", "Save image"),
                            h4("Data"),
                            DT::dataTableOutput("selectedData")
                        )
                    )
           ),
           tabPanel("Exploratory Analysis",
                    sidebarLayout(
                        sidebarPanel(
                            checkboxGroupInput("showVars", "Please select variables for Principal Component Analysis:",
                                               names(BostonHousing), selected = list("crim", "medv")),
                            hr(),
                            includeMarkdown("variables.md")
                        ),
                        mainPanel(
                            uiOutput('PCAmath'),
                            verbatimTextOutput("PCAinfo"),
                            plotOutput("biPlot"),
                            downloadButton("download_biplot", "Save image")
                        )
                    )
           ),
           tabPanel("Statistical Modeling",
                    sidebarLayout(
                        sidebarPanel(
                            conditionalPanel(condition="input.action=='original dataset'", 
                                            sliderInput("selectPercent", "Please select fraction of data for training: ", 
                                                        min = 0.6, max = 1, value = 0.8, step = 0.1),
                                            hr(),
                                            selectInput("selectModel", "Please select model: ", 
                                                        selected = "Linear Model",
                                                        choices = c("Linear Model", "Random Forest")),
                                            hr(),
                                            conditionalPanel(condition="input.selectModel=='Random Forest'", 
                                                             sliderInput("treeNumber", "Number of trees in Random Forest:",
                                                                         min = 5, max = 20, value = 5, step = 1)
                                                             ),
                                            checkboxGroupInput("inputVars", "Please select variables for model input:",
                                                               names(BostonHousing)[1:13], 
                                                               selected = names(BostonHousing)[1:13]),
                                            hr(),
                                            includeMarkdown("variables.md")
                            ),
                            conditionalPanel(condition="input.action=='user input'",
                                             conditionalPanel(condition="input.inputVars.includes('crim')",
                                                              sliderInput("selectCrim", "crim:", 
                                                                          min = 0., max = 100, 
                                                                          value = 4, step = 1)
                                             ),
                                             conditionalPanel(condition="input.inputVars.includes('zn')",
                                                              sliderInput("selectZn", "zn:", 
                                                                          min = 0., max = 100, 
                                                                          value = 10, step = 1)
                                             ),
                                             conditionalPanel(condition="input.inputVars.includes('indus')",
                                                              sliderInput("selectIndus", "indus:", 
                                                                          min = 0., max = 100, 
                                                                          value = 11, step = 1)
                                             ),
                                             conditionalPanel(condition="input.inputVars.includes('chas')",
                                                              sliderInput("selectChas", "chas:", 
                                                                          min = 0., max = 1, 
                                                                          value = 0, step = 1)             
                                             ),
                                             conditionalPanel(condition="input.inputVars.includes('nox')",
                                                              sliderInput("selectNox", "nox:", 
                                                                          min = 0., max = 1, 
                                                                          value = 0.5, step = 0.1)                 
                                             ),
                                             conditionalPanel(condition="input.inputVars.includes('rm')",
                                                              sliderInput("selectRm", "rm:", 
                                                                          min = 2, max = 10, 
                                                                          value = 6, step = 1)
                                             ),
                                             conditionalPanel(condition="input.inputVars.includes('age')",
                                                              sliderInput("selectAge", "age:", 
                                                                          min = 0, max = 100, 
                                                                          value = 70, step = 1)
                                             ),
                                             conditionalPanel(condition="input.inputVars.includes('dis')",
                                                              sliderInput("selectDis", "dis:", 
                                                                          min = 0, max = 20, 
                                                                          value = 4, step = 1)
                                             ),
                                             conditionalPanel(condition="input.inputVars.includes('rad')",
                                                              sliderInput("selectRad", "rad:", 
                                                                          min = 1, max = 24, 
                                                                          value = 10, step = 1)
                                             ),
                                             conditionalPanel(condition="input.inputVars.includes('tax')",
                                                              sliderInput("selectTax", "tax:", 
                                                                          min = 100, max = 1000, 
                                                                          value = 400, step = 100)
                                             ),
                                             conditionalPanel(condition="input.inputVars.includes('ptratio')",
                                                              sliderInput("selectPtratio", "ptratio:", 
                                                                          min = 10, max = 30, 
                                                                          value = 20, step = 1)
                                             ),
                                             conditionalPanel(condition="input.inputVars.includes('b')",
                                                              sliderInput("selectB", "b:", 
                                                                          min = 0., max = 400, 
                                                                          value = 350, step = 50)
                                             ),
                                             conditionalPanel(condition="input.inputVars.includes('lstat')",
                                                              sliderInput("selectLstat", "lstat:", 
                                                                          min = 0, max = 100, 
                                                                          value = 10, step = 1)
                                             ),
                                             uiOutput("ui")
                            )
                        ),
                        mainPanel(
                            tabsetPanel(
                                id = "action",
                                tabPanel("original dataset", 
                                         h4("Model performance for training dataset:"),
                                         verbatimTextOutput("trainingSummary"),
                                         h6("Brush and double-click to zoom"),
                                         plotOutput("trainPlot",  
                                                    dblclick = "trainplot_dblclick",
                                                    brush = brushOpts(
                                                      id = "trainplot_brush",
                                                      resetOnNew = TRUE)),
                                         downloadButton("download_trainplot", "Save image"),
                                         h4("Model performance for testing dataset:"),
                                         verbatimTextOutput("testingSummary"),
                                         h6("Brush and double-click to zoom"),
                                         plotOutput("testPlot",  
                                                    dblclick = "testplot_dblclick",
                                                    brush = brushOpts(
                                                      id = "testplot_brush",
                                                      resetOnNew = TRUE)),
                                         downloadButton("download_testplot", "Save image")
                                         ),
                                tabPanel("user input", 
                                         h4("Your input is"),
                                         tableOutput("userInputinfo"),
                                         h4("The prediction value of medv is "),
                                         verbatimTextOutput("userInputresult")
                                         )
                            )
                        )
                    )
           ),
           tabPanel("Data",
                    h4("The Boston Housing dataset"),
                    DT::dataTableOutput("dataTable"),
                    downloadButton("download_data", "Save data"),
                    includeMarkdown("variables.md")
           ),
           tabPanel("About",
                    includeMarkdown("about.md")
           )
)

