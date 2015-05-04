
# install package if not installed
list.of.packages <- c("minpack.lm","ggplot2", "dplyr","gridExtra","shinyBS")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# library packages
sapply(list.of.packages, library,character.only=T)

# ui.r
shinyUI(navbarPage("NanoPHEAT", theme = "bootstrap.min.css",
    
    tabPanel("Introduction",
        fluidPage(
            titlePanel("Nano Product Hazard and Exposure Assessment Tool (NanoPHEAT)"),
            
            fluidRow(
                column(width = 10,
                    p("This model provides the estimation of endpoint response based on the nanomaterial ambient concentration, matrix, potency factor, and organism."),
                    p("The user can choose the following mode: simple and advanced. The simple mode gives the calculated result based on the well-established database and settings. The advanced mode allows user to change source data, model setting and fitting control based on the user's needs."),
                    p("The data used in this model is obtained from published literatures. The exposure characterizations are done at Duke by CEINT collaborators. ")
                )
            ),
            
            fluidRow(
                column(width = 10,
                    img(
                        src="art.png",
                        width = 600
                    )
                )
            )
        )
    ),
    
    tabPanel("Load",
        titlePanel("Upload your own data"),
        
        fluidRow(
            column(width = 10,
                p("If you want a sample .csv file to upload, you can first download the sample",
                    a(href = "NanoPHEAT.csv", "NanoPHEAT.csv"), 
                    "file, and then try uploading them.")
            )
        ),
        
        fluidRow(
            column(width = 4,  
                wellPanel(style="min-height:160px;",
                    fileInput(
                        inputId="file_data", 
                        label=NULL,
                        accept = c(
                            "text/csv",
                            "text/comma-separated-values",
                            "text/tab-separated-values",
                            "text/plain",
                            ".csv"
                        )
                    )
                )
            ),
            
            column(width = 2, 
                wellPanel(style="min-height:160px;",
                    radioButtons(
                        inputId="header", 
                        label = "Header",
                        choices =  c(Yes=TRUE, No=FALSE),
                        selected =     TRUE
                    )
                )
            ),
            
            column(width = 2,
                wellPanel(style="min-height:160px;",
                    radioButtons(
                        inputId="sep", 
                        label = "Separator",
                        choices =  c(Comma=",", Semicolon=";",  Tab="\t"),
                        selected =  ","
                    )
                )
            ),
            
            column(width = 2,
                wellPanel(style="min-height:160px;",
                    radioButtons(
                        inputId="quote",
                        label =  "Quote",
                        choices =  c(None="", "Double"="\"",   "Single"="\'"),
                        selected =  "\""
                    )
                )
            )
        ),
        
        fluidRow(
            column(width = 5,   
                actionButton("upload_button", "Process uploaded file",class="btn btn-primary")
            ),
            column(width = 5,   
                bsAlert("alert_upload")
            )
        ),
        
        tags$hr(),
        
        fluidRow(
            column(width = 10,
                dataTableOutput(outputId="table_upload")
            )
        )
    ),
    
    tabPanel("Filter",
        titlePanel("Filter the data"),
        
        fluidRow(
            column(width = 10,
                p("Filter Filter Filter Filter Filter Filter Filter Filter Filter Filter Filter Filter ")
            )
        ),
        
        fluidRow( 
            wellPanel(style="min-height:130px;",
                column(width = 3,  
                    selectizeInput(
                        inputId="select_enm",
                        label="Nanomaterial", 
                        choices = NULL,
                        options = list(
                            placeholder = "Select an option",
                            onInitialize = I("function() { this.setValue(''); }")
                        )
                    )
                ),
                
                column(width = 3,   
                    selectizeInput(
                        inputId="select_endpoint",
                        label="Endpoint", 
                        choices = NULL,
                        options = list(
                            placeholder = "Select an option",
                            onInitialize = I("function() { this.setValue(''); }")
                        )
                    )
                ),
                
                column(width = 3,   
                    selectizeInput(
                        inputId="select_organism",
                        label="Organism", 
                        choices = NULL,
                        options = list(
                            placeholder = "Select an option",
                            onInitialize = I("function() { this.setValue(''); }")
                        )
                    )
                ),            
                
                column(width = 3,   
                    selectizeInput(
                        inputId="select_matrix",
                        label="Matrix", 
                        choices = NULL,
                        options = list(
                            placeholder = "Select an option",
                            onInitialize = I("function() { this.setValue(''); }")
                        )
                    )
                )
            )
        ),
        
        fluidRow(
            column(width = 5,
                actionButton("filter_button", "Show filtered dataset",class="btn btn-primary")
            ) ,
            
            column(width = 5,   
                bsAlert("alert_filter")
            )
            
        ),
        
        tags$hr(),
        
        fluidRow(
            column(width = 10,
                dataTableOutput(outputId="table_filtered")
            )
        )
    ),
    
    tabPanel("Model",
        titlePanel("Model the data"),
        
        fluidRow(
            column(width = 10,
                p("Model Model Model Model Model Model Model Model Model Model Model Model ")
            )
        ),
        
        fluidRow(
            sidebarLayout(
                sidebarPanel( width = 3,
                    strong("Plot"),
                    p(""),
                    actionButton("plot_button", "Plot selected data",class="btn btn-primary"),
                    
                    tags$hr(),
                    
                    selectizeInput(
                        inputId="select_fit_method",
                        label="Fitting method", 
                        choices = list(
                            Linear=c(Linear="linear"),
                            Nonlinear=c(Logistic="logistic",  Other="other")
                        ),
                        options = list(
                            placeholder = "Select an option",
                            onInitialize = I("function() { this.setValue(''); }")
                        )
                    ),
                    
                    
                    # show formula and fitting parameter
                    p(""),
                    
                    # workaround https://github.com/rstudio/shiny/issues/692
                    p(withMathJax()),
                    
                    strong("Formula"),
                    
                    
                    uiOutput("text_formula"),
                    
                    tags$hr(),
                    actionButton("fit_button", "Fit this model",class="btn btn-primary")
                    
                ),
                mainPanel(
                    tabsetPanel(
                        tabPanel("Plot", 
                            
                            p(""),
                            # show formula and fitting parameter
                            uiOutput("text_parameter"),
                            
                            bsAlert("alert_plot"),
                            bsAlert("alert_fitting"),
                            bsAlert("alert_fit_method"),
                            bsAlert("alert_curve"),
                            bsAlert("alert_fitted"),
                            
                            plotOutput("plot")
                        ),
                        tabPanel("Statistics", 
                            p(""),
                            
                            bsAlert("alert_fit_stat"),
                            
                            verbatimTextOutput("fit_stat")
                        )
                        
                    )
                )
            )
        )
    ),
    
    tabPanel("Predict",
        titlePanel("Predict the data"),
        
        fluidRow(
            column(width = 10,
                p("Model Model Model Model Model Model Model Model Model Model Model Model ")
            )
        )
    ),
    
    tabPanel("Glossary",
        titlePanel("meaning of term"),
        
        fluidRow(
            column(width = 10,
                p("Model Model Model Model Model Model Model Model Model Model Model Model ")
            )
        )
    ),
    
    tabPanel("About",
        titlePanel("team"),
        
        fluidRow(
            column(width = 10,
                p("Model Model Model Model Model Model Model Model Model Model Model Model ")
            )
        )
    )
    
))
