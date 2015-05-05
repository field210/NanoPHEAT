# public function source file
source("function.R")

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
        titlePanel("data source"),
        
        fluidRow(
            column(width = 10,
                p("This tool uses Choose dataset."),  
                wellPanel(
                    radioButtons(
                        inputId="data_source", 
                        label = "Data source",
                        choices =  c("CEINT-NIKC dataset"=1, "Your own dataset"=2,  "CEINT-NIKC dataset + Your own dataset"=3),
                        selected =  1
                    )
                )
            )
        ),
        
        # show upload ui
        uiOutput("ui_upload"),
        
        
        fluidRow(
            column(width = 5,   
                actionButton("load_button", "Process dataset",class="btn btn-primary")
            ),
            column(width = 5,   
                bsAlert("alert_file")
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
                p("placeholder placeholder placeholder placeholder placeholder placeholder placeholder placeholder ")
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
                p("placeholder placeholder placeholder placeholder placeholder placeholder placeholder  ")
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
                            
                            bsAlert("alert_subset"),
                            bsAlert("alert_plot"),
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
                p("placeholder placeholder placeholder placeholder placeholder placeholder placeholder placeholder  ")
            )
        )
    ),
    
    tabPanel("Glossary",
        titlePanel("term exploration"),
        
        fluidRow(
            column(width = 10,
                p("placeholder placeholder placeholder placeholder placeholder placeholder placeholder placeholder placeholder placeholder  ")
            )
        )
    ),
    
    tabPanel("About",
        titlePanel("team"),
        
        fluidRow(
            column(width = 10,
                p("placeholder placeholder placeholder placeholder placeholder placeholder placeholder placeholder placeholder placeholder ")
            )
        )
    )
    
))
