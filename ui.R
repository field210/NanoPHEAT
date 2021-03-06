# public function source file
source('function.R')

# ui.r
shinyUI(    
    fluidPage(
        list(tags$head(HTML("<link rel='icon', href='favicon.png', 
            type='image/png' />"))),
        div(style='padding: 1px 0px; width: 100%;',
            titlePanel(
                title='', 
                windowTitle='Nano Product Hazard and Exposure Assessment Tool (NanoPHEAT)'
            )
        ),
        
        navbarPage(
            title=div(
                img(src='favicon.png',width='32px'), 
                strong('NanoPHEAT')
                ), 
            theme = 'bootstrap.css',
            tabPanel('Introduction',
                fluidPage(
                    titlePanel('Nano Product Hazard and Exposure Assessment Tool'),
                    
                    fluidRow(
                        column(width = 10,
                            p('This model predicts an estimated exposure to materials of interest and superimpose this effective dose onto endpoint-specific dose-response curves for a variety of nanomaterials. This prediction base on the specific nanomaterial ambient concentration, matrix, matrix factor, organism and endpoint.'),
                            p('The data used in this model is obtained from published literatures. The exposure characterizations are done at Duke by CEINT collaborators. Users are also allowed to use their own data, which turns NanoPHEAT into a straightforward and expandable approach to use the existing toxicity data to offer insights on expected responses based on a series of exposure assumptions or based on specific scenario data.')
                        )
                    ),
                    p(''),
                    
                    fluidRow(
                        column(width = 10,
                            img(src='art.png',style="width: 90% ;")
                        )
                    )
                )
            ),
            
            tabPanel('Load',
                titlePanel('Data source'),
                
                fluidRow(
                    column(width = 10,
                        p(''),  
                        wellPanel(
                            radioButtons(
                                inputId='data_source', 
                                label = 'Data source',
                                choices =  c('CEINT-NIKC dataset'=1, 'Your own dataset'=2,  'CEINT-NIKC dataset + Your own dataset'=3),
                                selected =  1
                            )
                        )
                    )
                ),
                
                # show upload ui
                uiOutput('ui_upload'),
                
                fluidRow(
                    column(width = 5,   
                        actionButton('load_button', 'Process dataset',class='btn btn-primary')
                    ),
                    column(width = 5,   
                        bsAlert('alert_file'),
                        bsAlert('alert_format')
                    )
                ),
                
                tags$hr(),
                
                fluidRow(
                    column(width = 10,
                        DT::dataTableOutput(outputId='table_load')
                    )
                )
            ),
            
            tabPanel('Filter',
                titlePanel('Filter the data'),
                
                fluidRow(
                    column(width = 10,
                        p('')
                    )
                ),
                
                fluidRow( 
                    wellPanel(style='min-height:130px;',
                        column(width = 3,  
                            selectizeInput(
                                inputId='select_enm',
                                label='Nanomaterial', 
                                choices = NULL
                            )
                        ),
                        
                        column(width = 3,   
                            selectizeInput(
                                inputId='select_endpoint',
                                label='Endpoint', 
                                choices = NULL
                            )
                        ),
                        
                        column(width = 3,   
                            selectizeInput(
                                inputId='select_organism',
                                label='Organism', 
                                choices = NULL
                            )
                        ),            
                        
                        column(width = 3,   
                            selectizeInput(
                                inputId='select_matrix',
                                label='Matrix', 
                                choices = NULL
                            )
                        )
                    )
                ),
                
                fluidRow(
                    column(width = 5,
                        actionButton('filter_button', 'Show filtered dataset',class='btn btn-primary')
                    ) ,
                    
                    column(width = 5,   
                        bsAlert('alert_filter')
                    )
                    
                ),
                
                tags$hr(),
                
                fluidRow(
                    column(width = 10,
                        DT::dataTableOutput(outputId='table_filtered')
                    )
                )
            ),
            
            tabPanel('Model',
                titlePanel('Model the data'),
                
                fluidRow(
                    column(width = 10,
                        p(' ')
                    )
                ),
                
                fluidRow(
                    sidebarLayout(
                        sidebarPanel( width = 3,
                            strong('Plot'),
                            p(''),
                            div(actionButton('plot_button', 'Plot selected data',class='btn btn-primary'),class='text-center'),
                            
                            tags$hr(),
                            
                            selectizeInput(
                                inputId='select_model',
                                label='Fitting method', 
                                choices = NULL,
                                options = list(
                                   placeholder = 'Select an option',
                                    onInitialize = I('function() { this.setValue(""); }')
                                )
                            ),
                            
                            # show definition, formula and fitting parameter
                            textOutput('text_definition'),
                            
                            # workaround https://github.com/rstudio/shiny/issues/692
                            p(withMathJax()),
                            uiOutput('text_formula'),
                            
                            tags$hr(),
                            div(actionButton('fit_button', 'Fit this model',class='btn btn-primary'),class='text-center')
                        ),
                        mainPanel(
                            tabsetPanel(
                                tabPanel('Plot and fit',  
                                    p(''),
                                    # show formula and fitting parameter
                                    uiOutput('text_parameter'),
                                    
                                    bsAlert('alert_subset'),
                                    bsAlert('alert_plot'),
                                    bsAlert('alert_fit_method'),
                                    bsAlert('alert_curve'),
                                    bsAlert('alert_fitted_initial'),
                                    bsAlert('alert_fitted_converge'),
                                    
                                    plotOutput('plot')
                                ),
                                tabPanel('Statistics', 
                                    p(''),
                                    bsAlert('alert_fit_stat'),
                                    verbatimTextOutput('fit_stat')
                                )
                            )
                        )
                    )
                )
            ),
            
            tabPanel('Predict',
                titlePanel('Predict the data'),
                
                fluidRow(
                    column(width = 10,
                        p(' ')
                    )
                ),
                
                fluidRow(
                    sidebarLayout(
                        sidebarPanel( width = 3,
                            numericInput(
                                inputId='pf', 
                                label='Matrix factor', 
                                value=NULL
                            ),
                            div(actionButton('pf_button', 'Use default'),class='text-center'),
                            
                            tags$hr(),
                            
                            numericInput(
                                inputId='m', 
                                label='Product Dose in System, M (g/L)', 
                                value=NULL
                            ),
                            numericInput(
                                inputId='q', 
                                label='ENM Content in Product, q (mg/g)', 
                                value=NULL
                            ),
                            tags$hr(),
                            div(actionButton('predict_button', 'Predict the response',class='btn btn-primary'),class='text-center')
                        ),
                        
                        mainPanel(
                            tabsetPanel(
                                tabPanel('Predict', 
                                    p(''),
                                    bsAlert('alert_predict'),
                                    bsAlert('alert_predict_parameter'),
                                    bsAlert('alert_predicted'),
                                    plotOutput('predict')
                                ),
                                
                                tabPanel('Conclusion', 
                                    p(''),
                                    bsAlert('alert_predict_stat'),
                                    htmlOutput('predict_stat')
                                )
                                
                            )
                        )
                    )
                )
            ),
            
            tabPanel('Glossary',
                titlePanel('Glossary'),
                
                fluidRow(
                    column(width = 10,
                        p(' ')
                    )
                ),
                
                fluidRow(
                    column(width = 10,
                        DT::dataTableOutput(outputId='table_glossary')
                    )
                )
            ),
            
            tabPanel('About',
                titlePanel('Team'),
                
                fluidRow(
                    column(width = 10,
                        p('')
                    )
                ),
                
                fluidRow(
                    column(width = 2,   
                        img(  src='tian.png', width = 120 )
                    ),
                    column(width = 8,   
                        h4('Yuan Tian', a(icon('envelope'), href='mailto:yt.210@duke.edu')),
                        p('Postdoctoral Associate'),
                        p('Duke University', a(icon('external-link-square'), href='http://www.ceint.duke.edu/profile/yuantian'),', Durham NC 27708 US')
                    )
                ),
                p(''),
                
                fluidRow(
                    column(width = 2,   
                        img( src='hendren.png', width = 120 )
                    ),
                    column(width = 8,  
                        h4('Christine Hendren', a(icon('envelope'), href='mailto:christine.hendren@duke.edu')),
                        p('CEINT Executive Director'),
                        p('Duke University', a(icon('external-link-square'), href='http://www.ceint.duke.edu/profile/christine-hendren'),', Durham NC 27708 US')
                    )
                ),
                p(''),
                
                fluidRow(
                    column(width = 2,   
                        img(   src='wiesner.png',  width = 120 )
                    ),
                    column(width = 8,  
                        h4('Mark Wiesner', a(icon('envelope'), href='mailto:wiesner@duke.edu')),
                        p('CEINT Director, Professor of Civil and Environmental Engineering'),
                        p('Duke University', a(icon('external-link-square'), href='http://wiesner.cee.duke.edu/'),', Durham NC 27708 US')
                    )
                )
            )
        )
        )
    )
