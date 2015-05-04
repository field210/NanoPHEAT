alert=function(session,anchorId,title,content,style="danger",append=FALSE){
    createAlert(
        session=session, 
        anchorId=anchorId, 
        alertId=paste0("bs_", anchorId),  
        title = title,  
        content = content,
        style=style,  
        append = append )
}

# server.r
shinyServer(function(input, output, session) {
    # read ceint nikc data
    data=read.csv("data.csv",stringsAsFactors=FALSE)
    
    # define upload button
    upload_action=eventReactive(input$upload_button, { 
        inFile=input$file_data
        
        if (is.null(inFile)) { 
            alert(session,
                "alert_upload",
                "No file found!",
                "Please select a file to proceed."
                )
            return()
        }
        
        closeAlert(session, "bs_alert_upload")
        
        read.csv(inFile$datapath, header=as.logical(input$header),
            sep=input$sep, quote=input$quote,stringsAsFactors=FALSE)
    })
    
    # process uploaded data after click the button
    output$table_upload <- renderDataTable(upload_action(),
        options = list(pageLength = 10)
    )
    
    # initialize the enm select (level 1)
    enm=data %>% 
        select(Nanomaterial) %>% 
        distinct(Nanomaterial)%>% 
        .[[1]]
    updateSelectizeInput(session, 
        inputId='select_enm', 
        choices=enm, 
        server=F
    )
    
    # create reactive value for resetting
    v <- reactiveValues()
    
    # update endpoint select after changing enm value (level 2)
    observeEvent(input$select_enm, {
        endpoint=data %>% 
            filter(Nanomaterial==input$select_enm ) %>% 
            select(Endpoint) %>% 
            distinct(Endpoint)%>% 
            arrange(Endpoint)%>% 
            .[[1]]
        
        updateSelectizeInput(session, 
            inputId='select_endpoint', 
            choices=endpoint, 
            server=F
        )
        updateSelectizeInput(session, 
            inputId='select_organism', 
            choices="", 
            options=list(
                placeholder="Select an option",
                onInitialize=I("function() { this.setValue(''); }")
            ),
            server=F
        )
        updateSelectizeInput(session, 
            inputId='select_matrix', 
            choices="", 
            options=list(
                placeholder="Select an option",
                onInitialize=I("function() { this.setValue(''); }")
            ),
            server=F
        )
        
        v$data_filtered=NULL
        v$plot=NULL
    })
    
    # update organism select after changing endpoint value (level 3)
    observeEvent(input$select_endpoint, {
        organism=data %>% 
            filter(Nanomaterial==input$select_enm , 
                Endpoint==input$select_endpoint ) %>% 
            select(Organism) %>% 
            distinct(Organism)%>% 
            arrange(Organism)%>% 
            .[[1]]
        
        updateSelectizeInput(session,
            inputId='select_organism', 
            choices=organism,
            server=F
        )
        updateSelectizeInput(session, 
            inputId='select_matrix', 
            choices="", 
            options=list(
                placeholder="Select an option",
                onInitialize=I("function() { this.setValue(''); }")
            ),
            server=F
        )
        
        v$data_filtered=NULL
        v$plot=NULL
    })
    
    # update matrix select after changing organism value (level 4)
    observeEvent(input$select_organism, {
        matrix=data %>% 
            filter(Nanomaterial==input$select_enm , 
                Endpoint==input$select_endpoint, 
                Organism==input$select_organism ) %>% 
            select(Matrix) %>% 
            distinct(Matrix)%>% 
            arrange(Matrix)%>% 
            .[[1]]
        
        updateSelectizeInput(session,
            inputId='select_matrix', 
            choices=matrix ,
            server=F
        )
        
        v$data_filtered=NULL
        v$plot=NULL
    })
    
    # set to v$data_filtered after click the button
    observeEvent(input$filter_button, { 
        if (input$select_enm=="" | 
                input$select_endpoint=="" | 
                input$select_organism=="" | 
                input$select_matrix=="") {
            alert(session, 
                "alert_filter",  
                "No option selected!", 
                "Please select options above."
                )
            
            return()
        }
        
        closeAlert(session, "bs_alert_filter")
        closeAlert(session, "bs_alert_plot")
        
        v$data_filtered=data %>% 
            filter(Nanomaterial==input$select_enm , 
                Endpoint==input$select_endpoint, 
                Organism==input$select_organism,
                Matrix==input$select_matrix) 
        
        v$dose_lower=min(v$data_filtered$Dose)-(max(v$data_filtered$Dose)-min(v$data_filtered$Dose))*0.1
        v$dose_upper=max(v$data_filtered$Dose)+(max(v$data_filtered$Dose)-min(v$data_filtered$Dose))*0.1
        
        v$response_lower=min(v$data_filtered$Response)-(max(v$data_filtered$Response)-min(v$data_filtered$Response))*0.1
        v$response_upper=max(v$data_filtered$Response)+(max(v$data_filtered$Response)-min(v$data_filtered$Response))*0.1
    })
    
    # show filtered data after click the button
    output$table_filtered <- renderDataTable(
        v$data_filtered
    )
    
    
    # plot raw data after click plot button
    observeEvent(input$plot_button, { 
        if (is.null(v$data_filtered)) { 
            alert(session, 
                "alert_plot", 
                "No dataset selected!",  
                "Please select targeted dataset in the \"filter\" tab before plot."
                )
            
            return()
        }
        
        closeAlert(session, "bs_alert_plot")
        closeAlert(session, "bs_alert_fitting")
        
        v$plot= ggplot( data=v$data_filtered, aes(x=Dose,y=Response)) +
            geom_point(size=5)+ 
            coord_cartesian(xlim=c(v$dose_lower,v$dose_upper),ylim=c(v$response_lower,v$response_upper))+ 
            labs(title = "Dose-response curve") + 
            theme_bw() + 
            theme(text=element_text(size=16), 
                plot.title=element_text(vjust=3), 
                axis.title.y=element_text(vjust=3), 
                axis.title.x=element_text(vjust=-3), 
                plot.margin=unit(c(1, 1, 1, 1), "cm")
            )
    })
    
    # show plot without fitting 
    output$plot <- renderPlot({
        v$plot
    }) 
    
    # when changing select_fit_method, set values to fit_method, formula, func, parameter
    observeEvent(input$select_fit_method,{
        closeAlert(session, "bs_alert_fit_method")
        closeAlert(session, "bs_alert_curve")
        
        v$fit_method=input$select_fit_method
        
        if(v$fit_method=="linear"){
            v$formula=withMathJax('$$y=\\alpha+\\beta x$$')
            v$func=function(x) { 
                isolate({ 
                    linear_alpha=as.numeric(input$linear_alpha)
                    linear_beta=as.numeric(input$linear_beta)
                    invalid=is.na(linear_alpha) | is.na(linear_beta) 
                    func=linear_alpha * x + linear_beta
                    ifelse(invalid ,return(NULL),return ( func ))
                }) 
            }
            
            v$parameter=wellPanel(
                fluidRow(
                    strong("Parameter initial value")
                ),
                
                fluidRow(
                    column(width = 4,
                        numericInput(
                            inputId="linear_alpha", 
                            label=withMathJax('$$\\alpha$$'), 
                            value=1
                        )
                    ),
                    
                    column(width = 4,
                        numericInput(
                            inputId="linear_beta", 
                            label=withMathJax('$$\\beta$$'), 
                            value=0
                        )
                    )
                ),
                
                fluidRow(
                    actionButton("curve_button", "Show curve using parameter initial value",class="btn btn-primary")
                )
            )
        }
        
        if(v$fit_method=="logistic"){
            v$formula=withMathJax('$$y=\\frac{L}{1+e^{-k(x-x_0)}}$$')
            v$func=function(x) { 
                isolate({ 
                    logistic_l=as.numeric(input$logistic_l)
                    logistic_k=as.numeric(input$logistic_k)
                    logistic_x0=as.numeric(input$logistic_x0)
                    invalid=is.na(logistic_l) | is.na(logistic_k) | is.na(logistic_x0)
                    func=logistic_l / (1 + exp(-logistic_k * (x - logistic_x0 )))
                    ifelse(invalid ,return(NULL),return ( func ))
                }) 
            }
            v$parameter=wellPanel(
                fluidRow(
                    strong("Parameter initial value")
                ),
                
                fluidRow(
                    column(width = 4,
                        numericInput(
                            inputId="logistic_l", 
                            label=withMathJax('$$L$$'), 
                            value=0.1
                        )
                    ),
                    
                    column(width = 4,
                        numericInput(
                            inputId="logistic_k", 
                            label=withMathJax('$$k$$'), 
                            value=1
                        )
                    ),
                    
                    column(width = 4,
                        numericInput(
                            inputId="logistic_x0", 
                            label=withMathJax('$$x_0$$'), 
                            value=5
                        )
                    )
                ),
                
                fluidRow(
                    actionButton("curve_button", "Show curve using parameter initial value",class="btn btn-primary")
                )
            )
        }
    })
    
    # show fitting formula 
    output$text_formula <- renderUI({
        if (v$fit_method=="") { 
            return()
        }
        
        
        v$formula
    })
    
    
    # show parameter control
    output$text_parameter <- renderUI({
        if (v$fit_method=="") { 
            return()
        }
        
        v$parameter
    })
    
    # show function plot
    observeEvent(input$curve_button, { 
        if(is.null( v$plot)) { 
            alert(session, 
                "alert_fitting", 
                "No plot found!", 
                "Please plot dataset to proceed."
                )
            
            return()
        }
        
        closeAlert(session, "bs_alert_fitting")
        
        if(is.null( v$func(0))) { 
            alert(session,
                "alert_curve",
                "Invalid parameter!", 
                "Please input number only to proceed."
                )
            
            return()
        }
        
        closeAlert(session, "bs_alert_curve")
        
        
        v$plot=v$plot+  stat_function(fun = v$func,color="blue")
    }) 
    
    # plot fitting after click fit button
    observeEvent(input$fit_button, { 
        if(is.null( v$plot)) {
            alert(session, 
                "alert_fitting", 
                "No plot found!", 
                "Please plot dataset to proceed."
                )
            
            return()
        }
        
        closeAlert(session, "bs_alert_fitting")
        closeAlert(session, "bs_alert_fit_stat")
        
        if(v$fit_method=="") {
            alert(session, 
                "alert_fit_method", 
                "No fitting method selected!",
                "Please select a fitting method to proceed."
                )
            
            return()
        }
        
        if (v$fit_method=="linear") {
            fit=lm(Response~Dose, data=v$data_filtered)
            
            linear_alpha=coef(fit)["Dose"]
            linear_beta=coef(fit)["(Intercept)"] 
            
            v$plot=v$plot+
                stat_function(fun =function(x) { 
                    linear_alpha * x + linear_beta
                    },
                    color="red"
                    )
            v$fit_stat=summary(fit)
        } 
        
        if(v$fit_method=="logistic"){
            fit=try(
                nlsLM(Response~l / (1 + exp(-k * (Dose - x0 ))),
                data=v$data_filtered, 
                start = list(l=input$logistic_l, 
                    k=input$logistic_k, 
                    x0=input$logistic_x0)
                ),
                silent = T
                )
            
            if(length(fit)==1){
                alert(session, 
                    "alert_fitted",
                    "Fitting failed!",  
                    "Please select reasonable value for the initial parameters and try again."
                    )
            } else {
                closeAlert(session, "bs_alert_fitted")
                
                logistic_l=coef(fit)["l"] 
                logistic_k=coef(fit)["k"] 
                logistic_x0=coef(fit)["x0"] 
                
            v$plot=v$plot+
                stat_function(fun =function(x) { 
                    logistic_l / (1 + exp(-logistic_k * (x - logistic_x0 ))) 
                    },
                    color="red")
                v$fit_stat=summary(fit)
                
            }
        }
        
        
    })
    
    
    # show fitting statistics 
    output$fit_stat <- renderPrint({
        if (is.null(v$fit_stat)) { 
            alert(session, 
                "alert_fit_stat",
                "No fitting statistics available!", 
                "Please try to fit the dataset first.",
                "info",  
                )
            
            return(invisible(NULL))
        }
        
        closeAlert(session, "bs_alert_fit_stat")
        
        v$fit_stat
    })
    
    
})
