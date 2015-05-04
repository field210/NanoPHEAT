# define public function
alert_on=function(session,id,title,content,style="danger",append=FALSE){
    createAlert(
        session=session, 
        anchorId=id, 
        alertId=paste0("bs_", id),  
        title = title,  
        content = content,
        style=style,  
        append = append )
}

alert_off=function(session, bs_id){
    sapply(bs_id,function(x) {
        closeAlert(session,alertId=x)
        })
}

axis_range=function(df,colname,extended=0.1){
    min=min(df[colname])
    max=max(df[colname])
    lower=min-(max-min)*extended
    upper=max+(max-min)*extended
    c(lower,upper)
}

plot_raw=function(df){
    dose_range=axis_range(df,"Dose")
    response_range=axis_range(df,"Response")
    
    ggplot( data=df, aes(x=Dose,y=Response)) +
        geom_point(size=5)+ 
        coord_cartesian(xlim=dose_range,ylim=response_range)+ 
        labs(title = "Dose-response curve") + 
        theme_bw() + 
        theme(text=element_text(size=16), 
            plot.title=element_text(vjust=3), 
            axis.title.y=element_text(vjust=3), 
            axis.title.x=element_text(vjust=-3), 
            plot.margin=unit(c(1, 1, 1, 1), "cm")
        )
}

# server.r
shinyServer(function(input, output, session) {
    # read ceint nikc data
    data=read.csv("data.csv",stringsAsFactors=FALSE)
    
    # define upload button
    upload_action=eventReactive(input$upload_button, { 
        inFile=input$file_data
        
        if (is.null(inFile)) { 
            alert_on(session,
                "alert_file",
                "No file found!",
                "Please select a file to proceed."
                )
            return()
        }
        
        alert_off(session, c("bs_alert_file", "bs_alert_filter", "bs_alert_subset", "bs_alert_plot", "bs_alert_fit_method", "bs_alert_curve", "bs_alert_fitted"))
        
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
            alert_on(session, 
                "alert_filter",  
                "No option selected!", 
                "Please select options above."
                )
            
            return()
        }
        
        alert_off(session, c("bs_alert_filter", "bs_alert_subset", "bs_alert_plot", "bs_alert_fit_method", "bs_alert_curve", "bs_alert_fitted"))
        
        v$data_filtered=data %>% 
            filter(Nanomaterial==input$select_enm , 
                Endpoint==input$select_endpoint, 
                Organism==input$select_organism,
                Matrix==input$select_matrix) 
    })
    
    # show filtered data after click the button
    output$table_filtered <- renderDataTable(
        v$data_filtered
    )
    
    
    # plot raw data after click plot button
    observeEvent(input$plot_button, { 
        if (is.null(v$data_filtered)) { 
            alert_on(session, 
                "alert_subset", 
                "No dataset selected!",  
                "Please select targeted dataset in the \"filter\" tab before plot."
                )
            
            return()
        }
        
        alert_off(session, c("bs_alert_subset", "bs_alert_plot", "bs_alert_fit_method", "bs_alert_curve", "bs_alert_fitted"))
        
        v$plot= plot_raw(v$data_filtered)
    })
    
    # show plot without fitting 
    output$plot <- renderPlot({
        v$plot
    }) 
    
    # when changing select_fit_method, set values to fit_method, formula, func, parameter
    observeEvent(input$select_fit_method,{

        alert_off(session, c("bs_alert_fit_method", "bs_alert_curve", "bs_alert_fitted"))
        
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
            alert_on(session, 
                "alert_plot", 
                "No plot found!", 
                "Please plot dataset to proceed."
                )
            
            return()
        }
        
        alert_off(session,c("bs_alert_plot", "bs_alert_curve", "bs_alert_fitted"))
        
        if(is.null( v$func(0))) { 
            alert_on(session,
                "alert_curve",
                "Invalid parameter!", 
                "Please input number only to proceed."
                )
            
            return()
        }
        
        alert_off(session, c("bs_alert_curve", "bs_alert_fitted"))
        v$plot= plot_raw(v$data_filtered)
        v$plot=v$plot+  stat_function(fun = v$func,color="blue")
    }) 
    
    # plot fitting after click fit button
    observeEvent(input$fit_button, { 
        if(is.null( v$plot)) {
            alert_on(session, 
                "alert_plot", 
                "No plot found!", 
                "Please plot dataset to proceed."
                )
            
            return()
        }
        
        alert_off(session, c("bs_alert_plot", "bs_alert_curve", "bs_alert_fit_stat", "bs_alert_fitted"))
        
        if(v$fit_method=="") {
            alert_on(session, 
                "alert_fit_method", 
                "No fitting method selected!",
                "Please select a fitting method to proceed."
                )
            
            return()
        }
        
        v$plot= plot_raw(v$data_filtered)
        
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
                alert_on(session, 
                    "alert_fitted",
                    "Fitting failed!",  
                    "Please select reasonable value for the initial parameters and try again."
                    )
            } else {
                alert_off(session, c("bs_alert_fitted"))
                
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
            alert_on(session, 
                "alert_fit_stat",
                "No fitting statistics available!", 
                "Please try to fit the dataset first.",
                "info",  
                )
            
            return(invisible(NULL))
        }
        
        alert_off(session, c("bs_alert_fit_stat"))
        
        v$fit_stat
    })
    
    
})

