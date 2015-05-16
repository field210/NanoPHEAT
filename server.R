
# server.r
shinyServer(function(input, output, session) {
    # create reactive value for resetting
    v = reactiveValues()
    
    # define upload ui
    v$ui_upload=  fluidRow(
        column(width = 4,  
            wellPanel(style='min-height:200px;',
                fileInput(
                    inputId='upload_data', 
                    label='Your dataset',
                    accept = c(
                        'text/csv',
                        'text/comma-separated-values',
                        'text/tab-separated-values',
                        'text/plain',
                        '.csv'
                    )
                ),
                p('Dataset template: ',
                    a(href = 'NanoPHEAT.csv', 'NanoPHEAT.csv'))
            )
        ),
        
        column(width = 2, 
            wellPanel(style='min-height:200px;',
                radioButtons(
                    inputId='header', 
                    label = 'Header',
                    choices =  c(Yes=TRUE, No=FALSE),
                    selected =     TRUE
                )
            )
        ),
        
        column(width = 2,
            wellPanel(style='min-height:200px;',
                radioButtons(
                    inputId='sep', 
                    label = 'Separator',
                    choices =  c(Comma=',', Semicolon=';',  Tab='\t'),
                    selected =  ','
                )
            )
        ),
        
        column(width = 2,
            wellPanel(style='min-height:200px;',
                radioButtons(
                    inputId='quote',
                    label =  'Quote',
                    choices =  c(None='', 'Double'='\"',   'Single'='\''),
                    selected =  '\"'
                )
            )
        )
    )
    
    # clear data when change data source
    observeEvent(input$data_source,{
        alert_off(session, 1:alert_error_row)
        v$source_user=NULL
        v$data=NULL
        
        reset_select(session, c('select_enm','select_endpoint', 'select_organism','select_matrix'))
    } )
    
    # show upload ui
    output$ui_upload = renderUI({
        if (input$data_source==1) { 
            return()
        }
        v$ui_upload
    })
    
    # observe the uploaded file
    observeEvent(input$upload_data,{
        v$source_user=input$upload_data
    })
    
    # define load button
    observeEvent(input$load_button, {
        alert_off(session, 1:alert_error_row)
        reset_select(session, c('select_enm','select_endpoint', 'select_organism','select_matrix'))
        
        if (input$data_source==1) {
            v$data=data_ceint
        } else {
            if (is.null(v$source_user)) { 
                alert_on(session,1)
                return()
            }
            data_user=read.csv(v$source_user$datapath, 
                header=as.logical(input$header),
                sep=input$sep, 
                quote=input$quote,
                stringsAsFactors=FALSE)
            
            v$data=switch(input$data_source,
                '2'=data_user,
                '3'=bind_rows(data_user,data_ceint)
            )
        }
        
        # initialize the enm select (level 1)
        enm=v$data %>% 
            select(Nanomaterial) %>% 
            distinct(Nanomaterial)%>% 
            .[[1]]
        
        choose_select(session,'select_enm', enm)
    })
    
    # process uploaded data after click the button
    output$table_load = renderDataTable(v$data,
        options = list(pageLength = 10)
    )
    
    # update endpoint select after changing enm value (level 2)
    observeEvent(input$select_enm, {
        if(input$select_enm==''){
            return()
        }
        
        alert_off(session, 2:alert_error_row)
        
        endpoint=v$data %>% 
            filter(Nanomaterial==input$select_enm ) %>% 
            select(Endpoint) %>% 
            distinct(Endpoint)%>% 
            arrange(Endpoint)%>% 
            .[[1]]
        
        choose_select(session,'select_endpoint', endpoint)
        reset_select(session, c( 'select_organism','select_matrix'))
        
        v$data_filtered=NULL
        v$plot=NULL
    })
    
    # update organism select after changing endpoint value (level 3)
    observeEvent(input$select_endpoint, {
        if(input$select_endpoint==''){
            return()
        }
        
        alert_off(session, 2:alert_error_row)
        
        organism=v$data %>% 
            filter(Nanomaterial==input$select_enm , 
                Endpoint==input$select_endpoint ) %>% 
            select(Organism) %>% 
            distinct(Organism)%>% 
            arrange(Organism)%>% 
            .[[1]]
        
        choose_select(session,'select_organism', organism)
        reset_select(session, c('select_matrix'))
        
        v$data_filtered=NULL
        v$plot=NULL
    })
    
    # update matrix select after changing organism value (level 4)
    observeEvent(input$select_organism, {
        if(input$select_organism==''){
            return()
        }
        
        alert_off(session, 2:alert_error_row)
        
        matrix=v$data %>% 
            filter(Nanomaterial==input$select_enm , 
                Endpoint==input$select_endpoint, 
                Organism==input$select_organism ) %>% 
            select(Matrix) %>% 
            distinct(Matrix)%>% 
            arrange(Matrix)%>% 
            .[[1]]
        
        choose_select(session,'select_matrix',  matrix)
        
        v$data_filtered=NULL
        v$plot=NULL
    })
    
    # set to v$data_filtered after click the button
    observeEvent(input$filter_button, { 
        if (input$select_enm=='' | 
                input$select_endpoint=='' | 
                input$select_organism=='' | 
                input$select_matrix=='') {
            alert_on(session, 2  )
            return()
        }
        
        alert_off(session, 2:alert_error_row)
        
        v$data_filtered=v$data %>% 
            filter(Nanomaterial==input$select_enm , 
                Endpoint==input$select_endpoint, 
                Organism==input$select_organism,
                Matrix==input$select_matrix) 
    })
    
    # show filtered data after click the button
    output$table_filtered = renderDataTable(
        v$data_filtered
    )
    
    
    # plot raw data after click plot button
    observeEvent(input$plot_button, { 
        if (is.null(v$data_filtered)) { 
            alert_on(session,3 )
            return()
        }
        
        alert_off(session, 3:alert_error_row)
        
        v$plot_raw= plot_raw(v$data_filtered)
        v$plot= v$plot_raw
    })
    
    # show plot without fitting 
    output$plot = renderPlot({
        v$plot
    }) 
    
    # initiate select_fit_method from data frame models
    choose_select(session,'select_fit_method', models$term)
    v$fit_method=''
    
    # when changing select_fit_method, set values to fit_method, formula, func, parameter
    observeEvent(input$select_fit_method,{
        if(input$select_fit_method==''){
            v$fit_method=''
            return()
        }
        
        v$fit=NULL
        v$plot= v$plot_raw
        alert_off(session, 5:alert_error_row)
        
        model_selected=models%>%
            filter(term==input$select_fit_method)
        
        v$fit_method=model_selected%>%
            select(term) %>% 
            .[[1]]
        
        v$formula=eval(parse(
            text=model_selected%>%
                select(formula) %>% 
                .[[1]]
        ))
        
        v$func=eval(parse(
            text=model_selected%>%
                select(func) %>% 
                .[[1]]
        ))
        
        parameter_title="wellPanel(
        fluidRow(
        strong('Parameter initial value')
        )"
        
        parameter_content=model_selected%>%
            select(ui) %>% 
            .[[1]]
        
        parameter_button="fluidRow(
        actionButton('curve_button', 'Show curve using parameter initial value',class='btn btn-primary')
    )
        )"
        
        parameter=paste(parameter_title,parameter_content,parameter_button,sep=',')
        
        v$parameter=eval(parse(text=parameter))
    })
    
    # show fitting formula 
    output$text_formula = renderUI({
        if (input$select_fit_method=='') { 
            return()
        }
        v$formula
    })
    
    
    # show parameter control
    output$text_parameter = renderUI({
        if (input$select_fit_method=='') { 
            return()
        }
        v$parameter
    })
    
    # show function plot
    observeEvent(input$curve_button, { 
        if(is.null( v$plot)) { 
            alert_on(session, 4)
            return()
        }
        
        alert_off(session,4:alert_error_row)
        
        if(is.null( v$func(0))) { 
            alert_on(session,6)
            return()
        }
        
        alert_off(session, 6:alert_error_row)
        
        v$plot=v$plot_raw+  stat_function(fun = v$func,color='blue',data=data.frame(Dose=axis_range(v$data_filtered,'Dose'),Response=c(0)),n=500)
    }) 
    
    # plot fitting after click fit button
    observeEvent(input$fit_button, { 
        v$fit=NULL
        
        if(is.null( v$plot)) {
            alert_on(session, 4)
            return()
        }
        
        alert_off(session, 4:alert_error_row)
        
        if(v$fit_method=='') {
            alert_on(session, 5)
            return()
        }
        
        if (v$fit_method=='linear') {
            fit=lm(Response~Dose, data=v$data_filtered)
            linear_alpha=coef(fit)['Dose']
            linear_beta=coef(fit)['(Intercept)'] 
            func =function(x) { 
                linear_alpha * x + linear_beta
            }
        } 
        
        if(v$fit_method=='logistic'){
            fit=try(
                nlsLM(Response~l / (1 + exp(-k * (Dose - x0 ))),
                    data=v$data_filtered, 
                    start = list(l=input$logistic_l, 
                        k=input$logistic_k, 
                        x0=input$logistic_x0)
                ),
                silent = T
            )
            
            if(!fit_test(session,fit)) {
                return()
            } else{
                alert_off(session, 7:alert_error_row)
                
                logistic_l=coef(fit)['l'] 
                logistic_k=coef(fit)['k'] 
                logistic_x0=coef(fit)['x0'] 
                func =function(x) { 
                    logistic_l / (1 + exp(-logistic_k * (x - logistic_x0 ))) 
                }
                
            }
        }
        
        # set predict function 
        v$func_predict=func
        
        # set plot and fit if succeed 
        v$plot=v$plot_raw+  stat_function(fun = v$func_predict,color='red',data=data.frame(Dose=axis_range(v$data_filtered,'Dose'),Response=c(0)),n=500)
        
        v$fit=fit
        
        # also set plot to prediction
        v$plot_predict=v$plot
    })
    
    # show fitting statistics 
    output$fit_stat = renderPrint({
        if (is.null(v$fit)) { 
            alert_on(session, 1 ,alert=alert_info)
            
            return(invisible(NULL))
        }
        
        alert_off(session, 1,alert=alert_info)
        
        summary( v$fit)
    })
    
    # show plot with fitting 
    output$predict = renderPlot({
        v$plot_predict
    }) 
    
    # use default potency factor
    observeEvent(input$pf_button,{
        pf= pf_ceint%>% 
            filter(Nanomaterial==input$select_enm , 
                Matrix==input$select_matrix ) %>% 
            select(PF) %>% 
            .[[1]]
        
        updateNumericInput(session, 
            inputId='pf',
            value=pf
        )  
    })
    
    # plot prediction 
    observeEvent(input$predict_button,{
        v$plot_predict=v$plot
        v$predict_stat=NULL
        
        if (is.null(v$fit)) { 
            alert_on(session, 9 )
            return()
        }
        
        alert_off(session, 9:alert_error_row)
        
        pf=as.numeric(input$pf)
        q=as.numeric(input$q)
        m=as.numeric(input$m)
        invalid=pf<0 | q<0 | m<0 | is.na(pf) | is.na(q) | is.na(m)
        
        if(invalid){
            alert_on(session,10)
            return()
        }
        
        alert_off(session,10:alert_error_row)
        
        # calculate the effective dose
        dose=q* m* pf
        response=v$func_predict(dose)
        
        # determine new range for extrapolation
        df=bind_rows(v$data_filtered,data.frame(Dose=dose,Response=response))  
        dose_range=axis_range(df,'Dose')
        response_range=axis_range(df,'Response')
        
        # plot prediction together with model
        v$plot_predict=v$plot_raw +
            coord_cartesian(xlim=dose_range,ylim=response_range)+ 
            stat_function(fun =v$func_predict, color='red',data=data.frame(Dose=axis_range(df,'Dose'),Response=c(0)),n=500)+  
            geom_point(x=dose,y=response,color='green',size=5)+ 
            geom_segment(x=dose,y=response_range[1],xend=dose,yend=response, color='grey', linetype='dashed') +
            geom_segment(x=dose_range[1],y=response,xend=dose,yend=response, color='grey', linetype='dashed')
        
        # if response less than 0, give a warning
        if(response<0){
            alert_on(session,11)
            return()
        }
        
        alert_off(session,11:alert_error_row)
        
        # query the direction more? or less?
        direction=v$data_filtered %>% 
            select(Direction) %>% 
            distinct(Direction)%>% 
            .[[1]]
        
        # prediction statement
        v$predict_stat=paste0( 'With an applied dose of ',
            signif(  dose,3) ,
            ' mg/L of ',
            input$select_enm ,
            ' as released from a ',
            input$select_matrix,
            ' matrix, with a resulting potency factor of ',
            input$pf,
            ', one could expect the ',
            input$select_endpoint ,
            ' of ',
            input$select_organism,
            ' to be ',
            signif(  response,3),
            ' times ',
            direction,
            ' than a control system with no exposure to ',
            input$select_enm ,
            '.')
        
    })
    
    # show prediction statement 
    output$predict_stat = renderText({
        if (is.null(v$predict_stat)) { 
            alert_on(session, 2, alert=alert_info)  
            return()
        }
        
        alert_off(session, 2, alert=alert_info)    
        v$predict_stat
    })
    
    # show glossary table
    output$table_glossary = renderDataTable(glossary,
        options = list(pageLength = 10)
    )
    
    
})

