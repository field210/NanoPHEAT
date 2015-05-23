
# read ceint pre-defined data: toxicity data, potency factor, alert file, model file , glossary
filenames = list.files(pattern='.csv')
filelist = lapply(filenames, function(x) read.csv(x,stringsAsFactors=FALSE) )
names(filelist) =filenames
invisible(lapply(filenames, function(x) assign(file_path_sans_ext(x),filelist[[x]],envir=.GlobalEnv)))


# server.r
shinyServer(function(input, output, session) {
    # create reactive value for resetting
    rv = reactiveValues()
    reset_rv(rv, id='source_user')
    
    # clear data when change data source
    observeEvent(input$data_source,{
        alert_off(session, alert, id= 'alert_file')
        reset_rv(rv, id='source_user')
        reset_select(session, selects, id = 'enm')
    })
    
    # show upload ui
    output$ui_upload = renderUI({
        if (input$data_source==1) { 
            return()
        }
        fluidRow(
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
                        selected = TRUE
                    )
                )
            ),
            
            column(width = 2,
                wellPanel(style='min-height:200px;',
                    radioButtons(
                        inputId='sep', 
                        label = 'Separator',
                        choices =  c('Comma'=',', 'Semicolon'=';',  'Tab'='\t'),
                        selected = ','
                    )
                )
            ),
            
            column(width = 2,
                wellPanel(style='min-height:200px;',
                    radioButtons(
                        inputId='quote',
                        label =  'Quote',
                        choices =  c('None'='', 'Double'='\"', 'Single'='\''),
                        selected =  '\"'
                    )
                )
            )
        )
    })
    
    # observe the uploaded file
    observeEvent(input$upload_data,{
        rv$source_user=input$upload_data
    })
    
    # define load button
    observeEvent(input$load_button, {
        alert_off(session, alert, id= 'alert_file')
        reset_rv(rv, id='data')
        reset_select(session, selects, id = 'enm')
        
        if (input$data_source==1) {
            rv$data=data_ceint
        } else {
            if (is.null(rv$source_user)) { 
                alert_on(session, alert, id='alert_file')
                return()
            }
            data_user=read.csv(rv$source_user$datapath, 
                header=as.logical(input$header),
                sep=input$sep, 
                quote=input$quote,
                stringsAsFactors=FALSE)
            
            if(!identical(lapply(data_ceint, class),lapply(data_user, class))){
                alert_on(session, alert, id='alert_format')
                return()
            } else{
    
            rv$data=switch(input$data_source,
                '2'=data_user,
                '3'=bind_rows(data_user,data_ceint)
            )
            }
        }
        
        # initialize the enm select (level 1)
        enm=rv$data %>% 
            select(Nanomaterial) %>% 
            distinct(Nanomaterial)%>% 
            .[[1]]
        
        choose_select(session,'select_enm', enm)
    })
    
    # process uploaded data after click the button
    output$table_load = renderDataTable(rv$data,
        options = list(pageLength = 10)
    )
    
    # update endpoint select after changing enm value (level 2)
    observeEvent(input$select_enm, {
        reset_rv(rv, id='data_filtered')
        if(input$select_enm==''){
            return()
        }
        
        alert_off(session, alert, id= 'alert_filter')
        
        endpoint=rv$data %>% 
            filter(Nanomaterial==input$select_enm ) %>% 
            select(Endpoint) %>% 
            distinct(Endpoint)%>% 
            arrange(Endpoint)%>% 
            .[[1]]
        
        choose_select(session,'select_endpoint', endpoint)
        reset_select(session, selects, id = 'organism')
    })
    
    # update organism select after changing endpoint value (level 3)
    observeEvent(input$select_endpoint, {
        reset_rv(rv, id='data_filtered')
        if(input$select_endpoint==''){
            return()
        }
        
        alert_off(session, alert, id= 'alert_filter')
        
        organism=rv$data %>% 
            filter(Nanomaterial==input$select_enm , 
                Endpoint==input$select_endpoint ) %>% 
            select(Organism) %>% 
            distinct(Organism)%>% 
            arrange(Organism)%>% 
            .[[1]]
        
        choose_select(session,'select_organism', organism)
        reset_select(session, selects, id = 'matrix')
    })
    
    # update matrix select after changing organism value (level 4)
    observeEvent(input$select_organism, {
        reset_rv(rv, id='data_filtered')
        if(input$select_organism==''){
            return()
        }
        
        alert_off(session, alert, id= 'alert_filter')
        
        matrix=rv$data %>% 
            filter(Nanomaterial==input$select_enm , 
                Endpoint==input$select_endpoint, 
                Organism==input$select_organism ) %>% 
            select(Matrix) %>% 
            distinct(Matrix)%>% 
            arrange(Matrix)%>% 
            .[[1]]
        
        choose_select(session,'select_matrix',  matrix)
    })
    
    # set to rv$data_filtered after click the button
    observeEvent(input$filter_button, { 
        reset_rv(rv, id='data_filtered')
        if (input$select_enm=='' | 
                input$select_endpoint=='' | 
                input$select_organism=='' | 
                input$select_matrix=='') {
            alert_on(session, alert, id= 'alert_filter')
            return()
        }
        
        alert_off(session, alert, id= 'alert_filter')
        
        rv$data_filtered=rv$data %>% 
            filter(Nanomaterial==input$select_enm , 
                Endpoint==input$select_endpoint, 
                Organism==input$select_organism,
                Matrix==input$select_matrix) 
    })
    
    # show filtered data after click the button
    output$table_filtered = renderDataTable(
        rv$data_filtered
    )
    
    # plot raw data after click plot button
    observeEvent(input$plot_button, { 
        reset_rv(rv, id='plot_raw')
        if (is.null(rv$data_filtered)) { 
            alert_on(session, alert, id= 'alert_subset')
            return()
        }
        
        alert_off(session, alert, id= 'alert_subset')
        
        rv$plot_raw= plot_raw(rv$data_filtered)
        rv$plot= rv$plot_raw
    })
    
    # show plot without fitting 
    output$plot = renderPlot({
        rv$plot
    }) 
    
    # initiate select_model from data frame models
    choose_select(session,'select_model', models$term)
    
    # when changing select_model, set values to term, formula, func, parameter
    observeEvent(input$select_model,{
        reset_rv(rv, id='model')
        if(input$select_model==''){
            reset_rv(rv, id='model')
            return()
        }
        
        rv$plot= rv$plot_raw
        alert_off(session, alert, id= 'alert_fit_method')
        
        rv$model=models%>%
            filter(term==input$select_model)
        
        rv$term=rv$model%>%
            select(term) %>% 
            .[[1]]
        
        rv$formula=eval(parse(
            text=rv$model%>%
                select(formula) %>% 
                .[[1]]
        ))
        
        rv$func=eval(parse(
            text=rv$model%>%
                select(func) %>% 
                .[[1]]
        ))
        
        parameter_title="wellPanel(
        fluidRow(column(width = 12,
        strong('Parameter initial value')
        )
        )"
        
        parameter_content=rv$model%>%
            select(ui) %>% 
            .[[1]]
        
        parameter_button="fluidRow(
        div(actionButton('curve_button', 'Show curve using parameter initial value',class='btn btn-primary'),class='text-center')
    )
        )"
        
        parameter=paste(parameter_title,parameter_content,parameter_button,sep=',')
        
        rv$parameter=eval(parse(text=parameter))
    })
    
    # show fitting formula 
    output$text_formula = renderUI({
        if (input$select_model=='') { 
            return()
        }
        rv$formula
    })
    
    
    # show parameter control
    output$text_parameter = renderUI({
        if (input$select_model=='') { 
            return()
        }
        rv$parameter
    })
    
    # show function plot
    observeEvent(input$curve_button, { 
        if(is.null( rv$plot)) { 
            alert_on(session, alert, id= 'alert_plot')
            return()
        }
        
        alert_off(session, alert, id= 'alert_plot')
        
        if(is.null( rv$func(0))) { 
            alert_on(session, alert, id='alert_curve')
            return()
        }
        
        alert_off(session, alert, id= 'alert_curve')
        
        rv$plot=rv$plot_raw+  stat_function(fun = rv$func,color='blue',data=data.frame(Dose=axis_range(rv$data_filtered,'Dose'),Response=c(0)),n=500)
    }) 
    
    # plot fitting after click fit button
    observeEvent(input$fit_button, { 
        reset_rv(rv, id='fitted')
        if(is.null( rv$plot)) {
            alert_on(session, alert, id= 'alert_plot')
            return()
        }
        
        alert_off(session, alert, id= 'alert_plot')
        
        if(is.null(rv$term)) {
            alert_on(session, alert, id= 'alert_fit_method')
            return()
        }
        
        fit=eval(parse(
            text=rv$model%>%
                select(fitting) %>% 
                .[[1]]
        ))
        
        if(!fit_test(session,fit)) {
            return()
        } else{
            alert_off(session, alert, id= 'alert_fitted_initial')
            # set predict function 
            rv$func_fitted =eval(parse(
                text=rv$model%>%
                    select(fitted) %>% 
                    .[[1]]
            ))
        }

        # set plot and fit if succeed 
        rv$plot=rv$plot_raw+  stat_function(fun = rv$func_fitted,color='red',data=data.frame(Dose=axis_range(rv$data_filtered,'Dose'),Response=c(0)),n=500)
        
        rv$fitted=fit
        
        # also set plot to prediction
        rv$plot_predict=rv$plot
    })
    
    # show fitting statistics 
    output$fit_stat = renderPrint({
        if (is.null(rv$fitted)) { 
            alert_on(session, alert, id= 'alert_fit_stat')
            return(invisible(NULL))
        }
        alert_off(session, alert, id= 'alert_fit_stat',single=T,type='note')
        summary( rv$fitted)
    })
    
    # show plot with fitting 
    output$predict = renderPlot({
        rv$plot_predict
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
        reset_rv(rv, id='stat_predict')
        rv$plot_predict=rv$plot
        if (is.null(rv$fitted)) { 
            alert_on(session, alert, id= 'alert_predict' )
            return()
        }
        
        alert_off(session, alert, id=  'alert_predict' )
        
        pf=as.numeric(input$pf)
        q=as.numeric(input$q)
        m=as.numeric(input$m)
        invalid=pf<0 | q<0 | m<0 | is.na(pf) | is.na(q) | is.na(m)
        
        if(invalid){
            alert_on(session, alert, id='alert_predict_parameter')
            return()
        }
        
        alert_off(session, alert, id='alert_predict_parameter')
        
        # calculate the effective dose
        dose=q* m* pf
        response=rv$func_fitted(dose)
        
        # determine new range for extrapolation
        df=bind_rows(rv$data_filtered,data.frame(Dose=dose,Response=response))  
        dose_range=axis_range(df,'Dose')
        response_range=axis_range(df,'Response')
        
        # plot prediction together with model
        rv$plot_predict=rv$plot_raw +
            coord_cartesian(xlim=dose_range,ylim=response_range)+ 
            stat_function(fun =rv$func_fitted, color='red',data=data.frame(Dose=axis_range(df,'Dose'),Response=c(0)),n=500)+  
            geom_point(x=dose,y=response,color='green',size=5)+ 
            geom_segment(x=dose,y=response_range[1],xend=dose,yend=response, color='grey', linetype='dashed') +
            geom_segment(x=dose_range[1],y=response,xend=dose,yend=response, color='grey', linetype='dashed')
        
        # if response less than 0, give a warning
        if(response<0){
            alert_on(session, alert, id='alert_predicted')
            return()
        }
        
        alert_off(session, alert, id='alert_predicted')
        
        # query the direction more? or less?
        direction=rv$data_filtered %>% 
            select(Direction) %>% 
            distinct(Direction)%>% 
            .[[1]]
        
        # prediction statement
        rv$stat_predict=paste0( 'With an applied dose of ',
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
    output$predict_stat = renderPrint({
        if (is.null(rv$stat_predict)) { 
            alert_on(session, alert, id= 'alert_predict_stat')  
            return(invisible())
        }
        
        alert_off(session, alert, id= 'alert_predict_stat',single=T,type='note')    
        p(rv$stat_predict,class='alert alert-success lead')
    })
    
    # show glossary table
    output$table_glossary = renderDataTable(glossary,
        options = list(pageLength = 10)
    )
    
    
})

