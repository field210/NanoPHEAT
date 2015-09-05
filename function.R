
# clear
rm(list = ls())

# list.of.packages <- c('tools','ggplot2', 'dplyr','gridExtra','shinyBS','minpack.lm','DT')
#
# new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,'Package'])]
# if(length(new.packages)) install.packages(new.packages)
#
# # library packages
# sapply(list.of.packages, library,character.only=T)

library('tools')
library('minpack.lm')
library('ggplot2')
library('dplyr')
library('gridExtra')
library('shinyBS')
library('DT')


# define public function

# reset reactivevalue rv
reset_rv=function(v, id, single=FALSE){
    order_selected= rvs%>%
        filter(value==id) %>%
        select(order) %>%
        .[[1]]
    order_last=ifelse(single,order_selected,nrow(rvs) )

    sapply(order_selected:order_last,function(x) {
        value_selected=rvs%>%
            filter(order==x) %>%
            select(value)%>%
            .[[1]]

        v[[value_selected]]=NULL
    })
}

# show alert
alert_on=function(session, df, id){
    item=df%>%filter(anchorId==id)
    anchorId=item %>%select(anchorId)%>%.[[1]]
    title=item %>%select(title)%>%.[[1]]
    content=item %>%select(content)%>%.[[1]]
    style=item %>%select(style)%>%.[[1]]

    createAlert(
        session=session,
        anchorId=anchorId,
        alertId=paste0('bs_', anchorId),
        title = title,
        content = content,
        style=style,
        append = FALSE
    )
}

# dismiss alert
alert_off=function(session, df, id, single=FALSE, type='error'){
    order_selected= df%>%
        filter(anchorId==id) %>%
        select(order) %>%
        .[[1]]
    order_last=ifelse(single,order_selected,
        df%>%
        filter(types==type) %>%
        select(order) %>%
        max
    )

    sapply(order_selected:order_last,function(x) {
        anchorId=df%>%
            filter(order==x) %>%
            select(anchorId)%>%
            .[[1]]
        alertId=paste0('bs_', anchorId)
        closeAlert(
            session=session,
            alertId=alertId
        )
    })
}

# reset select options
reset_select=function(session, df, id){
    order_selected= df%>%
        filter(inputId==id) %>%
        select(order) %>%
        .[[1]]
    order_last= nrow(df)

    sapply(order_selected:order_last,function(x) {
        select_id=paste0('select_',df%>%
                filter(order==x) %>%
                select(inputId)%>%
                .[[1]])
        if_empty=df%>%
            filter(order==x) %>%
            select(empty)%>%
            .[[1]]

        updateSelectizeInput(
            session=session,
            inputId=select_id,
            choices=if(if_empty==1) '' else  NULL,
            options=list(
                placeholder='Select an option',
                onInitialize=I('function() { this.setValue(""); }')
            ),
            server=F
        )
    })
}

# choose select option
choose_select=function(session,inputId,choices){
    updateSelectizeInput(
        session=session,
        inputId=inputId,
        choices=choices,
        server=F
    )
}

# set axis range for plot
axis_range=function(df,colname,extended=0.1){
    min=min(df[colname])
    max=max(df[colname])
    lower=min-(max-min)*extended
    upper=max+(max-min)*extended
    c(lower,upper)
}

# plot raw data without curve and fitting
plot_raw=function(df){
    dose_range=axis_range(df,'Dose')
    response_range=axis_range(df,'Response')

    ggplot( data=df, aes(x=Dose,y=Response)) +
        geom_point(size=5)+
        coord_cartesian(xlim=dose_range,ylim=response_range)+
        labs(title = 'Dose-response curve') +
        theme_bw() +
        theme(text=element_text(size=16),
            plot.title=element_text(vjust=3),
            axis.title.y=element_text(vjust=3),
            axis.title.x=element_text(vjust=-3),
            plot.margin=unit(c(1, 1, 1, 1), 'cm')
        )
}

# return true if fit succeed
fit_test=function(session,fit){
    if(length(fit)==1 ){
        # singular gradient matrix at initial parameter estimates
        alert_on(session,alert, id=  'alert_fitted_initial' )
        return(FALSE)
    } else {
        if(!fit$convInfo$isConv){
            # cannot converge, need change formula
            alert_on(session,alert, id=  'alert_fitted_converge'  )
            return(FALSE)
        } else{
            return(TRUE)
        }
    }
}
