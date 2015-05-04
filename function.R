
# install package if not installed
list.of.packages <- c("minpack.lm","ggplot2", "dplyr","gridExtra","shinyBS")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# library packages
sapply(list.of.packages, library,character.only=T)

# define public function
# show alert
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

# dismiss alert
alert_off=function(session, bs_id){
    sapply(bs_id,function(x) {
        closeAlert(session,alertId=x)
    })
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

# test if fit succeed
fit_test=function(session,fit){
    if(length(fit)==1 ){
        # singular gradient matrix at initial parameter estimates
        alert_on(session, 
            "alert_fitted",
            "Fitting failed!",  
            "Please select reasonable value for the initial parameters and try again."
        )
        return(FALSE)
    } else {
        if(!fit$convInfo$isConv){
            # cannot converge, need change formula
            alert_on(session, 
                "alert_fitted",
                "Fitting failed!",  
                "The chosen formula does not fit the data. Please try another formula.",
                "warning"
            )
            return(FALSE)
        } else{
            return(TRUE)
        }
    }
}