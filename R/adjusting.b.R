
# This file is a generated template, your changes will not be overwritten

adjustingClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "adjustingClass",
    inherit = adjustingBase,
    private = list(
        .time = NULL,
        .ready = FALSE,
        .smartObjs = list(),
        .plotter = NULL,
        .runner = NULL,
        .init =function() {
          
                jinfo(paste("MODULE:  Adjusting #### phase init  ####"))
                private$.time <- Sys.time()
                class(private$.results) <- c("adjusting", class(private$.results)) ## this is useful in R interface

                ### set up the R6 workhorse class
                private$.runner <- Runner$new(self)
                ### info table ###
                aSmartObj <- SmartTable$new(self$results$varstab, private$.runner)
                ladd(private$.smartObjs) <- aSmartObj
                ### final table ###
                aSmartObj <- SmartTable$new(self$results$final, private$.runner)
                aSmartObj$hideOn <- list(df = NA)
                ladd(private$.smartObjs) <- aSmartObj
                ### comb_details table ###
                aSmartObj <- SmartTable$new(self$results$comb_details, private$.runner)
                aSmartObj$expandOnRun<-T
                aSmartObj$expandFrom<-2
                ladd(private$.smartObjs) <- aSmartObj

                ### univariate table ###
                aSmartObj <- SmartTable$new(self$results$univariate, private$.runner)
                aSmartObj$spaceBy <-"name"
                aSmartObj$combineBelow <-"name"
                aSmartObj$hideOn <- list(r2 = NA,df=NA)
                ladd(private$.smartObjs) <- aSmartObj

                ### multiple table ###
                aSmartObj <- SmartTable$new(self$results$multiple, private$.runner)
                ladd(private$.smartObjs) <- aSmartObj
                aSmartObj$hideOn <- list(df = NA)

                ### scores_es table ###
                aSmartObj <- SmartTable$new(self$results$scores$es, private$.runner)
                aSmartObj$combineBelow <- "method"
                ladd(private$.smartObjs) <- aSmartObj

                ### scores_percentiles table ###
                aSmartObj <- SmartTable$new(self$results$scores$percentiles, private$.runner)
                ladd(private$.smartObjs) <- aSmartObj

                ### scores_raws table ###
                aSmartObj <- SmartTable$new(self$results$scores$raws, private$.runner)
                aSmartObj$expandOnRun<-T
                aSmartObj$expandFrom<-2
                ladd(private$.smartObjs) <- aSmartObj
                
                ### init all ####
                for (tab in private$.smartObjs) {
                    tab$initTable()
                }
                
                private$.plotter <- Plotter$new(self, private$.runner)
                private$.plotter$init_plots()
            
        },
        
        .run = function() {

            jinfo(paste("MODULE:  Adjusting #### phase run  ####"))

            private$.runner$data<-stats::na.omit(self$data)
            private$.runner$run()
            for (tab in private$.smartObjs) {
                    tab$runTable()
            }
            private$.plotter$prepare_plots()
            
            
        },
        .cleandata=function() {
            ### here we check the data, remove missing, and change variables if necessary
            ### here you want to check if the transformations can be applied, if factors are
            ### coded well, etc.
            ### for now, it just remove the missing
            private$.data<-jmvcore::naOmit(self$data)
        },
        ### plots
        
        .plot_adj=function(image, ggtheme, theme, ...) {
            plot <- private$.plotter$plot_adjusted(image, ggtheme, theme)
            return(plot)
          
        },
        .plot_adj_obs=function(image, ggtheme, theme, ...) {
            plot <- private$.plotter$plot_adj_obs(image, ggtheme, theme)
            return(plot)
          
        },
        .plot_adj_pred=function(image, ggtheme, theme, ...) {
            plot <- private$.plotter$plot_adj_pred(image, ggtheme, theme)
            return(plot)
          
        }
        
        

    )
)

