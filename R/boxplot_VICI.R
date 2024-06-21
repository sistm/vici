#'Plotting function for displaying boxplots and associated p-values
#'
#'Internal function for displaying significance boxplots
#'
#'@param res_2plot a \code{data.frame}.
#'@param pval_2plot a \code{data.frame} with the p-values to display.
#'@param inter a logical flag indicating whether we are in the interarm setting or not.
#'Default is \code{TRUE}.
#'@param baseline baseline value used in title when \code{inter} is \code{FALSE}.
#'Default is \code{NULL}.
#'
#'@return a \code{ggpubr} plot object
#'
#'@author Boris Hejblum
#'
#'@keywords internal
#'
#'@import ggplot2
#'@import ggpubr
#'@import RColorBrewer
#'@import rlang

boxplot_VICI <- function(data_df, pval_2plot, response_name, input, inter=TRUE, baseline=NULL,fill=FALSE){
  
  if(!is.numeric(data_df$response)){
    data_df$response <- as.numeric(data_df$response)
  }
  
  
  
  p <- NULL
  #browser()
  if(inter){
    #browser()
    
    listRowClass <<- unique(data_df$arm)
    listCol <- c()
    for (x in 1:length(listRowClass)) {
      #browser()
      cat(paste("\n color of ",listRowClass[x]))
      #browser()
      cat(input[[paste0("color",listRowClass[x])]])
      #newColor <- list(input[[paste0("color",listRowClass[x])]])
      listCol <- c(listCol,input[[paste0("color",listRowClass[x])]])
    }
    
    listShape <- c()
    for (x in 1:length(listRowClass)) {
      #browser()
      cat(paste("\n shape of ",listRowClass[x]))
      cat(input[[paste0("shape",listRowClass[x])]])
      #newColor <- list(input[[paste0("color",listRowClass[x])]])
      #browser()
      listShape <- c(listShape,rlang::as_string(input[[paste0("shape",listRowClass[x])]]))
    }
    
    data_df$arm <- relevel(data_df$arm, ref=input$selectRefArmInter)
    suppressWarnings(
      if(input$jiter == "None"){
        #browser()
        p <- ggboxplot(na.omit(data_df), x="stim", y="response", color= "arm", palette = "RdGy",fill = "stim",#c("Red","Blue","Black"),#"RdBu",
                       #fill="white",#"arm",
                       alpha=0.3,)+
          #theme_bw() +
          theme_grey() + 
          theme(panel.grid.major.x = element_blank()) +
          scale_colour_manual(values = listCol) +
          #scale_color_brewer(palette = input$color) +#"RdGy") +
          #scale_fill_viridis_d("Arm: ") +
          #scale_color_viridis_d("Arm: ") +
          stat_pvalue_manual(data = pval_2plot, label = "pvalue_format",
                             tip.length = 0.025) +
          ylab(paste0("Response ", response_name)) +
          xlab("Stimulation") +
          ggtitle(paste0("Arm effect on ", response_name),
                  subtitle = "p-values taking into account background response levels through bivariate modeling") +
          labs(caption = "made with VICI")
      }else{
      #browser()
      p <- ggboxplot(na.omit(data_df), x="stim", y="response", color= "arm", palette = "RdGy",fill = "stim",#c("Red","Blue","Black"),#"RdBu",
                  
                  #fill="white",#"arm",
                  alpha=0.3,
                  add="jitter",
                  shape = "arm")+#as.numeric(input$jiter))+
        #theme_bw() +
        theme_grey() + 
        theme(panel.grid.major.x = element_blank()) +
        scale_colour_manual(values = listCol) +
        scale_shape_manual(values = listShape) +
        #scale_color_brewer(palette = input$color) +#"RdGy") +
        #scale_fill_viridis_d("Arm: ") +
        #scale_color_viridis_d("Arm: ") +
        stat_pvalue_manual(data = pval_2plot, label = "pvalue_format",
                           tip.length = 0.025) +
        ylab(paste0("Response ", response_name)) +
        xlab("Stimulation") +
        ggtitle(paste0("Arm effect on ", response_name),
                subtitle = "p-values taking into account background response levels through bivariate modeling") +
        labs(caption = "made with VICI")
      },
        # p +
        # #theme_bw() +
        # theme_grey() + 
        # theme(panel.grid.major.x = element_blank()) +
        # #scale_colour_manual(values = CPCOLS) +
        # scale_color_brewer(palette = input$color) +#"RdGy") +
        # #scale_fill_viridis_d("Arm: ") +
        # #scale_color_viridis_d("Arm: ") +
        # stat_pvalue_manual(data = pval_2plot, label = "pvalue_format",
        #                    tip.length = 0.025) +
        # ylab(paste0("Response ", response_name)) +
        # xlab("Stimulation") +
        # ggtitle(paste0("Arm effect on ", response_name),
        #         subtitle = "p-values taking into account background response levels through bivariate modeling") +
        # labs(caption = "made with VICI")
    )
  }else{
    #browser()

    data_df$time <- relevel(data_df$time, ref=input$selectRefTimeIntra)
    
    listRowClass <<- unique(data_df$time)
    #browser()
    
    listCol <- c()
    for (x in 1:length(listRowClass)) {
      #browser()
      cat(paste("\n color of ",listRowClass[x]))
      #browser()
      cat(input[[paste0("color",listRowClass[x])]])
      #newColor <- list(input[[paste0("color",listRowClass[x])]])
      listCol <- c(listCol,input[[paste0("color",listRowClass[x])]])
    }
    
    listShape <- c()
    for (x in 1:length(listRowClass)) {
      #browser()
      cat(paste("\n shape of ",listRowClass[x]))
      cat(input[[paste0("shape",listRowClass[x])]])
      #newColor <- list(input[[paste0("color",listRowClass[x])]])
      #browser()
      listShape <- c(listShape,rlang::as_string(input[[paste0("shape",listRowClass[x])]]))
    }
    
    #browser()
    #suppressWarnings(
    if(input$jiter == "None"){
      p <- ggboxplot(na.omit(data_df), x="stim", y="response", color= "time", palette = "RdGy",#c("Red","Blue","Black"),#"RdBu",
                     #fill="white",#"arm",
                     alpha=0.3,) + 
        
        #theme_bw() +
        theme_grey() + 
        theme(panel.grid.major.x = element_blank()) +
        scale_colour_manual(values = listCol) +
        #scale_color_brewer(palette = input$color)+#"RdGy") +
        #scale_fill_viridis_d("Time-point: ") +
        #scale_color_viridis_d("Time-point: ") +
        stat_pvalue_manual(data = pval_2plot, label = "pvalue_format",
                           tip.length =  0.025) +
        ylab(paste0("Response ", response_name)) +
        xlab("Stimulation") +
        ggtitle(paste0("Intra-arm vaccine effect on ", response_name, " compared to baseline ", baseline),
                subtitle = "p-values taking into account background response levels through bivariate modeling") +
        labs(caption = "made with VICI")
    }else{
      p <- ggboxplot(na.omit(data_df), x="stim", y="response", color= "time", palette = "RdGy",#c("Red","Blue","Black"),#"RdBu",
                     #fill="white",#"arm",
                     alpha=0.3,
                     add="jitter",
                     shape = "time")+#as.numeric(input$jiter))+
        
        #theme_bw() +
        theme_grey() + 
        theme(panel.grid.major.x = element_blank()) +
        scale_colour_manual(values = listCol) +
        scale_shape_manual( values = listShape)+
        #scale_color_brewer(palette = input$color)+#"RdGy") +
        #scale_fill_viridis_d("Time-point: ") +
        #scale_color_viridis_d("Time-point: ") +
        stat_pvalue_manual(data = pval_2plot, label = "pvalue_format",
                           tip.length =  0.025) +
        ylab(paste0("Response ", response_name)) +
        xlab("Stimulation") +
        ggtitle(paste0("Intra-arm vaccine effect on ", response_name, " compared to baseline ", baseline),
                subtitle = "p-values taking into account background response levels through bivariate modeling") +
        labs(caption = "made with VICI")
    }
      # p+
      #   #theme_bw() +
      #   theme_grey() + 
      #   theme(panel.grid.major.x = element_blank()) +
      #   #scale_colour_manual(values = CPCOLS) +
      #   scale_color_brewer(palette = input$color)+#"RdGy") +
      #   #scale_fill_viridis_d("Time-point: ") +
      #   #scale_color_viridis_d("Time-point: ") +
      #   stat_pvalue_manual(data = pval_2plot, label = "pvalue_format",
      #                      tip.length =  0.025) +
      #   ylab(paste0("Response ", response_name)) +
      #   xlab("Stimulation") +
      #   ggtitle(paste0("Intra-arm vaccine effect on ", response_name, " compared to baseline ", baseline),
      #           subtitle = "p-values taking into account background response levels through bivariate modeling") +
      #   labs(caption = "made with VICI")
    #)

  }
  return(p)

}
