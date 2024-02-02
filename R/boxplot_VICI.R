#'Plotting function for displaying boxplots and associated p-values
#'
#'Internal function for displaying significance boxplots
#'
#'@param data_df a \code{data.frame}.
#'@param pval_2plot a \code{data.frame} with the p-values to display.
#'@param response_name a character string indicating the name of the response.
#'@param input internal input from UI.
#'@param inter a logical flag indicating whether we are in the interarm setting or not.
#'Default is \code{TRUE}.
#'@param baseline baseline value used in title when \code{inter} is \code{FALSE}.
#'Default is \code{NULL}.
#'@param fill a logical flag indicating if the boxplot is filled
#'Default if \code{FALSE}
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

boxplot_VICI <- function(data_df, pval_2plot, response_name, input, inter=TRUE, baseline=NULL,fill=FALSE){
  
  if(!is.numeric(data_df$response)){
    data_df$response <- as.numeric(data_df$response)
  }
  
  p <- NULL
  
  if(inter){
    data_df$arm <- relevel(data_df$arm, ref=input$selectRefArmInter)
    suppressWarnings(
      if(input$jiter == "None"){
        p <- ggboxplot(na.omit(data_df), x="stim", y="response", color= "arm", 
                       palette = "RdGy", fill = "stim", alpha=0.3)
      }else{
        p <- ggboxplot(na.omit(data_df), x="stim", y="response", color= "arm", palette = "RdGy",fill = "stim",
                       alpha=0.3, add="jitter", shape = as.numeric(input$jiter))
      }
    )
  }else{
    data_df$time <- relevel(data_df$time, ref=input$selectRefTimeIntra)
    
    if(input$jiter == "None"){
      p <- ggboxplot(na.omit(data_df), x="stim", y="response", color= "time", palette = "RdGy", alpha=0.3)
    }else{
      p <- ggboxplot(na.omit(data_df), x="stim", y="response", color= "time", palette = "RdGy",
                     alpha=0.3, add="jitter", shape = as.numeric(input$jiter))
    }
  }
  
  p <- p + theme_grey() + 
    theme(panel.grid.major.x = element_blank()) +
    scale_color_brewer(palette = input$color) +
    stat_pvalue_manual(data = pval_2plot, label = "pvalue_format", tip.length = 0.025) +
    ylab(paste0("Response ", response_name)) +
    xlab("Stimulation") +
    ggtitle(paste0("Arm effect on ", response_name),
            subtitle = "p-values taking into account background response levels through bivariate modeling") +
    labs(caption = "made with VICI")
  
  return(p)
  
}
