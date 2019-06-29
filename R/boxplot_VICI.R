#'Plotting function for displaying boxplots and associated p-values
#'
#'Internal function for displaying significance boxplots
#'
#'@param res_2plot a \code{data.frame}
#'@param pval_2plot a \code{data.frame} with the p-values to display
#'
#'@return a \code{ggpubr} plot object
#'
#'@author Boris Hejblum
#'
#'@keywords internal
#'
#'@import ggplot2
#'@import ggpubr

boxplot_VICI <- function(data_df, pval_2plot, response_name){

  p <- ggboxplot(data_df, x="stim", y="response", color="arm", fill="arm", alpha=0.3) +
    #theme_bw() +
    theme(panel.grid.major.x = element_blank()) +
    scale_fill_viridis_d("Arm") +
    scale_color_viridis_d("Arm") +
    stat_pvalue_manual(data = pval_2plot, label = "pvalue_format", tip.length = 0.03*max(data_df$response)) +
    ylab(paste0("Response ", response_name)) +
    xlab("Stimulation") +
    ggtitle(paste0("Arm effect on ", response_name),
            subtitle = "taking into account background response levels") +
    labs(caption = "made with VICI")

  return(p)

}
