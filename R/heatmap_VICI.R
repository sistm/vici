#'A heatmap function for displaying
#'
#'Internal function for displaying significance heatmap when multiple conditions are tested
#'
#'@param res_2plot a \code{data.frame}
#'
#'@return a \code{ggplot2} plot object
#'
#'@author Boris Hejblum
#'
#'@keywords internal
#'
#'@import ggplot2

heatmap_vici <- function(res_2plot){

  p <- ggplot(data = res_2plot) +
    geom_tile(aes_string(x="Stimulation", y="response", fill="pvalue"), color="white") +
    theme_minimal() +
    theme(panel.grid = element_blank()) +
    ggtitle("Arm effect on ICS response",
            subtitle = "taking into account background response levels") +
    labs(caption = "made with VICI") +
    ylab("ICS response") +
    scale_fill_manual(values = c("red4", "red2", "coral2", "grey70", "grey75", "grey80", "grey85","grey90", "grey95"),
                      breaks = c("[0,0.001)", "[0.001,0.01)", "[0.01,0.05)", "[0.05,0.1)", "[0.1,0.2)", "[0.2,0.3)", "[0.3,0.4)", "[0.4,0.5)", "[0.5,1)"),
                      labels = c("[0,0.001[", "[0.001,0.01[", "[0.01,0.05[", "[0.05,0.1[", "[0.1,0.2[", "[0.2,0.3[", "[0.3,0.4[", "[0.4,0.5[", "[0.5,1]"),
                      name="P-value",
                      limits = c("[0,0.001)", "[0.001,0.01)", "[0.01,0.05)", "[0.05,0.1)", "[0.1,0.2)", "[0.2,0.3)", "[0.3,0.4)", "[0.4,0.5)", "[0.5,1)")
    )

  return(p)
}
