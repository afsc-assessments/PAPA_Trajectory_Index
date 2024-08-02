#' 
#' @title Plot cross-correlation 
#' @description Function to plot cross-correlations in [testcorr] fashion.
#' @import ggplot2 
#' @importFrom scales pretty_breaks
#' @export 
#' 
plotcorr<-function (max.lag, seq.max.lag, ac.cc, s.cb, r.cb, alpha, n, 
    uni.biv, my.names, scale.font) 
{
    options(scipen = 999)
    results <- cbind(seq.max.lag, ac.cc, s.cb, -s.cb, r.cb, -r.cb)
    if (uni.biv == 1) {
        colnames(results) <- c("lag", "AC", "scb", "scbl", "rcb", 
            "rcbl")
    }
    if (uni.biv == 2) {
        colnames(results) <- c("lag", "CC", "scb", "scbl", "rcb", 
            "rcbl")
    }
    df.results <- data.frame(results)
    my.ticks.x <- seq(seq.max.lag[1], max.lag, ceiling(0.1 * 
        max.lag))
    max.y.tick <- max(scales::pretty_breaks()(c(0, results[, 2:6])))
    min.y.tick <- min(scales::pretty_breaks()(c(0, results[, 2:6])))
    my.ticks.y <- scales::pretty_breaks()(c(0, results[, 2:6]))
    label.scb <- paste("Standard CB(", 100 * (1 - alpha), "%)", 
        sep = "")
    label.rcb <- paste("Robust CB(", 100 * (1 - alpha), "%)", 
        sep = "")
    if (scale.font != 1) {
        scale.fontaxis <- 0.9 * scale.font
    }
    else {
        scale.fontaxis <- scale.font
    }
    scale.key <- 15 + 5 * scale.font
    g.corr <- ggplot(df.results, aes_(x = ~lag)) + 
                theme_classic() + 
                theme(plot.title = element_text(size = 13 * scale.font), 
                      legend.text = element_text(size = 12 * scale.font), 
                       axis.text = element_text(size = 10 * scale.fontaxis), 
                       axis.title = element_text(size = 10 * scale.font)) + 
                theme(legend.position = "top", 
                      legend.key.size = unit(scale.key, "pt"), 
                      legend.text = element_text(margin = margin(r = 10, unit = "pt"))) + 
                theme(axis.title.x = element_text(margin = margin(t = 10))) + 
                theme(axis.text.x = element_text(margin = margin(t = 4))) + 
                guides(fill = guide_legend(order = 1)) + 
                theme(axis.title.y = element_blank()) + 
                labs(x = "Lag") + 
                scale_x_continuous(breaks = my.ticks.x) + 
                scale_y_continuous(breaks = my.ticks.y, limits = c(min.y.tick,max.y.tick), expand = c(0, 0)) + 
                geom_blank(aes(y = max.y.tick)) + 
                geom_blank(aes(y = min.y.tick)) + geom_hline(yintercept = 0)
    if (uni.biv == 1) {
        g.corr <- g.corr + 
                  geom_col(aes_(y = ~AC, fill = "AC"), width = 0.2) + 
                  scale_fill_manual("", breaks = "AC", values = "#4F91CD");
    }
    if (uni.biv == 2) {
        g.corr <- g.corr + 
                    geom_col(aes_(y = ~CC, fill = "CC"), width = 0.2) + 
                    scale_fill_manual("", breaks = "CC", values = "#4F91CD");
    }
    g.corr <- g.corr + 
              geom_line(aes_(y = ~scb, colour = "scb"), linetype = "dashed", linewidth = 1) + 
              geom_line(aes_(y = ~scbl), colour = "gray50", linetype = "dashed", linewidth = 1) + 
              geom_line(aes_(y = ~rcb, colour = "rcb"), linetype = "dashed", linewidth = 1) + 
              geom_line(aes_(y = ~rcbl), colour = "red", linetype = "dashed", linewidth = 1) + 
              scale_colour_manual("", breaks = c("scb", "rcb"), values = c("gray50", "red"), 
                                  labels = c(label.scb, label.rcb));
    if (uni.biv == 1) {
      g.corr <- g.corr + 
                  theme(plot.title = element_text(hjust = 0.5)) + 
                  ggtitle(bquote(Autocorrelation ~ of ~ .(my.names)[t]))
    }
    if (uni.biv == 2) {
        g.corr <- g.corr + theme(plot.title = element_text(hjust = 0.5)) + 
            ggtitle(bquote(Cross - correlation ~ of ~ .(my.names[1])[t] ~ 
                and ~ .(my.names[2])[t - k]))
    }
    return(g.corr)
}
