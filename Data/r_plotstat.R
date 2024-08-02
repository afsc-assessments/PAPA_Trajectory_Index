#' 
#' @title Plot cross-correlation statistics
#' @description Function to plot cross-correlation statistics in [testcorr] fashion.
#' @import ggplot2 
#' @importFrom scales pretty_breaks
#' @export 
#' 
plotstat<-function (max.lag, seq.max.lag, stat1.val, stat2.val, alpha, 
    stat.cv, n, uni.biv, my.names, stat1.name, stat2.name, stat.id, 
    scale.font) 
{
    options(scipen = 999)
    results <- cbind(seq.max.lag, stat1.val, stat2.val, stat.cv)
    colnames(results) <- c("lag", "stat1", "stat2", "cv")
    df.results <- data.frame(results)
    my.ticks <- seq(seq.max.lag[1], max.lag, ceiling(0.1 * max.lag))
    max.y.tick <- max(scales::pretty_breaks()(c(0, results[, 2:4])))
    min.y.tick <- min(scales::pretty_breaks()(c(0, results[, 2:4])))
    my.ticks.y <- scales::pretty_breaks()(c(0, results[, 2:4]))
    label.stat1 <- stat1.name
    label.stat2 <- stat2.name
    label.cv <- paste("cv(", 100 * alpha, "%)", sep = "")
    if (scale.font != 1) {
        scale.fontaxis <- 0.9 * scale.font
    }
    else {
        scale.fontaxis <- scale.font
    }
    scale.key <- 15 + 5 * scale.font
    g.stat <- ggplot(df.results, aes_(x = ~lag)) + theme_classic() + 
        theme(plot.title = element_text(size = 13 * scale.font), 
            legend.text = element_text(size = 12 * scale.font), 
            axis.text = element_text(size = 10 * scale.fontaxis), 
            axis.title = element_text(size = 10 * scale.font)) + 
        theme(legend.position = "top", legend.key.size = unit(scale.key, 
            "pt"), legend.text = element_text(margin = margin(r = 10, 
            unit = "pt"))) + theme(axis.title.x = element_text(margin = margin(t = 10))) + 
        theme(axis.text.x = element_text(margin = margin(t = 4))) + 
        guides(fill = guide_legend(order = 1)) + theme(axis.title.y = element_blank()) + 
        labs(x = "Lag") + scale_x_continuous(breaks = my.ticks) + 
        scale_y_continuous(breaks = my.ticks.y, limits = c(min.y.tick, 
            max.y.tick), expand = c(0, 0)) + geom_blank(aes(y = max.y.tick)) + 
        geom_blank(aes(y = min.y.tick)) + geom_line(aes_(y = ~stat1, 
        colour = "stat1", linetype = "stat1"), size = 1, na.rm = TRUE) + 
        geom_line(aes_(y = ~stat2, colour = "stat2", linetype = "stat2"), 
            size = 1, na.rm = TRUE) + geom_line(aes_(y = ~cv, 
        colour = "cv", linetype = "cv"), size = 1, na.rm = TRUE)
    if (stat.id == 1) {
        g.stat <- g.stat + scale_colour_manual("", breaks = c("stat1", 
            "stat2", "cv"), values = c(stat1 = "gray50", stat2 = "red", 
            cv = "black"), labels = c(label.stat1, label.stat2, 
            label.cv)) + scale_linetype_manual("", breaks = c("stat1", 
            "stat2", "cv"), values = c(stat1 = "solid", stat2 = "solid", 
            cv = "dashed"), labels = c(label.stat1, label.stat2, 
            label.cv))
    }
    if (stat.id == 2 | stat.id == 3) {
        g.stat <- g.stat + scale_colour_manual("", breaks = c("stat1", 
            "stat2", "cv"), values = c(stat1 = "red", stat2 = "gray50", 
            cv = "black"), labels = c(label.stat1, label.stat2, 
            label.cv)) + scale_linetype_manual("", breaks = c("stat1", 
            "stat2", "cv"), values = c(stat1 = "solid", stat2 = "solid", 
            cv = "dashed"), labels = c(label.stat1, label.stat2, 
            label.cv))
    }
    if (uni.biv == 1 && stat.id == 1) {
        g.stat <- g.stat + theme(plot.title = element_text(hjust = 0.5)) + 
            ggtitle(bquote(Cumulative ~ tests ~ "for" ~ zero ~ 
                autocorrelation ~ of ~ .(my.names)[t]))
    }
    if (uni.biv == 1 && stat.id == 2) {
        g.stat <- g.stat + theme(plot.title = element_text(hjust = 0.5)) + 
            ggtitle(bquote(Tests ~ "for" ~ iid ~ property ~ of ~ 
                .(my.names)[t]))
    }
    if (uni.biv == 1 && stat.id == 3) {
        g.stat <- g.stat + theme(plot.title = element_text(hjust = 0.5)) + 
            ggtitle(bquote(Cumulative ~ tests ~ "for" ~ iid ~ 
                property ~ of ~ .(my.names)[t]))
    }
    if (uni.biv == 2 && stat.id == 1) {
        g.stat <- g.stat + theme(plot.title = element_text(hjust = 0.5)) + 
            ggtitle(bquote(Cumulative ~ tests ~ "for" ~ zero ~ 
                cross - correlation ~ of ~ .(my.names[1])[t] ~ 
                and ~ .(my.names[2])[t - k]))
    }
    return(g.stat)
}
