#' port of testcorr::cc.test to provide table and plots as output list elements.
# source("r_plotcorr.R")
# source("r_plotstat.R")
cc.test<-
    function (x, y, max.lag, alpha = 0.05, lambda = 2.576, plot = TRUE, 
    table = TRUE, var.names = NULL, scale.font = 1, print = FALSE) 
{
    if (is.null(x)) 
        stop()
    if (is.null(y)) 
        stop()
    if (is.ts(x) | is.numeric(x) | is.data.frame(x)) {
    }
    else {
        stop("argument \"x\" must be numeric time series object or numeric vector or data frame")
    }
    if (is.ts(y) | is.numeric(y) | is.data.frame(y)) {
    }
    else {
        stop("argument \"y\" must be numeric time series object or numeric vector or data frame")
    }
    if (!is.numeric(max.lag)) 
        stop("argument \"max.lag\" must be numeric")
    if (!is.numeric(alpha)) 
        stop("argument \"alpha\" must be numeric")
    if (!is.numeric(lambda)) 
        stop("argument \"lambda\" must be numeric")
    if (!is.logical(plot)) 
        stop("argument \"plot\" must be logical")
    if (!is.logical(table)) 
        stop("argument \"table\" must be logical")
    if (!is.null(var.names) & NROW(var.names) == 2 & (!is.character(var.names[1]) | 
        !is.character(var.names[2]))) 
        stop("argument \"var.names\" must be NULL or string")
    if (!is.numeric(scale.font)) 
        stop("argument \"scale.font\" must be numeric")
    if (NCOL(x) != 1) 
        stop("argument \"x\" must be univariate")
    if (NCOL(y) != 1) 
        stop("argument \"y\" must be univariate")
    if (any(is.na(x))) 
        stop("argument \"x\" must not contain missing values")
    if (any(is.na(y))) 
        stop("argument \"y\" must not contain missing values")
    if (NCOL(x) != NCOL(y)) 
        stop("arguments \"x\" and \"y\" must have the same length")
    if (max.lag != ceiling(max.lag) | max.lag < 1 | max.lag > 
        (NROW(x) - 1)) 
        stop("argument \"max.lag\" must be an integer value greater or equal to one and less than the sample size")
    if (alpha < 0 | alpha > 1) 
        stop("argument \"alpha\" must be between 0 and 1")
    if (lambda < 0) 
        stop("argument \"lambda\" must be non-negative")
    if (is.null(var.names) & !is.null(colnames(x))) {
        if (NROW(colnames(x)) != NCOL(x)) 
            stop("argument \"x\" must have two names")
    }
    if (!is.null(var.names)) {
        if (NROW(var.names) != 2) 
            stop("argument \"var.names\" must contain two names")
    }
    if (scale.font <= 0) 
        stop("argument \"scale.font\" must be positive")
    if (is.null(var.names)) {
        if (!is.null(colnames(x))) {
            my.names <- colnames(x)
        }
        if (is.null(colnames(x))) {
            my.names <- c("x", "y")
        }
    }
    if (!is.null(var.names)) {
        my.names <- var.names
    }
    x <- as.matrix(x)
    x <- as.vector(x)
    y <- as.matrix(y)
    y <- as.vector(y)
    n <- NROW(x)
    results.tq.xyk <- testcorr:::cc.test.t.tmink(x, y, max.lag, alpha, lambda)
    results.tq.yxk <- testcorr:::cc.test.t.tmink(y, x, max.lag, alpha, lambda)
    results.tq <- rbind(results.tq.yxk[(max.lag + 1):2, ], results.tq.xyk)
    seqmaxlagmaxlag <- seq(-max.lag, max.lag, 1)
    cc <- results.tq[, 1]
    t.tilde <- results.tq[, 2]
    q.tilde <- results.tq[, 3]
    t <- results.tq[, 4]
    hb <- results.tq[, 5]
    z.cv <- qnorm(1 - alpha/2)
    vec1 <- matrix(1, nrow = 2 * max.lag + 1, ncol = 1)
    s.cb <- z.cv/sqrt(n) * vec1
    r.cb <- z.cv * cc/t.tilde
    lu.s.cb <- cbind(-s.cb, s.cb)
    lu.r.cb <- cbind(-r.cb, r.cb)
    rownames(lu.r.cb) <- rownames(lu.s.cb)
    colnames(lu.r.cb) <- colnames(lu.s.cb)
    pc = NULL; ps = NULL;
    if (plot == TRUE) {
        chisq.cv <- qchisq(alpha, abs(seqmaxlagmaxlag) + 1, lower.tail = FALSE)
        pc = plotcorr(max.lag, seqmaxlagmaxlag, cc, s.cb, r.cb, alpha, 
                      n, 2, my.names, scale.font);
        if (print) print(pc);
        ps = plotstat(max.lag, seqmaxlagmaxlag, hb, q.tilde, alpha, 
                      chisq.cv, n, 2, my.names, "HB", expression(tilde(Q)), 
                      1, scale.font)
        if (print) print(ps);
    }
    table.tq = NULL;
    if (table == TRUE) {
        options(max.print = 1000000)
        results.cb <- cbind(cc, s.cb, r.cb)
        results.cb <- format(round(results.cb, 3), nsmall = 3)
        results.t <- cbind(t, 2 * (1 - pnorm(abs(t))), t.tilde, 
            2 * (1 - pnorm(abs(t.tilde))))
        results.t <- format(round(results.t, 3), nsmall = 3)
        results.q <- cbind(hb, 1 - pchisq(hb, abs(seqmaxlagmaxlag) + 
            1), q.tilde, 1 - pchisq(q.tilde, abs(seqmaxlagmaxlag) + 
            1))
        results.q <- format(round(results.q, 3), nsmall = 3)
        results.tq <- cbind(seqmaxlagmaxlag, results.cb, seqmaxlagmaxlag, 
            results.t, seqmaxlagmaxlag, results.q)
        results.tq <- data.frame(results.tq, row.names = NULL)
        names(results.tq) <- c("Lag", "CC", paste("Stand. CB(", 
            100 * (1 - alpha), "%)", sep = ""), paste("Robust CB(", 
            100 * (1 - alpha), "%)", sep = ""), "Lag", "t", "p-value", 
            "t-tilde", "p-value", "Lag", "HB", "p-value", "Q-tilde", 
            "p-value")
        results.cb.lu <- cbind(s.cb, -s.cb, r.cb, -r.cb)
        results.cb.lu <- format(round(results.cb.lu, 3), nsmall = 3)
        results.tq[, 3] <- paste(results.cb.lu[, 2], paste(",", 
            results.cb.lu[, 1], sep = ""), sep = "")
        results.tq[, 4] <- paste(results.cb.lu[, 4], paste(",", 
            results.cb.lu[, 3], sep = ""), sep = "")
        results.tq[, 3] <- paste("(", paste(results.tq[, 3], 
            ")", sep = ""), sep = "")
        results.tq[, 4] <- paste("(", paste(results.tq[, 4], 
            ")", sep = ""), sep = "")
        table.tq <- knitr::kable(results.tq, align = "r")
        if (print){
          cat("\n")
          cat("Tests for zero cross-correlation of ", my.names[1], 
              " and ", my.names[2], sep = "")
          print(table.tq)
          cat("\n")
          }
    }
    colnames(seqmaxlagmaxlag) <- NULL
    rownames(seqmaxlagmaxlag) <- NULL
    colnames(cc) <- NULL
    rownames(cc) <- NULL
    colnames(lu.s.cb) <- NULL
    rownames(lu.s.cb) <- NULL
    colnames(lu.r.cb) <- NULL
    rownames(lu.r.cb) <- NULL
    colnames(t) <- NULL
    rownames(t) <- NULL
    colnames(t.tilde) <- NULL
    rownames(t.tilde) <- NULL
    colnames(hb) <- NULL
    rownames(hb) <- NULL
    colnames(q.tilde) <- NULL
    rownames(q.tilde) <- NULL
    invisible(structure(list(lag = as.numeric(seqmaxlagmaxlag), 
        cc = as.numeric(cc), scb = as.matrix(lu.s.cb), rcb = as.matrix(lu.r.cb), 
        t = as.numeric(t), pvt = as.numeric(2 * (1 - pnorm(abs(t)))), 
        ttilde = as.numeric(t.tilde), pvttilde = as.numeric(2 * 
            (1 - pnorm(abs(t.tilde)))), hb = as.numeric(hb), 
        pvhb = as.numeric(1 - pchisq(hb, abs(seqmaxlagmaxlag) + 
            1)), qtilde = as.numeric(q.tilde), pvqtilde = as.numeric(1 - 
            pchisq(q.tilde, abs(seqmaxlagmaxlag) + 1)),
        plots=list(corr=pc,stats=ps),
        table=table.tq), class = "cc.test"))
}