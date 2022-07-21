#' @param value_list A list where each element is a vector of values to plot
#' @param value_anchor The true value
#' @keywords internal
multigroup_boxplot <- function(value_list, value_anchor = NULL, anchor_col = "black", group_at = NULL, rep_id = NULL, boxgroup_gap = 0.5, boxset_gap = 0.5,
                               box_col = "black", box_border = F, box_lty = 1, whiskcol = "black", staplecol = "black", 
                               x_lab = NA, y_lab = NA, axis_log = NULL, fig_main = NA, 
                               plot_xaxis = T, xaxis_lab = NULL, xaxis_lab_cex = 0.65, xaxis_lab_line = -2, xaxis_side = 1,
                               plot_xaxis_boxgroup = T, xaxis_boxgroup_lab = NULL, xaxis_boxgroup_labcex = 0.65, xaxis_boxgroup_labline = -2, xaxis_boxgroup_side = 1,
                               plot_yaxis = T, yaxis_lab_line = -1.85, 
                               file_path = NA, pdf_height = 4, pdf_width = 6.5, pdf_mai = c(0.2, 0.2, 0.2, 0)) {
  
  if (is.null(group_at)) {
    group_at <- seq_along(value_list)
  }
  
  if (is.null(rep_id)) {
    rep_id <- rep(1, length(group_at))
  }
  
  x_lim <- range(group_at)
  if (length(unique(group_at)) == 1) {
    x_lim[2] <- x_lim[1] + 1
  }
  y_lim <- range(c(unlist(lapply(value_list, function(x) quantile(x, probs = c(0.025, 0.975), na.rm = T))), value_anchor), na.rm = T)
  
  # a box group is either a single box or a "small" group that's defined by replicated analyses
  # given a box group occupying one unit of horizontal distance, let's put 0.5 unit between each box group
  # 0.5 unit before the first group and after the last group, respectively, and 1 unit if the two box groups are in different "large" groups 
  # (let's call it box sets for now, denoted by the alternating grey background, presumably corresponds to different models)
  boxgroup_num <- sum(rep_id == 1)
  boxset_num <- length(unique(group_at))
  # (distance between each box group center = 1 + gap between each box group) * (number of box group gaps = boxgroup_num - 1) + (first half and last half box groups = 0.5 * 2) +
  # (space before the first group and after the last group = 0.5 * 2) + (extra gap between each box set = 0.5) * (number of box set gaps = boxset_num - 1)
  total_units <- (boxgroup_num - 1) * (1 + boxgroup_gap) + 0.5 * 2 + (boxset_gap + boxgroup_gap) / 2 * 2 + (boxset_num - 1) * boxset_gap
  oneunit_length <- diff(x_lim)/total_units
  
  boxgroup_at <- numeric(boxgroup_num)
  boxset_bound <- x_lim[1]
  k <- 1L
  for (i in 1:length(rep_id)) {
    if (k == 1) {
      boxgroup_at[k] <- x_lim[1] + oneunit_length * (boxset_gap + boxgroup_gap)/2 + oneunit_length * 0.5
      k <- k + 1L
    } else if (k > 1) {
      if (rep_id[i] == 1) {
        boxgroup_at[k] <- boxgroup_at[k - 1] + oneunit_length * (1 + boxgroup_gap)
        
        if (group_at[i] != group_at[i - 1]) { # proceed to next group set
          boxgroup_at[k] <- boxgroup_at[k] + oneunit_length * boxset_gap
          boxset_bound <- c(boxset_bound, boxgroup_at[k] - oneunit_length * (1 + boxgroup_gap + boxset_gap) / 2)
        }
        
        k <- k + 1L
      }
    }
  }
  boxset_bound <- c(boxset_bound, x_lim[2])
  
  # if there is any replicated box, then each box is one unit dividing by the max number of replicates wide
  box_wex <- oneunit_length/max(rep_id)
  box_at <- numeric(length(rep_id))
  repid_start <- which(rep_id == 1)
  
  for (i in 1:length(boxgroup_at)) {
    if (i < length(boxgroup_at)) {
      boxnum_thisgroup <- repid_start[i + 1] - repid_start[i]
    } else {
      boxnum_thisgroup <- length(rep_id) - repid_start[i] + 1
    }
    
    boxat_start <- boxgroup_at[i] - (boxnum_thisgroup - 1) * box_wex/2
    for (j in 1:boxnum_thisgroup) {
      box_at[repid_start[i] + j - 1] <- boxat_start + (j - 1) * box_wex
    }
  }
  
  if (!is.na(file_path)) {
    pdf(file = file_path, height = pdf_height, width = pdf_width)
    par(lend = 2, mai = pdf_mai, xpd = F)
  }
  
  if (!is.null(axis_log)) {
    plot(NULL, type = "n", main = fig_main, xlab = NA, ylab = NA, xlim = x_lim, ylim = y_lim, axes = F, log = axis_log)
  } else {
    plot(NULL, type = "n", main = fig_main, xlab = NA, ylab = NA, xlim = x_lim, ylim = y_lim, axes = F)
  }
  
  for (seti in 1:(length(boxset_bound) - 1)) {
    rect(boxset_bound[seti], y_lim[1], boxset_bound[seti + 1], y_lim[2], border = NA, col = ifelse(seti == floor(seti/2) * 2, "grey83", "grey92"))
  }
  
  if (!is.null(value_anchor)) {
    segments(x0 = x_lim[1], x1 = x_lim[2], y0 = value_anchor, y1 = value_anchor, lty = 2, col = anchor_col)
  }
  
  myboxplot(value_list, at = box_at, boxwex = box_wex * 0.975, col = as.vector(box_col), boxlty = box_lty, medcol = "black", medlwd = 1.25, 
            whiskcol = whiskcol, whisklwd = 0.75, staplecol = staplecol, staplewex = 0.4, staplelwd = 0.6, boxlwd = 0.35,
            border = box_border, outline = F, add = T, xlab = NA, ylab = NA, axes = F)
  
  if (is.null(xaxis_lab)) {
    xaxis_lab <- sort(unique(group_at))
  }
  if (plot_xaxis) {
    axis(side = xaxis_side, labels = xaxis_lab, at = boxset_bound[-length(boxset_bound)] + diff(boxset_bound)/2, 
         lwd = 0, lwd.ticks = 0, cex.axis = xaxis_lab_cex, line = xaxis_lab_line)
  }
  
  if (is.null(xaxis_boxgroup_lab)) {
    xaxis_boxgroup_lab <- seq_along(boxgroup_at)
  }
  if (plot_xaxis_boxgroup) {
    axis(side = xaxis_boxgroup_side, labels = xaxis_boxgroup_lab, at = boxgroup_at, 
         lwd = 0, lwd.ticks = 0, cex.axis = xaxis_boxgroup_labcex, line = xaxis_boxgroup_labline)
  }
  
  if (plot_yaxis) {
    axis(side = 2, labels = NA, lwd = 1, lwd.ticks = 1, line = -1.1, tck = -0.015)
    axis(side = 2, lwd = 0, lwd.ticks = 0, cex.axis = 0.6, line = yaxis_lab_line)
  }
  
  mtext(x_lab, side = 1, line = 0, cex = 0.85)
  mtext(y_lab, side = 2, line = 0, cex = 0.85)
  
  if (!is.null(names(box_col))) {
    legend("bottomright", legend = sort(unique(names(box_col))), bty = "n", col = box_col[sort(unique(names(box_col)))], 
           pch = 15, cex = 0.7, pt.cex = 1.3, adj = 0)
  }
  
  if (!is.na(file_path)) {
    dev.off()
  }
}

#' @keywords internal
myboxplot.stats <- function (x, coef = NULL, do.conf = TRUE, do.out = TRUE) {
  nna <- !is.na(x)
  n <- sum(nna)
  stats <- quantile(x, c(.025,.25,.5,.75,.975), na.rm = TRUE)
  iqr <- diff(stats[c(2, 4)])
  out <- x < stats[1] | x > stats[5]
  conf <- if (do.conf) 
    stats[3] + c(-1.58, 1.58) * diff(stats[c(2, 4)])/sqrt(n)
  list(stats = stats, n = n, conf = conf, out = x[out & nna])
}


#' @keywords internal
myboxplot <- function (x, ..., range = 1.5, width = NULL, varwidth = FALSE, 
                       notch = FALSE, outline = TRUE, names, plot = TRUE, border = par("fg"), 
                       col = NULL, log = "", pars = list(boxwex = 0.8, staplewex = 0.5, outwex = 0.5), horizontal = FALSE, add = FALSE, at = NULL) 
{
  args <- list(x, ...)
  namedargs <- if (!is.null(attributes(args)$names)) 
    attributes(args)$names != ""
  else rep_len(FALSE, length(args))
  groups <- if (is.list(x)) x
  else args[!namedargs]
  if (0L == (n <- length(groups))) stop("invalid first argument")
  if (length(class(groups))) groups <- unclass(groups)
  if (!missing(names)) attr(groups, "names") <- names
  else {
    if (is.null(attr(groups, "names"))) 
      attr(groups, "names") <- 1L:n
    names <- attr(groups, "names")
  }
  cls <- sapply(groups, function(x) class(x)[1L])
  cl <- if (all(cls == cls[1L])) cls[1L]
  else NULL
  for (i in 1L:n) groups[i] <- list(myboxplot.stats(unclass(groups[[i]]), range))
  stats <- matrix(0, nrow = 5L, ncol = n)
  conf <- matrix(0, nrow = 2L, ncol = n)
  ng <- out <- group <- numeric(0L)
  ct <- 1
  for (i in groups) {
    stats[, ct] <- i$stats
    conf[, ct] <- i$conf
    ng <- c(ng, i$n)
    if ((lo <- length(i$out))) {
      out <- c(out, i$out)
      group <- c(group, rep.int(ct, lo))
    }
    ct <- ct + 1
  }
  if (length(cl) && cl != "numeric") oldClass(stats) <- cl
  z <- list(stats = stats, n = ng, conf = conf, out = out, group = group, names = names)
  if (plot) {
    if (is.null(pars$boxfill) && is.null(args$boxfill)) 
      pars$boxfill <- col
    do.call("bxp", c(list(z, notch = notch, width = width, 
                          varwidth = varwidth, log = log, border = border, 
                          pars = pars, outline = outline, horizontal = horizontal, 
                          add = add, at = at), args[namedargs]))
    invisible(z)
  }
  else z
}
