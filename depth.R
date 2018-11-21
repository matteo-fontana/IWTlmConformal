function (fit, x = NULL, method = "MBD", depth = NULL, plot = TRUE, 
          prob = 0.5, color = 6, outliercol = 2, barcol = 4, fullout = FALSE, 
          factor = 1.5, xlim = c(1, nrow(fit)), ylim = c(min(fit) - 
                                                           0.5 * diff(range(fit)), max(fit) + 0.5 * diff(range(fit))), 
          ...) 
{
  tp = dim(fit)[1]
  n = dim(fit)[2]
  if (length(x) == 0) {
    x = 1:tp
  }
  if (length(depth) == 0) {
    if (method == "BD2") {
      depth = fBD2(fit)
    }
    else if (method == "MBD") {
      depth = fMBD(fit)
    }
    else if (method == "Both") {
      depth = round(fBD2(fit), 4) * 10000 + fMBD(fit)
    }
  }
  dp_s = sort(depth, decreasing = TRUE)
  index = order(depth, decreasing = TRUE)
  med = depth == max(depth)
  medavg = matrix(fit[, med], ncol = sum(med), nrow = tp)
  y = apply(medavg, 1, mean)
  if (plot) {
    plot(x, y, lty = 1, lwd = 2, col = 1, type = "l", xlim, 
         ylim, ...)
  }
  for (pp in 1:length(prob)) {
    m = ceiling(n * prob[pp])
    center = fit[, index[1:m]]
    out = fit[, index[(m + 1):n]]
    inf = apply(center, 1, min)
    sup = apply(center, 1, max)
    if (prob[pp] == 0.5) {
      dist = factor * (sup - inf)
      upper = sup + dist
      lower = inf - dist
      outly = (fit <= lower) + (fit >= upper)
      outcol = colSums(outly)
      remove = (outcol > 0)
      colum = 1:n
      outpoint = colum[remove == 1]
      out = fit[, remove]
      woout = fit
      good = woout[, (remove == 0), drop = FALSE]
      maxcurve = apply(good, 1, max)
      mincurve = apply(good, 1, min)
      if (sum(outly) > 0) {
        if (plot) {
          matlines(x, out, lty = 2, col = outliercol, 
                   type = "l", ...)
        }
      }
      barval = (x[1] + x[tp])/2
      bar = which(sort(c(x, barval)) == barval)[1]
      if (plot) {
        lines(c(x[bar], x[bar]), c(maxcurve[bar], sup[bar]), 
              col = barcol, lwd = 2)
        lines(c(x[bar], x[bar]), c(mincurve[bar], inf[bar]), 
              col = barcol, lwd = 2)
      }
    }
    xx = c(x, x[order(x, decreasing = TRUE)])
    supinv = sup[order(x, decreasing = TRUE)]
    yy = c(inf, supinv)
    if (plot) {
      if (prob[pp] == 0.5) {
        polygon(xx, yy, col = color[pp], border = barcol, 
                lwd = 2)
      }
      else {
        polygon(xx, yy, col = color[pp], border = NA)
      }
    }
  }
  if (plot) {
    lines(x, fit[, index[1]], lty = 1, lwd = 2, col = 1, 
          type = "l")
    lines(x, maxcurve, col = barcol, lwd = 2)
    lines(x, mincurve, col = barcol, lwd = 2)
    if (fullout) {
      if (sum(outly) > 0) {
        if (plot) {
          matlines(x, out, lty = 2, col = outliercol, 
                   type = "l", ...)
        }
      }
    }
  }
  return(list(depth = depth, outpoint = outpoint, medcurve = which(med)))
}