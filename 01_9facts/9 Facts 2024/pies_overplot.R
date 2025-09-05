#function to plot pie charts in a scatterplot
#function modified by SP from original caroline::pies
#The change was in line 12 from 'plot(x0, y0, pch = "", ...)' to 'points(x0, y0, pch = "", ...)  '

pies_overplot <- function (x, show.labels = FALSE, show.slice.labels = FALSE, 
          color.table = NULL, radii = rep(2, length(x)), x0 = NULL, 
          y0 = NULL, edges = 200, clockwise = FALSE, init.angle = if (clockwise) 90 else 0, 
          density = NULL, angle = 45, border = NULL, lty = NULL, other.color = "gray", 
          na.color = "white", ...) 
{
  if (!par()$new) {
    points(x0, y0, pch = "", ...)  #original: plot(x0, y0, pch = "", ...)
    par(new = TRUE)
  }
  if (!is.list(x)) 
    stop("x must be a list")
  if (length(x) != length(x0) | length(x0) != length(y0)) 
    stop(paste("x0 and y0 lengths (", length(x0), ",", length(y0), 
               ") must match length of x (", length(x), ")", sep = ""))
  if (length(radii) < length(x)) 
    radii <- rep(radii, length.out = length(x))
  cx <- 0.25 * par("cxy")[1]
  cy <- 0.19 * par("cxy")[2]
  radii <- radii
  xlim <- usr2lims()$x
  ylim <- usr2lims()$y
  y2x.asp <- diff(xlim)/diff(ylim)
  pie.labels <- names(x)
  if (is.null(color.table)) {
    unique.labels <- unique(unlist(lapply(x, names)))
    color.table <- rainbow(length(unique.labels))
    names(color.table) <- unique.labels
  }
  for (j in seq(along = x)) {
    X <- x[[j]]
    data.labels <- names(X)
    if (j != 1) 
      par(new = TRUE)
    if (!is.numeric(X) || any(is.na(X) | X < 0)) 
      stop("'x' values must be non-missing positive.")
    if (length(X) == 0) {
      warning(paste(names(x)[[j]], "has zero length vector"))
    }
    else {
      X <- c(0, cumsum(X)/sum(X))
      names(X) <- data.labels
      dx <- diff(X)
      nx <- length(dx)
      plot.new()
      pin <- par("pin")
      if (all(xlim == c(-1, 1)) && all(ylim == c(-1, 1))) {
        if (pin[1] > pin[2]) 
          xlim <- (pin[1]/pin[2]) * xlim
        else ylim <- (pin[2]/pin[1]) * ylim
      }
      plot.window(xlim, ylim, "")
      nolgnd <- names(X)[!names(X) %in% names(color.table)]
      color.table <- c(color.table, nv(rep(other.color, 
                                           length(nolgnd)), nolgnd))
      col <- color.table[names(X)]
      col[names(col) == "NA"] <- na.color
      if (length(border) > 1) {
        if (length(border) != length(x)) 
          stop("length of border doesnt equal length of x")
        this.brdr <- border[j]
      }
      else {
        this.brdr <- border
      }
      lty <- rep(lty, length.out = nx)
      angle <- rep(angle, length.out = nx)
      density <- rep(density, length.out = nx)
      twopi <- if (clockwise) 
        -2 * pi
      else 2 * pi
      t2xy <- function(t) {
        t2p <- twopi * t + init.angle * pi/180
        list(x = radii[j] * cx * cos(t2p), y = radii[j] * 
               cy * sin(t2p))
      }
      for (i in 1:nx) {
        lab <- as.character(names(X)[i])
        nx <- as.character(names(X)[i])
        n <- max(2, floor(edges * dx[i]))
        P <- t2xy(seq.int(X[i], X[i + 1], length.out = n))
        polygon(c(x0[j] + P$x, x0[j]), c(y0[j] + P$y, 
                                         y0[j]), density = density[i], angle = angle[i], 
                border = this.brdr, col = col[lab], lty = lty[i], 
                ...)
        P <- t2xy(mean(X[i + 0:1]))
        if (!is.na(lab) && nzchar(lab)) {
          if (show.slice.labels) {
            lines(x0[j] + c(1, 1.05) * P$x, y0[j] + c(1, 
                                                      1.05) * P$y)
            text(x0[j] + 1.1 * P$x, y0[j] + 1.1 * P$y, 
                 lab, xpd = TRUE, adj = ifelse(P$x < 0, 
                                               1, 0))
          }
        }
      }
      if (show.labels) 
        text(x0[j], y0[j] + radii[j] + 0.2, pie.labels[j])
      invisible(NULL)
    }
  }
}