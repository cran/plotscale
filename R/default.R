#' Determine Device Size by Default
#'
#' Determines smallest device width and height that give a plot width
#' and height at least as large as the supplied \code{width} and \code{height}.
#' Plot width and height are determined by \code{plotsize} (e.g., by the dimensions
#' of the first panel as for \code{\link{plotsize.default}}).

#' @export
#' @param x  object
#' @param width desired plot width in inches
#' @param height desired plot height in inches
#' @param device length 2 named list suggesting initial width and height for device
#' @param digits \code{\link{round}} width, height, device, and result to this many decimal places; passed to plotsize
#' @param plotsize a function of x, width, height, digits and \dots that returns a named list with elements 'width' and 'height' giving dimensions of x
#' @param width.major whether to optimize width first for fixed aspect plots (used internally)
#' @param optimize.minor whether to minor dimension for fixed aspect plots (used internally)
#' @param check.aspect whether to adjust width and height to conform to fixed aspect (used internally)
#' @param verbose whether to explain the process
#' @param ... other arguments passed to fun
#' @family devsize
#' @return length 2 named list giving device width and height in inches (class 'size')
#' @examples
#' options(example.ask = FALSE)
#' options(device.ask.default = FALSE)
#' library(lattice)
#' data(iris)
#' x <- xyplot(Petal.Width~Petal.Length, groups = Species, data = iris)
#' \donttest{
#' devsize(x, width = 4, height = 4, digits = 1, verbose = TRUE)
#' }
#' devsize(x, width = 4, height = 5, digits = 1, verbose = TRUE)
#' \donttest{
#' x <- update(x, aspect = 1)
#' devsize(x, width = 4, height = 4, digits = 1, verbose = TRUE)
#' }
#' devsize(x, width = 4, height = 5, digits = 1, verbose = TRUE)
#' \donttest{
#' devsize(splom(iris[,1:4]), width = 4, height = 4)
#' devsize(xyplot(Sepal.Length ~ Sepal.Width | Species, data = iris), width = 4, height = 4)
#' library(ggplot2)
#' p <- ggplot(data = iris, mapping = aes(y = Sepal.Length, x = Sepal.Width)) +
#'   geom_point() + facet_wrap(~Species)
#' devsize(p, width = 4, height = 4)
#' devsize(p + theme(aspect.ratio = 1), width = 4, height = 4)
#' }

devsize.default <- function(
  x,
  width,
  height,
  device = list(width = width * 1.1, height = height * 1.1),
  digits = getOption('plotscale_devsize_digits',2),
  plotsize = getOption('plotscale_plotsize', 'plotsize'),
  width.major = TRUE,
  optimize.minor = TRUE,
  check.aspect = TRUE,
  verbose = getOption('plotsize_verbose', FALSE),
  ...
){
  if(missing(width))stop('width cannot be missing')
  if(missing(height))stop('width cannot be missing')
  if(!is.list(device))device <- as.list(device)
  stopifnot(length(device) == 2)
  if(is.null(names(device)))names(device) <- c('width','height')
  stopifnot(all(c('width','height') %in% names(device)))
  args <- c(width, height, device$width, device$height)
  if(any(is.na(args)))stop('all arguments must be defined')
  stopifnot(is.numeric(args))
  stopifnot(all(args > 0))
  width <- round(width, digits = digits)
  height <- round(height, digits = digits)
  device <- lapply(device, round, digits = digits)
  fun <- match.fun(plotsize)
  ps <- fun(x, width = device$width * 1.0, height = device$height * 1.0, digits = digits,...)
  while(ps$width <= 0 | ps$height <= 0){
    device$width <- device$width * 1.1
    device$height <- device$height * 1.1
    ps <- fun(x, width = device$width * 1.0, height = device$height * 1.0, digits = digits,...)
  }
  w1 <- fun(x, width = device$width * 1.1, height = device$height * 1.0, digits = digits,...)
  h1 <- fun(x, width = device$width * 1.0, height = device$height * 1.1, digits = digits,...)
  if(verbose)message('plot size:  ',ps$width,', ',ps$height,' device size: ',device$width,', ',device$height)

  heightLimited <- h1$height > ps$height
  widthLimited  <- w1$width > ps$width
  unconstrained <- heightLimited & widthLimited
  optimized     <- !heightLimited & !widthLimited
  heightOK      <- ps$height == height & (unconstrained | optimized)
  widthOK       <- ps$width == width   & (unconstrained | optimized)
  if(heightOK & widthOK) {
    if (verbose)message('WIDTH AND HEIGHT OK')
    return(as.size(device))
  }
  if(unconstrained){
    if(verbose)message('rescaling width and height')
    actual <- c(ps$width,ps$height)
    desired <- c(width,height)
    minimized <- round(digits = digits, actual) == round(digits = digits, desired)
    if(all(minimized)) return(as.size(device)) # should not reach this
    gradient <- c(2,2)
    better <- c(FALSE,FALSE)
    scale <- c(1,1)
    while(any(!better & !minimized)){
      # estimate can oscillate around target without improving.
      # try shallower approach in that case, up to 10-fold (.5^10 = 0.00098)
      if(any(gradient < 0.001 & !better)) stop('gradient very small with no improvement in major dimension')
      if(!(better[[1]]) & !(minimized[[1]]))gradient[[1]] <- gradient[[1]] / 2
      if(!(better[[2]]) & !(minimized[[2]]))gradient[[2]] <- gradient[[2]] / 2
      gradient[minimized] <- 1
      #gradient[!better] <- gradient[!better]/2
      if(verbose)if(!all(gradient >= 1)) message('gradient: ',paste(gradient, collapse = ', '))
      scale <- 1 + (desired - actual) / actual * gradient
      trial <- fun(x, width = device$width * scale[[1]], height = device$height * scale[[2]], digits = digits,...)
      major <- c(trial$width,trial$height)
      trialDistance <- round(digits=digits, abs(major - desired))
      currentDistance <- round(digits=digits, abs(actual - desired))
      better <- trialDistance < currentDistance
      minimized <- round(digits = digits, major) == round(digits = digits, desired)
    }
    newDevice <- device
    newDevice$width <- newDevice$width * scale[[1]]
    newDevice$height <- newDevice$height * scale[[2]]
    return(
      devsize(
        x,
        width = width,
        height = height,
        digits = digits,
        device = newDevice,
        plotsize = plotsize,
        width.major = width.major,
        optimize.minor = optimize.minor,
        check.aspect = check.aspect,
        verbose = verbose,
        ...
      )
    )
  }
  # if(verbose) message('constrained')
  # At this point we are working with a fixed-aspect (constrained) plot.
  # Make sure to request reasonable dimensions.
  if(check.aspect){
    check.aspect <- FALSE # never check again
    aspect <- ps$height / ps$width
    requested <- height / width
    newWidth <- width
    newHeight <- height
    if(requested > aspect) newWidth <- height / aspect # req. is too big: inflate req. width to shrink req
    if(requested <= aspect) newHeight <- width * aspect  # req. is too small: inflate req. height to grow req
    newWidth <- round(newWidth, digits = digits)
    newHeight <- round(newHeight, digits = digits)
    if(newWidth != width || newHeight != height){
      stat <- signif(digits = 6, (requested/aspect) * 100)
      if(verbose) message('requested is ', stat,' percent of actual aspect')
      if(verbose) message('requesting width ',newWidth, ' and height ',newHeight, ' instead')
      return(
        devsize(
          x,
          width = newWidth,
          height = newHeight,
          digits = digits,
          device = device,
          plotsize = plotsize,
          width.major = width.major,
          optimize.minor = optimize.minor,
          check.aspect = check.aspect,
          verbose = verbose,
          ...
        )
      )
    }
  }

  # At least one of widthLimited and heightLimited is TRUE.
  # Both could be true, in which case we are optimized with
  # respect to aspect but not size.
  # If both were FALSE, we would have exited as unconstrained.

  # Address major first.
  # Make sure it is limiting (and therefore responsive)
  # By growing the minor dimension.
  if(!widthLimited & width.major){
    if(verbose) message('not width limited - trying bigger height')
    return(
      devsize(
        x,
        width = width,
        height = height,
        digits = digits,
        device = list(
          width = device$width,
          height = device$height * 2
        ),
        plotsize = plotsize,
        width.major = width.major,
        optimize.minor = optimize.minor,
        check.aspect = check.aspect,
        verbose = verbose,
        ...
      )
    )}
  if(!heightLimited & !width.major){
    if(verbose) message('not height limited - trying bigger width')
    return(
      devsize(
        x,
        width = width,
        height = height,
        digits = digits,
        device = list(
          width = device$width * 2,
          height = device$height
        ),
        plotsize = plotsize,
        width.major = width.major,
        optimize.minor = optimize.minor,
        check.aspect = check.aspect,
        verbose = verbose,
        ...
      )
    )}

  # At this point we have a major-limited fixed-aspect plot.
  # Optimize major while maintaining aspect ratio.
  actual <- if(width.major) ps$width else ps$height
  desired <- if(width.major) width else height
  minimized <- round(digits = digits, actual) == round(digits = digits, desired)

  if(!minimized){
    gradient <- 2
    better <- FALSE
    scale <- 1
    while(!better){
      if(gradient < 0.001) stop('gradient very small with no improvement in major dimension')
      # estimate can oscillate around target without improving.
      # try shallower approach in that case, up to 10-fold (.5^10 = 0.00098)
      gradient <- gradient/2
      if(verbose)if(gradient != 1) message('gradient: ',gradient)
      scale <- 1 + (desired - actual) / actual * gradient
      trial <- fun(x, width = device$width * scale, height = device$height * scale, digits = digits,...)
      major <- if(width.major) trial$width else trial$height
      trialDistance <- round(digits=digits, abs(major - desired))
      currentDistance <- round(digits=digits, abs(actual - desired))
      better <- trialDistance < currentDistance
    }
    newDevice <- device
    newDevice$width <- newDevice$width * scale
    newDevice$height <- newDevice$height * scale
    return(
      devsize(
        x,
        width = width,
        height = height,
        digits = digits,
        device = newDevice,
        plotsize = plotsize,
        width.major = width.major,
        optimize.minor = optimize.minor,
        check.aspect = check.aspect,
        verbose = verbose,
        ...
      )
    )
  }
  # Now major dimension is optimized for this device
  if(verbose) message('OPTIMIZED BY ', if(width.major) 'WIDTH' else 'HEIGHT')
  if(!optimize.minor)return(as.size(device))
  # Optimize the minor dimension
  other <-  devsize(
    x,
    width = width,
    height = height,
    digits = digits,
    device = device,
    plotsize = plotsize,
    width.major = !width.major,
    optimize.minor = FALSE,
    check.aspect = check.aspect,
    verbose = verbose,
    ...
  )
  # solution is a hybrid of this device and other
  res <- list(
    width =  if(width.major) device$width else other$width,
    height = if(width.major) other$height else device$height
  )
  class(res) <- c('size',class(res))
  attr(res,'unit') <- 'in'
  return(as.size(res))
}
#' Convert to PNG by Default
#'
#' Converts object to PNG.  By default, device is scaled by plot size. Device is automatically closed and filename argument is returned.

#' @export
#' @param x  object
#' @param width desired plot width
#' @param height desired plot height
#' @param devsize a function of x, width, height, and \dots that calculates device size
#' @param res passed to \code{\link[grDevices]{png}}
#' @param units NOT passed to \code{\link[grDevices]{png}}; width and height always treated as inches regardless
#' @param scaled whether to rescale width and height by plot size
#' @param filename file name passed to \code{\link[grDevices]{png}}
#' @param ... other arguments to \code{\link[grDevices]{png}} and devsize
#' @family as.png
#' @family devices
#' @importFrom grDevices png dev.off
#' @return (invisible) the filename argument
#' @examples
#' options(example.ask = FALSE)
#' options(device.ask.default = FALSE)
#' library(lattice)
#' as.png(xyplot(2~3), filename = tempfile())

as.png.default <- function(
  x,
  width = getOption('plotscale_png_width',3),
  height = getOption('plotscale_png_height',3),
  devsize = if(scaled) 'devsize' else 'unscaled',
  res = getOption('plotscale_res',300),
  units = 'in',
  scaled = getOption('plotscale_scaled',TRUE),
  filename = "Rplot%03d.png",
  ...
){
  fun <- match.fun(devsize)
  dim <- fun(x, width = width, height = height, ...)
  frm <- names(formals(png))
  arg <- list(...)
  arg <- arg[names(arg) %in% frm]
  arg$width <- dim$width
  arg$height <- dim$height
  arg$filename <- filename
  arg$res <- res
  arg$units <- 'in'

  tryCatch(
    {
      do.call(png,arg)
      print(x)
    },
    error = function(e) e,
    finally = dev.off()
  )
  invisible(filename)
}

#' Convert to PDF by Default
#'
#' Converts object to PDF.  By default, device is scaled by plot size. Device is automatically closed and file argument is returned.

#' @export
#' @param x  object
#' @param width desired plot width
#' @param height desired plot height
#' @param devsize a function of x, width, height, and \dots that calculates device size
#' @param scaled whether to rescale width and height by plot size
#' @param onefile logical: passed to \code{\link[grDevices]{pdf}}. If true (the default) allow multiple figures in one file. If false, generate a file with name containing the page number for each page. Defaults to TRUE, and forced to true if file is a pipe.
#' @param file file name passed to \code{\link[grDevices]{pdf}}
#' @param ... other arguments to \code{\link[grDevices]{pdf}} and fun
#' @family as.pdf
#' @family devices
#' @importFrom grDevices pdf dev.off
#' @return (invisible) the file argument
#' @examples
#' options(example.ask = FALSE)
#' options(device.ask.default = FALSE)
#' library(lattice)
#' as.pdf(xyplot(2~3),file = tempfile())

as.pdf.default <- function(
  x,
  width = getOption('plotscale_pdf_width',3),
  height = getOption('plotscale_pdf_height',3),
  devsize = if(scaled) 'devsize' else 'unscaled',
  scaled = getOption('plotscale_scaled',TRUE),
  onefile = TRUE,
  file = if (onefile) "Rplots.pdf" else "Rplot%03d.pdf",
  ...
){
  fun <- match.fun(devsize)
  dim <- fun(x, width = width, height = height, ...)
  frm <- names(formals(pdf))
  arg <- list(...)
  arg <- arg[names(arg) %in% frm]
  arg$width <- dim$width
  arg$height <- dim$height
  arg$onefile <- onefile
  arg$file <- file
  tryCatch(
    {
      do.call(pdf,arg)
      print(x)
    },
    error = function(e) e,
    finally = dev.off()
  )
  invisible(file)
}

#' Determine Unscaled Device Size by Default
#'
#' Determines the size of an unscaled device. Width and height
#' are interpreted directly as device dimensions rather than plot dimensions.
#' @export
#' @param x object
#' @param width desired width in inches
#' @param height desired height in inches
#' @param ... other arguments
#' @family devsize
unscaled.default <- function(x, width, height, ...)list(width = width, height = height)

#' Determine Plot Size by Default
#'
#' Determines the size of a grid graphics plot conditional on device dimensions.
#' Size is defined as physical width and height in inches
#' of the first encountered panel as rendered on a device with the
#' specified width and height. \code{pdf()} is used as the evaluation
#' device. 'First encountered panel' is resolved by evaluating the
#' output of a call to \code{\link[grid]{current.vpTree}} for the
#' first viewport name containing 'panel' or 'subpanel'. The latter
#' takes precedence if available, for intuitive handling of output from
#' \code{\link[lattice]{splom}}.

#' @export
#' @param x object
#' @param width device width in inches
#' @param height device height in inches
#' @param digits \code{\link{round}} result to this many decimal places
#' @param pattern character: vector of search patterns to identify critical viewport set; first non-empty set is retained
#' @param index integer: select this element of the critical viewport set
#' @param ... other arguments passed to \code{\link[grDevices]{pdf}}
#' @family plotsize
#' @return length 2 named list giving width and height in inches (class 'size') for the first panel
#' @importFrom grid seekViewport convertWidth convertHeight unit
#' @examples
#' options(ask.default = FALSE)
#' options(device.ask.default = FALSE)
#' library(lattice)
#' p <- xyplot(2~3)
#' plotsize(p, width = 7, height = 7)


plotsize.default <- function(
  x,
  width,
  height,
  digits = getOption('plotscale_plotsize_digits',3),
  pattern = c('subpanel\\b','panel\\b'),
  index = 1,
  ...
){
  if(missing(width))stop('width cannot be missing')
  if(missing(height))stop('height cannot be missing')
  frm <- names(formals(pdf))
  arg <- list(...)
  arg <- arg[names(arg) %in% frm]
  arg$width <- width
  arg$height <- height
  arg$file <- NULL # in case it was passed as ...
  arg <- c(arg,list(file = NULL))
  do.call(pdf, arg)
  res <- NULL
  tryCatch(
    {
      print(x)
      vp <- as.character(grid::current.vpTree())
      vp <- strsplit(vp, split = 'viewport[', fixed = TRUE)[[1]]
      vp <- sub('].*','',vp)
      set <- NULL
      i <- 0
      while(i < length(pattern) && !length(set)){
        i <- i + 1
        this <- pattern[[i]]
        set <- vp[grepl(this, vp)]
      }
      if(!length(set))stop('no viewport name matching pattern')
      if(index > length(set))stop('index is greater than available matching viewports')
      port <- set[[index]]
      grid::seekViewport(port)
      w <- convertWidth(unit(1,'npc'), 'inch', TRUE)
      h <- convertHeight(unit(1,'npc'), 'inch', TRUE)
      # lims <- current.panel.limits(unit = 'inches')
      res <- list(width = w, height = h)
    },
    error = function(e) e,
    finally = dev.off()
  )
  if(is.null(res))stop('could not determine plot size with width ', width,' and height ',height)
  res <- lapply(res, round, digits = digits)
  class(res) <- c('size',class(res))
  attr(res,'unit') <- 'in'
  res
}

#' Print Size
#'
#' Print object of class 'size', as returned by \code{\link{devsize}} and \code{\link{plotsize}}.

#' @export
#' @param x object
#' @param ... ignored
#' @family plotsize
#' @family devsize

print.size <- function(x,...){
  u <- attr(x,'unit')
  sep <- ','
  msg <- paste('width', x$width, u, sep, 'height',x$height, u)
  cat(msg)
  invisible(x)
}

