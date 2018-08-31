#' Coerce to PDF
#'
#' Coerces to PDF. Generic, with default method.
#' @param x object
#' @param ... passed arguments
#' @export
#' @family as.pdf
#' @family devices
#' @examples
#' example(as.pdf.default)

as.pdf <- function(x,...)UseMethod('as.pdf')

#' Coerce to PNG
#'
#' Coerces to PNG. Generic, with default method.
#' @param x object
#' @param ... passed arguments
#' @export
#' @family as.png
#' @family devices
#' @examples
#' example(as.png.default)
as.png <- function(x,...)UseMethod('as.png')

#' Determine Plot Size
#'
#' Determines the size of a plot.  Generic, with default method.
#' @export
#' @param x object
#' @param ... other arguments
#' @family generic functions
#' @family plotsize
#' @examples
#' example(plotsize.default)

plotsize <- function(x,...)UseMethod('plotsize')

#' Determine Unscaled Device Size
#'
#' Determines the size of an unscaled device.  Generic, with default method.
#' @export
#' @param x object
#' @param ... other arguments
#' @family generic functions
#' @family devsize
#' @examples
#' example(plotsize.default)

unscaled <- function(x,...)UseMethod('unscaled')

#' Determine Device Size
#'
#' Determines the size of a device.  Generic, with default method.
#' @export
#' @param x object
#' @param ... other arguments
#' @family generic functions
#' @family devsize
#' @examples
#' example(devsize.default)

devsize <- function(x,...)UseMethod('devsize')

#' Coerce to Size
#'
#' Coerce to class 'size'.  Generic, with method for list.
#' @export
#' @param x object
#' @param ... ignored
#' @family generic functions
#' @family devsize
#' @family plotsize
#' @family size

as.size <- function(x,...)UseMethod('as.size')
#' Coerce List to Size
#'
#' Coerce list to class 'size'.
#' @export
#' @param x object
#' @param ... ignored
#' @family size

as.size.list <- function(x,...){
  class(x) <- union('size', class(x))
  if(is.null(attr(x,'unit')))attr(x,'unit') <- 'in'
  x
}

