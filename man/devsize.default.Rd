% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/default.R
\name{devsize.default}
\alias{devsize.default}
\title{Determine Device Size by Default}
\usage{
\method{devsize}{default}(x, width, height, device = list(width = width * 1.1,
  height = height * 1.1), digits = getOption("plotscale_devsize_digits", 2),
  plotsize = getOption("plotscale_plotsize", "plotsize"),
  width.major = TRUE, optimize.minor = TRUE, check.aspect = TRUE,
  verbose = getOption("plotsize_verbose", FALSE), ...)
}
\arguments{
\item{x}{object}

\item{width}{desired plot width in inches}

\item{height}{desired plot height in inches}

\item{device}{length 2 named list suggesting initial width and height for device}

\item{digits}{\code{\link{round}} width, height, device, and result to this many decimal places; passed to plotsize}

\item{plotsize}{a function of x, width, height, digits and \dots that returns a named list with elements 'width' and 'height' giving dimensions of x}

\item{width.major}{whether to optimize width first for fixed aspect plots (used internally)}

\item{optimize.minor}{whether to minor dimension for fixed aspect plots (used internally)}

\item{check.aspect}{whether to adjust width and height to conform to fixed aspect (used internally)}

\item{verbose}{whether to explain the process}

\item{...}{other arguments passed to fun}
}
\value{
length 2 named list giving device width and height in inches (class 'size')
}
\description{
Determines smallest device width and height that give a plot width
and height at least as large as the supplied \code{width} and \code{height}.
Plot width and height are determined by \code{plotsize} (e.g., by the dimensions
of the first panel as for \code{\link{plotsize.default}}).
}
\examples{
options(example.ask = FALSE)
options(device.ask.default = FALSE)
library(lattice)
data(iris)
x <- xyplot(Petal.Width~Petal.Length, groups = Species, data = iris)
\donttest{
devsize(x, width = 4, height = 4, digits = 1, verbose = TRUE)
}
devsize(x, width = 4, height = 5, digits = 1, verbose = TRUE)
\donttest{
x <- update(x, aspect = 1)
devsize(x, width = 4, height = 4, digits = 1, verbose = TRUE)
}
devsize(x, width = 4, height = 5, digits = 1, verbose = TRUE)
\donttest{
devsize(splom(iris[,1:4]), width = 4, height = 4)
devsize(xyplot(Sepal.Length ~ Sepal.Width | Species, data = iris), width = 4, height = 4)
library(ggplot2)
p <- ggplot(data = iris, mapping = aes(y = Sepal.Length, x = Sepal.Width)) +
  geom_point() + facet_wrap(~Species)
devsize(p, width = 4, height = 4)
devsize(p + theme(aspect.ratio = 1), width = 4, height = 4)
}
}
\seealso{
Other devsize: \code{\link{as.size}},
  \code{\link{devsize}}, \code{\link{print.size}},
  \code{\link{unscaled.default}}, \code{\link{unscaled}}
}
