\name{hImg}
\alias{hImg}
\title{Print Plot to File and Return HTML}
\usage{
  hImg(my_plot, file, width = 4, height = 3, dpi = 300,
    dim = NULL, scale = 100, ...)
}
\arguments{
  \item{my_plot}{a plot object, or code that generates a
  plot}

  \item{file}{file location to output image to}

  \item{width}{width (in inches) of the plot}

  \item{height}{height (in inches) of the plot}

  \item{dpi}{the number of dots per inch used. Default is
  high to ensure plots are crisp on all displays}

  \item{dim}{passed to \code{par( mfrow )}; used if making
  multiple base-R plots}

  \item{scale}{the scale factor to use when scaling plots
  for web display. ie, \code{scale=100} implies a setting
  of \code{width=3} would translate to a width of 300
  pixels on a web browser.}

  \item{...}{optional arguments passed to
  \code{\link{png}}}
}
\description{
  A convenience function that prints a plot to file, and
  then returns HTML to embed that image in the page.
}
\details{
  The \code{dim} attribute is passed on to \code{par(
  mfrow='dim' )}; ie, it is used if you are called a plot
  function that writes more than one plot.

  The \code{png} device is used.
}
\examples{
## generate an xyplot, write it to file, and return HTML
## code that sources the generated image
dat <- data.frame( x = rnorm(100), y = rnorm(100) )
hImg( file = "plot_output.png",
  xyplot( y ~ x, dat )
  )
}

