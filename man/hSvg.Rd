\name{hSvg}
\alias{hSvg}
\title{Print SVG to File and Return HTML}
\usage{
hSvg(my_plot, file, width = 400, height = 300, dim = NULL, scale = 100,
  ...)
}
\arguments{
  \item{my_plot}{a plot object, or code that generates a
  plot}

  \item{file}{location to output file}

  \item{width}{width (in pixels) of the plot}

  \item{height}{height (in pixels) of the plot}

  \item{scale}{the scale used to scale the plot from inches
  to pixels, for display in a web browser}

  \item{dim}{passed to \code{par( mfrow )}; used if making
  multiple base-R plots}

  \item{...}{passed to \code{svg}}
}
\description{
A convenience function that prints a plot to file, and then
returns HTML to embed that image in the page. Used for SVG
images.
}
\examples{
library(lattice)
## generate an xyplot, write it to file, and return HTML code that
## sources the generated image
dat <- data.frame( x = rnorm(100), y = rnorm(100) )
hSvg( file = "plot_output.svg",
  xyplot( y ~ x, dat )
)
}

