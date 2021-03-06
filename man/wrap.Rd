\name{wrap}
\alias{wrap}
\title{Wrap a String}
\usage{
wrap(x, width = 8, ...)
}
\arguments{
  \item{x}{A character vectors, or an object which can be
  converted to a character vector by
  \code{\link{as.character}}.}

  \item{width}{A positive integer giving the number of
  characters a line can reach before we wrap and introduce
  a new line.}

  \item{...}{Optional arguments passed to
  \code{\link{strwrap}}.}
}
\description{
This function operates similarily to \code{\link{strwrap}},
but \code{paste}s the wrapped text back together with line
separators. Useful for automatically wrapping long labels.
}
\examples{
long_label <- "This is a very long label which needs wrapping."
wrap(long_label)
}

