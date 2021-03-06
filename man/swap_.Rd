\name{swap_}
\alias{swap_}
\title{Swap Elements in a Vector}
\usage{
swap_(vec, ...)
}
\arguments{
  \item{vec}{the vector of items whose elemetns you will be
  replacing.}

  \item{...}{A set of named arguments, whereby we translate
  from \code{names} to \code{values} of those arguments.}
}
\description{
This function swaps elements in a vector. See examples for
usage.
}
\details{
If \code{to} is of different type than \code{from}, it will
be coerced to be of the same type.
}
\examples{
x <- c('a', 'a', 'b', 'c')
swap_(x, a="A")
}
\seealso{
\code{\link{swap}}
}

