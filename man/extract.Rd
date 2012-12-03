\name{extract}
\alias{extract}
\title{Extract Variables from a List / Data Frame}
\usage{
  extract(dat, ...)
}
\arguments{
  \item{dat}{\code{list} or \code{data.frame} object, or
  other similar object with a \code{names} attribute}

  \item{...}{an optional number of 'names' to match in
  \code{dat}}
}
\description{
  Extract variables from a \code{list} / \code{data.frame}.
  The first argument is the \code{list} /
  \code{data.frame}, while the second is passed to and
  parsed from \code{...}. We return the \code{list} /
  \code{data.frame}, including only those whose names were
  found in \code{...}.
}
\details{
  We can be 'lazy' with how we name the variables. The
  \code{\link{name}}s passed to \code{...} are not
  evaluated directly; rather, their character
  representation is taken and used for extraction.
  Furthermore, for a given item submitted, all text before
  a \code{$} is removed.
}
\examples{
dat <- data.frame( x = c(1, 2, 3), y = c("a", "b", "c"), z=c(4, 5, 6) )
## all of these return identical output
dat[ names(dat) \%in\% c("x","z") ]
extract( dat, x, z)
extract( dat, dat$x, dat$z )
}
\seealso{
  \code{\link{without}} \code{\link{extract.re}}
}

