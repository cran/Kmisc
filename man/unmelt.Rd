\name{unmelt_}
\alias{unmelt_}
\title{Unmelt a Melted Data Frame}
\usage{
unmelt_(data, variable = "variable", value = "value")
}
\arguments{
  \item{data}{A \code{data.frame}.}

  \item{variable}{The index, or name, of the
  \code{variable} vector; analogous to the vector produced
  with name \code{variable.name}.}

  \item{value}{The value of the \code{value} vector;
  analogous to the vector produced with name
  \code{value.name}.}
}
\description{
This function undoes the \code{melt}ing process done by
either \code{reshape2::melt} or \code{\link{melt_}}.
}

