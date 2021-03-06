\name{remove_na}
\alias{remove_na}
\title{Remove NA Entries from a Vector}
\usage{
remove_na(x)
}
\arguments{
  \item{x}{An (atomic) vector, or a list / data.frame.}
}
\description{
This function removes all \code{NA} entries from a vector.
}
\details{
For \code{data.frames}, we use \code{complete.cases} to
remove \code{NA}s, and hence remove all rows for which an
\code{NA} value in encountered.
}

