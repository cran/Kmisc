\name{chunk}
\alias{chunk}
\title{Generate a Sequence of Integers, and Split into Chunks}
\usage{
chunk(min, max, size, by = 1)
}
\arguments{
  \item{min}{The lower end (start point) of the sequence.}

  \item{max}{The upper end (end point) of the sequence.}

  \item{size}{The number of elements to place in each
  chunk.}

  \item{by}{The difference between consecutive elements.}
}
\description{
This function takes a set of endpoints, and 'chunks' the
sequence from \code{min} to \code{max} into a list with
each element of size \code{size}.
}
\details{
If \code{max} is not specified, then we generate a chunk of
integers from 1 to \code{min}, each of size \code{size}.
This allows you to specify chunks with syntax like
\code{chunk(100, size=5)}.
}

