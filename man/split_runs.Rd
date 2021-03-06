\name{split_runs}
\alias{split_runs}
\title{Split by Runs}
\usage{
split_runs(x)
}
\arguments{
  \item{x}{A numeric or character vector.}
}
\description{
Split a vector into a list of runs, such that each entry in
the output list is a set of runs encountered. This function
accepts two forms of inputs: either a vector where each
element of the vector is of length 1 (e.g. \code{c("A",
"A", "C", "T")}), or a vector of length 1 interpretted as a
long string (e.g. \code{"AAAACCAGGGACGCCGCGGTTGG"}).
}
\details{
Factors will be coerced to character before splitting.
}
\examples{
x <- rbinom( 100, 2, 0.5 )
stopifnot( all( x == unlist( split_runs(x) ) ) )
stopifnot( all( as.character(x) == unlist( split_runs( as.character(x) ) ) ) )
y <- paste( collapse="", sample( LETTERS[1:5], 1E5, replace=TRUE ) )
stopifnot( y == paste( collapse="", split_runs(y) ) )
z <- replicate( 25, paste( collapse="", sample( LETTERS[1:5], 1E3, replace=TRUE ) ) )
system.time( lapply(z, split_runs) )
}
\seealso{
\code{\link{rle}}, for a similar function with different
output.
}

