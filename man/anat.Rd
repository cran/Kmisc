\name{anat}
\alias{anat}
\alias{anatomy}
\title{Display the Anatomy of a Data Frame}
\usage{
anat(df, n = 3, cols = 99)

anatomy(df, n = 3, cols = 99)
}
\arguments{
  \item{df}{An object inheriting class \code{data.frame}.}

  \item{n}{The number of elements to print from each
  vector.}

  \item{cols}{The number of columns to print from the
  \code{data.frame}.}
}
\description{
This function displays the 'anatomy' of a data frame. In
practice, it's used to implement a faster version of
\code{str} for data frames built entirely of atomic
vectors, as \code{str.data.frame} is very slow for large
data frames. If there are non-atomic vectors in \code{df},
we fall back to \code{base::str}.
}
\examples{
\dontrun{
local({
  bigDF <- as.data.frame( matrix( factor(1:1E3), nrow=1E3, ncol=1E3 ) )
  sink( tmp <- tempfile() )
  str <- system.time( str(bigDF, list.len=1E3) )
  anat <- system.time( anat(bigDF) )
  sink()
  unlink(tmp)
  print( rbind( str, anat ) )
})
}
}

