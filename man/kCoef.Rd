\name{kCoef}
\alias{kCoef}
\title{Nicely Formatted Model Coefficient Output}
\usage{
kCoef(fit, remove_underscore = TRUE, remove_dollar = TRUE,
  swap_periods = TRUE)
}
\arguments{
  \item{fit}{the model fit we wish to generate coefficients
  from.}

  \item{remove_underscore}{remove underscores (and all
  elements after) in a variable?}

  \item{remove_dollar}{remove all elements before and
  including a $ in a variable name?}

  \item{swap_periods}{swap periods with spaces?}
}
\value{
a matrix of coefficients with nicely formatted names.
}
\description{
A customized coefficient function that assigns better row
names to the coefficient matrix returned by
\code{\link{coef}}() for a model fit. Also includes some
arguments for parsing of variable names.
}
\details{
NOTE:

Models with interaction effects are currently not handled.
}
\note{
The names given assume default contrasts in your model fit;
ie, the default is \code{contr.treatment}, where each level
of a factor is compared to a reference.
}
\examples{
## How the remove_underscore and remove_dollar arguments act:
## An example:
##                     kDat$variable_other_stuff
## remove_underscore:  +++++++++++++------------
## remove_dollar:      -----++++++++++++++++++++

x <- rnorm(100); y <- x * runif(100)
z <- as.factor( rep( c("apple", "banana", "cherry", "date"), each=25 ) )
myFit <- lm( y ~ x + z )

## compare the output of these two: which do you prefer?
coef( summary( myFit ) )
kCoef( myFit )
}

