\name{makeHTMLTag}
\alias{makeHTMLTag}
\title{Make HTML Elements}
\usage{
  makeHTMLTag(tag, ...)
}
\arguments{
  \item{tag}{the HTML tag to use}

  \item{...}{a collection of named and unnamed arguments;
  named arguments are parsed as attributes of the tag,
  unnamed arguments are pasted together into the inner data
  of the tag.}
}
\description{
  Creates a function that returns a function that can be
  used to generate HTML elements. See examples for usage.
}
\details{
  This function returns a function that can be called as an
  HTML tag generating function. For example, by calling
  \code{p <- makeHTMLTag("p")}, we can generate a function
  that interprets all named arguments as attributes, and
  all unnamed arguments as 'data'. This is primarily used
  behind the scenes to generate all the HTML tagging
  functions in \code{\link{html}}.

  Note that this function assumes the presence of the
  \code{html} environment, which is where HTML function
  declarations live.
}
\seealso{
  \code{\link{html}}
}

