#' Print kHTML Objects
#' 
#' By default, we \code{cat} out kHTML objects as we typically
#' intend to embed them in R Markdown documents. This is mainly used for
#' printing of items in the environment \code{html}.
#' @param ... a set of kHTML objects (strings).
#' @method print kHTML
#' @seealso \code{\link{html}}
#' @examples
#' data(html)
#' html$br()
print.kHTML <- function(...) {
  cat( ... )
}

#' Make HTML Elements
#' 
#' Creates a function that returns a function that can be used to generate
#' HTML elements. See examples for usage.
#' 
#' This function returns a function that can be called as an HTML tag
#' generating function. For example, by calling
#' \code{p <- makeHTMLTag("p")}, we can generate a function that interprets
#' all named arguments as attributes, and all unnamed arguments as
#' 'data'. This is primarily used behind the scenes to generate all the HTML
#' tagging functions in \code{\link{html}}.
#' 
#' Note that this function assumes the presence of the \code{html} environment,
#' which is where HTML function declarations live.
#' 
#' @param tag the HTML tag to use
#' @param ... a collection of named and unnamed arguments;
#'  named arguments are parsed as attributes of the tag,
#'  unnamed arguments are pasted together into the inner data of the tag.
#' @seealso \code{\link{html}}
makeHTMLTag <- function(tag, ...) {
  
  if( !exists("html") ) {
    html <- NULL
    data("html", package="Kmisc" )
  }
  
  tag <- tag
  
  return( function(...) {
    
    dotArgs <- match.call(expand.dots=FALSE)$`...`
    if( is.null( names(dotArgs) ) ) {
      unnamedArgs <- unlist( lapply( dotArgs, function(x) {
        if( is.symbol(x) ) {
          return( get( as.character( x ), pos=1 ) )
        } else if( is.call(x) ) {
          eval( x, envir=html )
        } else {
          return( x )
        } } ) )
      namedArgs <- list()
    } else {
      unnamedArgs <- unlist( sapply( dotArgs[ names(dotArgs) == "" ], function(x) {
        if( is.symbol(x) ) {
          return( get( as.character( x ), pos=1 ) )
        } else if( is.call(x) ) {
          eval( x, envir=html )
        } else {
          return( x )
        } } ) )
      namedArgs <- dotArgs[ names(dotArgs) != "" ]
    }
    
    splitArgs <- list(namedArgs, unnamedArgs)
    names(splitArgs) <- c("named", "unnamed")
    
    
    if( length( splitArgs$named ) > 0 ) {
      attrs <- paste0( " ",
                       paste( names(splitArgs$named), 
                              paste0( "'",
                                      unlist( splitArgs$named ),
                                      "'" ),
                              sep = "=", collapse = "" )
      )
      
    } else {
      attrs <- NULL
    }
    
    if( length(splitArgs$unnamed) == 0 ) {
      out <- paste0( "<", tag, attrs, "/>" )
      class(out) <- "kHTML"
      return( out )
    } else {
      data <- paste( splitArgs$unnamed, collapse = " " )
      out <- paste0( "<", tag, attrs, ">", data, "</", tag, ">" )
      class(out) <- "kHTML"
      return( out )
    } 
    
  } )
    
}

# Generate HTML tag environment
# 
# Generates the environment used for the HTML utility functions. Included
# as means of documentation for where the HTML utility functions come from.
# html <- new.env()
# html_tags <- scan( what=character(), sep="\n", strip.white=TRUE,
#                    "./data/html.txt" )
# tags <- gsub( "(<)(.*?)(>)", "\\2", html_tags, perl=TRUE )
# for( tag in tags ) {
#   assign( tag, makeHTMLTag(tag), html )
# }
# save( html, file=paste0( getwd(), "/data/html.rda" ) )