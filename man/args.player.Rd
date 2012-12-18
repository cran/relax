\name{args.player}
\alias{args.player}
\title{ An interactive Tcl/Tk Tool for Evaluation of Functions Calls }
\description{
  \code{args.player} is a simple tool for checking the effect of different inputs to
  a function. \code{args.player} creates a small Tcl/Tk widget showing the arguments 
  of the function and the values of the arguments. The values can be modified by typing
  in new values. A click on the \code{Eval} button starts the function with the fixed 
  parameters and you can consider the results.
}
\usage{
args.player(fn.call, width.of.label = 20, width.of.entry = 25, main)
}
\arguments{
  \item{fn.call}{ a function call or a function name }
  \item{width.of.label}{ width of the region used for printing the argument names }
  \item{width.of.entry}{ width of the entry fields for the values of the arguments }
  \item{main}{ title for the widget }
}
\details{
  There are two ways of usage: 
  At first the input of \code{args.player} can be a complete function call.
  Then you should name the arguments correctly to prevent problems of identification.
  Secondly, it is possible to call \code{args.player} with a function name, only.
  In this case arguments will be searched by \code{arg}.
}
\author{ Peter Wolf, pwolf@wiwi.uni-bielefeld.de }
\examples{
# do not run
\dontrun{
    args.player(mean)
}
\dontrun{ 
    fun <- function(n = 100, m=0, sd=1, type="l", bty="n", 
                    xlim = c(0,1), col = "red", main = "title" ){
             x <- rnorm(n,m,sd)
             plot(x, type=type, bty=bty, xlim = xlim, col = col, main = main)
           }
    args.player( 
       fun( n = 100, m = 5, sd = 10, type = "l", bty = "n", 
            xlim = c(0,10), col = "blue", main = "effect of plot parameters" ), 
       main="some plot test" 
    ) 
}
}
\keyword{ utilities }

