\name{playground}
\alias{playground}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{playground for testing R code}
\description{
  \code{playground} opens a simple text window for testing R code.
}
\usage{
playground(playground.envir=NULL,code=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{playground.envir}{if not NULL \code{playground.envir} is used for evaluation}
  \item{code}{if not NULL \code{code} is inserted in the playground}
}
\details{
  Besides your usual work you often need little tools for writing memos or calculating simple
  arithmethic operations. Some times you take a sheet of paper, a pencil or your desk calculator to go ahead. 
  And there are situations in which you would like to have furthers fields for testing simple R codes. Perhaps you will start another R session in parallel for executing your R statements. 

  \code{playground()} is an alternative approach to this problem. After starting the function
  \code{playground} a text field (in a Tcl/Tk widget) is created and you are allow put some R commands
  into this field. At the bottom of the window you find two buttons: one for activating commands and 
  a second one for closing the window. The results will be printed on the current output device.
  All assignments take place in the global envirnment or if \code{revive.env} exists
  the new objects will be created in \code{revive.env}.

  May be you need the field for taking a lot of notes without any calculations only. 
  Anyway, it is no problem to call \code{playground} several times. 

  \code{playground} is in an experimental state yet. So it may be changed later on.
}
\value{
  \code{playground} does not have any explicit result.
}
\references{\url{http://www.wiwi.uni-bielefeld.de/com/wolf/software/relax.html}}
\author{Hans Peter Wolf}
\examples{
## playground()
}
\keyword{ misc }% at least one, from doc/KEYWORDS

