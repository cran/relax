\name{tangleR}
\alias{tangleR}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ function to tangle a file }
\description{
  \code{tangleR} reads a file that is written according to
  the rules of the \code{noweb} system and performs a specific kind
  of tangling. As a result a \code{.R}-file is generated.
}
\usage{
tangleR(in.file, out.file, expand.roots = NULL,
expand.root.start = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{in.file}{ name of input file }
  \item{out.file}{ name of output file; if missing
  the extension of the input file is turned to \code{.R} }
  \item{expand.roots}{ name(s) of root(s) to be expanded; if NULL
  all will be processed }
  \item{expand.root.start}{ if TRUE (default) root chunk
  "start" will be expanded }
}
\details{
  General remarks: A \code{noweb} file consists of a mixture of text
  and code chunks. An \code{@} character (in column 1 of a line)
  indicates the beginning of a text chunk. \code{<<name of code chunk>>=}
  (starting at column 1 of a line) is a header line of a code chunk with
  a name defined by the text between \code{<<} and \code{>>=}.
  A code chunk is finished by the beginning of the next text chunk.
  Within the code chunk you can use other code chunks by referencing
  them by name ( for example by: \code{<<name of code chunk>>} ).
  In this way you can separate a big job in smaller ones.

  Special remarks: \code{tangleR} expands code chunk \code{start}
   if flag \code{expand.root.start} is TRUE. Code chunks will be surrounded
   by comment lines showing the number of the code chunk the code is
   coming from.
   If you want to use \code{<<} or \code{>>} in your code 
   it may be necessary to escape them by an \code{@}-sign. Then
   you have to type in: \code{@<<} or \code{@>>}.
}
\value{
  usually a file with R code is generated
}
\references{ \url{http://www.eecs.harvard.edu/~nr/noweb/intro.html} }
\author{Hans Peter Wolf}

\seealso{  \code{\link{weaveR}} }
\examples{
\dontrun{
## This example cannot be run by examples() but should be work in an interactive R session
  tangleR("testfile.rev")
}
"tangleR(\"testfile.rev\")"
## The function is currently defined as
function(in.file,out.file,expand.roots=NULL,expand.root.start=TRUE){
# german documentation of the code:
# look for file webR.pdf, P. Wolf 050204
...
  }
}
\keyword{file}
\keyword{programming}



