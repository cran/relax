\name{weaveR}
\alias{weaveR}
\title{ function to weave a file }
\description{
  \code{weaveR} reads a file that is written according to
  the rules of the \code{noweb} system and performs a simple kind
  of weaving. As a result a LaTeX file is generated.
}
\usage{
weaveR(in.file,out.file,show.code=TRUE,show.text=TRUE,replace.umlaute=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{in.file}{ name of input file }
  \item{out.file}{ name of output file; if missing the extension of the
  input file is turned to \code{.tex} }
  \item{show.code}{ if FALSE the code will not be copied to the output file }
  \item{show.text}{ if FALSE the text will not be copied to the output file }
  \item{replace.umlaute}{ if TRUE german umlaute will be replaced by 
   TeX sequences}
}
\details{
  General remarks: A \code{noweb} file consists of a mixture of text
  and code chunks. An \code{@} character (in column 1 of a line)
  indicates the beginning of a text chunk. \code{<<name of code chunk>>=}
  (starting at column 1 of a line) is a header line of a code chunk with
  a name defined by the text between \code{<<} and \code{>>=}.
  A code chunk is finished by the beginning of hte next text chunk.
  Within the code chunk you are allowed to use other code chunks by referencing
  them by name ( for example by: \code{<<name of code chunk>>} ).
  In this way you can separate a big job in smaller ones.

  Technical remarks:
  To format small pieces of code in text chunks you have to put them in
  \code{[[...]]}-brackets: \code{text text [[code]] text text}.
  One occurence of such a code in a text line is assumed to work always.
  If an error emerges caused by formatting code in a text chunk
  simplify the line by splitting it.
  Sometimes you want to use
  \code{[[}- or even \code{<<}-characters in your text. Then it
  may be necessary to escape them by an \code{@}-sign and
  you have to type in: \code{@<<}, \code{@[[} and so on.

  \code{weaveR} expands the input by adding some latex macros
  to typeset code by a typewriter font.
  Furthermore chunk numbers are appended to code chunk headers.
  The number of the last code chunk is stored in LaTeX-counter \code{Rchunkno}.
  After defining
  \code{\newcommand\{\chunklabel\}[1]\{\newcounter\{#1\}\setcounter\{#1\}\{\value\{Rchunkno\} \} \}}
  and \code{\newcommand\{\chunkref\}[1]\{\arabic\{#1\} \} } you can label a code chunk
  by \code{\chunklabel\{xyzname\} } and reference it by \code{\chunkref\{xyzname\} }.
}
\value{
  a latex file is generated
}
\references{ \url{http://www.eecs.harvard.edu/~nr/noweb/intro.html} }
\author{Hans Peter Wolf}
\seealso{  \code{\link{tangleR}} }
\examples{
\dontrun{
## This example cannot be run by examples() but should be work in an interactive R session
  weaveR("testfile.rev","testfile.tex")
  weaveR("testfile.rev")
}
## The function is currently defined as
weaveR<-function(in.file,out.file){
  # german documentation of the code:
  # look for file webR.pdf, P. Wolf 050204
  ...
}
}
\keyword{file}
\keyword{documentation}
\keyword{programming}

