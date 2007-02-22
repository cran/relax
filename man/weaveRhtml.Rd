\name{weaveRhtml}
\alias{weaveRhtml}
\title{ function to weave a rev-file to a html-file}
\description{
  \code{weaveRhtml} reads a file that is written according to
  the rules of the \code{noweb} system and performs a simple kind
  of weaving. As a result a html-file is generated.
}
\usage{
weaveRhtml(in.file,out.file,replace.german.umlaute=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{in.file}{ name of input file }
  \item{out.file}{ name of output file; if this argument is missing the extension of the
   input file is turned to \code{.html} }
  \item{replace.german.umlaute}{ if TRUE german umlaute will be replaced by 
   html sequences}
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

  Rweb speciality: A code chunk with a code chunk name containing  \code{"Rweb"} 
  as the first four characters will be translated to a text input field and
  a submit button \code{compute via Rweb}. 
  By pressing this button the code of the text field will be
  sent for evaluation to Rweb \code{http://rweb.stat.umn.edu/Rweb/} 
  and the results appears in a new browser window. 
  This link to Rweb has been inspirited by web pages like 
  \url{http://www.stat.umn.edu/geyer/3011/examp/reg.html} written
  by Charlie Geyer \url{http://www.stat.umn.edu/~charlie}.

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

  \code{weaveRhtml} expands the input by adding a simple html-header
  as well as some links for navigation.
  Chunk numbers are written in front of the code chunk headers.

  Further details:
  Some LaTeX macros are transformed to improve the html document.
  1. \code{weaveRhtml} looks for the LaTeX macros \code{\author}, 
  \code{\title} and \code{\date} at the beginning of the input text.
  If these macros are found their arguments are used to construct a simple
  html-head. 
  2. \code{\section\{...\}, \subsection\{...\}, \paragraph\{...\}} macros will be extracted
  to include some section titles, subsection titles, paragraph titles in bold face fonts.
  Additionally a simple table of contents is generated.
  3. Text lines between \code{\begin\{center\}} and \code{\end\{center\}}
  are centered.
  4. Text lines between \code{\begin\{quote\}} and \code{\end\{quote\}}
  are shifted a little bit to the right.
  5. Text lines between \code{\begin\{itemize\}} and \code{\end\{itemize\}}
  define a listing. The items of such a list have to begin with \code{\item}.
  6. \code{\emh\{xyz\}} is transformed to \code{<i>xyz</i>} -- \code{xyz} will appear italic.
  7. \code{\texttt\{xyz\}} is transformed to \code{<code>xyz</code>} -- this is formated like code.
}
\value{
  a html file is generated
}
\references{ \url{http://www.eecs.harvard.edu/~nr/noweb/intro.html} }
\author{Hans Peter Wolf}
\seealso{  \code{\link{weaveR}}, \code{\link{tangleR}} }
\examples{
\dontrun{
## This example cannot be run by examples() but should be work in an interactive R session
  weaveRhtml("testfile.rev","testfile.tex")
  weaveR("testfile.rev")
}
## The function is currently defined as
weaveRhtml<-function(in.file,out.file){
  # german documentation of the code:
  # look for file webR.pdf, P. Wolf 060910
  ...
}
}
\keyword{file}
\keyword{documentation}
\keyword{programming}


