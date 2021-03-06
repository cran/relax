\name{weaveR}
\alias{weaveR}
\title{ function to weave a file }
\description{
  \code{weaveR} reads a file that is written according to
  the rules of the \code{noweb} system and performs a simple kind
  of weaving. As a result a LaTeX file is generated.
}
\usage{
weaveR(in.file,out.file,show.code=TRUE,show.text=TRUE,
       replace.umlaute=TRUE,eval_Sexpr=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{in.file}{ name of input file }
  \item{out.file}{ name of output file; if missing the extension of the
  input file is turned to \code{.tex} }
  \item{show.code}{ if FALSE the code will not be copied to the output file; 
    if show.code == "echo": a code chunk will be excluded if its the name 
    additionally contains the string  \code{echo=FALSE}. }
  \item{show.text}{ if FALSE the text will not be copied to the output file }
  \item{replace.umlaute}{ if TRUE german umlaute will be replaced by 
   TeX sequences}
  \item{eval_Sexpr}{ if TRUE \code{\\Sexpr}-expressions and 
       \verb{\\Splot}-expressions will be evaluated and replaced by the result(s) }
}
\details{
  General remarks: A \verb{noweb} file consists of a mixture of text
  and code chunks. An \verb{@} character (in column 1 of a line)
  indicates the beginning of a text chunk. \verb{<<name of code chunk>>=}
  (starting at column 1 of a line) is a header line of a code chunk with
  a name defined by the text between \verb{<<} and \verb{>>=}.
  A code chunk is finished by the beginning of hte next text chunk.
  Within the code chunk you are allowed to use other code chunks by referencing
  them by name ( for example by: \verb{<<name of code chunk>>} ).
  In this way you can separate a big job in smaller ones.

  Technical remarks:
  To format small pieces of code in text chunks you have to put them in
  \verb{[[...]]}-brackets: \verb{text text [[code]] text text}.
  One occurence of such a code in a text line is assumed to work always.
  If an error emerges caused by formatting code in a text chunk
  simplify the line by splitting it.
  Sometimes you want to use
  \verb{[[}- or even \verb{<<}-characters in your text. Then it
  may be necessary to escape them by an \verb{@}-sign and
  you have to type in: \verb{@<<}, \verb{@[[} and so on.

  \code{weaveR} expands the input by adding some latex macros
  to typeset code by a typewriter font.
  Furthermore chunk numbers are appended to code chunk headers
  together with some information showing where are code is used.
  If you want to suppress these hints you can include a line
  beginning with the string \code{no.used.in.infos}.

  Hint for references:
  The number of the last code chunk is stored in LaTeX-counter \code{Rchunkno}.
  After defining
  \verb{\\newcommand\{\\chunklabel\}[1]\{\\newcounter\{#1\}}
  \verb{  \\setcounter\{#1\}\{\\value\{Rchunkno\} \} \}}
  and \verb{\\newcommand\{\\chunkref\}[1]\{\\arabic\{#1\} \} } you can label a code chunk
  by \verb{\\chunklabel\{xyzname\} } and reference it by \verb{\\chunkref\{xyzname\} }.

  Chunk Index: 
  The user will get a list of all the code chunks if a line is found containing
  \verb{@list.of.chunks}. However, there must be a minimum number of 
  two list entries. Otherwise no index is appended to the text.

  Object Index: 
  An index of the objects 
  will be generated if a line is found with the string \verb{@index.of.objects}.
  Object names consisting of one character are excluded from the search. 
  However, you can define object names that should appear in the index by hand.
  This requires a line (or lines) in the document beginning with the string \verb{@index}
  followed by the names of the objects that should appear in the index. 
  This features is helpful if some objects are missing on the
  list or if a name of an object to be listed is of length 1 only.
  Bear in mind that the computation of the object index is time consuming.
  If you do not want an index computed automatically you can include a line
  with the string \verb{@index.reduced} at the beginning of the line. Then the
  object index will contain the object names of the \verb{@index} statements only.

  In case \verb{\\Sexpr} statements should be evaluated the text is scanned and 
  all \verb{\\Sexpr} statements are extracted and replaced by the values of the expressions.
  Furthermore, all \verb{\\Splot} statements are also searched. For each match
  the current plot of the current graphics device is copied into a postscript file
  and an \verb{\\includegraphics} LaTeX command is printed into the tex file.  
  Within the brackets of \verb{\\Splot} the file name of the postscript file
  has to be specified. Separated by "," the height of the plot may be set. E.g.
  \verb{\\Splot\{pic1.ps,latexheight="5cm"\}} means that the name of 
  the plot should be \verb{pic1.ps} and the height of it should be 5 cm. 
  In the same way arguments other arguments to be delivered to the postscript 
  function may be added.
}
\value{
  a latex file is generated
}
\references{ by using noweb is an alternative approach, see: 
            \url{http://www.eecs.harvard.edu/~nr/noweb/intro.html} }
\author{Hans Peter Wolf}
\seealso{  \code{\link{tangleR}} }
\examples{
\dontrun{
## This example cannot be run by examples() but should be work 
## in an interactive R session
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

