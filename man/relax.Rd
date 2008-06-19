\name{relax}
\alias{relax}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ R editor: relax }
\description{
  \code{relax} is an editor for doing data analysis with R and writing reports.
}
\usage{
relax(file.name, no.plots = FALSE, cmds = "", but.Wizardry = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file.name}{name of report file to be loaded }
  \item{no.plots}{ if \code{TRUE} no plots are integrated into the report text field}
  \item{cmds}{ special operations that are evaluated on start }
  \item{but.Wizardry}{ if \code{FALSE} menu \code{Wizardry} is not created }
}
\details{
  \code{relax} is written in R and Tcl/Tk.
  \code{relax} creates a new window (top level Tcl/Tk widget) that consists of
  two text fields and some buttons and menus. Text (chunks) and code (chunks)
  are inserted in the upper text field (report field). Code chunks are evaluated by clicking
  on \code{EvalRCode}. Results are shown in the lower text field (output field) and will be
  transfered to the report field by pressing on \code{Insert}.

  Chunks are separated by separators:
  A line containing the character "@" at its first position indicades the beginning of a text chunk.
  Patterns like \code{<<*>>=} or \code{<<}code chunk name\code{>>=} define code chunk
  headers and introduce code chunks.
  The code of a code chunk is the set of R expression found between the header and the
  following text chunk.

  How to work with relax?
  \itemize{
   \item{Load of package \code{relax}}{in \code{R} through: \code{library(relax)}.}

   \item{Start the editor by:}{\code{relax()}.}

   \item{PlanRCode:}{Press \code{PlanRCode} and an empty text chunk and an empty code
   chunk without contents appear in the report field (upper blue text field).
   Now you can write down any text between
   "@"-line and the header of next code chunk \code{<<*>>=}. The code has to be placed
   after the header line.}

   \item{EvalRCode:}{\code{EvalRCode} evaluates the code (the cursor is pointing at)
   and the results appear in the output field just as in \code{R};
   warnings will be converted in errors and will stop the evaluation.}

   \item{WarnEval:}{\code{WarnEval} evaluates the code  (the cursor is pointing at)
   and the results appear in the output field just as in \code{R};
   however warnings will not stop the evaluation of the code.}

   \item{RemoveOut:}{\code{RemoveOut} clears the output field.}

   \item{Insert:}{\code{Insert} transfers the contents of the output text field
   to the upper text field as a new text chunk.}

   \item{SavePlot:}{\code{SavePlot} copies the graphics device; it creates a \code{postscript}
         as well as a \code{jpeg} file and inserts the LaTeX includegraphics reference
         in the report field as a new text chunk.}

   \item{Find:}{\code{Find} finds some text string in the working text and highlights 
         matches found.}

   \item{TrashOutput:}{\code{TrashOutput} removes inserted \code{R} output from the upper field
   (the cursor is pointing at). }

   \item{Down:}{\code{Down} moves the cursor down to the next code chunk.}

   \item{Up:}{\code{Up} moves the cursor up to the next code chunk.}

    \item{File:}{The \code{File} menu in the menu bar of \code{relax} allows you save the report
   -- the contents of the blue text field. The saving operation generates two
   files: a text file (extension: \code{rev}) and a \code{html} file.
   You can load an old report into the upper text field (report field)
   and continue to work on the report.}

    \item{Edit:}{Several operations to make editing faster and easier are collected in the \code{Edit} menu:
   searching, starting an external editor, etc.}

   \item{Options:}{The entries of the \code{Options} menu change some of the parameter of relax.}

    \item{Wizardry:}{With \code{Wizardry} you are able to generate LaTeX documents
    (for LaTeXperts only).
    After saving the raw report you can \code{web} it: this process generates a \code{tex}-file
    that can be formated by LaTeX. See your LaTeX companion for syntax.
    (The web syntax fits to the rules of \code{noweb}.
    Therefore, the raw files can also be processed by Norman Ramsey's
    \code{noweb} system (see: \cr \url{http://www.eecs.harvard.edu/~nr/noweb/}).}

    \item{Configuration:}{
    Some parameters of \code{relax} can be modified in \code{...relax/config/settings.relax}.}

    \item{Environment for evaluation:}{
    Evaluations of code chunks take place in the environment (\code{revive.env}).}

    \item{cmds:}{
    The character vector \code{cmds} in environment (\code{revive.env}) enables you to
    define operations of your own. For example, \code{cmds<-"r test1.rev"} forces \code{relax}
    to load file \code{test1.rev}. \code{cmds<-c("> 1+1","> 2+2")} results in
    computing 1+1 and then 2+2. \code{cmds<-"s10"} evaluates code chunk number 10.}
   }
}
\value{
   --
}
\references{\url{http://www.wiwi.uni-bielefeld.de/~wolf/software/relax/relax.html}}
\author{Hans Peter Wolf}
\note{}

\seealso{  }
\examples{

## The function is currently defined as
function (workname.sys = "out.rev", no.plots = FALSE, cmds = "", but.Wizardry = TRUE){
      # nearly 5000 lines of R / Tcl/Tk code
}
}
\keyword{ documentation }% at least one, from doc/KEYWORDS

