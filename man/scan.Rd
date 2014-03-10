\name{scan}
\alias{scan}
\title{ modified version of scan for relax }
\description{ reads data into a vector or a list from console or file }
\usage{
scan(file = "", what = double(), nmax = -1L, n = -1L, sep = "", 
quote = if (identical(sep, "\n")) "" else "'\"", dec = ".", skip = 0L,
nlines = 0L, na.strings = "NA", flush = FALSE, fill = FALSE, 
strip.white = FALSE, quiet = FALSE, blank.lines.skip = TRUE, 
multi.line = TRUE, comment.char = "", allowEscapes = FALSE, fileEncoding = "", 
encoding = "unknown", text = NA)
}
\arguments{
  \item{file}{ file name }
  \item{what}{ type of data }
  \item{nmax}{ max number of data }
  \item{n}{ max number of data }
  \item{sep}{ separator }
  \item{quote}{ set of quoting strings }
  \item{dec}{ decimal point character }
  \item{skip}{ lines of input file to be skipped }
  \item{nlines}{ max number of lines }
  \item{na.strings}{ coding of NA entries }
  \item{flush}{ for ignoring comments }
  \item{fill}{ for adding empty fields }
  \item{strip.white}{ for ignoring leading or trailing blanks }
  \item{quiet}{ if FALSE number of items will be printed }
  \item{blank.lines.skip}{ ignore empty lines }
  \item{multi.line}{ if FALSE: record must be in lines } 
  \item{comment.char}{ character used as comment char }
  \item{allowEscapes}{ how to handle escape sequences }
  \item{fileEncoding}{ file encoding }
  \item{encoding}{ encoding assumed for the input }
  \item{text}{ concerning text connections }
}
\details{ see help of scan (package base) }
\value{ scanned object is returned ... }
\seealso{ \code{\link{scan}} }
\examples{ ## }
\keyword{ IO }
