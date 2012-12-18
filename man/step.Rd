\name{step}
\alias{step}
\title{ modified version of step for relax }
\description{Select a formula-based model by AIC.}
\usage{
step(object, scope, scale = 0, direction = c("both", "backward", "forward"), trace = 1, keep = NULL, steps = 1000, k = 2, ...)
}
\arguments{
  \item{object}{ model }
  \item{scope}{ range of model }
  \item{scale}{ used in the definiton of AIC statistic }
  \item{direction}{ mode of search }
  \item{trace}{ printing during running }
  \item{keep}{ filter function }
  \item{steps}{ max number of steps }
  \item{k}{ multiple of number of d.f. for penalty }
  \item{\dots}{ further arguments }
}
\details{ see help of step (package stats) }
\value{ stepwise-selected model is returned ... }
\seealso{ \code{\link{step}} }
\examples{ ## }
\keyword{ IO }

