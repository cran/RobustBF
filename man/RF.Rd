\name{RF}
\alias{RF}
\title{Robust Fiducial Based Test}
\description{Computes p-value for the robust fiducial based test in which AMML estimates are used.}
\usage{
RF(y1, y2, iter=5000)
}
\arguments{
  \item{y1}{numeric vector of sample 1}
  \item{y2}{numeric vector of sample 2}
  \item{iter}{the number of iterations for perfoming the RF test.}
}


\value{
  \item{p.value}{the p-value for the RF test.}
  \item{muhat_AMML}{the AMML estimates of the location parameters.}
  \item{sigmahat_AMML}{the AMML estimates of the scale parameters.}
}
\references{Guven, G., Acitas, S., Samkar, H., Senoglu, B. (2021). RobustBF: An R Package for Robust Solution to the Behrens-Fisher Problem. RJournal (to be submitted).}
\author{Gamze Guven <gamzeguven@ogu.edu.tr>}
\examples{
y1 <- 2*rt(10,3)
y2 <- rt(10,3)
RF(y1,y2,5000)
}
\keyword{file}
