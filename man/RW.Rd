\name{RW}
\alias{RW}
\title{Robust Welch's Two Sample t-Test}
\description{
Performs robust Welch's (RW) two sample t-test based on AMML estimates. }
\usage{
RW(y1, y2)
}
\arguments{
  \item{y1}{numeric vector of sample 1}
  \item{y2}{numeric vector of sample 2}
}

\value{
  \item{statistic}{the value of the robust Welch's two sample t-test.}
  \item{df}{the degrees of freedom for the  robust Welch's two sample t-test.}
  \item{p.value}{the p-value for the  robust Welch's two sample t-test.}
  \item{muhat_AMML}{the AMML estimates of the location parameters.}
  \item{sigmahat_AMML}{the AMML estimates of the scale parameters.}
}

\references{Guven, G., Acitas, S., Samkar, H., Senoglu, B. (2021). RobustBF: An R Package for Robust Solution to the Behrens-Fisher Problem. RJournal (to be submitted).}
\author{Gamze Guven <gamzeguven@ogu.edu.tr>}


\examples{
y1 <- 2*rt(10,3)
y2 <- rt(10,3)
RW(y1, y2)
}
\keyword{file}
