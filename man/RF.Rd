\name{RF}
\alias{RF}
\title{Robust Fiducial Based Test}
\description{Computes p-value for the robust fiducial (RF) based test for the equality of means of two long-tailed symmetric (LTS) distributions when the variances are not equal.
}
\usage{
RF(y1, y2, iter=5000)
}
\arguments{
  \item{y1}{numeric vector of sample 1}
  \item{y2}{numeric vector of sample 2}
  \item{iter}{the number of iterations for perfoming the RF test.}
}


\details{
RF test based on adaptive modified maximum likelihood (AMML) estimators (Tiku and Surucu, 2009; Donmez, 2010) is proposed using the fiducial model which is a special case of functional model given by Dawid and Stone (1982), see also Fisher (1933, 1935) for more information about the fiducial approach. It is one of the alternatives of Welch's t test (Welch, 1938) and its p-value is based on the iteration number. For further details, see Guven et al. (2021).
}


\value{
A list with class "\code{htest}" containing the following components:
  \item{p.value}{the p-value for the RF test.}
  \item{estimate}{the AMML estimates of the location and scale parameters.}
  \item{null.value}{the specified hypothesized value of the mean difference.}
  \item{alternative}{a character string describing the alternative hypothesis.}
  \item{method}{a character string indicating which test is used.}
  \item{data.name}{a character string giving the name(s) of the data.}
}



\references{
Dawid, A. P. and Stone, M. (1982). The functional-model basis of fiducial inference. The Annals of Statistics, 10(4):1054-1067.

Fisher, R. A. (1933). The concepts of inverse probability and fiducial probability referring to unknown parameters. Proceedings of Royal Society of London. Series A, 139(838):343-348

Fisher, R. A. (1935). The fiducial argument in statistical inference. Annals of eugenics, 6(4):391-398

Guven, G., Acitas, S., Samkar, H., Senoglu, B. (2021). RobustBF: An R Package for Robust Solution to the Behrens-Fisher Problem. RJournal (submitted).

Tiku, M. L. and Surucu, B. (2009). MMLEs are as good as M-estimators or better. Statistics & probability letters, 79(7):984-989.

Welch, B.L. (1938). The significance of the difference between two means when the population variances are unequal. Biometrika, 29(3/4):350–362.}

\author{Gamze Guven <gamzeguven@ogu.edu.tr>}

\examples{
# The following two samples (y1 and y2)
# come from LTS distributions with
# heterogeneous variances

y1 <- c(0.55, 1.39, 2.01, 0.41, 0.32, -0.31, -1.06, -0.84,
        1.02, 0.02, -0.96, 0.18, 0.49, 0.03, 0.77,  0.02,
        0.56,  0.46, -0.65, -0.27)
y2 <- c(7.25, 7.98, -0.24, 8.93, -0.16, 32.28,  3.81,
        2.32, 14.73, 6.27, 8.07,  7.24,  7.18,  3.75, 11.48,
        6.46, 1.01, 5.35, -0.34,  4.34)

# RF test
RF(y1, y2,5000)

}


\keyword{file}
