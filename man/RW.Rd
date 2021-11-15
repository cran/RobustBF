\name{RW}
\alias{RW}
\title{Robust Welch's Two Sample t-Test}
\description{
Performs robust Welch's (RW) two sample t-test to test the equality of means of two long-tailed symmetric (LTS) distributions when the variances are not equal.}
\usage{
RW(y1, y2)
}
\arguments{
  \item{y1}{numeric vector of sample 1}
  \item{y2}{numeric vector of sample 2}
}

\details{
RW test based on adaptive modifed maximum likelihood (AMML) estimators is proposed as a robust alternative to Welch's t-test (Welch, 1938). The test statistic is formulated as follows
\deqn{RW = \frac{({\hat{\mu}_{1}}-{\hat{\mu}_{2}})-(\mu_1-\mu_2)}{\sqrt{(\hat{\sigma}_{1}^2/M_1)+
(\hat{\sigma}_{2}^2/M_2)}}.}

where \eqn{\hat{\mu}_{i}} and \eqn{\hat{\sigma}_{i}} are the AMML estimators of the location and scale parameters (\eqn{i,=1,2}), see e.g. Tiku and Surucu (2009), Donmez (2010).

The null distribution of RW is approximately distributed as Student's t with degrees of freedom
\deqn{df = \frac{((\hat{\sigma}_{1}^2/M_1)+(\hat {\sigma}_{2}^2/M_2))^2}{{(\hat{\sigma}_{1}^2/M_1)^2/(n_1-1)}+{(\hat{\sigma}_{2}^2/M_2)^2/(n_2-1)}}.}
For further details, see  Guven et al. (2021)
}

\value{
A list with class "\code{htest}" containing the following components:
  \item{statistic}{the value of the robust Welch's two sample t-test.}
  \item{parameter}{the degrees of freedom for the  robust Welch's two sample t-test.}
  \item{p.value}{the p-value for the  robust Welch's two sample t-test.}
  \item{estimate}{the AMML estimates of the location and scale parameters.}
  \item{null.value}{the specified hypothesized value of the mean difference.}
  \item{alternative}{a character string describing the alternative hypothesis.}
  \item{method}{a character string indicating which test is used.}
  \item{data.name}{a character string giving the name(s) of the data.}
}

\references{
Donmez, A. (2010). Adaptive estimation and hypothesis testing methods [dissertation]. Ankara:METU.

Guven, G., Acitas, S., Samkar, H., Senoglu, B. (2021). RobustBF: An R Package for Robust Solution to the Behrens-Fisher Problem. RJournal (submitted).

Tiku, M. L. and Surucu, B. (2009). MMLEs are as good as M-estimators or better. Statistics & probability letters, 79(7):984-989.

Welch, B.L. (1938). The significance of the difference between two means when the population variances are unequal. Biometrika, 29(3/4):350–362.


}

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

# RW test
RW(y1, y2)


}
\keyword{file}
