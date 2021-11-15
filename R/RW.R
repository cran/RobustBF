RW <- function(y1,y2){
  n <- c(length(y1),length(y2))
  #data.name <- c(deparse(substitute(y1)), deparse(substitute(y2)))
  data.name <- paste("y1", " and ", "y2", sep = "")

  null.value  <- 0;
  attr(null.value, "names") <- "difference between in means";
  alternative <- "two.sided";



  muhat <- round(calc_estimates(y1,y2)$muhat_AMML,digits=4)
  sigmahat <- round(calc_estimates(y1,y2)$sigmahat_AMML,digits=4)
  estimate <- c(muhat,sigmahat)
  attr(estimate, "names") <- c("mean of y1","mean of y2","sd of y1","sd of y2");

  M <- calc_estimates(y1,y2)$M
  test.stat.WR_AMML <- (muhat[1]-muhat[2])/sqrt(sum(sigmahat^2/M))
  statistic <- round(test.stat.WR_AMML,digits=4)
  attr(statistic, "names") <- "RW";

  df.AMML <-((sum(sigmahat^2/M))^2)/(sum((1/(n-1))*(((sigmahat^2)/M)^2)))
  parameter <- round(df.AMML,digits=4)
  attr(parameter, "names") <- "df"

  p.WR_AMML <- 2*min(pt( test.stat.WR_AMML, df.AMML, ncp=0, lower.tail = TRUE),pt(test.stat.WR_AMML, df.AMML, ncp=0, lower.tail = FALSE))
  p.value  <- round(p.WR_AMML, digits = 4)
  attr(p.value, "names") <- NULL

  method <- paste("Robust Welch's Two Sample t-Test ", sep = "")

  # res.RW <- list(statistic = round(test.stat.WR_AMML,digits=4),
  #             df = round(df.AMML,digits=4),
  #             p.value = round(p.WR_AMML,digits=4),
  #             c(muhat_AMML=muhat, sigmahat_AMML=sigmahat), null.value = mu,
  #             method = method, data.name = data.name )

  res.RW <- list(method = method, data.name = data.name,
       null.value = null.value, alternative = alternative, parameter = parameter,
       estimate = estimate, statistic = statistic, p.value = p.value);
  class(res.RW) <- "htest"
  return(res.RW)
}




