RW <- function(y1,y2){
  n <- c(length(y1),length(y2))
  muhat <- calc_estimates(y1,y2)$muhat_AMML
  sigmahat <- calc_estimates(y1,y2)$sigmahat_AMML
  M <- calc_estimates(y1,y2)$M
  test.stat.WR_AMML <- (muhat[1]-muhat[2])/sqrt(sum(sigmahat^2/M))
  df.AMML <-((sum(sigmahat^2/M))^2)/(sum((1/(n-1))*(((sigmahat^2)/M)^2)))
  p.WR_AMML <- 2*min(pt( test.stat.WR_AMML, df.AMML, ncp=0, lower.tail = TRUE),pt(test.stat.WR_AMML, df.AMML, ncp=0, lower.tail = FALSE))

  list(statistic = round(test.stat.WR_AMML,digits=4), df = round(df.AMML,digits=4), p.value = round(p.WR_AMML,digits=4), muhat_AMML = round(muhat,digits=4), sigmahat_AMML = round(sigmahat,digits=4))

}




