RF <- function(y1,y2,iter=5000){

  data.name <- paste("y1", " and ", "y2", sep = "")

  null.value  <- 0;
  attr(null.value, "names") <- "difference in means";
  alternative <- "two.sided";

  method <- paste("Robust Fiducial Based Test ", sep = "")


  n <- c(length(y1),length(y2))
  a <- length(n)
  muhat <- round(calc_estimates(y1,y2)$muhat_AMML,digits=4)
  sigmahat <- round(calc_estimates(y1,y2)$sigmahat_AMML,digits=4)
  estimate <- c(muhat,sigmahat)
  attr(estimate, "names") <- c("mean of y1","mean of y2","sd of y1","sd of y2");

  test_fid_obs <- (muhat[1]-muhat[2])^2
  M <- calc_estimates(y1,y2)$M

  RF_AMML=0;
  for(i in 1:iter){
    t=rt(a,n-1);
    test.fid1 <- ((rt(a,(n-1))*sigmahat)/sqrt(M))
    test.fid <- (test.fid1[1]-test.fid1[2])^2
    if(test.fid >=test_fid_obs){RF_AMML=RF_AMML+1}
  }

  pval_fid <- RF_AMML/iter;
  p.value  <- round(pval_fid,4)
  attr(p.value, "names") <- NULL


  list(p.value = round(pval_fid,digits=4), muhat_AMML=c(round(muhat,digits=4)), sigmahat_AMML=c(round(sigmahat,digits=4)))

  res.RF <- list(method = method, data.name = data.name,
                 null.value = null.value, alternative = alternative,
                 estimate = estimate, p.value = p.value);
  class(res.RF) <- "htest"
  return(res.RF)

}




