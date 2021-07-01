RF <- function(y1,y2,iter=5000){

  n <- c(length(y1),length(y2))
  a <- length(n)
  muhat <- calc_estimates(y1,y2)$muhat_AMML
  sigmahat <- calc_estimates(y1,y2)$sigmahat_AMML
  test_fid_obs <- (muhat[1]-muhat[2])^2
  M <- calc_estimates(y1,y2)$M

  RF_AMML=0;
  for(i in 1:iter){
    t=rt(a,n-1);
    test.fid1 <- ((rt(a,(n-1))*sigmahat)/sqrt(M))
    test.fid <- (test.fid1[1]-test.fid1[2])^2
    if(test.fid >=test_fid_obs){RF_AMML=RF_AMML+1}
  }

  pval_fid=RF_AMML/iter;
  list(p.value = round(pval_fid,digits=4), muhat_AMML=c(round(muhat,digits=4)), sigmahat_AMML=c(round(sigmahat,digits=4)))

}




