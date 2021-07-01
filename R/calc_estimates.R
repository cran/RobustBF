calc_estimates <- function(y1,y2){
  n <- c(length(y1),length(y2))
  bar_y=c(mean(y1),mean(y2))
  sd_LS=c(sd(y1),sd(y2))
  N=sum(n);
  T0 <- c(median(y1),median(y2))
  S0 <- c(1.483*median(abs(y1-T0[1])), 1.483*median(abs(y2-T0[2])))
  t_ij <- c((y1-T0[1])/S0[1],(y2-T0[2])/S0[2])
  b_ij <- c(1+((1/30)*(t_ij[1:n[1]])^2), 1+((1/30)*(t_ij[n[1]+1:n[2]])^2))
  c_ij <- c((b_ij[1:n[1]])^2, (b_ij[n[1]+1:n[2]])^2)
  beta_ij <- c(1/c_ij[1:n[1]], 1/c_ij[n[1]+1:n[2]])
  alfa_ij <- c((1/30)*(t_ij[1:n[1]]/c_ij[1:n[1]]), (1/30)*(t_ij[n[1]+1:n[2]]/ c_ij[n[1]+1:n[2]]))
  f <- c(sum(beta_ij[1:n[1]]*y1), sum(beta_ij[n[1]+1:n[2]]*y2))
  m <-c(sum(beta_ij[1:n[1]]),sum(beta_ij[n[1]+1:n[2]]))
  muhat <- f/m
  B <- c( (2*16.5/30)*sum((y1-muhat[1])*alfa_ij[1:n[1]]), (2*16.5/30)*sum((y2-muhat[2])*alfa_ij[n[1]+1:n[2]]))
  C <- c((2*16.5/30)*sum(((y1-muhat[1])^2)*beta_ij[1:n[1]]), (2*16.5/30)*sum(((y2-muhat[2])^2)*beta_ij[n[1]+1:n[2]]))
  sigmahat <- c(((B[1]+sqrt(B[1]^2+4*n[1]*C[1]))/(2*sqrt(n[1]*(n[1]-1)))), ((B[2]+sqrt(B[2]^2+4*n[2]*C[2]))/(2*sqrt(n[2]*(n[2]-1)))))
  M <- c((2*16.5*m[1])/30, (2*16.5*m[2])/30)
  T0 <- muhat
  S0 <- sigmahat
  t_ij <- c((y1-T0[1])/S0[1],(y2-T0[2])/S0[2])
  b_ij <- c(1+((1/30)*(t_ij[1:n[1]])^2), 1+((1/30)*(t_ij[n[1]+1:n[2]])^2))
  c_ij <- c((b_ij[1:n[1]])^2, (b_ij[n[1]+1:n[2]])^2)
  beta_ij <- c(1/c_ij[1:n[1]], 1/c_ij[n[1]+1:n[2]])
  alfa_ij <- c((1/30)*(t_ij[1:n[1]]/c_ij[1:n[1]]), (1/30)*(t_ij[n[1]+1:n[2]]/ c_ij[n[1]+1:n[2]]))
  f <- c(sum(beta_ij[1:n[1]]*y1), sum(beta_ij[n[1]+1:n[2]]*y2))
  m <-c(sum(beta_ij[1:n[1]]),sum(beta_ij[n[1]+1:n[2]]))
  muhat <- f/m
  B <- c((2*16.5/30)*sum((y1-muhat[1])*alfa_ij[1:n[1]]), (2*16.5/30)*sum((y2-muhat[2])*alfa_ij[n[1]+1:n[2]]))
  C <- c((2*16.5/30)*sum(((y1-muhat[1])^2)*beta_ij[1:n[1]]), (2*16.5/30)*sum(((y2-muhat[2])^2)*beta_ij[n[1]+1:n[2]]))
  sigmahat <- c(((B[1]+sqrt(B[1]^2+4*n[1]*C[1]))/(2*sqrt(n[1]*(n[1]-1)))), ((B[2]+sqrt(B[2]^2+4*n[2]*C[2]))/(2*sqrt(n[2]*(n[2]-1)))))
  M <- c((2*16.5*m[1])/30, (2*16.5*m[2])/30)

  #list(muhat_AMML=muhat,y_bar_LS=bar_y,sigmahat_AMML=sigmahat,sd_LS=sd_LS,bigM=M)
  list(muhat_AMML=muhat, sigmahat_AMML=sigmahat, M = M)
  }
