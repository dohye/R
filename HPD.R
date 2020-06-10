# HPD 신뢰구간

# grid method
HPDgrid = function(prob, level=0.95){
  prob.sort=sort(prob, decreasing=T)
  M=min(which(cumsum(prob.sort)>=level))
  height = prob.sort[M]
  HPD.index=which(prob>=height)
  HPD.level=sum(prob[HPD.index])
  res=list(index=HPD.index, level=HPD.level)
  return(res)
}

N=1001; level=0.95
theta=seq(-3,3,length=N)
prob=exp(-0.5/0.25*(theta-0.3)^2) # likelihood
prob=prob/sum(prob)

HPD=HPDgrid(prob,level)
HPDgrid.hat = c(min(theta[HPD$index]), max(theta[HPD$index]))
plot(theta, prob,type="l")
abline(v=HPDgrid.hat, lty=2, col="blue")
HPD$level


# sample method
HPDsample = function(th, level=.95){
  N = length(th)
  theta.sort = sort(th)
  M = ceiling(N*level)
  nCI = N-M+1
  CI.width = theta.sort[1:nCI+M-1]-theta.sort[1:nCI]
  HPD.index = which.min(CI.width)
  HPD = c(theta.sort[HPD.index], theta.sort[HPD.index+M-1])
  return(HPD)
}

N = 10000; level=0.95
theta = rnorm(N, 0.3, 0.5) #posterior
HPDsample.hat = HPDsample(theta,level)

th = seq(-3,3,length=N)
post = dnorm(th, 0.3,0.5)

plot(th, post, type="l")
abline(v=HPDsample.hat, lty=2)
