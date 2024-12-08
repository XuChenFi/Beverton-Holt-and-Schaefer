#all processes were conducted in R2jags package
library(R2jags)

nyr<-length(ct)
N<-length(SSB)
jags.data<- c('ct','btj', "SSB","N",'nyr', 'prior.k', 'startbio', 'q.priorj',"Q.priorj","init.Q",'init.q','init.h','init.k','pen.bk','pen.F','b.yrs','bk.beta','CV.C','CV.cpue','nbks')

jags.save.params <- c('h','k','q',"Q",'P','ct.jags','cpuem', "cpuemm",'proc.logB','B','F')#if run the Schsefer function, use r to replace h. And x is added in BHDPF2.

j.inits <- function(){list("h"=rlnorm(1, mean=log(init.h), sd=0.17), "k"=rlnorm(1, mean=log(init.k), sd=0.2),"q"=rlnorm(1,mean=log(init.q),sd=0.2),"Q"=rlnorm(1,mean=log(init.Q),sd=0.2),"itau2"=1000,"isigma2"=1000)}
#if run the Schsefer function, use r and init.r to replace h and init.h.

#Schaefer function
Model = "model{
eps<-0.01
for(t in 1:nyr){
ct.jags[t] ~ dlnorm(log(ct[t]),pow(CV.C,-2))
}
penm[1]  <- 0 
Pmean[1] <- log(alpha)
P[1]     ~ dlnorm(Pmean[1],itau2)
for (t in 2:nyr) {
Pmean[t] <- ifelse(P[t-1] > 0.25,
log(max(P[t-1] + r*P[t-1]*(1-P[t-1]) - ct.jags[t-1]/k,eps)),  
log(max(P[t-1] + 4*P[t-1]*r*P[t-1]*(1-P[t-1]) - ct.jags[t-1]/k,eps))) 
P[t]     ~ dlnorm(Pmean[t],itau2) 
penm[t]  <- ifelse(P[t]<(eps+0.001),log((q+Q)/2*k*P[t])-log((q+Q)/2*k*(eps+0.001)),
# ifelse(P[t]>1,ifelse((ct[t]/max(ct))>0.2,log((q+Q)/2*k*P[t])-log((q+Q)/2*k*(0.99)),0),0)) 
ifelse(P[t]>1.1,log((q+Q)/2*k*P[t])-log((q+Q)/2*k*(0.99)),0))
}
for(t in 1:nyr){
proc.logB[t] <- log(P[t]*k)-log(exp(Pmean[t])*k)}
for(i in 1:nbks){
bk.mu[i] ~ dbeta(bk.beta[1,i],bk.beta[2,i])
bk.beta[3,i] ~ dnorm(bk.mu[i]-P[b.yrs[i]],10000)
}
for (t in 1:nyr){
Fpen[t]   <- ifelse(ct[t]>(0.9*k*P[t]),ct[t]-(0.9*k*P[t]),0) # Penalty term on F > 1, i.e. ct>B
pen.F[t]  ~ dnorm(Fpen[t],1000)
pen.bk[t] ~ dnorm(penm[t],10000)
cpuem[t]  <- log(q*P[t]*k);
btj[t]     ~ dlnorm(cpuem[t],pow(sigma2,-1));
}
for(t in 1:N){
cpuemm[t]<-log(Q*P[t]*k)
SSB[t]~dlnorm(cpuemm[t], pow(sigma2,-1))
}#we used two data to represent the biomass index, therefore there are two catchabilities need to be estimated
# priors
log.alpha               <- log((startbio[1]+startbio[2])/2) # needed for fit of first biomass
sd.log.alpha            <- (log.alpha-log(startbio[1]))/4
tau.log.alpha           <- pow(sd.log.alpha,-2)
alpha                   ~  dlnorm(log.alpha,tau.log.alpha)
log.qm              <- mean(log(q.priorj))
sd.log.q            <- (log.qm-log(q.priorj[1]))/2
tau.log.q           <- pow(sd.log.q,-2)
q                   ~  dlnorm(log.qm,tau.log.q)
log.Qm              <- mean(log(Q.priorj))
sd.log.Q            <- (log.Qm-log(Q.priorj[1]))/2
tau.log.Q           <- pow(sd.log.Q,-2)
Q                   ~  dlnorm(log.Qm,tau.log.Q)
itau2 ~ dgamma(4,0.01)
tau2  <- 1/itau2
tau   <- pow(tau2,0.5)
isigma2 ~ dgamma(2,0.01)
sigma2 <- 1/isigma2+pow(CV.cpue,2) # Add minimum realistic CPUE CV
sigma  <- pow(sigma2,0.5)
#we give up considering the correlation between r and k for creating a similar scenario for BHDPF
log.rm              <- log(init.r)
sd.log.r            <- 0.17
tau.log.r           <- pow(sd.log.r,-2)
r~dlnorm(log.rm, tau.log.r)
log.km              <- log(mean(prior.k))
sd.log.k            <- 0.2
tau.log.k           <- pow(sd.log.k,-2)
k~dlnorm(log.km, tau.log.k)
for (t in 1:nyr){
B[t] <- P[t]*k # biomass
F[t] <- ct.jags[t]/B[t]
}
}"

#BHDPF
Model = "model{
eps<-0.01
m<-0.9#setting natural mortality
for(t in 1:nyr){
ct.jags[t] ~ dlnorm(log(ct[t]),pow(CV.C,-2))
}
penm[1]  <- 0
Pmean[1] <- log(alpha)
P[1]     ~ dlnorm(Pmean[1],itau2)
for (t in 2:nyr) {
Pmean[t] <- log(max(P[t-1]*(1-m) + h*P[t-1]*4*m/(1+P[t-1]*(h-0.2)/(0.2*k-0.2*h*k)*k) - ct.jags[t-1]/k,eps))#The equation of the BHDPF
P[t]     ~ dlnorm(Pmean[t],itau2)
penm[t]  <- ifelse(P[t]<(eps+0.001),log((q+Q)/2*k*P[t])-log((q+Q)/2*k*(eps+0.001)),
# ifelse(P[t]>1,ifelse((ct[t]/max(ct))>0.2,log((q+Q)/2*k*P[t])-log((q+Q)/2*k*(0.99)),0),0))
ifelse(P[t]>1.1,log((q+Q)/2*k*P[t])-log((q+Q)/2*k*(0.99)),0))

}
for(t in 1:nyr){
proc.logB[t] <- log(P[t]*k)-log(exp(Pmean[t])*k)}
for(i in 1:nbks){
bk.mu[i] ~ dbeta(bk.beta[1,i],bk.beta[2,i])
bk.beta[3,i] ~ dnorm(bk.mu[i]-P[b.yrs[i]],10000)
}
for (t in 1:nyr){
Fpen[t]   <- ifelse(ct[t]>(0.9*k*P[t]),ct[t]-(0.9*k*P[t]),0) # Penalty term on F > 1, i.e. ct>B
pen.F[t]  ~ dnorm(Fpen[t],1000)
pen.bk[t] ~ dnorm(penm[t],10000)
cpuem[t]  <- log(q*P[t]*k);
btj[t]     ~ dlnorm(cpuem[t],pow(sigma2,-1));
}
for(t in 1:N){
cpuemm[t]<-log(Q*P[t]*k)
SSB[t]~dlnorm(cpuemm[t], pow(sigma2,-1))
}
# priors
log.alpha               <- log((startbio[1]+startbio[2])/2) # needed for fit of first biomass
sd.log.alpha            <- (log.alpha-log(startbio[1]))/4
tau.log.alpha           <- pow(sd.log.alpha,-2)
alpha                   ~  dlnorm(log.alpha,tau.log.alpha)
log.qm              <- mean(log(q.priorj))
sd.log.q            <- (log.qm-log(q.priorj[1]))/2
tau.log.q           <- pow(sd.log.q,-2)
q                   ~  dlnorm(log.qm,tau.log.q)
log.Qm              <- mean(log(Q.priorj))
sd.log.Q            <- (log.Qm-log(Q.priorj[1]))/2
tau.log.Q           <- pow(sd.log.Q,-2)
Q                   ~  dlnorm(log.Qm,tau.log.Q)
itau2 ~ dgamma(4,0.01)
tau2  <- 1/itau2
tau   <- pow(tau2,0.5)
isigma2 ~ dgamma(2,0.01)
sigma2 <- 1/isigma2+pow(CV.cpue,2)
sigma  <- pow(sigma2,0.5)
log.hm              <- log(init.h)
sd.log.h            <- 0.17
tau.log.h           <- pow(sd.log.h,-2)
h~dlnorm(log.hm, tau.log.h)
# bias-correct lognormal for k
log.km              <- log(mean(prior.k))
sd.log.k            <- 0.2
tau.log.k           <- pow(sd.log.k,-2)
k~dlnorm(log.km, tau.log.k)
for (t in 1:nyr){
B[t] <- P[t]*k # biomass
F[t] <- ct.jags[t]/B[t]
}
}"

#BHDPF2
Model = "model{
eps<-0.01
m<-0.9
for(t in 1:nyr){
ct.jags[t] ~ dlnorm(log(ct[t]),pow(CV.C,-2))
}
penm[1]  <- 0 
Pmean[1] <- log(alpha)
P[1]     ~ dlnorm(Pmean[1],itau2)
xmean[1]=0
sd<-pow(0.04, -2)
x[1]~dlnorm(xmean[1], sd)
for(i in 2:6){xmean[i]~dunif(0.95, 1.2)}
for(i in 7:8){xmean[i]~dunif(0.8, 1)}
for(i in 9:18){xmean[i]~dunif(0.6, 0.8)}
for(i in 19:25){xmean[i]~dunif(0.8, 1)}
for(i in 26:28){xmean[i]~dunif(0.6, 0.8)}
for(i in 29:34){xmean[i]~dunif(0.8, 1)}
for (t in 2:nyr) {
Pmean[t] <- log(max(P[t-1]*(1-x[t]*m) + h*P[t-1]*4*m/(1+P[t-1]*(h-0.2)/(0.2*k-0.2*h*k)*k) - ct.jags[t-1]/k,eps))
x[t]~dlnorm(log(xmean[t]), pow(0.1, -2))
P[t]     ~ dlnorm(Pmean[t],itau2) # Introduce process error
penm[t]  <- ifelse(P[t]<(eps+0.001),log((q+Q)/2*k*P[t])-log((q+Q)/2*k*(eps+0.001)),
# ifelse(P[t]>1,ifelse((ct[t]/max(ct))>0.2,log((q+Q)/2*k*P[t])-log((q+Q)/2*k*(0.99)),0),0))
ifelse(P[t]>1.1,log((q+Q)/2*k*P[t])-log((q+Q)/2*k*(0.99)),0))

}
for(t in 1:nyr){
proc.logB[t] <- log(P[t]*k)-log(exp(Pmean[t])*k)}
for(i in 1:nbks){
bk.mu[i] ~ dbeta(bk.beta[1,i],bk.beta[2,i])
bk.beta[3,i] ~ dnorm(bk.mu[i]-P[b.yrs[i]],10000)
}
for (t in 1:nyr){
Fpen[t]   <- ifelse(ct[t]>(0.9*k*P[t]),ct[t]-(0.9*k*P[t]),0) # Penalty term on F > 1, i.e. ct>B
pen.F[t]  ~ dnorm(Fpen[t],1000)
pen.bk[t] ~ dnorm(penm[t],10000)
cpuem[t]  <- log(q*P[t]*k);
btj[t]     ~ dlnorm(cpuem[t],pow(sigma2,-1));
}
for(t in 1:N){
cpuemm[t]<-log(Q*P[t]*k)
SSB[t]~dlnorm(cpuemm[t], pow(sigma2,-1))
}
# priors
log.alpha               <- log((startbio[1]+startbio[2])/2) # needed for fit of first biomass
sd.log.alpha            <- (log.alpha-log(startbio[1]))/4
tau.log.alpha           <- pow(sd.log.alpha,-2)
alpha                   ~  dlnorm(log.alpha,tau.log.alpha)
log.qm              <- mean(log(q.priorj))
sd.log.q            <- (log.qm-log(q.priorj[1]))/2
tau.log.q           <- pow(sd.log.q,-2)
q                   ~  dlnorm(log.qm,tau.log.q)
log.Qm              <- mean(log(Q.priorj))
sd.log.Q            <- (log.Qm-log(Q.priorj[1]))/2
tau.log.Q           <- pow(sd.log.Q,-2)
Q                   ~  dlnorm(log.Qm,tau.log.Q)
itau2 ~ dgamma(4,0.01)
tau2  <- 1/itau2
tau   <- pow(tau2,0.5)
isigma2 ~ dgamma(2,0.01)
sigma2 <- 1/isigma2+pow(CV.cpue,2) # Add minimum realistic CPUE CV
sigma  <- pow(sigma2,0.5)
log.hm              <- log(init.h)
sd.log.h            <- 0.17
tau.log.h           <- pow(sd.log.h,-2)
h~dlnorm(log.hm, tau.log.h)
# bias-correct lognormal for k
log.km              <- log(mean(prior.k))
sd.log.k            <- 0.2
tau.log.k           <- pow(sd.log.k,-2)
k~dlnorm(log.km, tau.log.k)
for (t in 1:nyr){
B[t] <- P[t]*k # biomass
F[t] <- ct.jags[t]/B[t]
}
}"

#Original bsm jags model
Model = "model{
    # to reduce chance of non-convergence, Pmean[t] values are forced >= eps
    eps<-0.01
    #><> Add Catch.CV
    for(t in 1:nyr){
      ct.jags[t] ~ dlnorm(log(ct[t]),pow(CV.C,-2))
    }

    penm[1]  <- 0 # no penalty for first biomass
    Pmean[1] <- log(alpha)
    P[1]     ~ dlnorm(Pmean[1],itau2)

    for (t in 2:nyr) {
      Pmean[t] <- ifelse(P[t-1] > 0.25,
        log(max(P[t-1] + r*P[t-1]*(1-P[t-1]) - ct.jags[t-1]/k,eps)),  # Process equation
        log(max(P[t-1] + 4*P[t-1]*r*P[t-1]*(1-P[t-1]) - ct.jags[t-1]/k,eps))) # linear decline of r at B/k < 0.25
      P[t]     ~ dlnorm(Pmean[t],itau2) # Introduce process error
      penm[t]  <- ifelse(P[t]<(eps+0.001),log(q*k*P[t])-log(q*k*(eps+0.001)),
                   # ifelse(P[t]>1,ifelse((ct[t]/max(ct))>0.2,log(q*k*P[t])-log(q*k*(0.99)),0),0)) # penalty if Pmean is outside viable biomass
                    ifelse(P[t]>1.1,log(q*k*P[t])-log(q*k*(0.99)),0))
    }

    # Get Process error deviation
    for(t in 1:nyr){
      proc.logB[t] <- log(P[t]*k)-log(exp(Pmean[t])*k)}

    # ><> b.priors with penalties
    # Biomass priors/penalties are enforced as follows
    for(i in 1:nbks){
    bk.mu[i] ~ dbeta(bk.beta[1,i],bk.beta[2,i])
    bk.beta[3,i] ~ dnorm(bk.mu[i]-P[b.yrs[i]],10000)
    }

    for (t in 1:nyr){
      Fpen[t]   <- ifelse(ct[t]>(0.9*k*P[t]),ct[t]-(0.9*k*P[t]),0) # Penalty term on F > 1, i.e. ct>B
      pen.F[t]  ~ dnorm(Fpen[t],1000)
      pen.bk[t] ~ dnorm(penm[t],10000)
      cpuem[t]  <- log(q*P[t]*k);
      btj[t]     ~ dlnorm(cpuem[t],pow(sigma2,-1));
    }
    for(t in 1:13){
    cpuemm[t]<-log(Q*P[t]*k)
    SSB[t]~dlnorm(cpuemm[t], pow(sigma2,-1))
    }

  # priors
  log.alpha               <- log((startbio[1]+startbio[2])/2) # needed for fit of first biomass
  sd.log.alpha            <- (log.alpha-log(startbio[1]))/4
  tau.log.alpha           <- pow(sd.log.alpha,-2)
  alpha                   ~  dlnorm(log.alpha,tau.log.alpha)

  # set realistic prior for q
  log.qm              <- mean(log(q.priorj))
  sd.log.q            <- (log.qm-log(q.priorj[1]))/2
  tau.log.q           <- pow(sd.log.q,-2)
  q                   ~  dlnorm(log.qm,tau.log.q)
    log.Qm              <- mean(log(Q.priorj))
  sd.log.Q            <- (log.Qm-log(Q.priorj[1]))/2
  tau.log.Q           <- pow(sd.log.Q,-2)
  Q                   ~  dlnorm(log.Qm,tau.log.Q)

  # define process (tau) and observation (sigma) variances as inversegamma priors
  itau2 ~ dgamma(4,0.01)
  tau2  <- 1/itau2
  tau   <- pow(tau2,0.5)

  isigma2 ~ dgamma(2,0.01)
  sigma2 <- 1/isigma2+pow(CV.cpue,2) # Add minimum realistic CPUE CV
  sigma  <- pow(sigma2,0.5)

  log.rm              <- mean(log(prior.r))
  sd.log.r            <- abs(log.rm - log(prior.r[1]))/2
  tau.log.r           <- pow(sd.log.r,-2)

  # bias-correct lognormal for k
  log.km              <- mean(log(prior.k))
  sd.log.k            <- abs(log.km-log(prior.k[1]))/2
  tau.log.k           <- pow(sd.log.k,-2)

  # Construct Multivariate lognormal (MVLN) prior
  mu.rk[1] <- log.rm
  mu.rk[2] <- log.km

  # Prior for correlation log(r) vs log(k)
  #><>MSY: now directly taken from mvn of ki = 4*msyi/ri
  rho <- rk.cor

  # Construct Covariance matrix
  cov.rk[1,1] <- sd.log.r * sd.log.r
  cov.rk[1,2] <- rho
  cov.rk[2,1] <- rho
  cov.rk[2,2] <- sd.log.k * sd.log.k

  # MVLN prior for r-k
  log.rk[1:2] ~ dmnorm(mu.rk[],inverse(cov.rk[,]))
  r <- exp(log.rk[1])
  k <- exp(log.rk[2])

  #><>MSY get posterior predictive distribution for rk
  ppd.logrk[1:2] ~ dmnorm(mu.rk[],inverse(cov.rk[,]))

  # ><>HW: Get B/Bmsy and F/Fmsy directly from JAGS
  Bmsy <- k/2
  Fmsy <- r/2
  for (t in 1:nyr){
  B[t] <- P[t]*k # biomass
  F[t] <- ct.jags[t]/B[t]
  BBmsy[t] <- P[t]*2 #true for Schaefer
  FFmsy[t] <- ifelse(BBmsy[t]<0.5,F[t]/(Fmsy*2*BBmsy[t]),F[t]/Fmsy)
  }
} " 
