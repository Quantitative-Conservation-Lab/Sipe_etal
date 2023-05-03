#for running NIMBLE on windows
# Sys.setenv(PATH=paste("C:/Rtools/bin", Sys.getenv("PATH"), sep=";"))
# Sys.setenv(BINPREF="C:/Rtools/mingw_$(WIN)/bin/")

library(nimble)

mod<-nimbleCode({
  
  #variable selection priors
  #PSI
  alpha~ dnorm(0,sd=10)
  sig_beta.area~dexp(1)
  beta.area~dnorm(0, sd=sig_beta.area)
  sig_beta.peri~dexp(1)
  beta.elev~dnorm(0, sd=sig_beta.elev)
  sig_beta.hii~dexp(1)
  beta.hii~dnorm(0,sd=sig_beta.hii)
  sig_beta.pc~dexp(1)
  beta.pc~dnorm(0, sd=sig_beta.pc)
  sig_beta.canopy~dexp(1)
  beta.canopy~dnorm(0,sd=sig_beta.canopy)
  sig_beta.11~dexp(1)
  beta.11~dnorm(0,sd=sig_beta.11)
  sig_beta.2~dexp(1)
  beta.2~dnorm(0,sd=sig_beta.2)
  sig_beta.31~dexp(1)
  beta.31~dnorm(0,sd=sig_beta.31)
  sig_gamma.focal~dexp(1)
  gamma.focal~dnorm(0,sd=sig_gamma.focal)
  #random site and year effects for psi
  sd_epsilon_t~dunif(0,sd=10)
  sd_epsilon_i~dunif(0,sd=10)
  
  for(t in 1:T){
    epsilon_t[t]~dnorm(0,sd=sd_epsilon_t)
  }
  for(i in 1:N){
    epsilon_i[i]~dnorm(0,sd=sd_epsilon_i)
  }
  
  #R
  mu ~ dnorm(0,sd=10)
  sig_theta.area~dexp(1)
  theta.area~dnorm(0,sd=sig_theta.area)
  sig_theta.hii~dexp(1)
  theta.hii~dnorm(0,sd=sig_theta.hii)
  sig_theta.elev~dexp(1)
  theta.elev~dnorm(0,sd=sig_theta.elev)
  sig_theta.pc~dexp(1)
  theta.pc~dnorm(0, sd=sig_theta.pc)
  sig_theta.canopy~dexp(1)
  theta.canopy~dnorm(0, sd=sig_theta.canopy)
  sig_theta.11~dexp(1)
  theta.11~dnorm(0,sd=sig_theta.11)
  sig_theta.2~dexp(1)
  theta.2~dnorm(0,sd=sig_theta.2)
  sig_theta.31~dexp(1)
  theta.31~dnorm(0,sd=sig_theta.31)
  
  #detection
  rho.1 ~ dnorm(0,sd=10)
  rho.2 ~ dnorm(0,sd=10)
  sig_lambda.survey1~dexp(1)
  lambda.survey1~dnorm(0,sd=sig_lambda.survey1)
  sig_lambda.survey2~dexp(1)
  lambda.survey2~dnorm(0, sd=sig_lambda.survey2)
  
  #ebird detection in non-breeding, p11 - eta1
  sig_eta1.area~dexp(1)
  eta1.area~dnorm(0, sd=sig_eta1.area)
  sig_eta1.dur~dexp(1)
  eta1.dur~dnorm(0, sd=sig_eta1.dur)
  sig_eta1.start~dexp(1)
  eta1.start~dnorm(0, sd=sig_eta1.start)
  sig_eta1.dist~dexp(1)
  eta1.dist~dnorm(0, sd=sig_eta1.dist)
  
  #WDFW detection non-breeding, omega1
  sig_omega1.area~dexp(1)
  omega1.area~dnorm(0, sd=sig_omega1.area)
  sig_omega1.dur~dexp(1)
  omega1.dur~dnorm(0, sd=sig_omega1.dur)
  sig_omega1.start~dexp(1)
  omega1.start~dnorm(0, sd=sig_omega1.start)
  
  #ebird detection breeding, p21 - eta2
  sig_eta2.area~dexp(1)
  eta2.area~dnorm(0, sd=sig_eta2.area)
  sig_eta2.dur~dexp(1)
  eta2.dur~dnorm(0, sd=sig_eta2.dur)
  sig_eta2.start~dexp(1)
  eta2.start~dnorm(0, sd=sig_eta2.start)
  sig_eta2.dist~dexp(1)
  eta2.dist~dnorm(0, sd=sig_eta2.dist)
  
  #WDFW detection in breeding - p22, omega2
  sig_omega2.area~dexp(1)
  omega2.area~dnorm(0, sd=sig_omega2.area)
  sig_omega2.dur~dexp(1)
  omega2.dur~dnorm(0, sd=sig_omega2.dur)
  sig_omega2.start~dexp(1)
  omega2.start~dnorm(0, sd=sig_omega2.start)
  delta ~ dbeta(1,1)
  
  
  ####Something here
  for(i in 1:N){
    logit(psi[i,1])<-alpha +epsilon_i[i]+epsilon_t[1]+
      beta.elev*elev[i]+
      beta.pc*pc[i]+
      beta.hii*hii[i]+
      beta.area*area[i]+
      beta.canopy*canopy[i]+
      beta.11*x11[i]+beta.2*x2[i]+beta.31*x31[i]
    
    logit(R[i]) <- mu +
      theta.elev*elev[i]+
      theta.pc*pc[i]+ 
      theta.hii*hii[i]+
      theta.area*area[i]+
      theta.canopy*canopy[i]+
      theta.11*x11[i]+theta.2*x2[i]+theta.31*x31[i]
  }
  for(i in 1:N){
    for(t in 2:T){
      #function to get the previous z for each site
      m[i,t]<-getFocalOcc(z[i,t-1])
      logit(psi[i,t])<-alpha +epsilon_i[i]+epsilon_t[t]+
        gamma.focal*m[i]+
        beta.elev*elev[i]+
        beta.pc*pc[i]+
        beta.hii*hii[i]+
        beta.area*area[i]+
        beta.canopy*canopy[i]+
        beta.11*x11[i]+beta.2*x2[i]+beta.31*x31[i]
    }
  }  
  
  
  
  for(i in 1:N){
    for(t in 1:T){
      z[i,t] ~ dcat(phi[i,t,1:3])
      phi[i,t,1]<-1-psi[i,t]
      phi[i,t,2]<-psi[i,t]*(1-R[i])
      phi[i,t,3]<-psi[i,t]*R[i]
    }
  }
  
  #IND ==
  #JM ==
  
  for(i in 1:N){
    for(t in 1:T){
      for(j in 1:Jm[i,t]){
        
        #p1 ==
        #p2==
        
        #ebird detection
        logit(p1[i,t,j,1]) <- rho.1 + lambda.survey1*(ind[i,t,j] - 1) +
          eta1.area*area[i]+eta1.dur*dur[i,t,j]+
          eta1.start*timeobs[i,t,j]+eta1.dist*dist[i,t,j]
        logit(p2[i,t,j,1]) <- rho.2 + lambda.survey2*(ind[i,t,j] - 1) +
          eta2.area*area[i]+eta2.dur*dur[i,t,j]+eta2.start*timeobs[i,t,j]+
          eta2.dist*dist[i,t,j]
        #wdfw detection
        logit(p1[i,t,j,2]) <- rho.1 + lambda.survey1*(ind[i,t,j] - 1) +
          omega1.area*area[i]+omega1.dur*dur[i,t,j]+omega1.start*timeobs[i,t,j]
        logit(p2[i,t,j,2]) <- rho.2 + lambda.survey2*(ind[i,t,j] - 1) + 
          omega2.area*area[i]+omega2.dur*dur[i,t,j]+omega2.start*timeobs[i,t,j]
        
        #Upsilon == with dimensions and ind
        
        Upsilon[i,t,j,1,1,1:3]<-c(1,0,0)
        Upsilon[i,t,j,1,2,1:3]<-c((1-p1[i,t,j,1]), p1[i,t,j,1],0)
        Upsilon[i,t,j,1,3,1:3]<-c((1-p2[i,t,j,1]), p2[i,t,j,1],0)
        
        Upsilon[i,t,j,2,1,1:3]<-c(1,0,0)
        Upsilon[i,t,j,2,2,1:3]<-c((1-p1[i,t,j,2]), p1[i,t,j,2],0)
        Upsilon[i,t,j,2,3,1:3]<-c((1-p2[i,t,j,2]), (p2[i,t,j,2]*(1-delta)), (p2[i,t,j,2]*delta))
        
        y[i,t,j]~dcat(Upsilon[i,t,j,ind[i,t,j],z[i,t],1:3])
        
      }
    }
  }
})

#nimble function to get the previous z for t>1
getFocalOcc<-nimbleFunction(
  run=function(z=double()){
    if(z>1) return(1)
    else return(0)
    returnType(integer())
  })

#load in data arrays - output from FormattedData.R
#load("DATA.Rdata")
N<-766
T<-18
J1<-100 #####change this to reflect how many

Jmvec<-array(dim=c(N,T))
for(i in 1:N){
  for(t in 1:T){
    Jmvec[i,t]<-length(which(!is.na(dataMod$y[i,t,1:J1])))
  }
}

y<-dataMod$y[1:N, 1:T, 1:J1]
dat1<-list(y=y, 
           #covariates for psi and R
           area=scale(sitecovs$areaSQKM[1:N]),
           elev=scale(sitecovs$elev_m[1:N]),
           hii=scale(sitecovs$hii[1:N]),
           pc=scale(sitecovs$periCom[1:N]),
           canopy=scale(sitecovs$canopy[1:N]),
           x11=sitecovs$Forest.Type[1:N],
           x2=sitecovs$Developed.Type[1:N],
           x31=sitecovs$Other.Type[1:N],
           #covariates for detection
           timeobs=array(scale(covdata$timeobs[1:N, 1:T, 1:J1]), dim=c(N,T,J1)),
           dur=array(scale(covdata$dur_m[1:N, 1:T, 1:J1]), dim=c(N,T,J1)),
           dist=array(scale(dist[1:N, 1:T, 1:J1]), dim=c(N,T,J1)))


z=dataMod$z[1:N, 1:T]
z[z==0]<-NA
rn<-sample(seq(1,10000, by=1))
set.seed(rn[1])
inits<-list(z=z,
            #initial values for  PSI
            sd_epsilon_t=runif(1,0,10), sd_epsilon_i=runif(1,0,10),
            alpha=rnorm(1,0,10),sig_beta.elev=rexp(1), sig_beta.pc=rexp(1),
            sig_beta.area=rexp(1),sig_beta.hii=rexp(1),
            sig_beta.canopy=rexp(1), sig_beta.11=rexp(1),
            sig_beta.2=rexp(1), sig_beta.31=rexp(1),
            sig_gamma.focal=rexp(1),
            #R
            mu=rnorm(1,0,10),sig_theta.area=rexp(1),
            sig_theta.hii=rexp(1),sig_theta.elev=rexp(1),
            sig_theta.pc=rexp(1),sig_theta.canopy=rexp(1),
            sig_theta.11=rexp(1),sig_theta.2=rexp(1),
            sig_theta.31=rexp(1),        
            
            #detection
            rho.1=rnorm(1,0,10), sig_lambda.survey1=rexp(1),
            #detection not breeding
            #ebird
            sig_eta1.area=rexp(1), sig_eta1.dur=rexp(1),
            sig_eta1.start=rexp(1), sig_eta1.dist=rexp(1),
            #wdfw
            sig_omega1.area=rexp(1), sig_omega1.dur=rexp(1),
            sig_omega1.start=rexp(1),
            
            #Detection breeding
            rho.2=rnorm(1,0,10),sig_lambda.survey2=rexp(1),
            #ebird
            sig_eta2.area=rexp(1),sig_eta2.dur=rexp(1),
            sig_eta2.start=rexp(1), sig_eta2.dist=rexp(1),
            #wdfw
            sig_omega2.area=rexp(1),sig_omega2.dur=rexp(1),
            sig_omega2.start=rexp(1),delta=rbeta(1,1,1))

constants<-list(N=N, T=T,J1=J1,Jm=Jmvec,ind=dataMod$ind[1:N,1:T,1:J1])
Rmodel<-nimbleModel(code=mod, constants=constants, data=dat1, inits=inits)
conf<-configureMCMC(Rmodel)

conf$addMonitors(c("z", "psi","R",
                   #psi
                   "alpha","epsilon_i", "epsilon_t","beta.elev",
                   "beta.area", "beta.pc","beta.hii","beta.canopy", 
                   "beta.11","beta.2","beta.31", "gamma.focal",
                   #R
                   "mu","theta.elev","theta.area", "theta.pc","theta.hii",
                   "theta.canopy", "theta.11","theta.2","theta.31",
                   #detection, nonbreeding
                   "rho.1","lambda.survey1",
                   #ebird
                   "eta1.area","eta1.dur","eta1.start","eta1.dist",
                   #wdfw
                   "omega1.area","omega1.dur","omega1.start",
                   #detection breeding
                   "rho.2","lambda.survey2",
                   #ebird
                   "eta2.area","eta2.dur","eta2.start","eta2.dist",
                   #wdfw
                   "omega1.area","omega1.dur","omega1.start","delta"))


Rmcmc<-buildMCMC(conf)
Cmodel<-compileNimble(Rmodel, showCompilerOutput = TRUE)

Cmodel$setData(dat1)
Cmodel$setInits(inits)

Cmcmc<-compileNimble(Rmcmc,project=Cmodel, showCompilerOutput = TRUE)
Cmcmc$run(thin=10, reset=T, niter=1500000, nburnin=700000)
x1<-(as.data.frame(as.matrix(Cmcmc$mvSamples)))
#save model output
temp2<-paste("MonthDay",1, sep="") #as monthday with chain number
write.csv(as.data.frame(as.matrix(x1)), file=temp2)


