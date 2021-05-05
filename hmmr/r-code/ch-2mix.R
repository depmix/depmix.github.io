## ----setup, echo=FALSE, results='hide', message = FALSE, warning = FALSE--------------------------------------------------------------------------------------------------
options(prompt = "R> ", continue = "+  ", width = 60,
  digits = 4, show.signif.stars = FALSE, 
  useFancyQuotes = FALSE)

options(SweaveHooks = list(onefig =   function() {par(mfrow = c(1,1))},
                           twofig =   function() {par(mfrow = c(1,2))},                           
                           threefig = function() {par(mfrow = c(1,3))},
                           fourfig =  function() {par(mfrow = c(2,2))},
                           sixfig =   function() {par(mfrow = c(3,2))}))

require(hmmr)
require(knitr)

textwidth = 4.606299
      figwidth_10 <- 6
      figwidth_09 <- 5.75
      figwidth_08 <- 5.5
      figwidth_07 <- 5.25
      figwidth_06 <- 5


opts_chunk$set(
  engine='R',  
  strip.white = TRUE, 
  tidy='styler', 
  tidy.opts = list(scope="indentation"), 
  message = FALSE, 
  warning = FALSE, 
  prompt = TRUE, 
  comment = NA, 
  cache = TRUE, 
  fig.width=figwidth_06,
  fig.height=figwidth_06, 
  out.width=".6\\textwidth"
)

one_string <- function(x, ...) paste(x, ..., collapse = '\n')


hook_output = knit_hooks$get('output')
knit_hooks$set(output = function(x, options) {
  if (!is.null(n <- options$linewidth)) {
    exd <- 4
    x <- knitr:::split_lines(x)
    if(any(nchar(x) > n)) {
      runningx <- vector()
      ids <- which(nchar(x) > n)
      for(i in 1:length(ids)) {
        if(i == 1) {
          if(ids[i] == 1) {
            runningx <- c(runningx,strwrap(x[ids[i]], width = n))
          } else {
            runningx <- c(runningx,x[1:(ids[i]-1)],strwrap(x[ids[i]], width = n, exdent = exd))
          }
        } else {
          if(ids[i]-ids[i-1] > 1) { 
            runningx <- c(runningx,x[(ids[i-1]+1):(ids[i]-1)],strwrap(x[ids[i]], width = n, exdent = exd))
          } else {
            runningx <- c(runningx,strwrap(x[ids[i]], width = n, exdent = exd)) 
          }
        }
        if(ids[length(ids)] < length(x)) {
          runningx <- c(runningx,x[(ids[length(ids)] + 1):length(x)])
        }
      }
      x <- paste(runningx, collapse = '\n')
    } else {
      x <- paste(x,collapse = '\n')
    }
  }
  hook_output(x, options)
})

knit_hooks$set(document = function(x) {
  gsub('(\\\\end\\{knitrout\\}+)', '\\1\\\\noindent ', paste(x, collapse = '\n'))
})

source("deparse.R")

set.seed(1072)



## ----include = FALSE------------------------------------------------------------------------------------------------------------------------------------------------------
opts_chunk$set(cache.path = "cache/ch2/", fig.path = "figure/ch2/")
opts_chunk$set(cache.path = "cache/ch2/", fig.path = "figure/ch2/")


## ----echo=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------
library("hmmr")
data(speed1)
mrt <- mean(speed1$RT)
sdrt <- sd(speed1$RT)


## ----fig-sp1rtdens, echo=TRUE, eval=FALSE---------------------------------------------------------------------------------------------------------------------------------
## library("hmmr")
## data(speed1)
## mrt <- mean(speed1$RT)
## sdrt <- sd(speed1$RT)
## singleDensity <- function(x) {dnorm(x,mrt,sdrt)}
## truehist(speed1$RT, nb=23, col=0, xlab="RT (log ms)", las=1,
## 	ylab="Density")
## curve(singleDensity,add=TRUE)


## ----fig-sp1rtdens-drawing,echo=FALSE,fig.width=1.15*figwidth_08, fig.height=1.15*.7*figwidth_08, out.width=".8\\textwidth"-----------------------------------------------
library("hmmr")
data(speed1)
mrt <- mean(speed1$RT)
sdrt <- sd(speed1$RT)
singleDensity <- function(x) {dnorm(x,mrt,sdrt)}
truehist(speed1$RT, nb=23, col=0, xlab="RT (log ms)", las=1, 
	ylab="Density")
curve(singleDensity,add=TRUE)


## ----rt-shapiro, echo=TRUE------------------------------------------------------------------------------------------------------------------------------------------------
shapiro.test(speed1$RT)


## ----sample-states--------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(2)
states <- sample(c(1,2),1000,replace=TRUE) 


## ----sample-obs-----------------------------------------------------------------------------------------------------------------------------------------------------------
m1=0; m2=3 
mm <- c(m1,m2)[states] # choose mean according to states
# draw Y from the corresponding density
y <- rnorm(1000, mm, sd=1) 


## ----mix-sample-shapiro---------------------------------------------------------------------------------------------------------------------------------------------------
shapiro.test(y)


## ----truehist-gaussian-mixture,echo=FALSE,fig.height=5,fig.width=7,out.width=".8\\textwidth"------------------------------------------------------------------------------
mrt <- mean(y)
sdrt <- sd(y)
singleDensity <- function(x) {dnorm(x,mrt,sdrt)}
truehist(y, nb=23, col=0, xlab="Y", las=1, 
	ylab="Density")
curve(.5*dnorm(x,mean=0,sd=1),add=TRUE,lty=2)
curve(.5*dnorm(x,mean=3,sd=1),add=TRUE,lty=2)
curve(.5*dnorm(x,mean=0,sd=1) + .5*dnorm(x,mean=3,sd=1),add=TRUE)


## ----mix-sample-ll--------------------------------------------------------------------------------------------------------------------------------------------------------
sum(log(0.5*dnorm(y,0,1)+0.5*dnorm(y,3,1))) 


## ----posterior-speeddata1,echo=TRUE---------------------------------------------------------------------------------------------------------------------------------------
post <- function(state,y,prior1,mean1,mean2,sd1,sd2) {
  prior2 <- 1-prior1
  # compute unnormalized posterior probabilties 
  post <- cbind(prior1*dnorm(y,mean1,sd1),
  	prior2*dnorm(y,mean2,sd2))
  # normalize 
  post <- post/rowSums(post)
  # return posterior for given state
  return(post[cbind(1:length(y),state)])
}


## ----posterior-simdata,echo=FALSE,results='hide'--------------------------------------------------------------------------------------------------------------------------
p <- post(state=1,y=y,prior1=.5,mean1=0,mean2=3,sd1=1,sd2=1)


## ----fig-rt-2state-posterior-hist, echo=FALSE, fig.height=.75*5, fig.width=.75*7------------------------------------------------------------------------------------------
hist(p, xlab="probability")#, main="Posterior probabilities, component 1")


## ----constrained-LL-speed-data,echo=FALSE,results='hide'------------------------------------------------------------------------------------------------------------------
require(hmmr)
pc1 <- .5
pc2 <- .5
mc1 <- 0
sd1 <- 1
mc2 <- 3
sd2 <- 1


## ----fig-rt-2state-posterior, echo=FALSE, fig.height=5, fig.width=7, out.width=".8\\textwidth"----------------------------------------------------------------------------
c1 <- function(x) {pc1*dnorm(x,mc1,sd1)}
c2 <- function(x) {pc2*dnorm(x,mc2,sd2)}
truehist(y, nb=50, col=0, xlab="y", las=1, ylab="Density",ymax=1.1)
curve(c1,add=TRUE)
curve(c2,add=TRUE)
sp1 <- order(y)
lines(y[sp1],post(state=1,y=y,prior1=.5,mean1=0,mean2=3,sd1=1,sd2=1)[sp1], lty=2)
lines(y[sp1],post(state=2,y=y,prior1=.5,mean1=0,mean2=3,sd1=1,sd2=1)[sp1], lty=3)
abline(v=1.5)


## ----mu-profile, echo=FALSE, fig.height=.75*5, fig.width=.75*7------------------------------------------------------------------------------------------------------------
set.seed(2)
states <- sample(c(1,2),1000,replace=TRUE) 
m1=0; m2=3 
mm <- c(m1,m2)[states]
y <- rnorm(1000,mm,sd=1) 
ll <- function(par) {
  sapply(par, function(par) {
    sum(log(0.5*dnorm(y,0,1)+0.5*dnorm(y,par,1))) })}
plot(ll,-9,12, xlab=expression(mu[2]),ylab="log likelihood")#main="Likelihood profile of the mean.",)


## ----gauss-mix-likelihood-------------------------------------------------------------------------------------------------------------------------------------------------
# define a function which computes the log likelihood
ll <- function(par) {
  sum(log(0.5*dnorm(y,0,1)+0.5*dnorm(y,par,1)))
}
# run optimize with sensible lower and upper bound from plot
optimize(ll, interval=c(-9, 12), maximum=TRUE)


## ----gauss-mix-simulation-------------------------------------------------------------------------------------------------------------------------------------------------
GaussMix2 <- function(par,y) {
  # set parameter values
  p1 <- par[1]; p2 <- 1-par[1]
  m1 <- par[2]; m2 <- par[3]; s1 <- par[4]; s2 <- par[5]
  # return negative log-likelihood
  -sum(log(p1*dnorm(y,mean=m1,sd=s1) + 
           p2*dnorm(y,mean=m2,sd=s2)))
}
# define starting values
pstart <- c("p1"=.5,"m1"=1,"m2"=2,"s1"=.5,"s2"=.5)
# run optim with appropriate lower and upper bounds
opt <- optim(pstart,fn=GaussMix2,y=y,
  lower=c(0,-Inf,-Inf,0,0),upper=c(1,rep(Inf,4)),
  method="L-BFGS-B")
opt$par


## ----gauss-mix-speed1-----------------------------------------------------------------------------------------------------------------------------------------------------
library(hmmr)
data(speed1)
y <- speed1$RT
pstart <- c("p1"=.5,"m1"=5.5,"m2"=6.4,"s1"=3,"s2"=.3)
opt <- optim(pstart,fn=GaussMix2,y=y,
  lower=c(0,-Inf,-Inf,0,0),upper=c(1,rep(Inf,4)),
  method="L-BFGS-B")
opt$par


## ----echo=FALSE, results='hide'-------------------------------------------------------------------------------------------------------------------------------------------
set.seed(3)
fm2 <- lca(speed1$RT,nc=2,verbose=FALSE)


## ----echo=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------
bic2 <- BIC(fm2)
pars <- getpars(fm2)
pc1 <- pars[1]
pc2 <- pars[2]
mc1 <- pars[3]
sd1 <- pars[4]
mc2 <- pars[5]
sd2 <- pars[6]


## ----fig-rt-2state, echo=FALSE, fig.height=.9*5, fig.width=.9*7,out.width=".8\\textwidth"---------------------------------------------------------------------------------
c1 <- function(x) {pc1*dnorm(x,mc1,sd1)}
c2 <- function(x) {pc2*dnorm(x,mc2,sd2)}
truehist(speed1$RT, nb=23, col=0, xlab="RT (log ms)", las=1, ylab="Density")
curve(c1,add=TRUE)
curve(c2,add=TRUE)


## ----posterior-speeddata1-2,echo=FALSE,results='hide',eval=FALSE----------------------------------------------------------------------------------------------------------
## library(hmmr)
## data(speed1)
## y <- speed1$RT
## means <- c(5,6); sds <- c(.2,.2); ps <- c(.5,.5)
## B <- cbind(dnorm(y,mean=means[1],sd=sds[1]),dnorm(y,mean=means[2],sd=sds[2]))
## prevLL <- sum(log(colSums(apply(B,1,"*",ps))))
## converge <- FALSE; i <- 2
## while(!converge) {
##   gamma <- t(apply(B,1,"*",ps))
##   gamma <- gamma/rowSums(gamma)
##   for(j in 1:2) {
##     means[j] <- sum(gamma[,j]*y)/sum(gamma[,j])
##     sds[j] <- sqrt(sum(gamma[,j]*(y-means[j])^2)/sum(gamma[,j]))
##   }
##   ps <- colMeans(gamma)
##   B <-  cbind(dnorm(y,mean=means[1],sd=sds[1]),dnorm(y,mean=means[2],sd=sds[2]))
##   LL <- sum(log(colSums(apply(B,1,"*",ps))))
##   if(LL - prevLL < 10e-5) converge <- TRUE
##   i <- i + 1; prevLL <- LL
## }
## par <- c(p1=ps[1],m1=means[1],m2=means[2],s1=sds[1],s2=sds[2])


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------
EM_GaussMix2 <- function(par,y) {
  ps <- par[1:2]; means <- par[3:4]; sds <- par[5:6]
  # compute a matrix with component density values
  B <- cbind(dnorm(y,mean=means[1],sd=sds[1]),
             dnorm(y,mean=means[2],sd=sds[2]))
  # log likelihood to check cnvergence
  prevLL <- sum(log(colSums(apply(B,1,"*",ps))))
  converge <- FALSE; i <- 1
  while(!converge) {
    # posterior state probabilities
    gamma <- t(apply(B,1,"*",ps))
    gamma <- gamma/rowSums(gamma)
    # parameter estimates
    for(j in 1:2) {
      means[j] <- sum(gamma[,j]*y)/sum(gamma[,j])
      sds[j] <- sqrt(sum(gamma[,j]*(y-means[j])^2)/
                       sum(gamma[,j]))
    }
    ps <- colMeans(gamma)
    # component density values with new parameters
    B <- cbind(dnorm(y,mean=means[1],sd=sds[1]),
               dnorm(y,mean=means[2],sd=sds[2]))
    # log-likelihood    
    LL <- sum(log(colSums(apply(B,1,"*",ps)))) 
    # check convergence
    if(LL - prevLL < 10e-8) converge <- TRUE
    i <- i + 1; prevLL <- LL
  }
  return(list(par = c(p1=ps[1], 
  	m1=means[1], m2=means[2], 
  	s1=sds[1], s2=sds[2]), 
	niter = i, logL = LL))
}
pstart <- c("p1"=.5,"p2"=.5,"m1"=5,"m2"=6,"s1"=.2,"s2"=.2)
res <- EM_GaussMix2(pstart,speed1$RT)
res$par


## ----echo=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------
nriter <- res$niter


## ----constrained-LL-speed-data-2------------------------------------------------------------------------------------------------------------------------------------------
EM_GaussMix2_c <- function(par,y) {
  ps <- par[1:2]; means <- par[3:4]; sd <- par[5]
  B <- cbind(dnorm(y,mean=means[1],sd=sd),
             dnorm(y,mean=means[2],sd=sd))
  prevLL <- sum(log(colSums(apply(B,1,"*",ps))))
  converge <- FALSE; i <- 1
  while(!converge) {
    gamma <- t(apply(B,1,"*",ps))
    gamma <- gamma/rowSums(gamma)
    for(j in 1:2) {
      means[j] <- sum(gamma[,j]*y)/sum(gamma[,j])
    }
    # single estimate of sd
    sd <- sqrt((sum(gamma[,1]*(y-means[1])^2) +
             sum(gamma[,2]*(y-means[2])^2))/
            length(y))
    ps <- colMeans(gamma)
    B <- cbind(dnorm(y,mean=means[1],sd=sd),
               dnorm(y,mean=means[2],sd=sd))
    LL <- sum(log(colSums(apply(B,1,"*",ps))))
    if(LL - prevLL < 10e-8) converge <- TRUE
    i <- i + 1; prevLL <- LL
  }
  return(list(par = c(p1=ps[1], m1=means[1], m2=means[2], 
  	sd=sd), niter = i, logL = LL))
}
ps <- c(.5,.5); means <- c(5,6); sds <- .2;
pstart <- c(ps,means,sds)
res <- EM_GaussMix2_c(pstart,speed1$RT)
res$par


## ----gauss-mix-constraint, linewidth=60-----------------------------------------------------------------------------------------------------------------------------------
require(Rsolnp)
pstart <- c(.5,.5,5.5,6.4,.3,.3)
names(pstart) <- c("p1","p2","m1","m2","s1","s2")
# define matrix A
lineq <- matrix(0,2,6)
lineq[1,c(1,2)] <- 1
lineq[2,c(5,6)] <- c(1,-1)
# define equality values (l = u)
eqB <- c(1,0)
# define equality function for solnp
eqfun <- function(par,y) {
	as.vector(lineq%*%par)
}
llFun <- function(par,y) {
  p1 <- par[1]; p2 <- par[2]
  m1 <- par[3]; m2 <- par[4]; s1 <- par[5]; s2 <- par[6]
  ll <- -sum(log(p1*dnorm(y,mean=m1,sd=s1) + 
    p2*dnorm(y,mean=m2,sd=s2))) # negative log L
  llmax <- 1e7 # to avoid Inf
  if(ll>llmax) return(llmax) else return(ll)
}
# run solnp with equality function and parameter bounds
res <- solnp(pstart,fun=llFun,y=speed1$RT,
  LB=c(0,0,-Inf,-Inf,0,0),UB=c(1,1,rep(Inf,4)),
  eqfun=eqfun,eqB=eqB)
pars <- res$pars; names(pars) <- names(pstart)
pars


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------
km <- kmeans(speed1$RT,2)
means <- as.vector(by(speed1$RT,km$cluster,mean))
sds <- as.vector(by(speed1$RT,km$cluster,sd))
ps <- c(.5,.5)
pstart <- c(ps,means,sds)
res <- EM_GaussMix2(pstart,speed1$RT)
res$par 


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------
EM_GaussMix2_gamma <- function(gamma,y) {
  prevLL <- -Inf; means <- sds <- rep(0.0,2)
  converge <- FALSE; i <- 1
  while(!converge) {
    for(j in 1:2) {
      means[j] <- sum(gamma[,j]*y)/sum(gamma[,j])
      sds[j] <- sqrt(sum(gamma[,j]*(y-means[j])^2)/
                       sum(gamma[,j]))
    }
    B <- cbind(dnorm(y,mean=means[1],sd=sds[1]),
               dnorm(y,mean=means[2],sd=sds[2]))
    gamma <- t(apply(B,1,"*",ps))
    gamma <- gamma/rowSums(gamma)
    ps <- colMeans(gamma)
    LL <- sum(log(colSums(apply(B,1,"*",ps))))
    if(LL - prevLL < 10e-8) converge <- TRUE
    i <- i + 1; prevLL <- LL
  }
  return(list(par = c(p1=ps[1], m1=means[1], m2=means[2], 
  	s1=sds[1], s2=sds[2]), niter = i, logL = LL))
}
set.seed(3)
gamma <- sample(1:2,length(speed1$RT),replace=TRUE)
gamma <- matrix(c(c(0,1)[gamma], c(1,0)[gamma]),
	nr=length(speed1$RT))
res <- EM_GaussMix2_gamma(gamma,speed1$RT)
res$par


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(gtools) # for the Dirichlet distribution
set.seed(3)
gamma <- rdirichlet(length(speed1$RT),alpha=rep(.1,2))
res <- EM_GaussMix2_gamma(gamma,speed1$RT)
res$par


## ----llratio-test-example, linewidth=60-----------------------------------------------------------------------------------------------------------------------------------
data(speed1)
set.seed(5)
spmix2 <- mix(RT~1, data=speed1, nstates=2)
fspmix2 <- fit(spmix2,verbose=FALSE)
pst <- getpars(fspmix2)
pst[c(4,6)] <- 0.25 # set sd to 0.25
spm2 <- setpars(spmix2,pst)
fspmix2c <- fit(spm2, equal=c(1,1,1,2,1,2), 
	method="rsolnp")
llratio(fspmix2,fspmix2c)


## ----gaussmix2, echo=FALSE, result='hide'---------------------------------------------------------------------------------------------------------------------------------
y <- speed1$RT
pstart <- c(.5,6.4,5.5,.3,.3)
names(pstart) <- c("p1","m1","m2","s1","s2")


## ----nlmHess, echo=TRUE, result='hide'------------------------------------------------------------------------------------------------------------------------------------
nlmEst <- nlm(GaussMix2,pstart,y=y,hessian=TRUE)
hess1 <- nlmEst$hessian 


## ----sesNlm, echo=TRUE----------------------------------------------------------------------------------------------------------------------------------------------------
nlmSes <- sqrt(diag(solve(hess1))) # solve for inverse
names(nlmSes) <- c("p1","m1","sd1","m2","sd2")
nlmSes


## ----sesNlmConfidenceNormal, echo=TRUE------------------------------------------------------------------------------------------------------------------------------------
rbind(lower=nlmEst$estimate - qnorm(.975)*nlmSes,
      upper=nlmEst$estimate + qnorm(.975)*nlmSes)


## ----finDiff, echo=TRUE, result='hide'------------------------------------------------------------------------------------------------------------------------------------
pstart <- c("p1"=.5,"m1"=6.4,"m2"=5.5,"s1"=.3,"s2"=.3)
opt <- optim(pstart,fn=GaussMix2,y=speed1$RT,
  lower=c(0,-Inf,-Inf,0,0),upper=c(1,rep(Inf,4)),
  method="L-BFGS-B")
par <- opt$par
# finite difference SE using numDeriv
require(numDeriv)
hess2 <- hessian(GaussMix2, par, y=y)
fdseNum <- sqrt(diag(solve(hess2)))
# finite difference SE using nlme
require(nlme)
hess3 <- fdHess(par, GaussMix2, y=y)$Hessian 
fdseNlme <- sqrt(diag(solve(hess3)))


## ----parametric-bootstrap-speed-code,eval=FALSE---------------------------------------------------------------------------------------------------------------------------
## require(boot)
## # define a function to produce a bootstrap sample
## speed.rg <- function(data,mle) {
##   simulate(data)
## }
## # define what to do with a sample (estimate parameters)
## speed.fun <- function(data) {
##   getpars(fit(data,verbose=FALSE,emcontrol=
##               em.control(random.start=FALSE)))
## }
## # produce 1000 bootstrap samples (may take some time!)
## speed_boot_par <- boot(fspmix2,speed.fun,R=1000,
##                        sim="parametric",ran.gen = speed.rg)


## ----echo=FALSE,results='hide'--------------------------------------------------------------------------------------------------------------------------------------------
require(boot)
data(speed_boot_par)
sqrt(diag(cov(speed_boot_par$t)))[-1] -> seBoot


## ----echo=TRUE------------------------------------------------------------------------------------------------------------------------------------------------------------
confint <- apply(speed_boot_par$t,2, quantile, 
                 probs=c(.025,.975))
colnames(confint) <- c("p1","p2","m1","sd1","m2","sd2")
confint


## ----echo=TRUE------------------------------------------------------------------------------------------------------------------------------------------------------------
ses <- sqrt(diag(cov(speed_boot_par$t)))
names(ses) <- c("p1","p2","m1","sd1","m2","sd2")
ses


## ----gaussMixConstraintHessian, echo=FALSE, results='hide'----------------------------------------------------------------------------------------------------------------
require(Rsolnp)
require(hmmr)
require(nlme)
require(numDeriv)

data(speed1)
y <- speed1$RT
y <- speed1$RT
GaussMix2 <- function(par,y) {
	llmax <- 1000
	p1 <- par[1]; p2 <- par[2]
	m1 <- par[3]; m2 <- par[4]
	s1 <- par[5]; s2 <- par[6]
	ll <- -sum(log(p1*dnorm(y,mean=m1,sd=s1) + 
			p2*dnorm(y,mean=m2,sd=s2))) # negative log L
	ll <- min(ll,llmax)
	return(ll)
}
pstart <- c(.5,.5,6.4,5.5,.3,.3)
names(pstart) <- c("p1","p2","m1","m2","s1","s2")
lineq <- matrix(0,1,6)
lineq[1,1] <- lineq[1,2] <- 1
eqB <- c(1)
eqfun <- function(par,y) {
	ans = as.vector(lineq%*%par)
	ans
}
res <- solnp(pstart,fun=GaussMix2,y=y,
  LB=c(0,0,-Inf,-Inf,0,0),UB=c(1,1,rep(Inf,4)),
  eqfun=eqfun,eqB=eqB)
pars <- res$pars
names(pars) <- names(pstart)
pars

hess <- fdHess(pars, GaussMix2, y=y)$Hessian
linSes <- sqrt(diag(depmixS4:::hessian2vcov(hess,lineq)))


## ----se-table, echo=FALSE, results='asis'---------------------------------------------------------------------------------------------------------------------------------
par <- par[c(1,2,4,3,5)]
fdseNum <- fdseNum[c(1,2,4,3,5)]
fdseNlme <- fdseNlme[c(1,2,4,3,5)]
nlmSes <- nlmSes[c(1,2,4,3,5)]
linSes <- linSes[c(1,3,5,4,6)]

xt <- data.frame(est=par, fdD=fdseNum, fdN=fdseNlme, boot=seBoot, 
BFGS=nlmSes,lin=linSes)
require(xtable)
xt <- xtable(xt,caption="Parameter estimates and standard errors 
using different methods. `fdD' are the standard errors from finite 
differences from \\pkg{numDeriv}; `fdN' are those from \\pkg{nlme}; 
`boot' are those from the parametric bootstrap; `BFGS' are those 
derived from the optimization using \\pkg{nlm}; `lin' are those 
obtained from the corrected Hessian when the model is fitted using a 
linear constraint.", align="lrrrrrr", 
label="tab:stdErrors", 
digits=5, nrow=2, byrow=TRUE)
print(xt,caption.placement="top", table.placement = "th")


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(5)
fspmix3 <- fit(mix(RT~1,data=speed1,nstates=3), 
	verbose=FALSE)
llratio(fspmix3,fspmix2)


## ----eval=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------
## set.seed(5)
## library(boot)
## fspmix3 <- fit(mix(RT~1,data=speed1,nstates=3))
## llratio(fspmix3,fspmix2)
## set.seed(5)
## speed.fun.LR <- function(model) {
##   # simulate data until both models can be estimated
##   ok <- FALSE # set to TRUE when both models estimated
##   while(!ok) {
##     bootdat <- data.frame(RT =
## 		simulate(model)@response[[1]][[1]]@y)
##     fboot2 <- try({
##       mod <- mix(RT~1,data=bootdat,nstates=2)
##       mod <- setpars(mod,getpars(fspmix2))
##       fit(mod,emcontrol=em.control(random.start=FALSE),
##           verbose=FALSE)
##     },TRUE)
##     fboot3 <- try({
##       mod <- mix(RT~1,data=bootdat,nstates=3)
##       mod <- setpars(mod,getpars(fspmix3))
##       fit(mod,emcontrol=em.control(random.start=FALSE),
## 	  	verbose=FALSE)
##     },TRUE)
##     if(!inherits(fboot2,"try-error") &
##        !inherits(fboot3,"try-error")) ok <- TRUE
##   }
##   # ensure boot3 fits as well as boot2
##   if(ok & logLik(fboot3) < logLik(fboot2)) ok <- FALSE
##   while(!ok) {
##     cat("trying different starting-values")
##     fboot3 <- try({
##       mod <- mix(RT~1,data=bootdat,nstates=3)
##       mod <- setpars(mod,getpars(fspmix3))
##       fit(mod,verbose=FALSE)
##     },TRUE)
##     if(!inherits(fboot3,"try-error") &
##        logLik(fboot3) > logLik(fboot2)) ok <- TRUE
##   }
##   llratio(fboot3,fboot2)@value
## }
## speed_boot_LR <- boot(fspmix2,speed.fun.LR,R=1000,
##                       sim="parametric")


## ----echo=FALSE,results='hide'--------------------------------------------------------------------------------------------------------------------------------------------
data(speed_boot_LR) 


## ----fig-histogram-boot-speed-LR,echo=FALSE,fig.align="center"------------------------------------------------------------------------------------------------------------
x <- seq(0,max(speed_boot_LR$t),by=.05)
hist(speed_boot_LR$t[speed_boot_LR$t >= 0],breaks=50,prob=TRUE,xlab="LR",ylim=c(0,max(dchisq(x,df=3))),main="")
lines(x,dchisq(x,df=3))


## ----eval=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------
## (1+sum(speed_boot_LR$t >
## 	llratio(fspmix3,fspmix2)@value))/(1+speed_boot_LR$R)


## ----BLRT-speed-extra,eval=FALSE------------------------------------------------------------------------------------------------------------------------------------------
## set.seed(7)
## speed_boot_LR_extra <- boot(fspmix2,speed.fun.LR,R=4000,
##     sim="parametric",parallel="multicore",ncpus=6)
## (1+sum(c(speed_boot_LR$t,speed_boot_LR_extra$t) >
##     llratio(fspmix3,fspmix2)@value))/
##     ((speed_boot_LR$R+speed_boot_LR_extra$R)+1)

## ----BLRT-speed-p-value,echo=FALSE----------------------------------------------------------------------------------------------------------------------------------------
data(speed_boot_LR_extra)
(1+sum(c(speed_boot_LR$t,speed_boot_LR_extra$t) > 
    llratio(fspmix3,fspmix2)@value))/
    ((speed_boot_LR$R+speed_boot_LR_extra$R) + 1)


## ----BLRT-speed-p-values,echo=FALSE---------------------------------------------------------------------------------------------------------------------------------------
p <- rep(0.0,5000)
testval <- llratio(fspmix3,fspmix2)@value
for(i in 1:1000) {
  p[i] <- sum(speed_boot_LR$t[1:i] > testval)/i
}
for(i in 1:4000) {
  p[i+1000] <- sum(c(speed_boot_LR$t,speed_boot_LR_extra$t[1:i]) > testval)/(i + 1000)
}
plot(p,type="l",xlab="replication")
abline(h=.05,lty=2)


## ----gof-speed1-rt, echo=FALSE, results='hide'----------------------------------------------------------------------------------------------------------------------------
set.seed(2)
m1 <- lca(speed1[,"RT"], nc=1)
m2 <- lca(speed1[,"RT"], nc=2)
m3 <- lca(speed1[,"RT"], nc=3)
set.seed(1)
m4 <- mix(RT~1, nstates=4, data=speed1)
m4 <- fit(m4,emcontrol=em.control(maxit=1000))

aics <- c(AIC(m1),AIC(m2),AIC(m3),AIC(m4))
bics <- c(BIC(m1),BIC(m2),BIC(m3),BIC(m4))
ll <- c(logLik(m1),logLik(m2),logLik(m3),logLik(m4))
dfs <- c(freepars(m1),freepars(m2),freepars(m3),freepars(m4))
nob <- nobs(m1)
cbics <- -2*ll + dfs*(log((nob+2)/24))
caics <- aics + (2*dfs*(dfs+1))/(nob - dfs - 1)
gof <- data.frame(components=c("1","2","3","4"),logLik=ll,npar=dfs,AIC=aics,AICc=caics,BIC=bics,CBIC=cbics)


## ----gof-table-speed1-rt, echo=FALSE, results='asis'----------------------------------------------------------------------------------------------------------------------
require(xtable)
xt <- xtable(gof,caption="Goodness of fit statistics 1--4 component
mixture models of the \\code{speed1} data.", 
align="llrrrrrr", label="tab:gof-speed1-rt-mix", 
display=c("d","d","f","d","f","f","f","f"))
print(xt, include.rownames=FALSE, caption.placement="top", table.placement = "th")


## ----fig-rt-hist-mixmodels, echo=FALSE, fig.height=.55*9, fig.width=.55*7-------------------------------------------------------------------------------------------------
par(mar=c(4,3,1,1))
pars <- getpars(m2) # get the parameters of the 2-state model
pc1 <- pars[1]
pc2 <- pars[2]
mc1 <- pars[3]
sd1 <- pars[4]
mc2 <- pars[5]
sd2 <- pars[6]
layout(matrix(c(1,1,2,2,3,3),nr=6)) # plot the 2 state model
nbre=23
c1 <- function(x) {pc1*dnorm(x,mc1,sd1)}
c2 <- function(x) {pc2*dnorm(x,mc2,sd2)}
truehist(speed1$RT, nb=nbre, col=0, xlab="RT (log ms)", las=1, ylab="Density")
curve(c1,add=TRUE,lty=2)
curve(c2,add=TRUE,lty=2)
c3 <- function(x) {c1(x)+c2(x)}
curve(c3,add=TRUE)
pars <- getpars(m3)
pc1 <- pars[1]
pc2 <- pars[2]
pc3 <- pars[3]
mc1 <- pars[4]
sd1 <- pars[5]
mc2 <- pars[6]
sd2 <- pars[7]
mc3 <- pars[8]
sd3 <- pars[9]
c1 <- function(x) {pc1*dnorm(x,mc1,sd1)}
c2 <- function(x) {pc2*dnorm(x,mc2,sd2)}
c3 <- function(x) {pc3*dnorm(x,mc3,sd3)}
truehist(speed1$RT, nb=nbre, col=0, xlab="RT (log ms)", las=1, 
ylab="Density") # plot the 3 state model
curve(c1,add=TRUE,lty=2)
curve(c2,add=TRUE,lty=2)
curve(c3,add=TRUE,lty=2)
c4 <- function(x) {c1(x)+c2(x)+c3(x)}
curve(c4,add=TRUE)
pars <- getpars(m4)
pc1 <- pars[1]
pc2 <- pars[2]
pc3 <- pars[3]
pc4 <- pars[4]
mc1 <- pars[5]
sd1 <- pars[6]
mc2 <- pars[7]
sd2 <- pars[8]
mc3 <- pars[9]
sd3 <- pars[10]
mc4 <- pars[11]
sd4 <- pars[12]
c1 <- function(x) {pc1*dnorm(x,mc1,sd1)}
c2 <- function(x) {pc2*dnorm(x,mc2,sd2)}
c3 <- function(x) {pc3*dnorm(x,mc3,sd3)}
c4 <- function(x) {pc4*dnorm(x,mc4,sd4)}
truehist(speed1$RT, nb=nbre, col=0, xlab="RT (log ms)", las=1, 
ylab="Density") # plot the 3 state model
curve(c1,add=TRUE,lty=2)
curve(c2,add=TRUE,lty=2)
curve(c3,add=TRUE,lty=2)
curve(c4,add=TRUE,lty=2)
c5 <- function(x) {c1(x)+c2(x)+c3(x)+c4(x)}
curve(c5,add=TRUE)

