## ----setup, echo=FALSE, results='hide', message = FALSE, warning = FALSE----
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



## ----include = FALSE-----------------------------------------------
opts_chunk$set(cache.path = "cache/ch4/", fig.path = "figure/ch4/")
opts_chunk$set(cache.path = "cache/ch4/", fig.path = "figure/ch4/")


## ----transition-example1, echo=TRUE--------------------------------
trm <- matrix(c(0.8,0.2,0.4,0.6),nrow=2,byrow=TRUE)
init <- matrix(c(0.5,0.5),nrow=1)
trm2 <- trm%*%trm
trm4 <- trm2%*%trm2
init%*%trm4


## ----transition-example2, echo=TRUE--------------------------------
trm2 # A^2
trm%*%trm2 # A^3
trm4 # A^4
trm4%*%trm #A^5
trm4%*%trm2 # A^6


## ----stationary-function, echo=TRUE--------------------------------
stationary <- function(tpm) {
  e1 <- as.double(eigen(t(tpm))$vectors[,1])
  return(e1/sum(e1))
}


## ----stationary-example, echo=TRUE---------------------------------
stationary(trm) 


## ----dwell-times-graph,echo=FALSE,fig.height=4,fig.width=5---------
nconsec <- 1:15
plot(c(1,15),c(0,.5),type="n",ylab="Probability",xlab="Dwell time (consecutive state visits)",frame=FALSE)
lines(nconsec,.5^(nconsec-1)*.5,lty=1)
lines(nconsec,.8^(nconsec-1)*.2,lty=2)
lines(nconsec,.95^(nconsec-1)*.05,lty=3)
legend(15,.5,lty=1:3,legend=c("p=.5","p=.8","p=.95"),xjust=1,bty="n")


## ----golden-mean-paths, echo=TRUE----------------------------------
x <- matrix(c(1,1,1,0), nrow=2, ncol=2) # adjacency matrix
sum(x) # path length 1, string length 2
sum(x%*%x) # path length 2, string length 3
sum(x%*%x%*%x) # path length 3, string length 4
sum(x%*%x%*%x%*%x) # path length 4, string length 5


## ------------------------------------------------------------------
eigen(x)$values[1]


## ----bernouilli-switch, echo=FALSE---------------------------------
set.seed(2)
y1 <- sample(c(0,1),10,c(0.5,0.5),rep=T)
y2 <- sample(c(0,1),10,c(0.05,0.95),rep=T)
y <- factor(c(y1,y2))


## ----bernouilli-switch-2, echo=TRUE--------------------------------
set.seed(2)
y1 <- sample(c(0,1),10,c(0.5,0.5),replace=T)
y2 <- sample(c(0,1),10,c(0.05,0.95),replace=T)
y <- factor(c(y1,y2))


## ----echo=FALSE,results='hide'-------------------------------------
library("hmmr")
data(disc42)
set.seed(1)
nti <- attr(disc42,"ntimes") 

lt <- length(nti)
et <- cumsum(nti)
bt <- c(1,et[-lt]+1)

m2 <- hmm(disc42,ns=2)
x <- summary(m2)
disc42$post <- x[posterior(m2, type="global"),2]


## ----fig-disc42, echo=FALSE, fig.height=4, fig.width=7,out.width="\\textwidth"----
mt <- matrix(c(1:6),ncol=2)
layout(mt)
for(i in 1:lt) {
	par(mar=c(3,5,2,5))
	plot(1:nti[i],disc42[bt[i]:et[i],1], las=1, xaxp=c(1,49,4), 
	xlim=c(1,50), frame.plot=FALSE, ylab="acc", xlab="trial", 
	type="l", yaxt="n")
	axis(side=2, at=c(1,2), labels=c("inc", "cor"), las=1)
	par(new=TRUE)
	plot(1:nti[i],disc42[bt[i]:et[i],2], las=1, xaxp=c(1,49,4),
			xaxt="n",yaxt="n",ty="l",ylim=c(0,1),lty=2,xlim=c(1,50),
			xlab="",ylab="",frame.plot=FALSE)
	axis(4)
	mtext("p(cor)",side=4,line=3,cex=0.6)
}


## ------------------------------------------------------------------
data(discrimination)
y <- discrimination$acc[1795:1805]
nt <- length(y)
prior <- c(1,0) # prior probabilities
A <- matrix(c(.8,0,0.2,1),ncol=2) # transition matrix
B <- cbind(rep(.5,nt),as.numeric(y == "correct")) # obs dens
alpha <- matrix(ncol=2,nrow=nt)
alpha[1,] <- prior*B[1,] # initialize
for(t in 2:nt) {
    alpha[t,] <- (t(A)%*%alpha[t-1,])*B[t,] # recursion
}


## ----eval=FALSE----------------------------------------------------
## alpha/rowSums(alpha)


## ----fig-filter, echo=FALSE, fig.height=5, fig.width=7, out.width=".8\\textwidth"----
pfs1 <- (alpha/rowSums(alpha))[,2]
beta <- matrix(ncol=2,nrow=nt)
beta[nt,] <- 1 # initialize
for(t in (nt-1):1) {
    beta[t,] <- (A%*%(B[t+1,]*beta[t+1,]))
}
pss1 <- (alpha*beta/rowSums(alpha*beta))[,2]
plot(c(1,11),c(0,1),type="n",ylab=expression(P(S[t] == 2)),xlab=expression(t),frame=FALSE)
points(1:11,as.numeric(y=="correct"))
lines(1:11,pfs1)
lines(1:11,pss1,lty=2)
legend(c(1,1),lty=c(1,2),legend=c("filtering","smoothing"),bty="n")


## ------------------------------------------------------------------
sum(alpha[nt,]) 


## ----beta unscaled-------------------------------------------------
beta <- matrix(ncol=2,nrow=nt)
beta[nt,] <- 1 # initialize
for(t in (nt-1):1) {
    beta[t,] <- (A%*%(B[t+1,]*beta[t+1,])) # recursion
}


## ----eval=FALSE----------------------------------------------------
## alpha*beta/rowSums(alpha*beta)


## ------------------------------------------------------------------
scaled_alpha <- matrix(ncol=2,nrow=nt)
ct <- vector(length=nt)
scaled_alpha[1,] <- prior*B[1,] # initialize
ct[1] <- 1/sum(scaled_alpha[1,]) # scaling factor
scaled_alpha[1,] <- ct[1]*scaled_alpha[1,] # rescale
for(t in 1:(nt-1)) { # recursion:
    scaled_alpha[t+1,] <- 
		(t(A)%*%scaled_alpha[t,])*B[t+1,] 
    ct[t+1] <- 1/sum(scaled_alpha[t+1,])
    scaled_alpha[t+1,] <- ct[t+1]*scaled_alpha[t+1,] 
}


## ----beta scaled---------------------------------------------------
scaled_beta <- matrix(ncol=2,nrow=nt)
scaled_beta[nt,] <- 1*ct[nt] # initialize
for(t in (nt-1):1) { # recursion:
    scaled_beta[t,] <- 
		(A%*%(B[t+1,]*scaled_beta[t+1,]))*ct[t]
}


## ----eval=FALSE----------------------------------------------------
## scaled_alpha*scaled_beta/ct


## ------------------------------------------------------------------
1/prod(ct) # likelihood
-sum(log(ct)) # log-likelihood


## ------------------------------------------------------------------
data(discrimination)
y <- discrimination$acc[1795:1805] # pick a single series
AONnlL <- function(par,y) {
  a12 <- par # free parameter
  nt <- length(y)
  prior <- c(1, 0) # initial state probability
  A <- matrix(c(a12, 0, 1 - a12, 1), ncol = 2) # transition
  B <- cbind(rep(.5, nt), as.numeric(y == "correct")) # dens
  # scaled forward recursions:
  alpha <- matrix(ncol = 2, nrow = nt)
  ct <- vector(length=nt)
  alpha[1, ] <- prior * B[1, ] # initialize
  ct[1] <- 1/sum(alpha[1,]) # scaling factor
  alpha[1,] <- ct[1] * alpha[1,] # rescale
  for (t in 1:(nt - 1)) { # recursion:
    alpha[t+1,] <- (t(A) %*% alpha[t, ]) * B[t + 1, ]
    ct[t+1] <- 1/sum(alpha[t+1,])
    alpha[t+1,] <- ct[t+1] * alpha[t+1,]
  }
  return(sum(log(ct))) # minus log-likelihood
}
opt <- optim(par=.8,fn=AONnlL,y=y,lower=0,upper=1,
             method="L-BFGS-B")
opt$par


## ----results='hide'------------------------------------------------
data(discrimination)
ntimes <- attr(discrimination,"ntimes")
m1 <- depmix(response=acc~1,nstates=2,family=binomial(),
  data=discrimination, instart=c(1,0), trstart=c(.8,.2,0,1), 
  respstart=c(.5,.95),ntimes=ntimes)
fm1 <- fit(m1,
  fixed=c(TRUE,TRUE,FALSE,FALSE,TRUE,TRUE,TRUE,FALSE),
  method="rsolnp")


## ----results='hide'------------------------------------------------
fm1EM <- fit(m1,emcontrol=em.control(random.start=FALSE))


## ----eval=FALSE----------------------------------------------------
## posterior(fm1EM, type="local")


## ----eval=FALSE----------------------------------------------------
## apply(forwardbackward(fm1EM)$gamma,1,which.max)


## ----eval=FALSE----------------------------------------------------
## posterior(fm1EM, type="global")


## ----fitsimplemodel,results='hide'---------------------------------
data(simplehmm)
set.seed(214)
mod1 <- depmix(obs~1,data=simplehmm,nstates=2,
	family=multinomial("identity"), inst=c(1,0),
	respst=c(.6,0,.4,0,.2,.8), trst=runif(4))
fm1 <- fit(mod1, emcontrol=em.control(random.start=FALSE))


## ----eval=FALSE----------------------------------------------------
## nsamples <- 1000
## SEsamples <- matrix(0,ncol=npar(fm1),nrow=nsamples)
## for(i in 1:nsamples) {
##   sample <- simulate(fm1)
##   fmsam <- fit(sample,
##                emcontrol=em.control(random.start=FALSE))
##   SEsamples[i,] <- getpars(fmsam)
## }


## ----parametricBootstrapHMM----------------------------------------
data(SEsamples)
bootses <- apply(SEsamples,2,sd) # bootstrap s.e.
bootses[which(bootses==0)] <- NA # no s.e for fixed pars
ses <- cbind(standardError(fm1),bootses)
ses


## ------------------------------------------------------------------
data(discrimination)
y_miss <- discrimination$acc[1795:1805] # pick single series
y_miss[6] <- NA # set missing value
nt <- length(y)
prior <- c(1,0) # prior probabilities
A <- matrix(c(.8,0,0.2,1),ncol=2) # transition matrix
# observation density
B_miss <- cbind(rep(.5,nt),as.numeric(y == "correct"))
B_miss[is.na(y_miss),] <- 1 # set to 1 for missing value(s)
scaled_alpha_miss <- matrix(ncol=2,nrow=nt)
# scaled forward filtering
c_miss <- vector(length=nt)
scaled_alpha_miss[1,] <- prior*B_miss[1,] # initialize
c_miss[1] <- 1/sum(scaled_alpha_miss[1,])
scaled_alpha_miss[1,] <- ct[1]*scaled_alpha_miss[1,]
for(t in 1:(nt-1)) {
  scaled_alpha_miss[t+1,] <- 
  	(t(A)%*%scaled_alpha_miss[t,])*B_miss[t+1,]
  c_miss[t+1] <- 1/sum(scaled_alpha_miss[t+1,])
  scaled_alpha_miss[t+1,] <- 
  	c_miss[t+1]*scaled_alpha_miss[t+1,]
}


## ----fig-filter-missing, echo=FALSE, fig.height=.9*5, fig.width=.9*7,out.width=".8\\textwidth"----
plot(c(1,11),c(0,1),type="n",ylab=expression(P(S[t] == 2)),xlab=expression(t),frame=FALSE)
points(1:11,as.numeric(y=="correct"))
lines(1:11,scaled_alpha[,2])
lines(1:11,scaled_alpha_miss[,2],lty=2)
legend(x=6.5,y=.3,lty=c(1,2),legend=c("filtering","filtering (missing data)"),bty="n")


## ----missingness-simulation-settings,echo=FALSE--------------------
require(kableExtra)

nsim <- 1000
nrep <- 100
nt <- 50


## ----missingness-simulation-data,echo=FALSE,message=FALSE----------
data("MAR_simulation_results")
data("MNAR_simulation_results")


## ----MAR_simulation_table,echo=FALSE,results='asis'----------------
options(knitr.kable.NA = '-')
      kable(MAR_simulation_results,
            format = "latex", 
            booktabs = T, 
            col.names=c("parameter","true value","mean", "SD", "MAE", "mean", "SD", "MAE", "rel. MAE"),
            caption = "Results of Simulation 1 (MAR). For both models, the mean and standard deviation (SD) of the parameter estimates are given, as well as the mean absolute error (average absolute deviation between the estimates and true values). The relative MAE (rel. MAE) is the MAE of the MNAR model divided by the MAE of the MAR model.",
            label = "missingsim1",
            escape=FALSE,
            digits = 3,
            linesep = c('', '', '\\addlinespace')) %>%
        kable_styling() %>%
          add_header_above(c(" " = 1, " " = 1, "MAR model" = 3, "MNAR model" = 3, " " = 1))


## ----MNAR_simulation_table,echo=FALSE,results='asis'---------------
options(knitr.kable.NA = '-')
      kable(MNAR_simulation_results, 
            format = "latex", 
            booktabs = T, 
            col.names=c("parameter","true value","mean", "SD", "MAE", "mean", "SD", "MAE", "rel. MAE"),
            caption = "Results of Simulation 2 (MNAR). For both models, the mean and standard deviation (SD) of the parameter estimates are given, as well as the mean absolute error (average absolute deviation between the estimates and true values). The relative MAE (rel. MAE) is the MAE of the MNAR model divided by the MAE of the MAR model.",
            label="missingsim2",
            escape=FALSE,
            digits = 3,
            linesep = c('', '', '\\addlinespace')) %>%
        kable_styling() %>%
          add_header_above(c(" " = 1, " " = 1, "MAR model" = 3, "MNAR model" = 3, " " = 1))

