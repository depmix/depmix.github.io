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
opts_chunk$set(cache.path = "cache/ch7/", fig.path = "figure/ch7/")


## ----fit-higher-order-HMM, results='hide'--------------------------
data("discrimination")
ntim <- attr(discrimination,"ntimes") # timeseries lengths
set.seed(2)
# define and fit first-order model
fomod <- depmix(acc~1,family=multinomial("identity"),
    data=discrimination,nstates=2,ntimes=ntim)
fomod <- fit(fomod)
# define and fit second-order model
somod <- depmix(acc~1,family=multinomial("identity"),
    data=discrimination,nstates=4,ntimes=ntim)
# set initial parameter values
pars <- getpars(somod)
pars[5:8] <- pars[13:16] <- c(.5,.5,0,0)
pars[9:12] <- pars[17:20] <- c(0,0,.5,.5)
pars[c(21,25)] <- getpars(fomod)[7]
pars[c(22,26)] <- getpars(fomod)[8]
pars[c(23,27)] <- getpars(fomod)[9]
pars[c(24,28)] <- getpars(fomod)[10]
somod <- setpars(somod,pars)
# define equality constraints
equal <- as.numeric(pars!=0)
equal[c(1,3)] <- 2
equal[c(2,4)] <- 3
equal[c(21,25)] <- 4
equal[c(23,27)] <- 5
somod <- fit(somod,equal=equal)


## ----summary-higher-order-HMM--------------------------------------
summary(somod)


## ----echo=FALSE,eval=FALSE-----------------------------------------
## fo_vit <- viterbi(fomod)[,1]
## so_vit <- viterbi(somod)[,1]
## so_vit <- replace(so_vit,so_vit==3,1)
## so_vit <- replace(so_vit,so_vit==4,2)


## ------------------------------------------------------------------
require(hmmr)
data(conservation)
set.seed(12345)
m2 <- mix(r1~1,data=conservation,nstates=2,
	family=gaussian())
fm2_cem <- fit(m2,
	emcontrol=em.control(classification="hard"))
summary(fm2_cem)


## ------------------------------------------------------------------
set.seed(12345)
# em.control(classification="soft") default
fm2_em <- fit(m2)
summary(fm2_em)


## ----fit-FFBS-BinomialNormal-speed---------------------------------
data(speed)
set.seed(4)
hyperPars <- list(norm_invsigma_scale=.01,
  norm_invsigma_shape=.01,norm_mu_sca=.01)
mcmc_samples <- list()
for(i in 1:4) {
  mcmc_samples[[i]] <- FFBS_BinomialNormal(speed$corr,
    speed$rt, nstates=2, ntimes=c(168,134,137),
    niter=1000, hyperPars = hyperPars)
}


## ----traceplot-FFBS-BinomialNormal-speed,echo=FALSE,fig.width=6,fig.height=7,out.width=".8\\textwidth"----
my_mcmc_plot <- function(parameter,mcmc_list,niter,column=1,row=1,xlab,ylab,transform=function(x){x}) {
  if(missing(xlab)) xlab <- "Iteration"
  if(missing(ylab)) ylab <- parameter
  all_values <- matrix(0.0,ncol=length(mcmc_samples),nrow=niter)
  for(i in 1:length(mcmc_samples)) {
    if(is.matrix(mcmc_samples[[i]][[parameter]])) {
      all_values[,i] <- transform(mcmc_samples[[i]][[parameter]][,column]) 
    } else if(is.array(mcmc_samples[[i]][[parameter]])) {
      all_values[,i] <- transform(mcmc_samples[[i]][[parameter]][,column,row])
    } else {
      all_values[,i] <- transform(mcmc_samples[[i]][[parameter]])
    }
  }
  plot(c(1,niter),c(min(all_values),max(all_values)),type="n",xlab=xlab,ylab=ylab,frame=FALSE)
  for(i in 1:length(mcmc_samples)) {
    lines(1:niter,all_values[,i],lty=1,col=rgb(0,0,0,alpha=.5))
  }
}
layout(matrix(1:6,ncol=2,byrow=TRUE))
par(mar=c(4,4,1,1),cex.lab=1.5)
my_mcmc_plot("mu",mcmc_samples,niter=1000,column=1,ylab=expression(mu[1]),xlab="")
my_mcmc_plot("mu",mcmc_samples,niter=1000,column=2,ylab=expression(mu[2]),xlab="")
my_mcmc_plot("sigma",mcmc_samples,niter=1000,column=1,ylab=expression(sigma[1]),transform=sqrt,xlab="")
my_mcmc_plot("sigma",mcmc_samples,niter=1000,column=2,ylab=expression(sigma[2]),transform=sqrt,xlab="")
my_mcmc_plot("pcor",mcmc_samples,niter=1000,column=1,ylab=expression(phi[1]))
my_mcmc_plot("pcor",mcmc_samples,niter=1000,column=2,ylab=expression(phi[2]))


## ----scatterplot-FFBS-BinomialNormal-speed, echo=FALSE,fig.width=7,fig.height=8, out.width=".8\\textwidth"----
layout(matrix(1:6,ncol=2))
par(mar=c(4,4.5,2,1),cex.lab=1.5)
tmp <- rbind(
  mcmc_samples[[1]]$mu[201:1000,],
  mcmc_samples[[2]]$mu[201:1000,],
  mcmc_samples[[3]]$mu[201:1000,],
  mcmc_samples[[4]]$mu[201:1000,]
)
plot(tmp[,1],tmp[,2],xlab=expression(mu[1]),ylab=expression(mu[2]),col=rgb(0,0,0,alpha=.3),frame=FALSE)
plot(density(tmp[,1]),main=expression(mu[1]),frame=FALSE)
plot(density(tmp[,2]),main=expression(mu[2]),frame=FALSE)
tmp <- rbind(
  mcmc_samples[[1]]$pcor[201:1000,],
  mcmc_samples[[2]]$pcor[201:1000,],
  mcmc_samples[[3]]$pcor[201:1000,],
  mcmc_samples[[4]]$pcor[201:1000,]
)
plot(tmp[,1],tmp[,2],xlab=expression(phi[1]),ylab=expression(phi[2]),col=rgb(0,0,0,alpha=.3),frame=FALSE)
plot(density(tmp[,1]),main=expression(phi[1]),frame=FALSE)
plot(density(tmp[,2]),main=expression(phi[2]),frame=FALSE)



## ----echo=FALSE,results='asis'-------------------------------------
tab_stats <- function(x) {
  c(mean(x),sd(x),quantile(x,c(.025,.5,.975)))
}

# "note: relabel chains 1 and 3 so that mu[1] > mu[2]" 

tmp <- rbind(
  mcmc_samples[[1]]$init[201:1000,c(2,1)],
  mcmc_samples[[2]]$init[201:1000,],
  mcmc_samples[[3]]$init[201:1000,c(2,1)],
  mcmc_samples[[4]]$init[201:1000,]
)
tab <- tab_stats(tmp[,1])
tab <- rbind(tab,tab_stats(tmp[,2]))

tmp <- rbind(
  mcmc_samples[[1]]$transition[201:1000,2,c(2,1)],
  mcmc_samples[[2]]$transition[201:1000,1,],
  mcmc_samples[[3]]$transition[201:1000,2,c(2,1)],
  mcmc_samples[[4]]$transition[201:1000,1,]
)
tab <- rbind(tab,tab_stats(tmp[,1]))
tab <- rbind(tab,tab_stats(tmp[,2]))
tmp <- rbind(
  mcmc_samples[[1]]$transition[201:1000,1,c(2,1)],
  mcmc_samples[[2]]$transition[201:1000,2,],
  mcmc_samples[[3]]$transition[201:1000,1,c(2,1)],
  mcmc_samples[[4]]$transition[201:1000,2,]
)
tab <- rbind(tab,tab_stats(tmp[,1]))
tab <- rbind(tab,tab_stats(tmp[,2]))

tmp <- rbind(
  mcmc_samples[[1]]$pcor[201:1000,c(2,1)],
  mcmc_samples[[2]]$pcor[201:1000,],
  mcmc_samples[[3]]$pcor[201:1000,c(2,1)],
  mcmc_samples[[4]]$pcor[201:1000,]
)
tab <- rbind(tab,tab_stats(tmp[,1]))
tab <- rbind(tab,tab_stats(tmp[,2]))

tmp <- rbind(
  mcmc_samples[[1]]$mu[201:1000,c(2,1)],
  mcmc_samples[[2]]$mu[201:1000,],
  mcmc_samples[[3]]$mu[201:1000,c(2,1)],
  mcmc_samples[[4]]$mu[201:1000,]
)
tab <- rbind(tab,tab_stats(tmp[,1]))
tab <- rbind(tab,tab_stats(tmp[,2]))

tmp <- rbind(
  mcmc_samples[[1]]$sigma[201:1000,c(2,1)],
  mcmc_samples[[2]]$sigma[201:1000,],
  mcmc_samples[[3]]$sigma[201:1000,c(2,1)],
  mcmc_samples[[4]]$sigma[201:1000,]
)
tab <- rbind(tab,tab_stats(sqrt(tmp[,1])))
tab <- rbind(tab,tab_stats(sqrt(tmp[,2])))

tab <- as.data.frame(tab)

parnames <- c(expression(pi[1]),expression(pi[2]),expression(a[11]),expression(a[12]),expression(a[21]),expression(a[22]),expression(phi[1]),expression(phi[2]),expression(mu[1]),expression(mu[2]),expression(sigma[1]),expression(sigma[2]))

parnames <- c("$\\pi_1$","$\\pi_2$","$a_{11}$","$a_{12}$","$a_{21}$","$a_{22}$","$\\phi_{1}$","$\\phi_{2}$","$\\mu_{1}$","$\\mu_{2}$","$\\sigma_{1}$","$\\sigma_{2}$")

rownames(tab) <- parnames
colnames(tab) <- c("mean","SD","2.5\\%","50\\%","97.5\\%")

knitr::kable(tab,format="latex",escape=FALSE,digits=3,booktabs = TRUE, linesep="")


## ----ffbs-samples-init-trans, echo=FALSE, fig.height=3, fig.width=7, out.width=".8\\textwidth"----
tmp <- rbind(
  mcmc_samples[[1]]$init[201:1000,c(2,1)],
  mcmc_samples[[2]]$init[201:1000,],
  mcmc_samples[[3]]$init[201:1000,c(2,1)],
  mcmc_samples[[4]]$init[201:1000,]
)
pi1 <- tmp[,1]

tmp <- rbind(
  mcmc_samples[[1]]$transition[201:1000,2,c(2,1)],
  mcmc_samples[[2]]$transition[201:1000,1,],
  mcmc_samples[[3]]$transition[201:1000,2,c(2,1)],
  mcmc_samples[[4]]$transition[201:1000,1,]
)
a11 <- tmp[,1]
a12 <- tmp[,2]
tmp <- rbind(
  mcmc_samples[[1]]$transition[201:1000,2,c(2,1)],
  mcmc_samples[[2]]$transition[201:1000,1,],
  mcmc_samples[[3]]$transition[201:1000,2,c(2,1)],
  mcmc_samples[[4]]$transition[201:1000,1,]
)
a21 <- tmp[,1]
a22 <- tmp[,2]

par(mar=c(4,4,1,1))
layout(matrix(1:2,ncol=2))
plot(density(pi1),xlim=c(0,1),xlab=expression(pi[1]),main="",frame=FALSE)
abline(h=1,lty=2)
plot(density(a11),xlim=c(0,1),xlab=expression(alpha[11]),main="",ylab="",frame=FALSE)
abline(h=1,lty=2)

