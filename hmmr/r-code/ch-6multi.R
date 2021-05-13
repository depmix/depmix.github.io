## ----setup, echo=FALSE, results='hide', message = FALSE, warning = FALSE---------------------------------------
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



## ----include = FALSE-------------------------------------------------------------------------------------------
opts_chunk$set(cache.path = "cache/ch6/", fig.path = "figure/ch6/")



## ----ch6-init,echo=FALSE---------------------------------------------------------------------------------------
require("hmmr")


## ----balance8learndevelopch6, echo=FALSE, fig.height=.8*5, fig.width=.8*7--------------------------------------
library(hmmr)
data(balance8)
par(mar=c(5,4,1,1))
balance8$pc <- balance8$totalCor/balance8$totalTrials

pcgr <- aggregate(balance8$pc, list(balance8$group), mean, na.rm=TRUE)

plot(1:8,pcgr[,2],ty="b", frame=FALSE, 
xlab="measurement/group", ylab="total proportion correct") # main="Learning and development on the balance scale task"

pc <- matrix(balance8$pc,ncol=8, byrow=TRUE)

lines(1:8,colMeans(pc, na.rm=TRUE), type="b",lty=2)

legend("bottomright",inset=.0,legend=c("learning","development"),lty=c(2,1),bty="n")


## ----balance8glm,linewidth=62----------------------------------------------------------------------------------
summary(glm(cbind(totalCor,totalTrials-totalCor)~scale(age)+ 
            sex+I(time-1), family=binomial, data=balance8))


## ----balance8-sumscoreslearn, echo=FALSE, fig.height=.8*4.5, fig.width=.8*7,out.width=".9\\textwidth"----------
require("hmmr")
data(balance8)
par(mar=c(4,4,3,0))

layout(matrix(1:6,nrow=2,ncol=3,byrow=TRUE))

dt <- balance8[balance8$time==1,]

up=1
barplot((table(dt$dc)/nrow(dt)),ylim=c(0,up),main="distance",ylab="Proportion")
barplot((table(dt$cwc)/nrow(dt)),ylim=c(0,up),main="conflict-weight")
barplot((table(dt$cdc)/nrow(dt)),ylim=c(0,up),main="conflict-distance")

dt <- balance8[balance8$time==8,]

up=1
barplot((table(dt$dc)/nrow(dt)),ylim=c(0,up),main="distance",ylab="Proportion",xlab="Sum score")
barplot((table(dt$cwc)/nrow(dt)),ylim=c(0,up),main="conflict-weight",xlab="Sum score")
barplot((table(dt$cdc)/nrow(dt)),ylim=c(0,up),main="conflict-distance",xlab="Sum score")



## ----balance8-sumscoresdevelop, echo=FALSE, fig.height=.8*4.5, fig.width=.8*7,out.width=".9\\textwidth"--------
require("hmmr")
data(balance8)

par(mar=c(4,4,3,0))

layout(matrix(1:6,nrow=2,ncol=3,byrow=TRUE))

dt <- balance8[balance8$school=="primary",]

up=1
barplot((table(dt$dc)/nrow(dt)),ylim=c(0,up),main="distance",ylab="Proportion")
barplot((table(dt$cwc)/nrow(dt)),ylim=c(0,up),main="conflict-weight")
barplot((table(dt$cdc)/nrow(dt)),ylim=c(0,up),main="conflict-distance")

dt <- balance8[balance8$school=="secondary",]

up=1
barplot((table(dt$dc)/nrow(dt)),ylim=c(0,up),main="distance",ylab="Proportion",xlab="Sum score")
barplot((table(dt$cwc)/nrow(dt)),ylim=c(0,up),main="conflict-weight",xlab="Sum score")
barplot((table(dt$cdc)/nrow(dt)),ylim=c(0,up),main="conflict-distance",xlab="Sum score")


## ----fit-balance8models, eval=FALSE, linewidth=62--------------------------------------------------------------
## 
## library(hmmr)
## data(balance8)
## 
## set.seed(12)
## 
## hm3id <- depmix(
## 	list(cbind(wc,wi)~1,cbind(dc,di)~1,cbind(cwc,cwi)~1,
## 		cbind(cdc,cdi)~1,cbind(cbc,cbi)~1),
## 	data=balance8,
## 	family=list(binomial("identity"),
## 		binomial("identity"),
## 		binomial("identity"),
## 		binomial("identity"),
## 		binomial("identity")),
## 	ntimes=rep(8,1004), ns=3,
## 	respst=rep(0.5,15))
## 	
## fhm3id <- multistart(hm3id)
## 
## hm4id <- depmix(list(cbind(wc,wi)~1,cbind(dc,di)~1,
## 	cbind(cwc,cwi)~1,cbind(cdc,cdi)~1,cbind(cbc,cbi)~1),
## 	data=balance8,
## 	family=list(binomial("identity"),
## 		binomial("identity"),
## 	 	binomial("identity"),
## 		binomial("identity"),
## 		binomial("identity")),
## 	ntimes=rep(8,1004), ns=4,
## 	respst=rep(0.5,20))
## 	
## fhm4id <- multistart(hm4id)


## ----load-balance8models, echo=FALSE, results='hide'-----------------------------------------------------------
require(hmmr)
data(balance8)
data(balance8pars)
# reconstruct the list of fitted models from the parameters
balance8models <- list()
for(i in 1:length(balance8pars)) {
  # define model
  mod <- depmix(list(cbind(wc,wi)~1,cbind(dc,di)~1,cbind(cwc,cwi)~1,
    cbind(cdc,cdi)~1,cbind(cbc,cbi)~1), 
    data=balance8, family=list(binomial("identity"),binomial("identity"),
    binomial("identity"),binomial("identity"),binomial("identity")), 
    ntimes=rep(8,1004), ns=attr(balance8pars[[i]],"nstates"))
  # set the parameters to the estimated ones
  mod <- setpars(mod, balance8pars[[i]])
  # convert to a depmix.fitted object
  mod <- as(mod,"depmix.fitted")
  # set slots of depmix.fitted object
  mod@message <- attr(balance8pars[[i]],"message")
  mod@conMat <- attr(balance8pars[[i]],"conMat")
  mod@lin.upper <- attr(balance8pars[[i]],"lin.upper")
  mod@lin.lower <- attr(balance8pars[[i]],"lin.lower")
  mod@posterior <- viterbi(mod)
  # add to list of models
  balance8models[[i]] <- mod
}
names(balance8models) <- c("fhm3id","fhm4id","fhm5id","fhm6id","fhm7id","fhm8id")

resp5 <- summary(balance8models[[3]],"response")
resp6 <- summary(balance8models[[4]],"response")
resp7 <- summary(balance8models[[5]],"response")
resp8 <- summary(balance8models[[6]],"response")



## ----balance8-profiles, echo=FALSE, fig.height=7, fig.width=7,out.width=".8\\textwidth"------------------------
layout(matrix(1:4,nrow=2,ncol=2,byrow=TRUE))
matplot(t(resp5),ty="b",frame=FALSE, xlab="w-d-cw-cd-cb", main="5-state",col="black",ylab="p(correct)")
matplot(t(resp6),ty="b",frame=FALSE, xlab="w-d-cw-cd-cb", main="6-state",col="black",ylab="")
matplot(t(resp7),ty="b",frame=FALSE, xlab="w-d-cw-cd-cb", main="7-state",col="black",ylab="p(correct)")
matplot(t(resp8),ty="b",frame=FALSE, xlab="w-d-cw-cd-cb", main="8-state",col="black",ylab="")


## ----model-summary, echo=FALSE---------------------------------------------------------------------------------
summary(balance8models[[6]], which="transition")


## ----balance8-learning-regression, echo=FALSE, fig.height=.9*3.5, fig.width=.9*8,out.width=".8\\textwidth"-----
par(mar=c(4,4,3,1))
pst <- posterior(balance8models[[6]], type = "global")
balance8$pst <- pst
balance8$prevpst <- c(0,pst[-length(pst)])
balance8$trans <- 10*balance8$prevpst+balance8$pst

br <- balance8[which(balance8$time>1),]
ids <- br[which(br$trans==83),"id"]

layout(matrix(1:2,ncol=2))
cwcorrect <- matrix(balance8[balance8$id %in% ids, "cwc"],ncol=8,byrow=TRUE) 
plot(as.ts(colMeans(cwcorrect,na.rm=TRUE)/5),ylim=c(0,1),frame=FALSE,ylab="p(correct)", main = "cw items",xlab="Occasion")

dcorrect <- matrix(balance8[balance8$id %in% ids, "dc"],ncol=8,byrow=TRUE) 
plot(as.ts(colMeans(dcorrect,na.rm=TRUE)/5),ylim=c(0,1),frame=FALSE, ylab="p(correct)", main="d items", xlab="Occasion")



## ----balance8-regression, echo=FALSE, fig.height=.9*3.5, fig.width=.9*4,out.width=".4\\textwidth"--------------
par(mar=c(4,4,3,1))

pst <- posterior(balance8models[[6]], type="global")
balance8$pst <- pst
balance8$prevpst <- c(0,pst[-length(pst)])
balance8$trans <- 10*balance8$prevpst+balance8$pst

br <- balance8[which(balance8$time>1),]
ids <- br[which(br$trans==57),"id"]

cwcorrect <- matrix(balance8[balance8$id %in% ids, "cwc"],ncol=8,byrow=TRUE) 
plot(as.ts(colMeans(cwcorrect,na.rm=TRUE)/5),ylim=c(0,1),frame=FALSE,ylab="p(correct)",main="cw items")

balance8$pst[1] <- 3



## ----data-speed1, echo=FALSE-----------------------------------------------------------------------------------
data(speed1)
data(speed)


## ----data-speed, echo=FALSE------------------------------------------------------------------------------------
data(speed)
speed$pa2 <- speed$Pacc^2


## ----speed-lm, echo=TRUE---------------------------------------------------------------------------------------
fitlm <- manova(cbind(rt,corr)~Pacc+prev+I(Pacc^2),
                data=speed)
summary(fitlm)


## ----speed-lm-2, echo=TRUE-------------------------------------------------------------------------------------
summary.aov(fitlm)


## ----speed-hm2, echo=TRUE--------------------------------------------------------------------------------------
hm2 <- depmix(list(rt~1,corr~1), data=speed, nstates=2,
	family=list(gaussian(), multinomial("identity")), 
	ntimes=c(168,134,137))
set.seed(1)
fhm2 <- fit(hm2)
summary(fhm2)


## ----speed-hm2tr, echo=TRUE------------------------------------------------------------------------------------
hm2tr <- depmix(list(rt~1,corr~1), data=speed, nstates=2,
            family=list(gaussian(),multinomial("identity")), 
            ntimes=c(168,134,137), transition=~Pacc)
set.seed(1)
fhm2tr <- fit(hm2tr)


## ----speed-hm2-2, echo=TRUE------------------------------------------------------------------------------------
summary(fhm2tr,which="response") 


## ----speed-hm2tr-sum, echo=TRUE--------------------------------------------------------------------------------
summary(fhm2tr,which="transition") 


## ----speed-hm2tr-pars, echo=FALSE------------------------------------------------------------------------------
pars <- getpars(fhm2tr)


## ----speed-hm2tr-plot, echo=TRUE, eval=FALSE-------------------------------------------------------------------
## pars <- getpars(fhm2tr)
## logit1 <- function(value) plogis(pars[4]+value*pars[6])
## logit2 <- function(value) plogis(pars[8]+value*pars[10])
## plot(logit1, ylab="Probability", xlab="Pacc", lty = 1,
## 	axes=F)
## axis(1,c(0,0.5,1))
## axis(2,c(0,0.5,1),las=1)
## curve(logit2,lty=2,add=TRUE)
## legend("bottomright",
## 	leg=c("P(switch from FG to SC)","P(stay in SC)"),
## 	lty=c(1,2),bty="n")


## ----fig-speed-fitted-transition, echo=FALSE, fig.height=.8*5, fig.width=.8*7, out.width=".7\\textwidth"-------
pars <- getpars(fhm2tr)
logit1 <- function(value) plogis(pars[4]+value*pars[6])
logit2 <- function(value) plogis(pars[8]+value*pars[10])
plot(logit1, ylab="Probability", xlab="Pacc", lty = 1, 
	axes=F)
axis(1,c(0,0.5,1))
axis(2,c(0,0.5,1),las=1)
curve(logit2,lty=2,add=TRUE)
legend("bottomright",
	leg=c("P(switch from FG to SC)","P(stay in SC)"),
	lty=c(1,2),bty="n")


## ----speed-hm2trConstr, echo=TRUE------------------------------------------------------------------------------
pars <- getpars(fhm2tr)
pars[13] <- 0.5
pars[14] <- 0.5 
# set uniform initial probabilities
pars[c(1,2)] <- c(.5,.5) 
set.seed(1)
# randomize means
pars[c(11,15)] <- pars[c(11,15)] + rnorm(2,sd=.5)
hm2trConstr <- setpars(hm2tr,pars)


## ----speed-hm2trConstr-fit, echo=TRUE, warning=FALSE, linewidth=62---------------------------------------------
free <- c(1,1,rep(c(0,1),4),1,1,0,0,1,1,1,1)
fhm2trConstr <- fit(hm2trConstr,fixed=!free)


## ----speed-hm2trConstr-llratio, echo=TRUE----------------------------------------------------------------------
llratio(fhm2tr,fhm2trConstr)


## ----speed-hm2-scale, echo=TRUE, results='hide'----------------------------------------------------------------
hm2tr <- depmix(list(rt~1,corr~1), data=speed, nstates=2,
  family=list(gaussian(),multinomial("identity")), 
	ntimes=c(168,134,137), transition=~scale(Pacc))
set.seed(1)
fhm2tr <- fit(hm2tr)


## ----speed-hm2trConstr-2, echo=FALSE,results='hide'------------------------------------------------------------
pars <- getpars(fhm2tr)
pars[13] <- pars[14] <- 0.5
pars[c(1,2)] <- c(.5,.5) # set uniform initial probabilities
set.seed(1)
pars[c(11,15)] <- pars[c(11,15)] + rnorm(2,sd=.5) # randomize means
hm2trConstr <- setpars(hm2tr,pars)
free <- c(1,1,rep(c(0,1),4),1,1,0,0,1,1,1,1)
fhm2trConstr <- fit(hm2trConstr,fixed=!free)


## ----speed-hm2-hyst, echo=TRUE, results='hide'-----------------------------------------------------------------
pars <- getpars(fhm2tr)
pars[13] <- pars[14] <- 0.5
pars[6] <- pars[10] <- 3
pars[c(1,2)] <- c(.5,.5) # set uniform initial probabilities
set.seed(1)
# randomize means
pars[c(11,15)] <- pars[c(11,15)] + rnorm(2,sd=.5)
hyst <- setpars(hm2tr,pars)
# set fixed = 0 and free = 1 parameters
conpat <- c(1,1,rep(c(0,1),4),1,1,0,0,1,1,1,1)
# equality constraint on betas of transitions
conpat[6] <- conpat[10] <- 2
fhyst <- fit(hyst,equal=conpat)


## ----hyst-base-llr, echo=TRUE----------------------------------------------------------------------------------
llratio(fhm2trConstr,fhyst)


## ----speed-hm2-no-hyst1, echo=TRUE, results='hide'-------------------------------------------------------------
# reset earlier constrained parameters
pars[c(6,10)] <- getpars(fhm2tr)[c(6,10)] # reset values
conpat[c(6,10)] <- 1 # set as free
# equality constraint on intercepts of transitions
pars[4] <- pars[8] <- 1 # set values
conpat[4] <- conpat[8] <- 2 # set equality
nohyst1 <- setpars(hm2tr,pars) # assign initial values 
fnohyst1 <- fit(nohyst1,equal=conpat)


## ----speed-hm2-2-no-hyst2, echo=TRUE, results='hide'-----------------------------------------------------------
pars[6] <- pars[10] <- 3 # set values
conpat[6] <- conpat[10] <- 3 # set equality of betas
nohyst2 <- setpars(hm2tr,pars)
fnohyst2 <- fit(nohyst2,equal=conpat)


## ----speed-hyst-models-gof, echo=FALSE, results='asis'---------------------------------------------------------
require(xtable)
m<-list()
m[[1]]<-fhm2tr
m[[2]]<-fhm2
m[[3]]<-fhm2trConstr
m[[4]]<-fhyst
m[[5]]<-fnohyst1
m[[6]]<-fnohyst2
bics <- sapply(m,BIC)
aics <- sapply(m,AIC)
ll <- sapply(m,logLik)
dfs <- sapply(m,freepars)
llr2 <- llratio(m[[1]],m[[2]])
llr3 <- llratio(m[[1]],m[[3]])
llr4 <- llratio(m[[1]],m[[4]]) 
llr5 <- llratio(m[[1]],m[[5]])
llr6 <- llratio(m[[1]],m[[6]])
deltadf <- c(NA,llr2@df,llr3@df,llr4@df,llr5@df,llr6@df)
llr <- c(NA,llr2@value,llr3@value,llr4@value,llr5@value,llr6@value)
pval <- round(pchisq(llr,deltadf,lower=FALSE),3)
gof <- data.frame(model=c("fhm2tr","fhm2","fhm2trConstr","fhyst","fnohyst1","fnohyst2"),logLik=ll,npar=dfs,AIC=aics,BIC=bics,
		LR=llr,df=deltadf,p=pval)
xt <- xtable(gof, 
		caption="Goodness of fit statistics for \\code{speed} data models;
		model fhm2tr is the full model with \\code{Pacc} included as covariate; in
		model fhm2 \\code{Pacc} is not included as covariate; model fhm2trConstr implements
		the constraints of the initial state probabilities andthe accuracy
		parameters in the fast guessing state; model fhyst constrains the slopes of
		the influence of \\code{Pacc} to be equal in both states; model fnohyst1
		constrains the intercepts of \\code{Pacc} to be equal; model fnohyst2 combines
		both constraints from models fhyst and fnohyst1.",
		label="tab:gof-speed-hyst", 
		align="llrrrrrrr", 
		display=c("d","d","f","d","f","f","f","d","f")) 
print(xt, include.rownames=FALSE, caption.placement="top", table.placement = "th")


## ----speed-rescor, echo=FALSE, results='hide'------------------------------------------------------------------
speed$stateGaus <- posterior(fhyst, type="global")
require(polycor)
corr=hetcor(speed$rt[speed$state==2],speed$corr[speed$state==2])
rho=corr$correlations[1,2]
ste=corr$std.errors[1,2]
corr=hetcor(speed$rt[speed$state==1],speed$corr[speed$state==1])
rhoFG=corr$correlations[1,2]
steFG=corr$std.errors[1,2]


## ----speed-rescor-SC, linewidth=55-----------------------------------------------------------------------------
require(polycor)
hstate <- posterior(fhyst, type="global")
polyserial(speed$rt[hstate==2], speed$corr[hstate==2],
           ML=TRUE, std.err=TRUE)


## ----speed-rescor-SC-2, linewidth=55---------------------------------------------------------------------------
polyserial(speed$rt[hstate==1],speed$corr[hstate==1],
           ML=TRUE, std.err=TRUE)


## ----fig-speed-rescor, echo=FALSE, fig.height=4, fig.width=7,out.width=".8\\textwidth"-------------------------
layout(matrix(c(1,2),ncol=2))
plot(speed$corr[speed$state==1],speed$rt[speed$state==1],ylim=c(4.5,7.5), 
		main="Fast guessing state",ylab="RT (log ms)",frame.plot=FALSE, xlab="")
plot(speed$corr[speed$state==2],speed$rt[speed$state==2],ylim=c(4.5,7.5), 
		main="Stimulus controlled state",ylab="RT (log ms)",frame.plot=FALSE, xlab="")


## ----speed-rescor-mod, echo=TRUE-------------------------------------------------------------------------------
hm2rescor <- depmix(list(rt~corr,corr~1),data=speed,
	nstates=2,
	family=list(gaussian(),multinomial("identity")), 
	ntimes=c(168,134,137), transition=~scale(Pacc))


## ----speed-fit-rescor-mod, echo=FALSE, results='hide'----------------------------------------------------------
set.seed(1)
fhm2rescor <- fit(hm2rescor,verb=FALSE)
pars <- getpars(fhm2rescor)
pars[c(1,2)] <- .5
pars[6] <- pars[10] <- 2.5
pars[12] <- 0
pars[14] <- pars[15] <- 0.5
conpat <- c(1,1,
		rep(c(0,1),4),
		1,0,1,0,0,
		1,1,1,1,1) 
conpat[6] <- conpat[10] <- 2
hm2rescorCo <- setpars(fhm2rescor,pars)
fhm2rescorCo <- fit(hm2rescorCo,equal=conpat)
bic4b<-BIC(fhm2rescorCo)
llrat4b <- llratio(fhm2rescorCo,fhyst)


## ----speed-exGauss-def, echo=FALSE, results='hide'-------------------------------------------------------------
require(gamlss)
require(gamlss.dist)

setClass("exgaus", contains="response")
setGeneric("exgaus", function(y, x = NULL, pstart = NULL, fixed = NULL, ...) standardGeneric("exgaus"))

setMethod("exgaus", 
    signature(y="ANY"), 
    function(y,x=NULL,pstart=NULL,fixed=NULL, ...) {
				y <- matrix(y,length(y))
				if(length(x)>1&length(x)!=length(y)) stop("'x' and 'y' must be of 
						same length")
				if(is.null(x)) {x <- matrix(1); xx=FALSE}
				else xx=TRUE
				x <- as.matrix(x)
				parameters <- list()
				npar <- 3 
				if(xx) npar <- npar + ncol(x)
				if(is.null(fixed)) fixed <- as.logical(rep(0,npar))
				if(!is.null(pstart)) {
						if(length(pstart)!=npar) stop("length of 'pstart' must be ", npar)
						if(npar==3) {
								parameters$mu <- pstart[1]
								parameters$sigma <- log(pstart[2])
								parameters$nu <- log(pstart[3])
						} else {
								parameters$mu <- pstart[1:2]
								parameters$sigma <- log(pstart[3])
								parameters$nu <- log(pstart[4])
						}
				}
				mod <- new("exgaus",parameters=parameters,fixed=fixed,x=x,y=y,npar=npar)
				mod
		}
)

setMethod("show","exgaus",
    function(object) {
        cat("Model of type exgaus (see ?gamlss for details) \n")
        cat("Parameters: \n")
        cat("mu: ", object@parameters$mu, "\n")
        cat("sigma: ", object@parameters$sigma, "\n")
        cat("nu: ", object@parameters$nu, "\n")
    }
)

setMethod("dens","exgaus",
    function(object,log=FALSE) {
        dexGAUS(object@y, mu = predict(object), sigma = exp(object@parameters$sigma), nu = exp(object@parameters$nu), log = log)
    }
)

setMethod("getpars","response",
    function(object,which="pars",...) {
        switch(which,
            "pars" = {
                parameters <- numeric()
                parameters <- unlist(object@parameters)
                pars <- parameters
            },
            "fixed" = {
                pars <- object@fixed
            }
        )
        return(pars)
    }
)

setMethod("setpars","exgaus",
    function(object, values, which="pars", ...) {
        npar <- npar(object)
        if(length(values)!=npar) stop("length of 'values' must be",npar) # determine whether parameters or fixed constraints are being set
		nms <- names(object@parameters)
		switch(which,
		  "pars"= {
					if(npar(object)==3) {
							object@parameters$mu <- values[1]
							object@parameters$sigma <- values[2]
							object@parameters$nu <- values[3]
					} else {
							object@parameters$mu <- values[1:2]
							object@parameters$sigma <- values[3]
							object@parameters$nu <- values[4]
					}
		      },
					"fixed" = {
							object@fixed <- as.logical(values)
					}
			)
			names(object@parameters) <- nms
			return(object)
	}
)

setMethod("fit","exgaus",
    function(object,w) {
				if(missing(w)) w <- NULL
        y <- object@y
				x <- object@x
				if(length(x)==1) {
						fit <- gamlss(y~1,weights=w,family=exGAUS(),
								control=gamlss.control(n.cyc=100,trace=FALSE),
								mu.start=object@parameters$mu,
								sigma.start=exp(object@parameters$sigma),
								nu.start=exp(object@parameters$nu))
				} else {			
						fit <- gamlss(y~x,weights=w,family=exGAUS(),
								control=gamlss.control(n.cyc=100,trace=FALSE),
								mu.start=object@parameters$mu[1],
								sigma.start=exp(object@parameters$sigma),
								nu.start=exp(object@parameters$nu))
				}
				pars <- c(fit$mu.coefficients,fit$sigma.coefficients,fit$nu.coefficients)
				object <- setpars(object,pars)
				object
	}
)

setMethod("predict","exgaus", 
    function(object) {
        ret <- object@parameters$mu[1]
				if(length(object@x)>1) ret <- ret + object@x*object@parameters$mu[2]
        return(ret)
    }
)


## ----speed-exGauss, echo=FALSE, results='hide'-----------------------------------------------------------------
rModels <- list( list( # the full models
	exgaus(speed$rt, as.numeric(speed$corr), pstart=c(5,0,.1,.1)),
	GLMresponse(formula=corr~1,data=speed,family=multinomial("identity"),pstart=c(0.5,0.5))
	),list(
	exgaus(speed$rt, as.numeric(speed$corr)-1, pstart=c(6,0,.1,.1)),
	GLMresponse(formula=corr~1,data=speed,family=multinomial("identity"),pstart=c(.1,.9)) 
))

trstart=c(0.9,0.1,0.1,0.9)
transition <- list()
transition[[1]] <- transInit(~scale(speed$Pacc),nstates=2,pstart=c(trstart[1:2],0,0))
transition[[2]] <- transInit(~scale(speed$Pacc),nstates=2,pstart=c(trstart[3:4],0,0))

instart=c(.5,.5)
inMod <- transInit(~1,ns=2,ps=instart,data=data.frame(rep(1,3)),family=multinomial("identity"))

exg0 <- makeDepmix(response=rModels,transition=transition,prior=inMod,ntimes=c(168,134,137),homogeneous = FALSE)

fexg0 <- fit(exg0,emc=em.control(rand=FALSE))


rModels <- list( list( # restrict beta of corr on rt in fast guessing to zero by leaving it out
	exgaus(speed$rt, pstart=c(5,.1,.1)),
	GLMresponse(formula=corr~1,data=speed,family=multinomial("identity"),pstart=c(0.5,0.5))
	),list(
	exgaus(speed$rt, as.numeric(speed$corr)-1, pstart=c(6,0,.1,.1)),
	GLMresponse(formula=corr~1,data=speed,family=multinomial("identity"),pstart=c(.1,.9)) 
))

trstart=c(0.9,0.1,0.1,0.9)
transition <- list()
transition[[1]] <- transInit(~scale(speed$Pacc),nstates=2,pstart=c(trstart[1:2],0,0))
transition[[2]] <- transInit(~scale(speed$Pacc),nstates=2,pstart=c(trstart[3:4],0,0))

instart=c(.5,.5)
inMod <- transInit(~1,ns=2,ps=instart,data=data.frame(rep(1,3)),family=multinomial("identity"))

exg1<- makeDepmix(response=rModels,transition=transition,prior=inMod,ntimes=c(168,134,137),homogeneous = FALSE)

fexg1 <- fit(exg1,emc=em.control(rand=FALSE))

pars <- getpars(fexg1)
pars[1] <- 0
pars[2] <- 1
pars[4] <- 1
pars[6] <- pars[10] <- 2.5 # set beta's for pacc equal
pars[8] <- 2
pars[11] <- 5
pars[14] <- pars[15] <- 0.5 # set the guessing probs to 0.5
pars[16] <- 6

conpat <- c(0,0,
		rep(c(0,1),4),
		1,1,1,0,0,
		1,1,1,1,1,1) 
conpat[6] <- conpat[10] <- 2

exg2 <- setpars(exg1,pars)

fexg2 <- fit(exg2,equal=conpat) # constrained ExGaussian hysteresis model


## ----fig-speed-rtdist, echo=FALSE, fig.height=.9*5, fig.width=.9*7, out.width=".8\\textwidth"------------------
par(mar=c(4,4,3,1))
pc <- c(table(speed$stateGaus,speed$corr)/439)

pc <- rep(1,4)

pc1cor <- pc[3]
pc1inc <- pc[1]
pc2cor <- pc[4]
pc2inc <- pc[2]

gpars <- getpars(fhm2rescorCo)

gm1cor <- gpars[11]
gsd1cor <- gpars[13]
gm1inc <- gpars[11]
gsd1inc <- gpars[13]
gm2cor <- gpars[16]+1*gpars[17]
gsd2cor <- gpars[18]
gm2inc <- gpars[16]+0*gpars[17]
gsd2inc <- gpars[18]

c1 <- function(x) {pc1inc*dnorm(x,gm1inc,gsd1inc)}
c2 <- function(x) {pc2inc*dnorm(x,gm2inc,gsd2inc)}
c3 <- function(x) {pc1cor*dnorm(x,gm1cor,gsd1cor)}
c4 <- function(x) {pc2cor*dnorm(x,gm2cor,gsd2cor)}

layout(matrix(c(1:4),ncol=2))
truehist(speed$rt[speed$stateGaus==1&speed$corr=="inc"],main="Fast Guessing - incorrect",
		br=seq(from=4.5,to=7.5,by=0.15),col=0,xlab="", ylab="Density")
curve(c1,add=TRUE)	

truehist(speed$rt[speed$stateGaus==2&speed$corr=="inc"],main="Stimulus controlled - incorrect",
		br=seq(from=4.5,to=7.5,by=0.15),col=0,xlab="log RT", ylab="Density")
curve(c2,add=TRUE)

truehist(speed$rt[speed$stateGaus==1&speed$corr=="cor"],main="Fast Guessing - correct",
		br=seq(from=4.5,to=7.5,by=0.15),col=0,xlab="")
curve(c3,add=TRUE)

truehist(speed$rt[speed$stateGaus==2&speed$corr=="cor"],main="Stimulus controlled - correct",
		br=seq(from=4.5,to=7.5,by=0.15),col=0,xlab="log RT")
curve(c4,add=TRUE)



## ----fig-exgaus-distpars, echo=FALSE, results='hide'-----------------------------------------------------------
speed$stateExG <- posterior(fexg2, type="global")
pc <- c(table(speed$stateExG,speed$corr)/439)# exgaus model distributies
pc <- rep(1,4)
pc1cor <- pc[3]
pc1inc <- pc[1]
pc2cor <- pc[4]
pc2inc <- pc[2]

exgpars <- getpars(fexg2)
gm1inc <- exgpars[11]
gs1inc <- exgpars[12]
gn1inc <- exgpars[13]
gm1cor <- exgpars[11]
gs1cor <- exgpars[12]
gn1cor <- exgpars[13]
gm2inc <- exgpars[16] 
gs2inc <- exgpars[18]
gn2inc <- exgpars[19]
gm2cor <- exgpars[16] + exgpars[17]
gs2cor <- exgpars[18]
gn2cor <- exgpars[19]

c1 <- function(x) {pc1inc*dexGAUS(x,gm1inc,exp(gs1inc),exp(gn1inc))}
c2 <- function(x) {pc1inc*dexGAUS(x,gm2inc,exp(gs2inc),exp(gn2inc))}
c3 <- function(x) {pc1inc*dexGAUS(x,gm1cor,exp(gs1cor),exp(gn1cor))}
c4 <- function(x) {pc1inc*dexGAUS(x,gm2cor,exp(gs2cor),exp(gn2cor))}

exb <- 0.15
yl <- 2.7


## ----fig-speed-exgaus, echo=FALSE, fig.height=.9*5, fig.width=.9*7, out.width=".8\\textwidth"------------------
par(mar=c(4,4,3,1))
layout(matrix(c(1:4),ncol=2))
truehist(speed$rt[speed$stateExG==1&speed$corr=="inc"],main="Fast Guessing - incorrect",
		br=seq(from=4.5,to=7.5,by=exb),col=0,xlab="", ylim=c(0,yl), ylab="Density")
curve(c1,add=TRUE)	

truehist(speed$rt[speed$stateExG==2&speed$corr=="inc"],main="Stimulus controlled - incorrect",
		br=seq(from=4.5,to=7.5,by=exb),col=0,xlab="log RT", ylim=c(0,yl), ylab="Density")
curve(c2,add=TRUE)

truehist(speed$rt[speed$stateExG==1&speed$corr=="cor"],main="Fast Guessing - correct",
		br=seq(from=4.5,to=7.5,by=exb),col=0,xlab="", ylim=c(0,yl))
curve(c3,add=TRUE)

truehist(speed$rt[speed$stateExG==2&speed$corr=="cor"],main="Stimulus controlled - correct",
		br=seq(from=4.5,to=7.5,by=exb),col=0,xlab="log RT", ylim=c(0,yl))
curve(c4,add=TRUE)


## ----fit-IGT-hmm-1, results='hide'-----------------------------------------------------------------------------
data(IGT)
set.seed(345)
indep2 <- depmix(response=list(deck~1,wager~1), data=IGT,
  family=rep(list(multinomial("identity")),2), nstates=2,
  ntimes=rep(100,30))
findep2 <- fit(indep2)
indep3 <- depmix(response=list(deck~1,wager~1), data=IGT,
  family=rep(list(multinomial("identity")),2), nstates=3,
  ntimes=rep(100,30))
findep3 <- fit(indep3)


## ----IGT-fmod3a-summary----------------------------------------------------------------------------------------
summary(findep3)


## ----fit-IGT-hmm-2, results='hide'-----------------------------------------------------------------------------
set.seed(45)
dep2 <- depmix(response=list(deck~1,wager~gdeck+fdeck), 
	family=list(multinomial("identity"),binomial()), 
	data=IGT, nstates=2, ntimes=rep(100,30))
fdep2 <- fit(dep2)


## ----fit-IGT-hmm-3, echo=FALSE,results='hide'------------------------------------------------------------------
dep3 <- depmix(response=list(deck~1,wager~gdeck+fdeck),data=IGT,
    family=list(multinomial("identity"),binomial()),nstates=3,
    ntimes=rep(100,30))
fdep3 <- fit(dep3)


## ----IGT-gof, echo=FALSE, results='asis'-----------------------------------------------------------------------
require(xtable)
m<-list()
m[[1]]<-findep2
m[[2]]<-findep3
m[[3]]<-fdep2
m[[4]]<-fdep3
bics <- sapply(m,BIC)
aics <- sapply(m,AIC)
ll <- sapply(m,logLik)
dfs <- sapply(m,freepars)
gof <- data.frame(model=c("findep2","findep3","fdep2","fdep3"),logLik=ll,npar=dfs,AIC=aics,BIC=bics)
xt <- xtable(gof, 
		caption="Goodness of fit statistics for \\code{IGT} data models;
		model findep2 and findep3 are the 2 and 3-state models with independent wagering; 
    model fdep2 and fdep3 are the 2 and 3-state models wth deck-dependent wagering.",
		label="tab:gof-IGT", 
		align="llrrrr", 
		display=c("d","s","f","d","f","f")) 
print(xt, include.rownames=FALSE, caption.placement="top", table.placement = "tbh")


## ----IGT-fmod3b-results----------------------------------------------------------------------------------------
summary(fdep3,which="response")

## ----IGT-wager-probs,echo=FALSE--------------------------------------------------------------------------------
prs1 <- getpars(fdep3)[17:19]
prs2 <- getpars(fdep3)[24:26]
prs3 <- getpars(fdep3)[31:33]
prd1 <- getpars(fdep3)[13:16]
prd2 <- getpars(fdep3)[20:23]
prd3 <- getpars(fdep3)[27:30]
mm <- rbind(c(1,0,1),c(1,0,0),c(1,1,1),c(1,1,0))
probs1 <- round(1/(1+exp(-1*(mm %*% prs1))),3)
probs2 <- round(1/(1+exp(-1*(mm %*% prs2))),3)
probs3 <- round(1/(1+exp(-1*(mm %*% prs3))),3)


## ----echo=FALSE------------------------------------------------------------------------------------------------
se <- standardError(fdep3)
conf <- confint(fdep3)
mtab <- round(cbind(se,conf[,c(3,4)])[c(17:19,24:26,31:33),-2],3)


## --------------------------------------------------------------------------------------------------------------
llratio(fdep3,findep3)


## --------------------------------------------------------------------------------------------------------------
summary(fdep3,which="prior")


## ----IGT-posterior-plot,echo=FALSE,fig.width=6.5,fig.height=5.2,out.width=".8\\textwidth"----------------------
post <- posterior(fdep3, type="smoothing")
post <- aggregate(post~IGT$trial,FUN=mean)
plot(c(1,100),c(0,1),type="n",xlab="trial",ylab=expression(p(S[t] == i)))
lines(1:100,post[,2],lty=1)
lines(1:100,post[,3],lty=2)
lines(1:100,post[,4],lty=3)
legend(100,1,lty=1:3,legend=c("S1","S2","S3"),xjust=1,yjust=1,bty="n")


## --------------------------------------------------------------------------------------------------------------
summary(fdep3,which="transition")


## ----IGT-mod3c-------------------------------------------------------------------------------------------------
IGT$prevloss <- c(0,IGT$loss[-nrow(IGT)]) > 0
IGT$prevloss[IGT$trial==1] <- FALSE
set.seed(56)
dep3cov <- depmix(response=list(deck~1,wager~gdeck+fdeck),
  data=IGT,
  family=list(multinomial("identity"),binomial()),
  nstates=3,transition=~prevloss,ntimes=rep(100,30))
fdep3cov <- fit(dep3cov)


## ----IGT-llratio-fmod3b-fmod3c---------------------------------------------------------------------------------
llratio(fdep3cov,fdep3)


## ----echo=FALSE------------------------------------------------------------------------------------------------
pts1 <- matrix(getpars(fdep3cov)[4:9],ncol=2)
pts2 <- matrix(getpars(fdep3cov)[10:15],ncol=2)
pts3 <- matrix(getpars(fdep3cov)[16:21],ncol=2)
trans_loss <- rbind(as.vector(exp(pts1%*%c(1,1))),as.vector(exp(pts2%*%c(1,1))),as.vector(exp(pts3%*%c(1,1))))
trans_loss <- round(trans_loss/rowSums(trans_loss),3)
trans_win <- rbind(as.vector(exp(pts1%*%c(1,0))),as.vector(exp(pts2%*%c(1,0))),as.vector(exp(pts3%*%c(1,0))))
trans_win <- round(trans_win/rowSums(trans_win),3)

