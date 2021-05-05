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
opts_chunk$set(cache.path = "cache/ch5/", fig.path = "figure/ch5/")
opts_chunk$set(cache.path = "cache/ch5/", fig.path = "figure/ch5/")


## ----echo=FALSE----------------------------------------------------
options(digits=3)


## ----models-sp500, echo=TRUE, results='hide'-----------------------
data(sp500)
# fit mixture models
set.seed(1)
m1 <- lca(sp500$logret, nclasses=1) 
m2 <- lca(sp500$logret, nclasses=2)
m3 <- lca(sp500$logret, nclasses=3)
# fit hidden Markov models
set.seed(31)
hm1 <- hmm(sp500$logret, nstates=1)
hm2 <- hmm(sp500$logret, nstates=2)
hm3 <- hmm(sp500$logret, nstates=3)


## ----gof-table-sp500-hmm, echo=FALSE, results='asis'---------------
bic1 <- BIC(m1)
bic2 <- BIC(m2)
bic3 <- BIC(m3)
bich1 <- BIC(hm1)
bich2 <- BIC(hm2)
bich3 <- BIC(hm3)

bics <- c(bic1,bic2,bic3,bich1,bich2,bich3)

ll <- c(logLik(m1),logLik(m2),logLik(m3),logLik(hm1),logLik(hm2),logLik(hm3))
dfs <- c(freepars(m1),freepars(m2),freepars(m3),freepars(hm1),freepars(hm2),freepars(hm3))
gof <- data.frame(model=c("m1","m2","m3","hm1","hm2","hm3"),logLik=ll,npar=dfs,BIC=bics)
require(xtable)
xt <- xtable(gof,caption="Goodness of fit statistics for 1 through 3 component
Gaussian mixture models fitted on the 
\\code{sp500} data, and the corresponding hidden Markov models with 1 through 3 states.", 
align="llrrr", label="tab:gof-sp500-hmm", display=c("d","d","f","d","f"))
print(xt, include.rownames=FALSE, caption.placement="top", table.placement = "th")


## ----2-statemix-sp500,results='markup',echo=TRUE-------------------
summary(m2)


## ----2-statehmm-sp500,results='markup',echo=TRUE-------------------
summary(hm2)


## ----fig-sp500-marginal,echo=FALSE,fig.height=.7*5,fig.width=.7*9----
par(mar=c(5,4,1,1))

pars2 <- getpars(m2)
pr12 <- pars2[1]
pr22 <- pars2[2]
me12 <- pars2[3]
me22 <- pars2[5]
sd12 <- pars2[4]
sd22 <- pars2[6]

par(mar=c(5,5,1,3))
c1 <- function(x) {pr12*dnorm(x,me12,sd12)}
c2 <- function(x) {pr22*dnorm(x,me22,sd22)}
c3 <- function(x) {c1(x)+c2(x)}
plot(density(sp500$logret), las=1, frame=FALSE, xlab="log return", main="")
curve(c1,add=TRUE, lty=2)
curve(c2,add=TRUE, lty=2)
curve(c3,add=TRUE, lty=2)

parsh2 <- getpars(hm2)

tr <- matrix(getpars(hm2)[3:6],2,2,by=T)
stattr <- stationary(tr)

prh12 <- stattr[1]
prh22 <- stattr[2]

meh12 <- parsh2[7]
meh22 <- parsh2[9]
sdh12 <- parsh2[8]
sdh22 <- parsh2[10]

c1 <- function(x) {prh12*dnorm(x,meh12,sdh12)}
c2 <- function(x) {prh22*dnorm(x,meh22,sdh22)}
c3 <- function(x) {c1(x)+c2(x)}
lines(density(sp500$logret))
curve(c1,add=TRUE, lty=3)
curve(c2,add=TRUE, lty=3)
curve(c3,add=TRUE, lty=3)
legend("topleft",inset=0.02,lty=1:3,legend=c("data","mixture model","hidden Markov model"),bty="n")



## ----stationary----------------------------------------------------
tr <- matrix(getpars(hm2)[3:6],2,2,by=T)
stationary(tr)


## ----rootogram-sp500, echo=FALSE, fig.width=.8*8, fig.height=.8*5,out.width=".6\\textwidth"----
par(mar=c(5,4,1,1))
mc <- posterior(m2, type="smoothing")[,1]
hmc <- posterior(hm2, type="smoothing")[,2]
mc <- c(hist((mc),breaks=10,plot=FALSE)$counts,0)
hmc <- hist((hmc),breaks=10,plot=FALSE)$counts
barplot(rbind(sqrt(mc),sqrt(hmc)),
	beside=TRUE,
	names.arg=seq(from=.05,to=.95,by=.1), #main="Rootogram of posterior probabilities",
	ylab="Frequency",
	xlab="posterior probabilities")


## ----dccs-hist-learn, fig.height=3, fig.width=8, echo=FALSE,out.width=".9\\textwidth"----
data(dccs)
layout(matrix(1:2,nrow=1))
means <- apply(dccs[,8:13],2,mean)
barplot(table(dccs$nCorPost),xlab="sumscore",ylab="count", 
	main="Participant counts by sum score")
plot(1:6,means,frame=FALSE,ty="b", ylim=c(0,1), 
	main="Proportion correct by post-switch trial", 
	ylab="proportion", xlab="post-switch trial")


## ----select-dccs, echo=FALSE---------------------------------------
dccs$trans <- ifelse(dccs$nCorPost==6|dccs$nCorPost==0,0,1)
dccstrans <- dccs[dccs$trans==1,]


## ----dccs-models, echo=TRUE, results='hide'------------------------
data(dccslong)
data(dccs)
grad <- depmix(acc~I(ageM-36), family=multinomial(),
	data=dccslong,nstates=1, initdata=dccs,
	ntimes=as.numeric(table(dccslong$pp)))
learn <- depmix(acc~1, family=multinomial("identity"), 
	data=dccslong, nstates=2, prior=~I(ageM-36), 
	ntimes=as.numeric(table(dccslong$pp)), 
	initdata=dccs, trstart=c(.9,.1,0,1))
learnreg <- depmix(acc~1, family=multinomial("identity"), 
	data=dccslong, nstates=2, prior=~I(ageM-36), 
	initdata=dccs, instart=c(.9,.1,0,.2),
	ntimes=as.numeric(table(dccslong$pp)))
set.seed(3)
fgrad <- fit(grad)
flearn <- fit(learn)
flearnreg <- fit(learnreg)


## ----dccs-models-2, echo=FALSE, results='asis'---------------------

# a hack to get the correct df for this model 
fixed <- getpars(flearn,"fixed")
fixed[7] <- TRUE
flearn <- setpars(flearn,fixed,which="fixed")


mods <- list(fgrad,flearn,flearnreg)
gofdccs <- 
data.frame(Model=c("Gradual","Learning","Learning+Regression"), 
  logLik=c(sapply(mods,logLik)),
  npar=c(sapply(mods, freepars)),
  AIC=c(sapply(mods, AIC)),
  BIC=c(sapply(mods, BIC)))

require(xtable)
xt <- xtable(gofdccs,caption="Goodness-of-fit statistics for 2-state 
hidden Markov models models fitted on the \\code{dccs} data; see the 
text for details", 
align="llrrrr", label="tab:gof-dccs-hmms", 
display=c("d","d","f","d","f","f"))
print(xt, include.rownames=FALSE, caption.placement="top", table.placement = "th")

# undo the hack 
fixed[7] <- FALSE
flearn <- setpars(flearn,fixed,which="fixed")



## ----dccs-hmms-responses1, echo=FALSE, results='hide'--------------
rlearn <- summary(mods[[2]],"response")[,2,drop=FALSE]
rboth <- summary(mods[[3]],"response")[,2,drop=FALSE]
resp <- cbind("learning" = rlearn,"learning+regression"=rboth)


## ----dccs-hmms-responses2, echo=FALSE, results='asis'--------------
colnames(resp) <- c("learning","learning+regression")
xt <- xtable(resp, caption="Probabilities correct for the DCCS data in each of the two
hidden Markov models.", label="tab:dccs-responses", align="lc|c")
print(xt, include.rownames=FALSE, caption.placement="top", table.placement = "th")


## ----dccs-age, echo=FALSE, fig.height=4, fig.width=6.5,out.width=".7\\textwidth"----
pfgrad <- getpars(fgrad)
gradage <- function(x) plogis(pfgrad[4]+pfgrad[6]*(x-36))
plot(aggregate(acc~ageM, FUN=mean,data=dccslong),frame=FALSE,
ylab="accuracy/state membership", xlab="Age in months")
curve(gradage, from=36, to=71, add=TRUE)

pflearnreg <- getpars(flearnreg)
pswitch <- function(x) plogis(pflearnreg[2]+pflearnreg[4]*(x-36))
curve(pswitch, from=36, to=71, add=TRUE, lty=2)



## ----dccs-predict, echo=FALSE, results='hide'----------------------

grad <- depmix(acc~I(ageM-36)+sex+trial, family=multinomial(),
	data=dccslong, ntimes=as.numeric(table(dccslong$pp)),
	nstates=1, initdata=dccs)

fgrad <- fit(grad)

pcst1 <- getpars(flearnreg)[10]
pcst2 <- getpars(flearnreg)[12]

pst <- posterior(flearnreg, type="global")
dccslong$pst <- pst
dccslong$pred <- ifelse(dccslong$pst==1,pcst1,pcst2)
dccslong$predgrad <- predict(fgrad@response[[1]][[1]])[,2]

dccslongsel <- dccslong[which(dccslong$pp %in% dccstrans$pp),]
pst[1]



## ----fig-accuracy-dccs,echo=FALSE, fig.height=6, fig.width=8, out.width="\\textwidth"----
mt <- matrix(c(1:16),ncol=4,byrow=TRUE)
layout(mt)
for(i in 1:16) {
  if(i %in% c(13:16)) xlab = "trial" else xlab = ""
  if(i %in% c(1,5,9,13)) ylab = "acc" else ylab = ""
	par(mar=c(4,4,0,1))
	plot(1:6,dccstrans[i,8:13], las=1, 
	xlim=c(1,6), frame.plot=FALSE, ylab=ylab, xlab=xlab, 
	type="l", yaxt="n")
	axis(side=2, at=c(0,1), labels=c("inc", "cor"), las=1)
	pred <- dccslongsel[which(dccslongsel$pp==dccstrans[i,"pp"]),"pred"]
	lines(1:6,pred,lty=2)
	predg <- dccslongsel[which(dccslongsel$pp==dccstrans[i,"pp"]),"predgrad"]
	lines(1:6,predg,lty=3)
}



## ----fig-accuracy-dccs-2,echo=FALSE, fig.height=6, fig.width=8, out.width="\\textwidth"----
mt <- matrix(c(1:16),ncol=4)
layout(mt)
for(i in 17:32) {
	if(i %in% c(13:16)) xlab = "trial" else xlab = ""
  if(i %in% c(1,5,9,13)) ylab = "acc" else ylab = ""
	par(mar=c(4,4,0,1))
	plot(1:6,dccstrans[i,8:13], las=1, 
	xlim=c(1,6), frame.plot=FALSE, ylab=ylab, xlab=xlab, 
	type="l", yaxt="n")
	axis(side=2, at=c(0,1), labels=c("inc", "cor"), las=1)
	pred <- dccslongsel[which(dccslongsel$pp==dccstrans[i,"pp"]),"pred"]
	lines(1:6,pred,lty=2)
	predg <- dccslongsel[which(dccslongsel$pp==dccstrans[i,"pp"]),"predgrad"]
	lines(1:6,predg,lty=3)
}


## ----data-speed1, echo=FALSE---------------------------------------
data(speed1)
data(speed)
speed1$prev <- speed$prev[1:168]


## ----fig-speed1-ch5, echo=FALSE, fig.height=.72*figwidth_10, fig.width=figwidth_10, out.width="\\textwidth",eval=FALSE----
## mt <- matrix(c(1,1,1,1,2,2,3,3,3),9)
## layout(mt)
## par(mar=c(1,5,0,3))
## plot(as.ts(speed1[,1]), xaxp=c(1,165,4), xaxt="n", las=1, frame.plot=FALSE, ylab="RT (log ms)")
## par(mar=c(1,5,0,3))
## plot(as.ts(speed1[,2]), xaxp=c(1,165,4), xaxt="n", las=1, frame.plot=FALSE, ylab ="ACC")
## par(mar=c(5,5,1,3))
## plot(as.ts(speed1[,3]), xaxp=c(1,165,4), las=1, frame.plot=FALSE, xlab="Trials", ylab ="Pacc")


## ----fig-speed1-auto, echo=FALSE, fig.height=4, fig.width=6--------
acfrts <- acf(speed1$RT, plot=FALSE)
plot(acfrts,ci.col=1, frame.plot=FALSE, main="", ylab="Autocorrelation", las=1)



## ----rts-hmm2, echo=TRUE, results='hide'---------------------------
set.seed(5)
m2 <- hmm(speed1[,"RT"], nstates=2)


## ----rts-hmm2-summary, echo=TRUE-----------------------------------
summary(m2)


## ----rts-hmm2-getpars, echo=FALSE----------------------------------
pars <- getpars(m2)
inm2 <- pars[1:2]
trm2 <- pars[3:6]
mu1 <- pars[7]
sd1 <- pars[8]
mu2 <- pars[9]
sd2 <- pars[10]


## ----rts-lm, echo=TRUE---------------------------------------------
lm1 <- lm(RT~Pacc, data=speed1)
summary(lm1)


## ----rts-lm-pred, echo=TRUE, results='hide'------------------------
predlm1 <- predict(lm1) # predicted RTs
reslm1 <- residuals(lm1) # residuals


## ----2state-pred, echo=TRUE, results='hide'------------------------
pst <- posterior(m2, type="global") # posterior states
sdm <- getpars(m2)[c(7,9)] # state-dependent means
prt <- sdm[pst] # predicted RTs
res2state <- speed1$RT-prt # residuals


## ----fig-speed1-pred, echo=FALSE, fig.height=5, fig.width=7,out.width=".8\\textwidth"----
mt <- matrix(c(1,2),2)
layout(mt)
par(mar=c(2,5,1,2))
plot(as.ts(speed1$RT), xaxp=c(1,165,4), las=1, frame.plot=FALSE, ylab="RT (log ms)")
lines(predlm1,lty=2)
lines(prt,lty=1,lwd=1.5,col=grey(.5))
par(mar=c(3,5,2,2))
plot(as.ts(reslm1), xaxp=c(1,165,4), las=1, frame.plot=FALSE, ylab="residuals",lty=2)
lines(res2state,lty=1,col=grey(.5))


## ----echo=FALSE----------------------------------------------------
res2state <- speed1$RT-prt


## ----rts-acf-res, echo=TRUE, results='hide'------------------------
acflm1 <- acf(reslm1, plot=FALSE)
acf2state <- acf(res2state, plot=FALSE)


## ----fig-speed1-acfres,echo=FALSE,fig.height=.9*7,fig.width=.9*5----
layout(matrix(c(1:3),nrow=3))
par(mar=c(1,4,1,1))
plot(acfrts,ci.col=1,frame.plot=FALSE,main="", xaxt="n", ylab="Autocorrelation", las=1)
par(mar=c(1,4,1,1))
plot(acflm1,ci.col=1,frame.plot=FALSE,main="", xaxt="n", 
ylab="Autocorrelation", las=1, ylim=c(-0.2,1))
par(mar=c(3,4,1,1))
plot(acf2state,ci.col=1,frame.plot=FALSE,main="", ylab="Autocorrelation", las=1, ylim=c(-0.2,1))


## ----gof-speed1-rt, echo=FALSE, results='hide'---------------------
set.seed(2)
mix2 <- lca(speed1$RT,nc=2,verbose=FALSE)
set.seed(1)
m1 <- hmm(speed1[,"RT"], nstates=1)
m3 <- hmm(speed1[,"RT"], nstates=3)
aics <- c(AIC(lm1),AIC(m1),AIC(mix2),AIC(m2),AIC(m3))
bics <- c(BIC(lm1),BIC(m1),BIC(mix2),BIC(m2),BIC(m3))
ll <- c(logLik(lm1),logLik(m1),logLik(mix2),logLik(m2),logLik(m3))
dfs <- c(3,freepars(m1),freepars(mix2),freepars(m2),freepars(m3))
gof <- data.frame(model=c("lm","mix1","mix2","hmm2","hmm3"),LL=ll,df=dfs,AIC=aics,BIC=bics)


## ----gof-table-speed1-rt, echo=FALSE, results='asis'---------------
require(xtable)
xt <- xtable(gof,caption="Goodness of fit statistics for the linear 
model, a 1-class and 2-class mixture model, and hidden Markov models 
with 2--3 classes for the \\code{speed1} data.", 
align="llrrrr", label="tab:gof-speed1-rt", display=c("d","d","f","d","f","f"))
print(xt, include.rownames=FALSE, caption.placement="top", table.placement = "th")


## ----fig-rt-hist, echo=FALSE, fig.height=.6*6, fig.width=.6*7------
par(mar=c(4,4,1,1))
pars <- getpars(m2) # get the parameters of the 2-state model
tpst2 <- table(posterior(m2, type="global"))
tpst2 <- tpst2/sum(tpst2)
pc1 <- tpst2[1]
pc2 <- tpst2[2]
mc1 <- pars[7]
sd1 <- pars[8]
mc2 <- pars[9]
sd2 <- pars[10]
layout(matrix(c(1,1,2,2),nr=4)) # plot the 2 state model
c1 <- function(x) {pc1*dnorm(x,mc1,sd1)}
c2 <- function(x) {pc2*dnorm(x,mc2,sd2)}
truehist(speed1$RT, nb=23, col=0, xlab="", las=1, ylab="Density")
curve(c1,add=TRUE)
curve(c2,add=TRUE)
c3 <- function(x) {c1(x)+c2(x)}
curve(c3,add=TRUE)
pars <- getpars(m3)
tpst3 <- table(posterior(m3, type="global"))# get the parameters of the 3-state model
tpst3 <- tpst3/sum(tpst3)
pc1 <- tpst3[1]
pc2 <- tpst3[2]
pc3 <- tpst3[3]
mc1 <- pars[13]
sd1 <- pars[14]
mc2 <- pars[15]
sd2 <- pars[16]
mc3 <- pars[17]
sd3 <- pars[18]
c1 <- function(x) {pc1*dnorm(x,mc1,sd1)}
c2 <- function(x) {pc2*dnorm(x,mc2,sd2)}
c3 <- function(x) {pc3*dnorm(x,mc3,sd3)}
par(mar=c(5,4,0,1))
truehist(speed1$RT, nb=23, col=0, xlab="RT (log ms)", las=1, 
ylab="Density") # plot the 3 state model
curve(c1,add=TRUE)
curve(c2,add=TRUE)
curve(c3,add=TRUE)
c4 <- function(x) {c1(x)+c2(x)+c3(x)}
curve(c4,add=TRUE)


## ----perth-plot-lin, echo=FALSE, fig.height=1.2*3.5, fig.width=1.2*6, out.width=".8\\textwidth"----
par(mar=c(5,4,1,1))
data(perth)
lm1 <- lm(water~1,data=perth)
lmyr <- lm(water~yr,data=perth) 
lmyr2 <- lm(water~yr+I(yr^2),data=perth)
plot(1911:2017,perth$water,ylab="Inflow (GL)", main="", xlab="year", frame=FALSE, xaxp=c(1910,2020,10),type="l")
lines(1911:2017,predict(lm1),lty=2)
lines(1911:2017,predict(lmyr),lty=3)
lines(1911:2017,predict(lmyr2),lty=4)
legend("topright",inset=0,legend=c("data","average","linear","quadratic"),lty=1:4,bty="n")


## ----echo=FALSE----------------------------------------------------
data(perth)
lm1 <- lm(water~1,data=perth)
lmyr <- lm(water~yr,data=perth) 
lmyr2 <- lm(water~yr+I(yr^2),data=perth)
anova(lm1,lmyr,lmyr2)


## ----perth-AR-models, echo=TRUE------------------------------------
ar1 <- arima(perth$water,c(1,0,0))
aryr <- arima(perth$water,c(1,0,0),xreg=perth$yr)
aryr2 <- arima(perth$water,c(1,0,0), 
               xreg=data.frame("yr"=scale(perth$yr),
                               "yr2"=scale(perth$yr)^2))


## ----echo=FALSE, linewidth=56--------------------------------------
aryr2
lmtest::coeftest(aryr2) 


## ----perth-plot-AR, echo=FALSE, fig.height=1.2*3.5, fig.width=1.2*6, out.width=".8\\textwidth"----
par(mar=c(5,4,1,1))
pm1 <- perth$water-ar1$residuals
pm2 <- perth$water-aryr$residuals
pm3 <- perth$water-aryr2$residuals

plot(1911:2017,perth$water,ylab="Inflow (GL)", main="", xlab="year", frame=FALSE, xaxp=c(1910,2020,10),type="l")
lines(1911:2017,pm1,lty=2)
lines(1911:2017,pm2,lty=3)
lines(1911:2017,pm3,lty=4)
legend("topright",inset=0,legend=c("data","AR(1)","AR(1)+linear","AR(1)+quadratic"),lty=1:4,bty="n")


## ----perth-hmm3, echo=TRUE, results='hide'-------------------------
mod3 <- depmix(water~1, data=perth, ns=3, instart=c(1,0,0),
               trstart=c(0.9,0.1,0, 0,0.9,0.1, 0,0,1))
set.seed(1)
fm3 <- fit(mod3)


## ------------------------------------------------------------------
summary(fm3)


## ----perth-changepoints, echo=FALSE, results='hide'----------------
mod2 <- depmix(water~1, data=perth, ns=2,
          trst=c(0.9,0.1,0,1), inst=c(1,0))
set.seed(1)
fm2 <- fit(mod2, verbose=FALSE)


## ----perth-changepoints-fig, echo=FALSE, fig.height=1.2*3.5, fig.width=1.2*6, out.width=".8\\textwidth"----
par(mar=c(5,4,1,1))
pm2ch <- cbind(
	predict(fm2@response[[1]][[1]]),
	predict(fm2@response[[2]][[1]]))[cbind(1:107,fm2@posterior[,1])]

pm3ch <- cbind(
	predict(fm3@response[[1]][[1]]),
	predict(fm3@response[[2]][[1]]),
	predict(fm3@response[[3]][[1]]))[cbind(1:107,fm3@posterior[,1])]

plot(1911:2017,perth$water,ylab="Inflow (GL)", main="", xlab="year", frame=FALSE, xaxp=c(1910,2020,10),type="l")
lines(1911:2017,pm2ch,lty=2)
lines(1911:2017,pm3ch,lty=3)
legend("topright",inset=0,bty="n",legend=c("data","1 changepoint","2 changepoints"),lty=1:3)
xx <- perth$water


## ----perth-gofAR, results='asis', echo=FALSE-----------------------
library(xtable)
mods <- list(lm1,lmyr,lmyr2,ar1,aryr,aryr2,fm2,fm3)
names <- c("lm1","lmyr","lmyr2","ar1","aryr","aryr2","ch1","ch2")
ll <- lapply(mods,logLik)
df <- unlist(lapply(ll,function(x)attr(x,"df")))
df[7] <- 5
df[8] <- 8
logl <- unlist(ll)
bics <- -2*logl+df*log(nobs(fm2))
gofAR <- data.frame(models=names,logLik=logl,npar=df,AIC=unlist(lapply(mods,AIC)),BIC=bics)
xt <- xtable(gofAR, caption="Goodness-of-fit statistics for linear, AR, and change-point models fitted to the Perth dams data", 
align="llrrrr", label="tab:gof-perth",display=c("d","d","f","d","f","f"))
print(xt, include.rownames=FALSE, caption.placement="top", table.placement = "th")


## ----perth-acf-fig, echo=FALSE, fig.height=1.1*3.5, fig.width=1.1*8,out.width="\\textwidth"----
layout(matrix(1:2,nrow=1))
par(mar=c(5,4,4,1))
acf(perth$water,main="data",lag.max=10,ci.col = "black")
acf(pm3ch-perth$water,main="residuals",lag.max=10,ci.col = "black", ylab="")


## ----fit-WPT-HMMs,results='hide'-----------------------------------
data(WPT)
lrTrStart <- function(nstates,pdiag=.8) {
  trans <- pdiag*diag(nstates)
  for(i in 1:nstates) {
    if(i < nstates) trans[i,(i+1):nstates] <- 
        (1-pdiag)/(length((i+1):nstates))
  }
  trans[nstates,nstates] <- 1
  as.numeric(t(trans))  
}
wptModels <- list()
set.seed(20190430)
# fit 2- to 8-state models
for(i in 2:8) {
  mod <- depmix(r~c1+c2+c3+c4,data=WPT,family=binomial(),
                nstates=i,instart = c(1,rep(0,i-1)),
                trstart=lrTrStart(i),ntimes=rep(200,48)) 
# use multistart to fit each with multiple starting values
  wptModels[[i]] <- multistart(mod)
}


## ----fix-pars-WPT-HMMs,echo=FALSE,results='asis'-------------------
lrSetFixed <- function(fmod) {
  nstates <- fmod@nstates
  fmod@prior@fixed <- as.logical(c(TRUE,nstates))
  for(i in 1:nstates) {
    if(i > 1 & i < nstates) {
      fmod@transition[[i]]@fixed <- as.logical(c(rep(TRUE,i-1),rep(FALSE,nstates-i+1)))
    }
    if(i == nstates) fmod@transition[[i]]@fixed <- as.logical(rep(TRUE,nstates))
  }
  return(fmod)
}

modelTab <- data.frame()
for(i in 2:8) {
  mod <- lrSetFixed(wptModels[[i]])
  modelTab <- rbind(modelTab,
                    data.frame("nstates"=i,
                               "logLik" = logLik(mod), 
                               "npar" = attr(logLik(mod),"df"),
                               AIC = AIC(mod),
                               BIC = BIC(mod)))
}
require(xtable)
xt <- xtable(modelTab,caption="Goodness of fit statistics for 2 to 8 state hidden Markov models fitted to the
\\code{WPT} data.", 
align="llrrrr", label="tab:WPT-gof", display=c("d","d","f","d","f","f"))
print(xt, include.rownames=FALSE, caption.placement="top", table.placement = "th")


## ----WPT-five-state-response,echo=FALSE,results='asis'-------------
prs <- getpars(wptModels[[5]])[31:55]
cis <- confint(wptModels[[5]])[31:55,]
out <- as.character(round(prs,3))
for(i in 1:length(out)) {
  if(cis[i,3] > 0 | cis[i,4] < 0) out[i] <- paste0(out[i],"*") else  paste0(out[i]," ")
}
out <- cbind(1:5,matrix(out,ncol=5,byrow=TRUE))
colnames(out) <- c("state","(intercept)","c1","c2","c3","c4")
xt <- xtable(out,caption="Estimated parameters of the response models for the 5-state model fitted to the \\code{WPT} data. An asterix indicates that the approximate 95\\% confidence interval does not include 0.", 
align="llrrrrr", label="tab:WPT-coefficients", display=c("d","d","f","f","f","f","f"))
print(xt, include.rownames=FALSE, include.colnames=FALSE, only.contents = TRUE)


## ----WPT-posterior-state-plot,echo=FALSE,fig.width=1.1*6,fig.height=1.1*2.4,out.width="\\textwidth"----
WPT$pd <- as.numeric(WPT$group == "PD")
WPT$pdon <- as.numeric(WPT$group == "PD" & WPT$med == "on")

postS <- aggregate(posterior(wptModels[[5]], type="smoothing"),by=list(trial=WPT$trial,pdon=WPT$pdon,pd=WPT$pd),mean)
colnames(postS) <- c("trial","pdon","pd",paste0("S",1:5))
layout(matrix(c(1,2,3),ncol=3))
par(mar=c(5, 4, 4, 0) + 0.1)
plot(c(1,200),c(0,1),type="n",xlab="trial",ylab="posterior",main="PD on medication")
lines(1:200,subset(postS,pdon==1)$S1,col=grey(.8),lty=1)
lines(1:200,subset(postS,pdon==1)$S2,col=grey(.6),lty=1)
lines(1:200,subset(postS,pdon==1)$S3,col=grey(.4),lty=1)
lines(1:200,subset(postS,pdon==1)$S4,col=grey(.2),lty=1)
lines(1:200,subset(postS,pdon==1)$S5,col=grey(0),lty=1)

plot(c(1,200),c(0,1),type="n",xlab="trial",ylab="",main="Pd off medication")
lines(1:200,subset(postS,pdon==0 & pd == 1)$S1,col=grey(.8),lty=1)
lines(1:200,subset(postS,pdon==0 & pd == 1)$S2,col=grey(.6),lty=1)
lines(1:200,subset(postS,pdon==0 & pd == 1)$S3,col=grey(.4),lty=1)
lines(1:200,subset(postS,pdon==0 & pd == 1)$S4,col=grey(.2),lty=1)
lines(1:200,subset(postS,pdon==0 & pd == 1)$S5,col=grey(0),lty=1)
legend(200,1,legend=paste("state",1:5),lty=1,col=c(grey(.8),grey(.6),grey(.4),grey(.2),grey(0)),xjust=1,bty="n",cex=.8)

plot(c(1,200),c(0,1),type="n",xlab="trial",ylab="",main="Controls")
lines(1:200,subset(postS,pdon==0 & pd == 0)$S1,col=grey(.8),lty=1)
lines(1:200,subset(postS,pdon==0 & pd == 0)$S2,col=grey(.6),lty=1)
lines(1:200,subset(postS,pdon==0 & pd == 0)$S3,col=grey(.4),lty=1)
lines(1:200,subset(postS,pdon==0 & pd == 0)$S4,col=grey(.2),lty=1)
lines(1:200,subset(postS,pdon==0 & pd == 0)$S5,col=grey(0),lty=1)

