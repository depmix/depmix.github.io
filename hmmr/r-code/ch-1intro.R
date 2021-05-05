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
opts_chunk$set(cache.path = "cache/ch1/", fig.path = "figure/ch1/")


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------
2 + 11 # addition
2 * 11 # multiplication
2 / 11 # division
2^(11) # exponentiation
#R follows the usual rules of arithmetic
2 + 11*3 
(2 + 11)*3 


## ----installdepmix,eval=FALSE---------------------------------------------------------------------------------------------------------------------------------------------
## # download and install the packages
## install.packages(c("depmixS4","hmmr"))
## # load the packages into R
## library(depmixS4,hmmr)


## ----echo=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------
library(depmixS4,hmmr)

## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------
data(speed1)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------
head(speed1)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------
transport <- factor(c(1,2,1,3), 
	labels=c("bicycle", "foot", "car"))


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------
transport


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------
c(FALSE, TRUE, TRUE, FALSE) # a logical vector
c(FALSE, TRUE, TRUE, 2) # combining logical and numeric
#a numeric vector (even when only using integers)
c(0,1,0,2) 
c(0,1,0,"2") # combining numeric and character


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# create a logical vector by repetition (rep(x,times))
logi_vec <- rep(c(FALSE,TRUE),length=7) 
logi_vec
# initialize a double-precision (or "numeric") vector
num_vec <- vector("double",length=7) 
num_vec # default value is 0
num_vec[1] <- 1/9 # set the first element
num_vec
#regular sequence
num_vec <- seq(from=0,to=1,length=9) 
as.numeric(logi_vec)
as.character(num_vec)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------
short_vec <- num_vec[1:5] # take 5 elements from num_vec
short_vec + short_vec # element-wise addition
2 * short_vec # element-wise multiplication
short_vec * short_vec # same as short_vec^2


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#same as c(vec_num2,vec_num[1:2])*vec_num
short_vec*num_vec


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------
t(short_vec)%*%short_vec


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------
cbind(short_vec,2:6)
matrix(short_vec,nrow=4,ncol=2)
rbind(short_vec,num_vec)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------
my_list <- list(
  list(a = 1,"b" = c(1,2,1)),
  "num" = c(1,2,1)
)
my_list


## ----eval=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------
## speed1[speed1$RT < 5.3,]
## subset(speed1,RT < 5.3)


## ----eval=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------
## speed1[speed1$RT <= 5.4 & speed1$ACC == "cor",]
## subset(speed1,RT <= 5.4 & ACC != "inc")


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------
my_mean <- function(x) {
  sum(x)/length(x)
}


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------
my_mean <- function(...) {
  x <- as.numeric(unlist(c(...)))
  x <- x[!is.na(x)]
  return(sum(x)/length(x))
}


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(123)
my_mean(rnorm(100),rnorm(70),runif(20),NA,
  list(a=1,b=rnorm(100)),factor(c("a","b")))


## ----echo=FALSE, results='hide'-------------------------------------------------------------------------------------------------------------------------------------------
y <- speed1$RT

## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# construct a mixture model
mmod <- mix(RT~1, data=speed1, nstates=2, 
	family=gaussian())
is(mmod) # show the class name
slotNames(mmod) # get all slot names
slot(mmod,"response") # access the response slot
is(mmod@response[[1]][[1]])


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------
getClass("mix")


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------
showMethods("fit")


## ----plot-speed1,echo=TRUE, fig.keep='none'-------------------------------------------------------------------------------------------------------------------------------
plot(speed1) 


## ----fig-scatter-speed1, echo=FALSE, fig.height=figwidth_08, fig.width=figwidth_08, out.width=".8\\textwidth"-------------------------------------------------------------
plot(speed1)


## ----boxplot-RT-code,echo=TRUE, eval=FALSE--------------------------------------------------------------------------------------------------------------------------------
## boxplot(RT~ACC,data=speed1, frame.plot=FALSE)


## ----fig-rt-acc-boxplot, echo=FALSE, fig.width=figwidth_06, fig.height=figwidth_06, out.width=".6\\textwidth"-------------------------------------------------------------
boxplot(RT~ACC,data=speed1, frame.plot=FALSE)


## ----plot-speed1-ts-code,fig.keep='none', echo=TRUE-----------------------------------------------------------------------------------------------------------------------
plot(as.ts(speed1), main="Speed data, series 1")


## ----fig-asts-speed1, echo=FALSE, fig.height=.72*figwidth_10, fig.width=figwidth_10, out.width="\\textwidth"--------------------------------------------------------------
mt <- matrix(c(1,1,1,1,2,2,3,3,3),9)
layout(mt)
par(mar=c(1,5,0,3))
plot(as.ts(speed1[,1]), xaxp=c(1,165,4), xaxt="n", las=1, frame.plot=FALSE, ylab="RT (log ms)")
par(mar=c(1,5,0,3))
plot(as.ts(speed1[,2]), xaxp=c(1,165,4), xaxt="n", las=1, frame.plot=FALSE, ylab ="ACC")
par(mar=c(5,5,1,3))
plot(as.ts(speed1[,3]), xaxp=c(1,165,4), las=1, frame.plot=FALSE, xlab="Trials", ylab ="Pacc")


## ----histogram-density-speed1-code,echo=TRUE, eval=FALSE------------------------------------------------------------------------------------------------------------------
## layout(matrix(1:2,ncol=2))
## hist(speed1$RT, main = "Histogram", xlab= "RT")
## plot(density(speed1$RT), main = "Density")


## ----fig-rt-hist, echo=FALSE, fig.height=1.25*.5*figwidth_08, fig.width=1.25*figwidth_08, out.width=".8\\textwidth"-------------------------------------------------------
layout(matrix(1:2,ncol=2))
par(mar=c(5,4,4,.2) + .1)
hist(speed1$RT, main = "Histogram", xlab= "RT")
plot(density(speed1$RT), frame.plot=FALSE, main = "Density")


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------
summary(speed1)


## ----echo=TRUE------------------------------------------------------------------------------------------------------------------------------------------------------------
library(polycor)
hetcor(speed1)


## ----rts-lm, echo=TRUE----------------------------------------------------------------------------------------------------------------------------------------------------
lm1 <- lm(RT~Pacc, data=speed1)
summary(lm1)


## ----fig-lm-rt1, echo=TRUE, eval=FALSE------------------------------------------------------------------------------------------------------------------------------------
## plot(RT~Pacc,data=speed1)
## abline(coefficients(lm(RT~Pacc,data=speed1)))


## ----fig-lm-rt2, echo=FALSE, fig.width=4.5, fig.height=4.5, out.width=".6\\textwidth"-------------------------------------------------------------------------------------
plot(RT~Pacc,data=speed1,frame=FALSE,ylim=c(5,7.5),xlim=c(0,.8))
abline(coefficients(lm(RT~Pacc,data=speed1)))


## ----rts-lm2, echo=TRUE---------------------------------------------------------------------------------------------------------------------------------------------------
lm2 <- lm(RT~Pacc + ACC,data=speed1)
summary(lm2)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------
contrasts(speed1$ACC)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------
contrasts(speed1$ACC) <- contr.helmert(2)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------
contrasts(speed1$ACC) <- contr.treatment(2)


## ----echo=FALSE, results='hide'-------------------------------------------------------------------------------------------------------------------------------------------
y <- speed1$RT


## ----rts-lm3, echo=TRUE---------------------------------------------------------------------------------------------------------------------------------------------------
lm3 <- lm(RT~Pacc*ACC, data=speed1)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------
anova(lm1,lm2,lm3)


## ----acc-glm, echo=TRUE---------------------------------------------------------------------------------------------------------------------------------------------------
mod_acc <- glm(ACC~Pacc, data=speed1, 
               family=binomial())


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------
summary(mod_acc)


## ----speed-lm, echo=TRUE--------------------------------------------------------------------------------------------------------------------------------------------------
mlm <- lm(cbind(RT,ACC) ~ Pacc,data=speed1)
summary(manova(mlm))


## ----speed-lm-2, echo=TRUE------------------------------------------------------------------------------------------------------------------------------------------------
summary(aov(mlm))


## ----nultinomial-regression-estimation------------------------------------------------------------------------------------------------------------------------------------
require(nnet)
catRT <- factor(cut(speed1$RT,breaks=4),
           labels=c("fast","med-fast","med-slow","slow"))
multi_mod <- multinom(catRT ~ speed1$Pacc)
summary(multi_mod)


## ----multinomial-regression-predicted-probs-------------------------------------------------------------------------------------------------------------------------------
pred_prob <- function(mod,predictors,baseline) {
  coef <- coefficients(mod)
  # add intercept if (probably) not given
  if(ncol(coef) == (NCOL(predictors) + 1) && 
     colnames(coef)[1] == "(Intercept)") {
    predictors <- cbind(1,predictors)
  }
  # compute predictions on log odds scale
  preds <- predictors%*%t(coef)
  # add predictions for the baseline
  preds <- cbind(0,preds)
  colnames(preds) <- c(baseline,rownames(coef))
  # convert predictions using softmax
  pred_p <- exp(preds)
  pred_p <- pred_p/rowSums(pred_p)
  return(pred_p)
}


## ----multinomial_logistic_regression_predictions_example,echo=FALSE,fig.width=figwidth_08,fig.height=.67*figwidth_08, out.width=".8\\textwidth"---------------------------
probs <- pred_prob(multi_mod, seq(0,1, length=100), "fast")
plot(c(0,1), c(0,1), type="n", xlab="Pacc", 
     ylab="P(category)", frame.plot=FALSE)
for(i in 1:ncol(probs)) {
  lines(seq(0,1,length=100),probs[,i],lty=i)
}
legend(x=.7, y=.8, legend=colnames(probs), lty=1:4, cex=.8)


## ----multinomial-predictions-figure-code,eval=FALSE-----------------------------------------------------------------------------------------------------------------------
## probs <- pred_prob(multi_mod, seq(0,1, length=100), "fast")
## plot(c(0,1), c(0,1), type="n", xlab="Pacc",
##      ylab="P(category)", frame.plot=FALSE)
## for(i in 1:ncol(probs)) {
##   lines(seq(0,1,length=100),probs[,i],lty=i)
## }
## legend(x=.7, y=.8, legend=colnames(probs), lty=1:4, cex=.8)


## ----speed1-acfRT,eval=FALSE,echo=TRUE------------------------------------------------------------------------------------------------------------------------------------
## acfRT <- acf(speed1$RT, plot=FALSE)
## plot(acfRT,ci.col=1, frame.plot=FALSE,
##      ylab="Autocorrelation", las=1)


## ----echo=FALSE, results='hide'-------------------------------------------------------------------------------------------------------------------------------------------
y <- speed1$RT


## ----fig-speed1-acfRT, echo=FALSE, fig.height=.7*figwidth_06, fig.width=figwidth_06, out.width=".6\\textwidth"------------------------------------------------------------
acfRT <- acf(speed1$RT, plot=FALSE)
plot(acfRT,ci.col=1, frame.plot=FALSE, 
     ylab="Autocorrelation", las=1)


## ----echo=FALSE, results='hide'-------------------------------------------------------------------------------------------------------------------------------------------
y <- speed1$RT


## ----echo=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------
data(sp500)
lr <- sp500[,"logret"]
attributes(lr) <- NULL


## ----plot-log-returns,echo=FALSE, fig.height=.7*figwidth_08, fig.width=figwidth_08, out.width=".8\\textwidth"-------------------------------------------------------------
plot(as.ts(lr), las=1, frame.plot=FALSE, ylab="log return")


## ----sp500-make, echo=TRUE, eval=FALSE------------------------------------------------------------------------------------------------------------------------------------
## require(TTR)
## Sys.setenv(tz='UTC')
## sp500 <- getYahooData('^GSPC',start=19500101,end=20120229,
##                       freq='daily')
## ep <- endpoints(sp500, on="months", k=1)
## sp500 <- sp500[ep[2:(length(ep)-1)]]
## sp500$logret <- log(sp500$Close) - lag(log(sp500$Close))
## sp500 <- na.exclude(sp500)
## sp500 <- data.frame(sp500)


## ----echo=FALSE, results='hide'-------------------------------------------------------------------------------------------------------------------------------------------
y <- speed1$RT


## ----perth-data-plot, echo=FALSE, fig.height=.6*figwidth_08, fig.width=figwidth_08,out.width=".8\\textwidth"--------------------------------------------------------------
data(perth)
wts <- ts(perth$water,start=1911)
plot(wts,ylab="GL", xlab="year", frame=FALSE, xaxp=c(1910,2020,10))



## ----echo=FALSE, results='hide'-------------------------------------------------------------------------------------------------------------------------------------------
y <- speed1$RT


## ----data-disc42,echo=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------
require(hmmr)
data(disc42)


## ----nti-from-disc42,echo=TRUE--------------------------------------------------------------------------------------------------------------------------------------------
nti <- attr(disc42,"ntimes") 


## ----echo=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------
lt <- length(nti)
et <- cumsum(nti)
bt <- c(1,et[-lt]+1)


## ----plot-disc42,echo=FALSE, out.width=".8\\textwidth", fig.height=.6*figwidth_08, fig.width=figwidth_08------------------------------------------------------------------
mt <- matrix(c(1:6),ncol=2)
layout(mt)
for(i in 1:lt) {
	par(mar=c(3,5,2,1))
	plot(1:nti[i],disc42[bt[i]:et[i],1], las=1, xaxp=c(1,49,4), 
	xlim=c(1,50), frame.plot=FALSE, ylab="acc", xlab="trial", 
	type="l", yaxt="n")
	axis(side=2, at=c(1,2), labels=c("inc", "cor"), las=1)
}


## ----balance-sum-table, echo=FALSE, results='asis'------------------------------------------------------------------------------------------------------------------------
library("depmixS4") 
data(balance)
balance$sum <- rowSums(balance[,c("d1","d2","d3","d4")])
sumtab <- as.integer(t(table(balance$sum)))
ages <- as.numeric(by(balance$age,balance$sum,mean))
xt <- rbind(sumtab, ages)
rownames(xt) <- c("nr of participants","mean age")
colnames(xt) <- 0:4
require(xtable)
xt <- xtable(xt,caption="Number of participants per sum score
of four items on the balance scale task and their mean ages.", align="lrrrrr", label="tab:balsum", 
digits=matrix(c(0,0,0,0,0,0,1,1,1,1,1,1), nrow=2, byrow=TRUE))
print(xt, caption.placement = "top", table.placement = "th")


## ----balance8learndevelop, echo=FALSE, fig.height=.7*figwidth_07, fig.width=figwidth_07, out.width=".7\\textwidth"--------------------------------------------------------
library(hmmr)
data(balance8)

balance8$pc <- balance8$totalCor/balance8$totalTrials

pcgr <- aggregate(balance8$pc, list(balance8$group), mean, na.rm=TRUE)

plot(1:8,pcgr[,2],ty="b", frame=FALSE, 
xlab="measurement/group", ylab="total p correct", 
main="Learning and development on the balance scale task")

pc <- matrix(balance8$pc,ncol=8, byrow=TRUE)

lines(1:8,colMeans(pc, na.rm=TRUE), type="b",lty=2)

legend("bottomright",inset=.05,legend=c("learning","development"),lty=c(2,1))


## ----dccs logistic regression, linewidth=58-------------------------------------------------------------------------------------------------------------------------------
require(hmmr)
data(dccs)
summary(glm(cbind(nCorPost,6-nCorPost)~ageY,
          family=binomial(), data=dccs))


## ----dccs-logistic,echo=FALSE, fig.height=.8*3.8, fig.width=.8*10.5, out.width=".9\\linewidth"----------------------------------------------------------------------------
layout(matrix(1:2,nr=1))
barplot(table(dccs$nCorPost), 
	main=paste("Post-switch scores (n=",nrow(dccs),")",sep=""),xlab="Nr correct trials")
lreg <- glm(cbind(nCorPost,6-nCorPost)~ageY,family="binomial",data=dccs)
cf <- coef(lreg)
page3 <- dbinom(0:6,size=6,prob=plogis(cf[1] + 3*cf[2]))
page4 <- dbinom(0:6,size=6,prob=plogis(cf[1] + 4*cf[2]))
page5 <- dbinom(0:6,size=6,prob=plogis(cf[1] + 5*cf[2]))
lpred <- 43*page3 + 27*page4 + 23*page5
barplot(lpred, main="Model predicted data",names=0:6,xlab="Nr correct trials",ylim=c(0,40))


## ----echo=FALSE, results='hide'-------------------------------------------------------------------------------------------------------------------------------------------
y <- speed1$RT


## ----dccsHistAge,echo=FALSE, fig.height=.7*3.8, fig.width=.7*10.5, out.width = "\\textwidth"------------------------------------------------------------------------------
layout(matrix(1:3,nr=1))
barplot(table(dccs$nCorPost[dccs$ageY==3]), main="Post-switch scores, age 3", xlab="Nr correct trials")
barplot(table(dccs$nCorPost[dccs$ageY==4]), main="Post-switch scores, age 4", xlab="Nr correct trials")
barplot(table(as.factor(dccs$nCorPost)[dccs$ageY==5]), main="Post-switch scores, age 5", xlab="Nr correct trials")


## ----wpt-glm, linewidth=56------------------------------------------------------------------------------------------------------------------------------------------------
data(WPT)
mod <- glm(y~c1+c2+c3+c4, data=WPT, family=binomial())
summary(mod)


## ----wpt-glm-block, linewidth=58------------------------------------------------------------------------------------------------------------------------------------------
WPT$block <- cut(WPT$trial,breaks=2)
summary(glm(r~(c1+c2+c3+c4)*block, data=WPT, 
            family=binomial()))


## ----conservation-hist1,echo=FALSE, fig.height=.7*figwidth_06, fig.width=figwidth_06, out.width=".6\\textwidth"-----------------------------------------------------------
library("hmmr")
data(conservation)
hist(conservation$r1,breaks=11,las=0, main="", xlab="Indicated height at occasion 1")


## ----echo=FALSE, results='hide'-------------------------------------------------------------------------------------------------------------------------------------------
y <- speed1$RT


## ----IGT-plot,echo=FALSE,fig.height=5, fig.width=7, out.width = ".8\\textwidth"-------------------------------------------------------------------------------------------
require(hmmr)
data(IGT)
pGood <- aggregate(IGT$deck %in% c("C","D") ~ IGT$trial,FUN=mean)
colnames(pGood) <- c("trial","p")
pGood$response <- "choice"
pWager <- aggregate(IGT$deck %in% c("C","D") & IGT$wager == "high" | IGT$deck %in% c("A","B") & IGT$wager == "low" ~ IGT$trial,FUN=mean)
colnames(pWager) <- c("trial","p")
pWager$response <- "wager"
pGood <- rbind(pGood,pWager)
pGood$response <- factor(pGood$response)
lo1 <- loess(p~trial,data=subset(pGood,response=="choice"))
lo2 <- loess(p~trial,data=subset(pGood,response=="wager"))
plot(p~trial,data=pGood,pch=c(16,0)[as.numeric(pGood$response)],ylab="proportion advantageous responses", frame.plot=FALSE)
lines(predict(lo1))
lines(predict(lo2),lty=2)
legend(80,.4,c("choice","wager"),pch=c(16,0),lty=c(1,2))


## ----echo=FALSE, results='hide'-------------------------------------------------------------------------------------------------------------------------------------------
y <- speed1$RT

