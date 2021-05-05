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
opts_chunk$set(cache.path = "cache/ch3/", fig.path = "figure/ch3/")
opts_chunk$set(cache.path = "cache/ch3/", fig.path = "figure/ch3/")


## ----echo=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------
library("hmmr")


## ----sp500-summary, echo=FALSE--------------------------------------------------------------------------------------------------------------------------------------------
library(hmmr)
data(sp500)
msp1 <- mean(sp500$logret)
sdsp1 <- sd(sp500$logret)


## ----fig-sp500, echo=FALSE, fig.height=.7*figwidth_06, fig.width=figwidth_06, out.width=".6\\textwidth"-------------------------------------------------------------------
plot(density(sp500$logret), las=1, frame=FALSE, xlab="log return", main="")

## ----echo=FALSE, results='hide'-------------------------------------------------------------------------------------------------------------------------------------------
x <- sp500$logret


## ----sp500-shapiro, echo=TRUE---------------------------------------------------------------------------------------------------------------------------------------------
shapiro.test(sp500[,"logret"]) 


## ----mod-123-states-sp500, echo=TRUE, results='hide'----------------------------------------------------------------------------------------------------------------------
set.seed(1)
m1 <- lca(sp500$logret, nclasses=1)
m2 <- lca(sp500$logret, nclasses=2)
m3 <- lca(sp500$logret, nclasses=3)

## ----echo=FALSE, results='hide'-------------------------------------------------------------------------------------------------------------------------------------------
x <- sp500$logret


## ----gof-table-sp500, echo=FALSE, results='asis'--------------------------------------------------------------------------------------------------------------------------
bic1 <- BIC(m1)
bic2 <- BIC(m2)
bic3 <- BIC(m3)
bics <- c(bic1,bic2,bic3)
ll <- c(logLik(m1),logLik(m2),logLik(m3))
dfs <- c(freepars(m1),freepars(m2),freepars(m3))
gof <- data.frame(components=c(1,2,3),logLik=ll,npar=dfs,BIC=bics)
require(xtable)
xt <- xtable(gof,caption="Goodness of fit statistics for 1- to 3-component Gaussian mixture models fitted on the 
\\code{sp500} data.", 
align="llrrr", label="tab:gof-sp500", display=c("d","d","f","d","f"))
print(xt, include.rownames=FALSE, caption.placement="top", table.placement = "th")


## ----pars-sp500-m2, echo=FALSE--------------------------------------------------------------------------------------------------------------------------------------------
summary(m2)


## ----pars-sp500-models, echo=FALSE----------------------------------------------------------------------------------------------------------------------------------------
me11 <- getpars(m1)[2]
sd11 <- getpars(m1)[3]
pars2 <- getpars(m2)
pr12 <- pars2[1]
pr22 <- pars2[2]
me12 <- pars2[3]
me22 <- pars2[5]
sd12 <- pars2[4]
sd22 <- pars2[6]
pars3 <- getpars(m3)
pr13 <- pars3[1]
pr23 <- pars3[2]
pr33 <- pars3[3]
me13 <- pars3[4]
me23 <- pars3[6]
me33 <- pars3[8]
sd13 <- pars3[5]
sd23 <- pars3[7]
sd33 <- pars3[9]


## ----fig-sp500-models,echo=FALSE,fig.height=1.4*figwidth_08, fig.width=1.2*figwidth_08, out.width=".8\\textwidth"---------------------------------------------------------
mt <- matrix(c(1:2),2)
layout(mt)

par(mar=c(5,5,1,3))
c1 <- function(x) {dnorm(x,msp1,sdsp1)}
plot(density(sp500$logret), frame=FALSE, las=1, xlab="log return", main="")
curve(c1,add=TRUE,lty=2)
legend("topleft",inset=0.02,lty=1:2,legend=c("data","1-component Gaussian"),bty="n")

par(mar=c(5,5,1,3))
c1 <- function(x) {pr12*dnorm(x,me12,sd12)}
c2 <- function(x) {pr22*dnorm(x,me22,sd22)}
c3 <- function(x) {c1(x)+c2(x)}
plot(density(sp500$logret), las=1, frame=FALSE, xlab="log return", main="")
curve(c1,add=TRUE, lty=3)
curve(c2,add=TRUE, lty=4)
curve(c3,add=TRUE, lty=2)
legend("topleft",inset=0.02,lty=1:4,legend=c("data","full model","'bull' component", "'bear' component"),bty="n")


## ----sp500-postm2, echo=TRUE----------------------------------------------------------------------------------------------------------------------------------------------
pst1 <- posterior(m2, type = "smoothing")[,1]


## ----fig-sp500-posterior,echo=FALSE,fig.height=.7*figwidth_06, fig.width=figwidth_06, out.width=".6\\textwidth"-----------------------------------------------------------
hist(pst1, las=1, xlab="posterior probability of component 1", main="", br=21)


## ----sp500-post-table, echo=TRUE------------------------------------------------------------------------------------------------------------------------------------------
pstState <- posterior(m2, type="local")
table(pstState)/sum(table(pstState)) 


## ----conservation-load-data,echo=FALSE------------------------------------------------------------------------------------------------------------------------------------
data(conservation)


## ----conservation-models, echo=TRUE, results='hide'-----------------------------------------------------------------------------------------------------------------------
m1 <- lca(conservation[,"r1"], nclasses=1)
set.seed(1)
m2 <- lca(conservation[,"r1"], nclasses=2)
m31 <- lca(conservation[,"r1"], nclasses=3)
m32 <- lca(conservation[,"r1"], nclasses=3)
m33 <- lca(conservation[,"r1"], nclasses=3)


## ----BIC-weights, echo=TRUE-----------------------------------------------------------------------------------------------------------------------------------------------
mods <- list(m1,m2,m31,m32,m33)
bics <- c(sapply(mods, BIC))
bicws <- exp(-bics/2)/(sum(exp(-bics/2)))


## ----conservation-gof-table, echo=FALSE, results='asis'-------------------------------------------------------------------------------------------------------------------
gofConservation <- data.frame(components=c(1,2,3,3,3), 
	logLik=c(sapply(mods,logLik)),
	npar=c(sapply(mods, freepars)),
	BIC=c(sapply(mods, BIC)),
	"BIC weight"=bicws)


## ----conservation-gof-table-2, echo=FALSE, results='asis'-----------------------------------------------------------------------------------------------------------------
require(xtable)
xt <- xtable(gofConservation,caption="Goodness of fit for models of the \\code{conservation} data.", 
align="llrrrr", label="tab:gof-conservation", 
display=c("d","d","f","d","f","f"))
print(xt, include.rownames=FALSE, caption.placement="top", table.placement = "th")


## ----fig-conservation-models,echo=FALSE,fig.height=.9*6,fig.width=.9*7,out.width=".8\\textwidth"--------------------------------------------------------------------------
mt <- matrix(c(1:4),2)
layout(mt)

m2p <- getpars(m2)
par(mar=c(5,3,1,2))
c1 <- function(x) {m2p[1]*dnorm(x,m2p[3],m2p[4])}
c2 <- function(x) {m2p[2]*dnorm(x,m2p[5],m2p[6])}
truehist(conservation[,"r1"],nb=21, frame=FALSE, las=1, 
xlab="estimated water height", main="", col=0, ylim=c(0,5))
curve(c1,add=TRUE,lty=1)
curve(c2,add=TRUE,lty=1)

legend("topleft",inset=0.02, legend=paste("BIC: ", round(BIC(m2),3)),bt="n")

m3p <- getpars(m31)
par(mar=c(5,3,1,2))
c1 <- function(x) {m3p[1]*dnorm(x,m3p[4],m3p[5])}
c2 <- function(x) {m3p[2]*dnorm(x,m3p[6],m3p[7])}
c3 <- function(x) {m3p[3]*dnorm(x,m3p[8],m3p[9])}
truehist(conservation[,"r1"],nb=21, frame=FALSE, las=1, 
xlab="estimated water height", main="", col=0, ylim=c(0,5))
curve(c1,add=TRUE,lty=1)
curve(c2,add=TRUE,lty=1)
curve(c3,add=TRUE,lty=1)

legend("topleft",inset=0.02, legend=paste("BIC: ", round(BIC(m31),3)),bt="n")

m3p <- getpars(m32)
par(mar=c(5,3,1,2))
c1 <- function(x) {m3p[1]*dnorm(x,m3p[4],m3p[5])}
c2 <- function(x) {m3p[2]*dnorm(x,m3p[6],m3p[7])}
c3 <- function(x) {m3p[3]*dnorm(x,m3p[8],m3p[9])}
truehist(conservation[,"r1"],nb=21, frame=FALSE, las=1, 
xlab="estimated water height", main="", col=0, ylim=c(0,5))
curve(c1,add=TRUE,lty=1)
curve(c2,add=TRUE,lty=1)
curve(c3,add=TRUE,lty=1)

legend("topleft",inset=0.02, legend=paste("BIC: ", round(BIC(m32),3)),bt="n")


m3p <- getpars(m33)
par(mar=c(5,3,1,2))
c1 <- function(x) {m3p[1]*dnorm(x,m3p[4],m3p[5])}
c2 <- function(x) {m3p[2]*dnorm(x,m3p[6],m3p[7])}
c3 <- function(x) {m3p[3]*dnorm(x,m3p[8],m3p[9])}
truehist(conservation[,"r1"],nb=21, frame=FALSE, las=1, 
xlab="estimated water height", main="", col=0, ylim=c(0,5))
curve(c1,add=TRUE,lty=1)
curve(c2,add=TRUE,lty=1)
curve(c3,add=TRUE,lty=1)

legend("topleft",inset=0.02, legend=paste("BIC: ", round(BIC(m33),3)),bt="n")



## ----fit-multivariate-normal-mixtures-conservation,results='hide'---------------------------------------------------------------------------------------------------------
dat <- conservation[,c("r1","r2")]
# select complete responses
dat <- dat[rowSums(is.na(dat)) == 0,]
set.seed(78)
fModels <- list() # to store fitted models
for(nstate in 2:6) {
  rModels <- list() # for all response models in mixture
  for(i in 1:nstate) {
    # Specify a bivariate Normal model
    rModels[[i]] <- list(MVNresponse(cbind(r1,r2) ~ 1, 
		data=dat))
  }
  # Specify the prior model as a multinomial distribution
  priorMod <- transInit(~1,ns=nstate,data=dat,
                        family=multinomial("identity"))
  # Combine response and prior models into a mixture model
  mod <- makeMix(response=rModels,prior=priorMod)
  # Fit the model
  fModels[[paste0(nstate)]] <- fit(mod)
}


## ----echo=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------
EM_trace <-
  structure(c(-151.342563573861, -125.033487572021, -93.4348716441717, 
              -65.2182225365054, -40.7870957631479, -19.4990530567296, 0.591896766541698, 
              12.3331945923905, 13.7467876018702, 15.0400592809634, 19.8988311696955, 
              24.0472889385552, 49.2044489292603, 49.269670931937, -10.6143942143691, 
              0.155346298017085, 0.139874339558149, 0.179243248206812, 0.284514035970437, 
              0.396919578704885, 0.475047069742901, 0.510323667888633, 0.522725324006747, 
              0.529198283233655, 0.53252655611144, 0.534711972243578, 0.536057319729643, 
              0.537161462161858, 0.537748023847359, 0.538075052792199, 0.137299348931141, 
              0.109238114484637, 0.0802500614608877, 0.0546435940737103, 0.0416945817391899, 
              0.038755778090045, 0.0388568721083729, 0.0359991079355733, 0.0323688581601407, 
              0.0289078355104577, 0.027093380133044, 0.027194587223016, 0.0270270256760749, 
              0.0270270270270225, 0.0270270270270227, 0.151092537214953, 0.116902975968256, 
              0.0856511710465194, 0.0672310763325427, 0.0572787283708481, 0.0541044403177602, 
              0.0540539448055304, 0.0540540194528427, 0.0540540390904268, 0.0540540459739194, 
              0.0540540488637095, 0.0540540502291127, 0.0540540509356494, 0.054054051322397, 
              0.0540540515361524, 0.151126947405084, 0.131682020505396, 0.115928589144508, 
              0.118740841069959, 0.130152602643953, 0.147627934265539, 0.166602068944034, 
              0.180313397394987, 0.184696045222209, 0.188541797682984, 0.190813191470664, 
              0.191171982049236, 0.191373176960145, 0.191458514518998, 0.191506733803775, 
              0.153732805644023, 0.125748176814539, 0.113918454869628, 0.112429375914893, 
              0.119083794238574, 0.134073897641448, 0.149429237376914, 0.161390021121621, 
              0.170209283383056, 0.176472750811926, 0.180741911601237, 0.183564015964015, 
              0.18537165039403, 0.186469174678917, 0.187021266598129, 0.251402062787715, 
              0.376554372669023, 0.425008475271645, 0.362441076638458, 0.25487071430255, 
              0.150390879942307, 0.0807342088765162, 0.0455181300882294, 0.0294734909105124, 
              0.0194970139092726, 0.012585495687767, 0.00795804480497697, 0.00501263387224232, 
              0.00324320860530601, 0.00231586824272226, 1.8602522802314, 1.9894472015785, 
              2.02739178667663, 2.03655512674069, 2.03530429296487, 2.03145001048962, 
              2.02909162618897, 2.02908878201269, 2.02950005584456, 2.03025621193249, 
              2.03085188135684, 2.03123534944704, 2.03149877461247, 2.03163962549635, 
              2.03171310740568, 1.51585115827695, 1.64707175053656, 1.73188534721637, 
              1.8456744954308, 1.92749871959268, 1.98110584226656, 2.01608389623661, 
              2.03318531543912, 2.03394382676052, 2.03483773650619, 2.03559829543356, 
              2.03616316130515, 2.03641904986453, 2.03658889168769, 2.03667936024351, 
              0.276016786105577, 0.0525307563558172, 0.00794496351472131, 0.00344905440647831, 
              0.00314703217190603, 0.00333984025966758, 0.00369560796290033, 
              0.00400717072231294, 0.00412496085191575, 0.00426432671145284, 
              0.00437214254055646, 0.00444435788590953, 0.00448848802805465, 
              0.00451363098450984, 0.00452703515669885, 0.227158086830618, 
              0.0594690520071989, 0.00578281613110661, -0.00349494823492928, 
              -0.00288602696983062, -0.000749842677056953, 0.000150626911890047, 
              0.000463759402291789, 0.000624666773368937, 0.000792804440614298, 
              0.000924471037265219, 0.0010167976543533, 0.00106692636772108, 
              0.00109754179933505, 0.00111408156791731, 0.659897588820437, 
              0.463055378653333, 0.313489579712525, 0.187205559863816, 0.0989775238759309, 
              0.048344750119714, 0.0190615435917834, 0.00626529927670808, 0.00629451729113475, 
              0.00639727950552122, 0.00646817364886511, 0.00650576094211432, 
              0.00654377861070502, 0.00656095450223229, 0.00656984923173582, 
              1.68240589851455, 1.77643052387624, 1.84523622166382, 1.83623441537299, 
              1.7193213548726, 1.6431841544103, 1.63294733841633, 1.59787694327782, 
              1.54588397902092, 1.48779609925592, 1.45155097321103, 1.45400533102002, 
              1.45000000836002, 1.45000000000003, 1.45000000000003, 1.04774894621223, 
              0.840895896871493, 0.54434832141394, 0.317077323070581, 0.293592754387177, 
              0.484878629603382, 0.619706599648774, 0.6354504199837, 0.592570238351187, 
              0.515888729618152, 0.467975991257304, 0.473411193146792, 0.465000017317175, 
              0.465000000000058, 0.465000000000055, 0.526802825321429, 0.383255221305808, 
              0.242420983099207, 0.134087013636493, 0.131225219787055, 0.116081964362287, 
              0.108411851837242, 0.0992563949506591, 0.0833414529650219, 0.061758032034326, 
              0.0449711885039704, 0.0464156355278944, 0.0440999999999999, 0.0441, 
              0.0441, 0.167476940364983, 0.0796345710979105, 0.0311014558860401, 
              0.0203168351088746, 0.0257015230908839, 0.0761463605348886, 0.128298510991971, 
              0.143617113177133, 0.138179766981906, 0.113070346676193, 0.0930202196165735, 
              0.0962205986613311, 0.0913499999999998, 0.09135, 0.09135, 0.863405397856965, 
              0.871108168137904, 0.676323771008166, 0.393251953560866, 0.221350906145195, 
              0.197126295412133, 0.220556574862944, 0.237232798552464, 0.241195739087673, 
              0.214383625003754, 0.192523566558518, 0.199469339869726, 0.189225, 
              0.189225, 0.189225, 1.38620539224551, 1.07695469869914, 0.591837055362012, 
              0.209809345959366, 0.0395920688630401, 0.0176573180167647, 0.0174990539651313, 
              0.0174996939541165, 0.0174998660422078, 0.0174999273646801, 0.0174999533417243, 
              0.0174999656808854, 0.0174999720865757, 0.0174999755995285, 0.0174999775437425, 
              1.6970315010154, 1.7787166380427, 1.87145306269958, 1.96147144459332, 
              2.07107296301258, 2.1363105912935, 2.13750001429658, 2.13750000260238, 
              2.13750000059735, 2.13750000015021, 2.13750000002816, 2.13749999998865, 
              2.13749999997381, 2.13749999996762, 2.13749999996488, 0.900545265548691, 
              1.04052843065184, 0.854033981482313, 0.382417242864614, 0.113782181160676, 
              0.0937190424034488, 0.0937684610887545, 0.0937686565213099, 0.0937687093332475, 
              0.0937687284370259, 0.0937687365600968, 0.0937687404287456, 0.0937687424413317, 
              0.0937687435454748, 0.0937687441571778, -0.0422580143816774, 
              -0.142037569780439, -0.132710503877003, -0.0640404795358331, 
              -0.0159964064639479, 0.00240396612024278, 0.00261875107835353, 
              0.00261875046372499, 0.00261875022524858, 0.00261875011313572, 
              0.00261875006075517, 0.00261875003452242, 0.00261875002046619, 
              0.00261875001261965, 0.00261875000822144, 0.516227622323892, 
              0.405767062144606, 0.285364401872314, 0.198861166010398, 0.0950085628515368, 
              0.0190816618537701, 0.0176187823842049, 0.0176187605767157, 0.0176187546612606, 
              0.0176187525448904, 0.0176187516454911, 0.017618751217513, 0.017618750995119, 
              0.0176187508730657, 0.0176187508054918, 1.72691688580229, 1.8538379313749, 
              1.96396640626212, 2.00906101032828, 2.03795081588215, 2.05299040677904, 
              2.05360088900275, 2.05260627332575, 2.05294789885868, 2.05262856483219, 
              2.05249471595012, 2.05256013613172, 2.05260522483176, 2.0526237604287, 
              2.05263448293863, 0.930183966081055, 0.661755329666309, 0.374856879486928, 
              0.197711401447706, 0.11249393997911, 0.132530541944914, 0.217579114419247, 
              0.282636061118913, 0.30351078759791, 0.323775456255147, 0.336121579051405, 
              0.338957296032235, 0.340554332633743, 0.341234638318225, 0.341616463168829, 
              0.487141961703446, 0.283764643400131, 0.0828704296332276, 0.0245716265197412, 
              0.00754732826818712, 0.00260462530637961, 0.00251977468236756, 
              0.00245967822997425, 0.00245614047424494, 0.00242105770571139, 
              0.00240221396717943, 0.00240173360943437, 0.00240154042798532, 
              0.00240145030906314, 0.00240139278493757, -0.028938098940293, 
              -0.0506987004432796, -0.00684580819173975, -0.0129027880258252, 
              -0.00774124899241664, 0.0039785899948556, 0.00355292651736576, 
              0.00250246513816813, 0.00273175781325717, 0.00243835070085522, 
              0.00233941567180639, 0.00243464282879652, 0.00249974310544161, 
              0.00252651094804747, 0.00254192937642254, 0.989936201426143, 
              0.843199819510769, 0.548555850243044, 0.297040975785128, 0.168229809359209, 
              0.170424009759411, 0.234317669754938, 0.276131008422557, 0.290711611435221, 
              0.305383368515377, 0.315165636992112, 0.318900642490141, 0.320994083882174, 
              0.32189170874093, 0.322390666311481, 1.51724185645275, 1.41206806103144, 
              0.922508518780151, 0.386401549503741, 0.141665940262888, 0.0805022294722967, 
              0.0744837012539755, 0.0774240238245965, 0.0799523075859362, 0.0812563837668886, 
              0.0818286551831428, 0.0820130484855779, 0.0819957649743338, 0.0818811861012596, 
              0.0817069424989701, 1.45847785242812, 1.38021194523369, 0.935263209918119, 
              0.442769756894869, 0.233815306639525, 0.195040082141191, 0.19259914894453, 
              0.195028025839843, 0.196771700111183, 0.19737210383363, 0.197403724794808, 
              0.197212399429399, 0.196955093136151, 0.196706252028475, 0.196489691150578, 
              0.745586340430873, 0.828675399923129, 1.01401298095575, 0.627205986941489, 
              0.22574456843718, 0.105235219482667, 0.0925739427032044, 0.0897514787532497, 
              0.0882601924612793, 0.0873096741930346, 0.0866581774195823, 0.086196728366596, 
              0.0858675881979237, 0.0856341055710056, 0.0854704209521824, 0.570276172185881, 
              0.729592767477898, 0.936026692654508, 0.554892749640686, 0.166470520319257, 
              0.0653309804241615, 0.0599890548142598, 0.0588700210559144, 0.058365679842509, 
              0.0580693417258985, 0.0578632135209089, 0.0577059924000887, 0.0575821648832131, 
              0.0574844644237475, 0.0574047849586344, 0.714177403416417, 0.775318836668359, 
              0.970168534358013, 0.614287754982182, 0.251612248358256, 0.155481215833879, 
              0.142074675755687, 0.134577329506428, 0.129866124970137, 0.126842144855037, 
              0.124900212294014, 0.123652731612372, 0.122858533892152, 0.1223692006224, 
              0.122105691955523, 1.26745954080961, 1.38728821891016, 1.52316621668371, 
              1.56280339028804, 1.48458126026692, 1.32157430230555, 1.10414778350635, 
              0.891189013221566, 0.83210134010455, 0.811982022719909, 0.768359252657855, 
              0.691761281248047, 0.579743141440931, 0.429006174279191, 0.247753515069955, 
              1.29813663248046, 1.42346840780635, 1.55876099821984, 1.5983750269039, 
              1.52420519361723, 1.36765364463442, 1.1567255323787, 0.949944086791271, 
              0.891016536028305, 0.869723200878716, 0.826187083398308, 0.751030279257564, 
              0.64139952399959, 0.493402830310349, 0.314279924456075, 0.984662441458181, 
              0.896877540450311, 0.759474866228276, 0.706577975743568, 0.779673169760187, 
              0.901512069921068, 0.991376277388028, 0.999313591604367, 1.00604081712029, 
              1.00651526469271, 0.983867248671814, 0.92701653193883, 0.816061200438689, 
              0.618199813769225, 0.308216149509137, 0.922978668131023, 0.84497189218999, 
              0.714787847450155, 0.667688348792591, 0.738142949286853, 0.85388986005648, 
              0.940094520930634, 0.948427119015025, 0.956763452657383, 0.958752250501522, 
              0.938086677939364, 0.884216940583971, 0.778004837942951, 0.587551800575196, 
              0.287700015216079, 0.901712438570313, 0.81454669073849, 0.683248471865682, 
              0.637140859201004, 0.703732220287786, 0.812936219871526, 0.895132279001303, 
              0.903476370805686, 0.912925488387924, 0.916045763457982, 0.897109909261102, 
              0.846086397367565, 0.744598801577609, 0.561671698508383, 0.272233297187294
  ), .Dim = c(15L, 37L), .Dimnames = list(NULL, c("ll_trace", "pr1", 
                                                  "pr2", "pr3", "pr4", "pr5", "pr6", "coefficients1", "coefficients2", 
                                                  "Sigma1", "Sigma2", "Sigma3", "coefficients1", "coefficients2", 
                                                  "Sigma1", "Sigma2", "Sigma3", "coefficients1", "coefficients2", 
                                                  "Sigma1", "Sigma2", "Sigma3", "coefficients1", "coefficients2", 
                                                  "Sigma1", "Sigma2", "Sigma3", "coefficients1", "coefficients2", 
                                                  "Sigma1", "Sigma2", "Sigma3", "coefficients1", "coefficients2", 
                                                  "Sigma1", "Sigma2", "Sigma3")))
par2cov <- function(x) {
  npar <- length(x)
  dim <- (sqrt(8*npar + 1) - 1)/2
  if(abs(dim - round(dim)) >= .Machine$double.eps^0.5) stop("number of parameters not suitable for par2cov")
  cov <- matrix(0.0,ncol=dim,nrow=dim)
  cov[lower.tri(cov,diag=TRUE)] <- x
  cov[upper.tri(cov)] <- t(cov)[upper.tri(cov)]
  cov
}
Sigma62 <- par2cov(fModels[["6"]]@response[[2]][[1]]@parameters$Sigma)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------
sapply(fModels,logLik)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------
fModels[["3"]] <- fit(fModels[["3"]])
sapply(fModels,logLik)


## ----fig-multivariate-Gaussian-mixture, echo=FALSE, fig.height=.9*7, fig.width=.9*5.2, out.width=".8\\textwidth"----------------------------------------------------------
mt <- matrix(c(1:6),ncol=2,byrow=TRUE)
layout(mt)
par(mar=c(3,5,2,0),mgp=c(1.8,.75,0))
plot(c(-2,4),c(-2,4),type="n",xlab="", ylab="response 2",main="Data",frame.plot=FALSE)
points(dat,col=rgb(0,0,0,alpha=.2))
for(nstate in 2:6) {
  par(mar=c(3,5,2,0))
  if(nstate > 4) xlab = "response 1" else xlab = ""
  if(nstate %in% c(1,3,5)) ylab = "response 2" else ylab = ""
  plot(c(-2,4),c(-2,4),type="n",xlab=xlab, ylab=ylab,main=paste(nstate,"-component model",sep=""),frame.plot=FALSE)
  points(dat,col=rgb(0,0,0,alpha=.2))
  ell <- list()
  for(st in 1:nstate) {
    cv <- par2cov(fModels[[paste0(nstate)]]@response[[st]][[1]]@parameters$Sigma)
    mn <- fModels[[paste0(nstate)]]@response[[st]][[1]]@parameters$coefficients
    ell[[st]] <- ellipse::ellipse(cv,centre = mn)
    lines(ell[[st]],lty=st)
  }
}


## ----results='hide'-------------------------------------------------------------------------------------------------------------------------------------------------------
ok <- FALSE
while(!ok) {
  tmp <- try(fit(fModels[["6"]]),silent=TRUE)
  if(!inherits(tmp,"try-error") && 
     startsWith(tmp@message,"Log likelihood converged") &&
     logLik(tmp) >= logLik(fModels[["5"]]))  ok <- TRUE
}
fModels[["6"]] <- tmp


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------
rbind(AIC=sapply(fModels,AIC),BIC=sapply(fModels,BIC))


## ----results='hide'-------------------------------------------------------------------------------------------------------------------------------------------------------
summary(fModels[["4"]],compact=FALSE)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------
gm4indep <- fit(mix(list(r1~1,r2~1),nstates=4,
  family=list(gaussian(),gaussian()),data=dat))


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------
c("AIC"=AIC(gm4indep),"BIC"=BIC(gm4indep))


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------
llratio(fModels[["4"]],gm4indep)


## ----balance-data, echo=FALSE, results='hide'-----------------------------------------------------------------------------------------------------------------------------
require(hmmr)
require(xtable)
data(balance)
options(digits=4)


## ----balance-cor----------------------------------------------------------------------------------------------------------------------------------------------------------
data(balance)
cor(balance[,c("d1","d2","d3","d4")])


## ----balance-123-states, echo=FALSE, results='hide'-----------------------------------------------------------------------------------------------------------------------
mods <- list() # four binary items on the balance scale task

mods[[1]] <- mix(list(d1~1,d2~1,d3~1,d4~1), 
	data=balance, nstates=1,
	family=list(multinomial("identity"),
		multinomial("identity"),
		multinomial("identity"),
		multinomial("identity")))
		
mods[[2]] <- mix(list(d1~1,d2~1,d3~1,d4~1), 
	data=balance, nstates=2,
	family=list(multinomial("identity"),
		multinomial("identity"),
		multinomial("identity"),
		multinomial("identity")))
		
mods[[3]] <- mix(list(d1~1,d2~1,d3~1,d4~1), 
	data=balance, nstates=3,
	family=list(multinomial("identity"),
		multinomial("identity"),
		multinomial("identity"),
		multinomial("identity")))
		
mods[[4]] <- mix(list(d1~1,d2~1,d3~1,d4~1), 
	data=balance, nstates=4,
	family=list(multinomial("identity"),
		multinomial("identity"),
		multinomial("identity"),
		multinomial("identity")))
		
set.seed(1)
mods[[1]] <- fit(mods[[1]])
mods[[2]] <- fit(mods[[2]])
mods[[3]] <- fit(mods[[3]])
mods[[4]] <- fit(mods[[4]])



## ----balance-model-parameters,echo=FALSE,results='hide'-------------------------------------------------------------------------------------------------------------------
pr1 <- 1
pr2 <- getpars(mods[[2]])[1:2]
pr3 <- getpars(mods[[3]])[1:3]
pr4 <- getpars(mods[[4]])[1:4]

cps1 <- summary(mods[[1]])[,c(2,4,6,8)]
cps2 <- summary(mods[[2]])[,c(2,4,6,8)]
cps3 <- summary(mods[[3]])[,c(2,4,6,8)]
cps4 <- summary(mods[[4]])[,c(2,4,6,8)]


## ----fig-balance-lca,echo=FALSE,fig.height=7,fig.width=7,out.width=".9\\textwidth"----------------------------------------------------------------------------------------

layout(matrix(1:4,2,byrow=TRUE))

plot(1:4,cps1,
	ylim=c(0,1),
	type="b",
	frame=FALSE,
	las=1, 
	ylab="Probability of success", 
	xlab="item",
	axes=FALSE,
	main="LCA parameters of 1-class model.")
axis(side=1,at=1:4)
axis(side=2,las=1)
legend("bottomleft",inset=0.1,
	legend=paste("class 1, pr = 1.0"),
	lty=11,pch=1)

plot(1:4,cps2[1,],
	ylim=c(0,1),
	type="b",
	frame=FALSE,
	las=1, 
	ylab="Probability of success", 
	xlab="item",
	axes=FALSE,
	main="LCA parameters of 2-class model.")
axis(side=1,at=1:4)
axis(side=2,las=1)
lines(1:4,cps2[2,],type="b",lty=2,pch=2)
legend("bottomleft",inset=0.1,
	legend=c(paste("class 1, pr =",round(pr2[1],2)), 
		paste("class 2, pr =",round(pr2[2],2))),
	lty=1:2,pch=1:2)


plot(1:4,cps3[1,],
	ylim=c(0,1),
	type="b",
	frame=FALSE,
	las=1, 
	ylab="Probability of success", 
	xlab="item",
	axes=FALSE,
	main="LCA parameters of 3-class model.")
axis(side=1,at=1:4)
axis(side=2,las=1)
lines(1:4,cps3[2,],type="b",lty=2,pch=2)
lines(1:4,cps3[3,],type="b",lty=3,pch=3)
legend("topleft",inset=0.1,
	legend=c(paste("class 1, pr =",round(pr3[1],2)), 
		paste("class 2, pr =",round(pr3[2],2)),
		paste("class 3, pr =",round(pr3[3],2))),
	lty=1:3,pch=1:3)


plot(1:4,cps4[1,],
	ylim=c(0,1),
	type="b",
	frame=FALSE,
	las=1, 
	ylab="Probability of success", 
	xlab="item",
	axes=FALSE,
	main="LCA parameters of 4-class model.")
axis(side=1,at=1:4)
axis(side=2,las=1)
lines(1:4,cps4[2,],type="b",lty=2,pch=2)
lines(1:4,cps4[3,],type="b",lty=3,pch=3)
lines(1:4,cps4[4,],type="b",lty=4,pch=4)
legend("topleft",inset=0.1,
	legend=c(paste("class 1, pr =",round(pr4[1],2)), 
		paste("class 2, pr =",round(pr4[2],2)),
		paste("class 3, pr =",round(pr4[3],2)),
		paste("class 4, pr =",round(pr4[4],2))),
	lty=1:4,pch=1:4)



## ----echo=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------
options(digits=3)


## ----balance-cor-2--------------------------------------------------------------------------------------------------------------------------------------------------------
# assign participants to classes
balance$pst2 <- posterior(mods[[2]], type="local")
# compute correlation within each class
cor(balance[balance$pst2==1,c("d1","d2","d3","d4")])
cor(balance[balance$pst2==2,c("d1","d2","d3","d4")])

## ----echo=FALSE, results='hide'-------------------------------------------------------------------------------------------------------------------------------------------
x <- sp500$logret


## ----gof-table-balance-lca, echo=FALSE, results='asis'--------------------------------------------------------------------------------------------------------------------
require(xtable)

bics <- c(sapply(mods, BIC))
bicsd <- bics - min(bics)
bicws <- exp(-bicsd/2)/(sum(exp(-bicsd/2)))

gofBalance <- data.frame(classes=1:4, 
	logLik=c(sapply(mods,logLik)),
	npar=c(sapply(mods, freepars)),
	BIC=bics,
	"BIC weight"=bicws)
	
xt <- xtable(gofBalance,caption="Goodness of fit statistics for 1- to 4-class latent class models fitted on the four binary items of the \\code{balance} data.", 
align="llrrrr", label="tab:gof-balance", display=c("d","d","f","d","f","f"))
print(xt, include.rownames=FALSE, caption.placement="top", table.placement = "th")


## ----balance-3class, echo=FALSE, results='asis'---------------------------------------------------------------------------------------------------------------------------
pars <- getpars(mods[[3]])
unc <- pars[1:3]
cond <- matrix(matrix(pars[4:27],ncol=2,byrow=TRUE)[,2],ncol=4,byrow=TRUE)
pars <- data.frame(class=1:3, size=unc, cnd=cond)
names(pars) <- c("class", "size", "item 1", "item 2", "item 3", "item 4")
xt <- xtable(pars,caption="Fitted parameters of the latent class model with 3 classes of the \\code{balance} data. Size is the prior component probability, the remaining parameters are 
             probability correct for each item.", 
	align="llrrrrr", label="tab:balance-3class", 
display=c("d","d","f","f","f","f","f"))
print(xt, include.rownames=FALSE, caption.placement="top", table.placement = "th")


## ----balance-2class-homogeneity, echo=FALSE, results='hide'---------------------------------------------------------------------------------------------------------------
set.seed(2)
p1 <- runif(1,.8,1)
p2 <- runif(1,0,.2)
mod2 <- mix(list(d1~1,d2~1,d3~1,d4~1), 
	data=balance, nstates=2,
	family=list(multinomial("identity"),
  		multinomial("identity"),
  		multinomial("identity"),
  		multinomial("identity")),
	respst=c(rep(c(1-p1,p1),4),rep(c(1-p2,p2),4)))
  		
equal <- c(1,1,
	1,3,1,3,1,3,1,3,
 	1,5,1,5,1,5,1,5)



## ----balance-3class-homogeneity, echo=FALSE, results='hide'---------------------------------------------------------------------------------------------------------------
set.seed(1)
p1 <- runif(1,.8,1)
p2 <- runif(1,0,.2)
p3 <- runif(1,.4,.7)
mod3 <- mix(list(d1~1,d2~1,d3~1,d4~1), 
	data=balance, nstates=3,
	family=list(multinomial("identity"),
  		multinomial("identity"),
  		multinomial("identity"),
  		multinomial("identity")),
 		respst=c(rep(c(1-p1,p1),4),rep(c(1-p2,p2),4),rep(c(1-p3,p3),4)))
 		
equal <- c(1,1,1,
	1,3,1,3,1,3,1,3,
 	1,5,1,5,1,5,1,5,
 	1,4,1,4,1,4,1,4)		
 		
fmod3a <- fit(mod3,equal=equal)

set.seed(1)
p1 <- runif(1,.8,1)
p2 <- runif(1,0,.2)
mod3 <- mix(list(d1~1,d2~1,d3~1,d4~1), 
	data=balance, nstates=3,
	family=list(multinomial("identity"),
  		multinomial("identity"),
  		multinomial("identity"),
  		multinomial("identity")),
 		respst=c(rep(c(1-p1,p1),4),rep(c(1-p2,p2),4),runif(8)))
 		
equal <- c(1,1,1,
	1,3,1,3,1,3,1,3,
 	1,5,1,5,1,5,1,5,
 	1,1,1,1,1,1,1,1)		
 		
fmod3b <- fit(mod3,equal=equal)


## ----balance-3class-h-all, echo=FALSE, results='asis'---------------------------------------------------------------------------------------------------------------------
pars <- getpars(fmod3a)
unc <- pars[1:3]
cond <- matrix(matrix(pars[4:27],ncol=2,byrow=TRUE)[,2],ncol=4,byrow=TRUE)
pars <- data.frame(class=1:3, size=unc, cnd=cond)
names(pars) <- c("class", "size", "item 1", "item 2", "item 3", "item 4")
xt <- xtable(pars,caption="Fitted parameters of 3-class model of the \\code{balance} data with item homogeneity constraints applied.", 
	align="llrrrrr", label="tab:balance-3class-h-all", 
display=c("d","d","f","f","f","f","f"))
print(xt, include.rownames=FALSE, caption.placement="top", table.placement = "th")


## ----balance-3class-h-12, echo=FALSE, results='asis'----------------------------------------------------------------------------------------------------------------------
pars <- getpars(fmod3b)
unc <- pars[1:3]
cond <- matrix(matrix(pars[4:27],ncol=2,byrow=TRUE)[,2],ncol=4,byrow=TRUE)
pars <- data.frame(class=1:3, size=unc, cnd=cond)
names(pars) <- c("class", "size", "item 1", "item 2", "item 3", "item 4")
xt <- xtable(pars,caption="Fitted parameters of 3-class model of the \\code{balance} data with item homogeneity constraints in the first and second class.", 
align="llrrrrr", label="tab:balance-3class-h-12", 
display=c("d","d","f","f","f","f","f"))
print(xt, include.rownames=FALSE, caption.placement="top", table.placement = "th")


## ----gof-table-balance-homogeneity, echo=FALSE, results='asis'------------------------------------------------------------------------------------------------------------
require(xtable)

mds <- list(mods[[3]],fmod3a,fmod3b)

bics <- c(sapply(mds, BIC))
llr <- c(NA,llratio(mods[[3]],fmod3a)@value, 
llratio(mods[[3]],fmod3b)@value)
dfs <- c(sapply(mds, freepars))
ddfs <- c(NA,llratio(mods[[3]],fmod3a)@df, 
llratio(mods[[3]],fmod3b)@df)
ps <- c(NA,round(pchisq(llratio(mods[[3]],fmod3a)@value,llratio(mods[[3]],fmod3a)@df,lower.tail=FALSE),3),
round(pchisq(llratio(mods[[3]],fmod3b)@value,llratio(mods[[3]],fmod3b)@df,lower.tail=FALSE),3))

gofBalance <- data.frame(models=c("3","3-all","3-cor/inc"), 
	logLik=c(sapply(mds,logLik)),
	npar=dfs,
	"LR"=llr,
	df=ddfs,
	"p-value"=ps)
	
xt <- xtable(gofBalance,caption="Goodness of fit statistics for the 3-class model and the models incorporating item homogeneity constraints.", 
align="llrrrrr", label="tab:gof-balance-homo", 
display=c("d","d","f","d","f","d","f"))
print(xt, include.rownames=FALSE, caption.placement="top", table.placement = "th")


## ----fig-post-by-age,echo=FALSE,fig.height=2.5,fig.width=7.5,out.width=".9\\textwidth"------------------------------------------------------------------------------------
balance$pst <- posterior(mods[[3]], type="local")
layout(matrix(1:3,nrow=1))
hist(balance$age[balance$pst==1],br=seq(5,19,by=2), main="'correct' class",
		xlab="age")
hist(balance$age[balance$pst==2],br=seq(5,19,by=2), main="'incorrect' class", 
		xlab="age")
hist(balance$age[balance$pst==3],br=seq(5,19,by=2), main="intermediate class",
		xlab="age")


## ----echo=FALSE, results='hide'-------------------------------------------------------------------------------------------------------------------------------------------
x <- sp500$logret


## ----balance-sum-table, echo=FALSE, results='asis'------------------------------------------------------------------------------------------------------------------------
library("depmixS4")
data(balance)
balance$bsum <- rowSums(balance[,c("d1","d2","d3","d4")])
sumtab <- as.integer(t(table(balance$bsum)))
ages <- as.numeric(by(balance$age,balance$bsum,mean))
xt <- rbind(sumtab, ages)
rownames(xt) <- c("# participants","mean ages")
colnames(xt) <- 0:4
require(xtable)
xt <- xtable(xt,caption="Sum scores of four items on the balance 
scale task and the mean ages of participants with that score.", 
align="lrrrrr", label="tab:balsum2", 
digits=matrix(c(0,0,0,0,0,0,1,1,1,1,1,1), nrow=2, byrow=TRUE))
print(xt, caption.placement="top", table.placement = "th")


## ----balance-regression, echo=TRUE----------------------------------------------------------------------------------------------------------------------------------------
balance$bsum <- rowSums(balance[,c("d1","d2","d3","d4")])
binreg <- glm(cbind(bsum,4-bsum)~age+sex, family=binomial, 
              data=balance)


## ----balance-regression-summary, echo=TRUE, linewidth=52------------------------------------------------------------------------------------------------------------------
summary(binreg)


## ----balance-regression-pars, echo=FALSE, results='hide'------------------------------------------------------------------------------------------------------------------
pars <- coefficients(binreg)


## ----fig-balance-logitpredicted,echo=FALSE,fig.height=.9*4,fig.width=.9*7, out.width=".6\\textwidth"----------------------------------------------------------------------
par(mar=c(5,4,1,1))
plot(5:19,by(balance$bsum,balance$age,mean)/4,
	ylim=c(0,1),ty="b", frame=FALSE, #main="Probability correct by age, data and regression model", 
	xlab="age", ylab="p(correct)", las=1)
curve(plogis(pars[1]+x*pars[2]),5,19, add=TRUE)


## ----fig-balance-scoresbyage,echo=FALSE,fig.height=6,fig.width=7,out.width=".8\\textwidth"--------------------------------------------------------------------------------
nrgr=4
balance$agegr <- cut(balance$age,br=nrgr)
bsums <- by(balance$bsum,balance$agegr,table)
bsums <- matrix(unlist(bsums),nr=nrgr,by=T)
layout(matrix(1:nrgr,nr=nrgr/2,byrow=TRUE))
for(i in 1:nrgr) {
  if(i > 2) xlab <- "sum score" else xlab = ""
  if(i %in% c(1,3)) ylab <- "Frequency" else ylab = ""
	barplot(bsums[i,],names=0:4, main=paste("age",levels(balance$agegr)[i]),xlab=xlab,ylab=ylab)
}


## ----echo=FALSE, results='hide'-------------------------------------------------------------------------------------------------------------------------------------------
x <- sp500$logret


## ----echo=TRUE, eval=FALSE------------------------------------------------------------------------------------------------------------------------------------------------
## m2 <- mix(cbind(bsum,4-bsum)~1, data=balance, nstates=2,
##           family=binomial())


## ----balance-sum-12r-states, echo=TRUE, results='hide'--------------------------------------------------------------------------------------------------------------------
set.seed(2)
m1 <- fit(mix(cbind(bsum,4-bsum)~1, data=balance, nstates=1, 
              family=binomial()))
mr <- fit(mix(cbind(bsum,4-bsum)~age, data=balance, 
              nstates=1, family=binomial()))
m2 <- fit(mix(cbind(bsum,4-bsum)~1, data=balance, nstates=2, 
              family=binomial()))
m2aDir <- fit(mix(cbind(bsum,4-bsum)~age, data=balance, 
                  nstates=2, family=binomial()))
m2aInd <- fit(mix(cbind(bsum,4-bsum)~1, data=balance, 
                  nstates=2, family=binomial(),	prior=~age))


## ----balance-gof-measures, echo=FALSE-------------------------------------------------------------------------------------------------------------------------------------
mods <- list(m1,mr,m2,m2aDir,m2aInd)
gofBalance <- 
data.frame(model=c("m1","mr"," m2","m2aDir","m2aInd"),
  description = c("base 1","1 + age = regression","base 2","2 + age direct","2 + age class"),
	logLik=c(sapply(mods,logLik)),
	npar=c(sapply(mods, freepars)),
	BIC=c(sapply(mods, BIC)))


## ----gof-table-balance-sum, echo=FALSE, results='asis'--------------------------------------------------------------------------------------------------------------------
require(xtable)

xt <- xtable(gofBalance,caption="Goodness-of-fit statistics for 1 and 2 class binomial mixture models fitted on the \\code{balance} data. For comparison, also the goodness-of-fit of a regression model is included with \\code{age} as independent variable.", 
align="lllrrr", label="tab:gof-balance-sum", 
display=c("d","d","d","f","d","f"))
print(xt, include.rownames=FALSE, caption.placement="top", table.placement = "th")

## ----balance-sum-2class-summ, echo=FALSE, results='asis'------------------------------------------------------------------------------------------------------------------
pars <- getpars(m2)
unc <- pars[1:2]
cond <- plogis(pars[3:4])


## ----balance-sum-2class, echo=FALSE, results='asis'-----------------------------------------------------------------------------------------------------------------------
require(xtable)
pars <- getpars(m2)
unc <- pars[1:2]
cond <- plogis(pars[3:4])
pars <- data.frame(class=1:2, size=unc, conditional=cond)
xt <- xtable(pars, caption="Fitted parameters of the 2-class model of the \\code{balance} data. `Size' is the prior component probability and `conditional' the component specific probability of success for the binomial distribution.", 
align="llrr", label="tab:balance-sum-2class", 
display=c("d","d","f","f")) #print(xt, include.rownames=FALSE)


## ----fig-balance-modelpredicted,echo=FALSE,fig.height=4.5,fig.width=6,out.width=".6\\textwidth"---------------------------------------------------------------------------
par(mar=c(5,4,1,1))
pmr <- matrix(0,nrow=50,ncol=5)
pm2a <- matrix(0,nrow=50,ncol=5)
for(i in 1:50) {
    x <- simulate(mr)
    pmr[i,] <- table(x@response[[1]][[1]]@y[,1])
    z <- simulate(m2aInd)
    pm2a[i,] <- table(z@response[[1]][[1]]@y[,1])
}

prior2 <- predict(m2aInd@prior)

p1 <- predict(m2aInd@response[[1]][[1]])[1,]

predp1 <- dbinom(0:4,4,p1)

p2 <- predict(m2aInd@response[[2]][[1]])[1,]

predp2 <- dbinom(0:4,4,p2)


modelPred <- 
rbind(table(balance$bsum),round(colMeans(pm2a),0),round(colMeans(pmr),0))

barplot(modelPred,beside=TRUE, xlab="sum score",ylab = "Frequency")

legend("topleft", legend=c("data","best model","regression"), 
    col=grey.colors(3),inset=0.05,pch=15)



## ----echo=FALSE, results='hide'-------------------------------------------------------------------------------------------------------------------------------------------
x <- sp500$logret


## ----balance-sum-posterior, echo=FALSE, results='hide'--------------------------------------------------------------------------------------------------------------------
balance$post <- posterior(m2aInd, type="local")


## ----echo=FALSE, results='hide'-------------------------------------------------------------------------------------------------------------------------------------------
x <- sp500$logret


## ----fig-balance-2-class-age,echo=FALSE,fig.height=4,fig.width=7,out.width=".7\\textwidth"--------------------------------------------------------------------------------
tt <- table(balance$post,balance$age)
tt <- (t(tt))/colSums(tt)
barplot(t(tt),xlab="age",ylab="posterior probability")


## ----fig-balance-post,echo=FALSE,fig.height=3.5,fig.width=5---------------------------------------------------------------------------------------------------------------
par(mar=c(5,4,1,1))
balance$post <- posterior(m2aInd, type="smoothing")[,1]
hist(balance$post, br=11, xlab="Posterior probability",main="")


## ----dccs-sum-2class, echo=TRUE-------------------------------------------------------------------------------------------------------------------------------------------
data(dccs)
m2 <- mix(response=cbind(nCorPost,6-nCorPost)~1,nstates=2,
          data=dccs,family=binomial())
set.seed(1234)
fm2 <- fit(m2)
summary(fm2)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(1234)
fm3 <- fit(mix(response=cbind(nCorPost,6-nCorPost)~1, 
               nstates=3, data=dccs,family=binomial()))
summary(fm3)


## ----dccs-2-component-mixture-plot,fig.width=6,fig.height=2.8,echo=FALSE,out.width=".8\\textwidth"------------------------------------------------------------------------
layout(matrix(1:2,ncol=2,byrow=TRUE))
par(mar=c(5,4,1,1))

pars <- getpars(fm2)
pars[3:4] <- plogis(pars[3:4])
bp <- barplot(table(dccs$nCorPost), main="2 component",xlab="Nr correct trials",ylab="Frequency")
points(bp,(pars[1]*93*dbinom(0:6,size=6,prob=pars[3])+pars[2]*93*dbinom(0:6,size=6,prob=pars[4])),pch=16)

pars <- getpars(fm3)
pars[4:6] <- plogis(pars[4:6])
bp <- barplot(table(dccs$nCorPost), main="3 component",xlab="Nr correct trials")
points(bp,(pars[1]*93*dbinom(0:6,size=6,prob=pars[4])+pars[2]*93*dbinom(0:6,size=6,prob=pars[5])+pars[3]*93*dbinom(0:6,size=6,prob=pars[6])),pch=16)


## ----echo=FALSE, results='hide'-------------------------------------------------------------------------------------------------------------------------------------------
x <- sp500$logret


## ----echo=FALSE,results='hide'--------------------------------------------------------------------------------------------------------------------------------------------
seed <- 1234
set.seed(seed)
fm1 <- fit(mix(response=cbind(nCorPost,6-nCorPost)~1,nstates=1,data=dccs,family=binomial()))
set.seed(seed)
fm4 <- fit(mix(response=cbind(nCorPost,6-nCorPost)~1,nstates=4,data=dccs,family=binomial()))
mods <- list(fm1,fm2,fm3,fm4)
gofdccs <- 
data.frame(components=1:4, 
  logLik=c(sapply(mods,logLik)),
  npar=c(sapply(mods, freepars)),
  AIC=c(sapply(mods, AIC)),
	BIC=c(sapply(mods, BIC)))


## ----dccs-table-balance-sum, echo=FALSE, results='asis'-------------------------------------------------------------------------------------------------------------------
require(xtable)
xt <- xtable(gofdccs,caption="Goodness-of-fit statistics for 1- to 4-component binomial mixture models fitted on the \\code{dccs} data.", 
align="llrrrr", label="tab:gof-dccs-sum", 
display=c("d","d","f","d","f","f"))
print(xt, include.rownames=FALSE, caption.placement="top", table.placement = "th")


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------
llratio(fm3,fm2)


## ----dccs bootstrap,eval=FALSE, linewidth=54------------------------------------------------------------------------------------------------------------------------------
## library(boot)
## boot.fun <- function(model) {
##   ok <- FALSE
##   while(!ok) {
##     # simulate data
##     bootdat <- data.frame(
##                 simulate(model)@response[[1]][[1]]@y)
##     fboot2 <- try({
##         mod <- mix(cbind(X1,X2)~1, nstates=2,
##                    family=binomial(), data=bootdat)
##         mod <- setpars(mod,getpars(fm2))
##         fit(mod,emcontrol=em.control(random.start=FALSE))
##     },TRUE)
##     fboot3 <- try({
##         mod <- mix(cbind(X1,X2)~1, nstates=3,
##                    family=binomial(), data=bootdat)
##         mod <- setpars(mod,getpars(fm3))
##         fit(mod,emcontrol=em.control(random.start=FALSE))
##     },TRUE)
##     if(!inherits(fboot2,"try-error") &&
##        !inherits(fboot3,"try-error") &&
##        logLik(fboot3) >= logLik(fboot2)) ok <- TRUE
##   }
##   llratio(fboot3,fboot2)@value
## }
## set.seed(1234)
## dccs_boot_LR <- boot(fm2, boot.fun, R=10000,
##                      sim="parametric")


## ----load_dccs_boot-------------------------------------------------------------------------------------------------------------------------------------------------------
data("dccs_boot_LR")


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------
sum(dccs_boot_LR$t > llratio(fm3,fm2)@value)/dccs_boot_LR$R


## ----echo=FALSE,fig.height=5,fig.width------------------------------------------------------------------------------------------------------------------------------------
x <- seq(0,max(dccs_boot_LR$t),by=.05)
hist(dccs_boot_LR$t,breaks=100,prob=TRUE,xlab="LR",main="")
abline(v=llratio(fm3,fm2)@value,lty=2)
lines(x,dchisq(x,df=2))

