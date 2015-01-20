setwd("~/Dropbox/Poverty_suburbanization/data")
hh <- read.table("modeldata042213.csv", sep=",", header=T)
names(hh)
hh <- hh[, -c(34:43)] # the last variables are PUMA attributes
puma <- read.table("PUMA_data_040414.csv", sep=",", header=T)
names(puma)
require(gdata)
chicago <- read.xls("Puma2000_Chicago.xls")
puma$chicago <- 0
puma$chicago[puma$PUMA %in% chicago$PUMA1] <- 1

hh.ids <- unique(hh$SERIAL)
puma.ids <- unique(hh$PUMA)
data <- data.frame(expand.grid(puma.id=puma.ids,
                               hh.id=hh.ids)
)

#data <- data[,c('hh.id', 'puma.id')]

### begin testing code ###
if(0) {
hh.ids <- unique(hh$SERIAL)[1:9]
#puma.ids <- unique(hh$PUMA)
puma.ids <- c(3402, 3501, 3102)
data <- data.frame(expand.grid(puma.id=puma.ids,
                               hh.id=hh.ids)
                   )

data$choice <- c(1, 0, 0, 1, 0, 0, 1, 0, 0,
                 0, 1, 0, 0, 1, 0, 0, 1, 0,
                 0, 0, 1, 0, 0, 1, 0, 0, 1
)

data <- data[,c('hh.id', 'puma.id', 'choice')]

hh <- hh[, c("SERIAL", "PUMA")]
puma <- puma[, c("PUMA", "povrate", "count")]
}
### end testing code ###

#attach hh attrs
data <- merge(data, hh, by.x="hh.id", by.y="SERIAL", sort=FALSE)
#attach puma attrs
data <- merge(data, puma, by.x="puma.id", by.y="PUMA", sort=FALSE)
#id chosen puma

data$choice <- FALSE
data$choice[data$puma.id == data$PUMA] <- TRUE
data <- data[with(data, order(hh.id, puma.id)), ]

names(data)
dim(data)
head(data)
sum(data$choice)

puma
install.packages("mlogit")
install.packages("stargazer")
require(mlogit)
require(stargazer)
data.m <- mlogit.data(data, shape="long", choice="choice",
                      id.var="hh.id", alt.var='puma.id')

nlf <- as.integer(!data.m$lf)
#interaction variables
data.m <- within(data.m, {employed.x.aveaccPUMA <- employed * aveaccPUMA
                          unemployed.x.aveaccPUMA <- unemployed * aveaccPUMA
                          lf.x.aveaccPUMA <- lf * aveaccPUMA
                          nlf.x.aveaccPUMA <- nlf * aveaccPUMA
                          unemployed.x.aveaccPUMA <- unemployed * aveaccPUMA
                          Asian.x.asianpct <- Asian.x * Asian.y
                          AfriAme.x.blackpct <- AfriAme * blackpct
                          hispanic.x.hispanicpct <- hispanic * hispanicpct
                          wchi.x.schoolscore <- wchi * schscore
                          ln.units <- log(Hsing)
                          })

data.m
head(data.m)

mfR2.null <- function(object, id.var){
  data.name <- object$call$data
  data <- eval(data.name, envir=parent.frame())
  logLik0 <- sum(-log(table(data[[id.var]])))
  1-logLik(object)/logLik0
}

lratio.null <- function(object, id.var){
  freq <- object$freq
  data.name <- object$call$data
  data <- eval(data.name,envir=parent.frame())
  llo <- sum(-log(table(data[[id.var]])))
  parameter.0 <- 0
  stat <- -2*(llo-logLik(object))
  names(stat) <- "chisq"
  parameter <- length(coef(object))-parameter.0
  names(parameter) <- "df"
  pval <- pchisq(stat,df=parameter,lower.tail=FALSE)
  lrtest <- list(statistic = stat,
                 data.name = data.name,
                 p.value = pval,
                 parameter = parameter,
                 method = "likelihood ratio test")
  class(lrtest) <- "htest"
  lrtest
}

logLik.0 <- length(hh.ids) * log(1/length(puma.ids))

m.fit.c <- mlogit(choice~1, data.m); summary(m.fit.c)
logLik.c <- logLik(m.fit.c)

#base model
m.fit1 <- mlogit(choice~lowrentshare+Asian.x.asianpct+whitepct+AfriAme.x.blackpct+hispanic.x.hispanicpct+ln.units+schscore|0, data.m); summary(m.fit1)
#pseduo rho squared
with(m.fit1, 1 - logLik/logLik.c)
#or more aggressive pseduo rho squared
with(m.fit1, 1 - logLik/logLik.0)
m.fit1$mfR2 <- mfR2.null(m.fit1, 'hh.id')
m.fit1$lratio <- lratio.null(m.fit1, 'hh.id')
stargazer(m.fit1, out="m1.html")

#base model with chicago dummy
m.fit1c <- mlogit(choice~lowrentshare+Asian.x.asianpct+whitepct+AfriAme.x.blackpct+hispanic.x.hispanicpct+ln.units+schscore+chicago|0, data.m); summary(m.fit1)
#pseduo rho squared
with(m.fit1c, 1 - logLik/logLik.c)
#or more aggressive pseduo rho squared
with(m.fit1c, 1 - logLik/logLik.0)
m.fit1c$mfR2 <- mfR2.null(m.fit1c, 'hh.id')
m.fit1c$lratio <- lratio.null(m.fit1c, 'hh.id')
stargazer(m.fit1c, out="m1c.html")

m.fit2 <- mlogit(choice~aveaccPUMA+lowrentshare+Asian.x.asianpct+whitepct+AfriAme.x.blackpct+hispanic.x.hispanicpct+ln.units+schscore|0, data.m); summary(m.fit2)
#pseduo rho squared
with(m.fit2, 1 - logLik/logLik.c)
#or more aggressive pseduo rho squared
with(m.fit2, 1 - logLik/logLik.0)
lrtest(m.fit1, m.fit2)
m.fit2$mfR2 <- mfR2.null(m.fit2, 'hh.id')
m.fit2$lratio <- lratio.null(m.fit2, 'hh.id')
stargazer(m.fit1, m.fit2, out="m1_m2.html")

#aveaccPUMA is not significant for unemployed hhs
m.fit3 <- mlogit(choice~employed.x.aveaccPUMA+unemployed.x.aveaccPUMA+lowrentshare+Asian.x.asianpct+whitepct+AfriAme.x.blackpct+hispanic.x.hispanicpct+ln.units+schscore|0, data.m); summary(m.fit3)
with(m.fit3, 1 - logLik/logLik.c)
#or more aggressive pseduo rho squared
with(m.fit3, 1 - logLik/logLik.0)
lrtest(m.fit2, m.fit3)
m.fit3$mfR2 <- mfR2.null(m.fit3, 'hh.id')
m.fit3$lratio <- lratio.null(m.fit3, 'hh.id')
stargazer(m.fit2, m.fit3, out="m2_m3.html")
stargazer(m.fit1, m.fit2, m.fit3, type="text", out="m1_m2_m3.txt")
stargazer(m.fit1, m.fit2, m.fit3, out="m1_m2_m3.html")

#aveaccPUMA is not significant for unemployed hhs
m.fit3e <- mlogit(choice~employed.x.aveaccPUMA+lowrentshare+Asian.x.asianpct+whitepct+AfriAme.x.blackpct+hispanic.x.hispanicpct+ln.units+schscore|0, data.m); summary(m.fit3e)
with(m.fit3e, 1 - logLik/logLik.c)
#or more aggressive pseduo rho squared
with(m.fit3e, 1 - logLik/logLik.0)
lrtest(m.fit2, m.fit3e)
lrtest(m.fit3, m.fit3e)
m.fit3e$mfR2 <- mfR2.null(m.fit3e, 'hh.id')
m.fit3e$lratio <- lratio.null(m.fit3e, 'hh.id')
stargazer(m.fit2, m.fit3e, out="m2_m3.html")
stargazer(m.fit1, m.fit2, m.fit3e, type="text", out="m1_m2_m3.txt")
stargazer(m.fit1, m.fit2, m.fit3e, out="m1_m2_m3.html")

#aveaccPUMA coeffs by labor force participation
m.fit3l <- mlogit(choice~lf.x.aveaccPUMA+nlf.x.aveaccPUMA+lowrentshare+Asian.x.asianpct+whitepct+AfriAme.x.blackpct+hispanic.x.hispanicpct+ln.units+schscore|0, data.m); summary(m.fit3l)
with(m.fit3l, 1 - logLik/logLik.c)
#or more aggressive pseduo rho squared
with(m.fit3l, 1 - logLik/logLik.0)
lrtest(m.fit2, m.fit3l)
m.fit3l$mfR2 <- mfR2.null(m.fit3l, 'hh.id')
m.fit3l$lratio <- lratio.null(m.fit3l, 'hh.id')
stargazer(m.fit2, m.fit3l, out="m2_m3l.html")
stargazer(m.fit1, m.fit2, m.fit3l, type="text", out="m1_m2_m3l.txt")
stargazer(m.fit1, m.fit2, m.fit3l, out="m1_m2_m3l.html")

#aveaccPUMA is not significant for nlf hhs
m.fit3le <- mlogit(choice~lf.x.aveaccPUMA+lowrentshare+Asian.x.asianpct+whitepct+AfriAme.x.blackpct+hispanic.x.hispanicpct+ln.units+schscore|0, data.m); summary(m.fit3le)
with(m.fit3le, 1 - logLik/logLik.c)
#or more aggressive pseduo rho squared
with(m.fit3le, 1 - logLik/logLik.0)
lrtest(m.fit2, m.fit3le)
lrtest(m.fit3l, m.fit3le)
m.fit3le$mfR2 <- mfR2.null(m.fit3le, 'hh.id')
m.fit3le$lratio <- lratio.null(m.fit3le, 'hh.id')
stargazer(m.fit2, m.fit3le, out="m2_m3.html")
stargazer(m.fit1, m.fit2, m.fit3le, type="text", out="m1_m2_m3.txt")
stargazer(m.fit1, m.fit2, m.fit3le, out="m1_m2_m3.html")


m.fit4 <- mlogit(choice~employed.x.aveaccPUMA+unemployed.x.aveaccPUMA+lowrentshare+Asian.x.asianpct+whitepct+AfriAme.x.blackpct+hispanic.x.hispanicpct+ln.units+schscore|0, data.m); summary(m.fit3)

summary(hh$SEX)
