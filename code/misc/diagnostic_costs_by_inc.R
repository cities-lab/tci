# cluster-based
output.path <- file.path(OUTPUT_DIR, 'aggcostCm/')
output.file <- file.path(output.path, paste(AggTpCost.Zi.name, ".RData", sep=""))
load(output.file)

cm = "weighted"
tp = "peak"
ZiIccost.name <- paste(cm, tp, 'AggCost.ZiIc', sep="")
ZiIccost <- get(ZiIccost.name)
ZiIccost.df <- data.frame(ZiIccost)

ZiIccost.ldf <- gather(ZiIccost.df, inc.level, tcost, lowInc:highInc)
require(ggplot2)
m <- ggplot(ZiIccost.ldf, aes(x = tcost, colour=inc.level, group=inc.level))
m + geom_density(fill=NA, size=2) + labs(x="time costs ($)")


#OHAS
ohas.intm_file = file.path("~/tci/output/intermediate/OHAS", "tcost.RData")
load(ohas.intm_file)
require(ggplot2)
m <- ggplot(tcost.hh, aes(x = tcost, colour=inc.level, group=inc.level))
m + geom_density(fill=NA, size=2) + labs(x="time costs ($)")
