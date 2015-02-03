### to load logsum CSV files provided by Metro on the database using R
# The logsum of all modal utilities is a key input to the destination choice model
# It was generated for each trip purpose by income group,
# Ln(U(Drive Alone) + U(Drive with Passenger) + U(Auto Passenger) + U(Walk to Transit) + U(Park&Ride) + U(Bike) + U(Walk))

###
setwd("~/tci/data/TDM/logsums/")

### load csv files
# hbw
hbw_logsum_avg <- read.csv("hbw_logsum_avg.csv",header=T)
hbw_logsum_low <- read.csv("hbw_logsum_low.csv",header=T)
hbw_logsum_med <- read.csv("hbw_logsum_med.csv",header=T)
hbw_logsum_high <- read.csv("hbw_logsum_high.csv",header=T)
# hbs
hbs_logsum_avg <- read.csv("hbs_logsum_avg.csv",header=T)
hbs_logsum_low <- read.csv("hbs_logsum_low.csv",header=T)
hbs_logsum_med <- read.csv("hbs_logsum_med.csv",header=T)
hbs_logsum_high <- read.csv("hbs_logsum_high.csv",header=T)
# hbr
hbr_logsum_avg <- read.csv("hbr_logsum_avg.csv",header=T)
hbr_logsum_low <- read.csv("hbr_logsum_low.csv",header=T)
hbr_logsum_med <- read.csv("hbr_logsum_med.csv",header=T)
hbr_logsum_high <- read.csv("hbr_logsum_high.csv",header=T)
# hbo
hbo_logsum_avg <- read.csv("hbo_logsum_avg.csv",header=T)
hbo_logsum_low <- read.csv("hbo_logsum_low.csv",header=T)
hbo_logsum_med <- read.csv("hbo_logsum_med.csv",header=T)
hbo_logsum_high <- read.csv("hbo_logsum_high.csv",header=T)

### connect to the database
library(RPostgreSQL)
conn <- dbConnect(PostgreSQL(), host="", user="", password="", dbname="portland")

### load csv files
# hbw
table_name <- "hbw_logsum_avg"
if (dbExistsTable(conn, c("metro", table_name))) dbRemoveTable(conn, c("metro", table_name))
dbWriteTable(conn, c("metro", table_name), hbw_logsum_avg, row.names=F)
table_name <- "hbw_logsum_low"
if (dbExistsTable(conn, c("metro", table_name))) dbRemoveTable(conn, c("metro", table_name))
dbWriteTable(conn, c("metro", table_name), hbw_logsum_low, row.names=F)
table_name <- "hbw_logsum_med"
if (dbExistsTable(conn, c("metro", table_name))) dbRemoveTable(conn, c("metro", table_name))
dbWriteTable(conn, c("metro", table_name), hbw_logsum_med, row.names=F)
table_name <- "hbw_logsum_high"
if (dbExistsTable(conn, c("metro", table_name))) dbRemoveTable(conn, c("metro", table_name))
dbWriteTable(conn, c("metro", table_name), hbw_logsum_high, row.names=F)
# hbs
table_name <- "hbs_logsum_avg"
if (dbExistsTable(conn, c("metro", table_name))) dbRemoveTable(conn, c("metro", table_name))
dbWriteTable(conn, c("metro", table_name), hbs_logsum_avg, row.names=F)
table_name <- "hbs_logsum_low"
if (dbExistsTable(conn, c("metro", table_name))) dbRemoveTable(conn, c("metro", table_name))
dbWriteTable(conn, c("metro", table_name), hbs_logsum_low, row.names=F)
table_name <- "hbs_logsum_med"
if (dbExistsTable(conn, c("metro", table_name))) dbRemoveTable(conn, c("metro", table_name))
dbWriteTable(conn, c("metro", table_name), hbs_logsum_med, row.names=F)
table_name <- "hbs_logsum_high"
if (dbExistsTable(conn, c("metro", table_name))) dbRemoveTable(conn, c("metro", table_name))
dbWriteTable(conn, c("metro", table_name), hbs_logsum_high, row.names=F)
# hbr
table_name <- "hbr_logsum_avg"
if (dbExistsTable(conn, c("metro", table_name))) dbRemoveTable(conn, c("metro", table_name))
dbWriteTable(conn, c("metro", table_name), hbr_logsum_avg, row.names=F)
table_name <- "hbr_logsum_low"
if (dbExistsTable(conn, c("metro", table_name))) dbRemoveTable(conn, c("metro", table_name))
dbWriteTable(conn, c("metro", table_name), hbr_logsum_low, row.names=F)
table_name <- "hbr_logsum_med"
if (dbExistsTable(conn, c("metro", table_name))) dbRemoveTable(conn, c("metro", table_name))
dbWriteTable(conn, c("metro", table_name), hbr_logsum_med, row.names=F)
table_name <- "hbr_logsum_high"
if (dbExistsTable(conn, c("metro", table_name))) dbRemoveTable(conn, c("metro", table_name))
dbWriteTable(conn, c("metro", table_name), hbr_logsum_high, row.names=F)
# hbo
table_name <- "hbo_logsum_avg"
if (dbExistsTable(conn, c("metro", table_name))) dbRemoveTable(conn, c("metro", table_name))
dbWriteTable(conn, c("metro", table_name), hbo_logsum_avg, row.names=F)
table_name <- "hbo_logsum_low"
if (dbExistsTable(conn, c("metro", table_name))) dbRemoveTable(conn, c("metro", table_name))
dbWriteTable(conn, c("metro", table_name), hbo_logsum_low, row.names=F)
table_name <- "hbo_logsum_med"
if (dbExistsTable(conn, c("metro", table_name))) dbRemoveTable(conn, c("metro", table_name))
dbWriteTable(conn, c("metro", table_name), hbo_logsum_med, row.names=F)
table_name <- "hbo_logsum_high"
if (dbExistsTable(conn, c("metro", table_name))) dbRemoveTable(conn, c("metro", table_name))
dbWriteTable(conn, c("metro", table_name), hbo_logsum_high, row.names=F)
