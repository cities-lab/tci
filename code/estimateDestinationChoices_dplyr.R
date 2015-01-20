
require(dplyr)
db_src <- src_postgres(host="sapporo.usp.pdx.edu", 
                       user="smartdata", 
                       password="Smartaa00", 
                       dbname="portland",
                       options="-c search_path=ohas_v2, metro")

activity <- tbl(db_src, "activity2")
person <- tbl(conn, "person") # the person file from the 2011 OTAS
household <- tbl(conn, "household") # the household file from the 2011 OTAS
household <- tbl(conn, "vehicle") 


conn <- dbConnect(PostgreSQL(), host="sapporo.usp.pdx.edu", user="smartdata", password="Smartaa00", dbname="portland")
# 2011 OTAS
activity <- dbReadTable(conn, c("ohas_v2", "activity2")) # the activity file from the 2011 OTAS
person <- dbReadTable(conn, c("ohas_v2", "person")) # the person file from the 2011 OTAS
household <- dbReadTable(conn, c("ohas_v2", "household")) # the household file from the 2011 OTAS
# zonal data
vehicle <- dbReadTable(conn, c("ohas_v2", "vehicle"))
zonalEmpData <- dbReadTable(conn, c("metro", "zonalEmpData"))
# skim data
tdist <- dbReadTable(conn, c("metro", "mf61"))
# logsum data
# hbw
hbw_logsum_avg <- dbReadTable(conn, c("metro", "hbw_logsum_avg"))
hbw_logsum_low <- dbReadTable(conn, c("metro", "hbw_logsum_low"))
hbw_logsum_med <- dbReadTable(conn, c("metro", "hbw_logsum_med"))
hbw_logsum_high <-dbReadTable(conn, c("metro", "hbw_logsum_high"))
# hbs
hbs_logsum_avg <- dbReadTable(conn, c("metro", "hbs_logsum_avg"))
hbs_logsum_low <- dbReadTable(conn, c("metro", "hbs_logsum_low"))
hbs_logsum_med <- dbReadTable(conn, c("metro", "hbs_logsum_med"))
hbs_logsum_high <- dbReadTable(conn, c("metro", "hbs_logsum_high"))
# hbr
hbr_logsum_avg <- dbReadTable(conn, c("metro", "hbr_logsum_avg"))
hbr_logsum_low <- dbReadTable(conn, c("metro", "hbr_logsum_low"))
hbr_logsum_med <- dbReadTable(conn, c("metro", "hbr_logsum_med"))
hbr_logsum_high <- dbReadTable(conn, c("metro", "hbr_logsum_high"))
# hbo
hbo_logsum_avg <- dbReadTable(conn, c("metro", "hbo_logsum_avg"))
hbo_logsum_low <- dbReadTable(conn, c("metro", "hbo_logsum_low"))
hbo_logsum_med <- dbReadTable(conn, c("metro", "hbo_logsum_med"))
hbo_logsum_high <- dbReadTable(conn, c("metro", "hbo_logsum_high"))

require(dplyr)
activity <- activity %>%
  arrange(sampn, perno, plano) %>%
  group_by(sampn, perno, plano) %>%
  mutate(actNo=1:n(), maxAct=n())
linked <- activity %>%
  # remove the Change-Of-Mode trips (tpurp==7) in the middle of the day and
  # the first and last 'trip'
  filter(tpurp!=7 & actNo!=1 & actNo != maxAct) %>%
  arrange(sampn, perno, plano) %>%
  group_by(sampn, perno) %>%
  mutate(tripNo=1:n(), maxTrip=n(),
         perid=sampn*10 + perno,
         tripid=perid*100 + tripNo,
         otaz=lag(dtaz),       # origin taz
         last_tpurp=lag(tpurp) # last tpurp
  )

hbs <- linked %>%
  filter(!is.na(dtaz)&!is.na(otaz)) %>%
  filter((last_tpurp %in% c(1,2) & tpurp %in% c(13,14)) |
           (last_tpurp %in% c(13, 14) & tpurp %in% c(1,2)) )
hbr <- linked %>%
  filter(!is.na(dtaz)&!is.na(otaz)) %>%
  filter((last_tpurp %in% c(1,2) & tpurp %in% c(20,21)) |
           (last_tpurp %in% c(20, 21) & tpurp %in% c(1,2)))
tpurp.others <- c(4,6,7,8,9,10,11,12,15,16,17,18,19,22,96,97)
hbo <- linked %>%
  filter(!is.na(dtaz)&!is.na(otaz)) %>%
  filter((last_tpurp %in% c(1,2) & tpurp %in% tpurp.others) |
           (last_tpurp %in% tpurp.others & tpurp %in% c(1,2)))

linked.aa <- union( linked %>% filter(hbw==1) %>% mutate(tpurp='hbw'),
                    linked %>% filter(hbs==1) %>% mutate(tpurp='hbs')
)

sample_alt <- function(df, alts.all, n.alts) {
  n.alts <- n.alts - 1
  df %>%
    rowwise() %>%
    do(
      data.frame(.,
                 CS=c( .$dtaz, sample(setdiff(alts.all, .$dtaz), n.alts)) )
    ) %>%
    mutate(chosen=dtaz==CS) #%>%
  #select(-dtaz) %>%
  #rename(dtaz=alt.id)
}
hbw.long <- getLong(as.data.frame(hbw))
#hbw.long <- sample_alt(hbw, alts.all, n.alts)

person.s <- person %>%
  transmute(
    sampno = sampn, perno = perno,
    female = as.integer(gend==2),
    hsize1 = as.integer(hhsiz==1),
    hsize2 = as.integer(hhsiz==2),
    hsize3 = as.integer(hhsiz==3),
    hsize4 = as.integer(hhsiz>=4),
    inc.level = cut(data$control,
                    breaks=c(1, 3, 5, 9),
                    labels=c("low","medium","high"),
                    include.lowest=T, right=F)
    #low <- (1,2); median <- (3,4); high <- 5:8
  )
zonalEmpData <- select(zonalEmpData, TotEmp) #only pull variables that may be needed
get_accvar <- function(df, acc.df) {
  #df: contains otaz and dtaz cols
  #acc.df: the first and second col is origin & dest TAZ respectively
  #the third col is the value
  max.taz.1 <- with(df, max(otaz, dtaz))
  max.taz.2 <- max(acc.df[,c(1,2)])
  max.taz <- max(c(max.taz.1, max.taz.2))
  acc.m <- matrix(NA, nrow=max.taz, ncol=max.taz)
  acc.m[cbind(acc.df[[1]], acc.df[[2]])] <- acc.df[[3]]
  #acc.m[cbind(acc.df[, 1], acc.df[, 2])] <- acc.df[, 3]
  results <- acc.m[cbind(df$otaz, df$dtaz)]
  results
}

hbw.long <- left_join(hbw.long, person.s, by=c("sampn","perno"))
hbw.long <- left_join(hbw.long, hbw, by=c("tripid"))
get_accvar(hbw.long, tdist)