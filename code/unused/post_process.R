#plot results from cluster-based approach
require(reshape2)
load("output/cluster/aggcostCm/weightedAggCost.ZiIcPr.RData") # This data is calcuated by first version of cluster method

dtcost.htaz_tpurp_inc <- melt(weightedAggCost.ZiIcPr, 
                             varnames = c('htaz', 'inc.level', 'TripPurpose'),
                             value.name = "tcost")

dtrips.htaz_tpurp_inc <- melt(TripProd.ZiIcPr,
                             varnames = c('htaz', 'inc.level', 'TripPurpose'),
                             value.name = "trips")

dhhs.inc <- melt(Hh.ZiIc,
                varnames = c('htaz', 'inc.level'),
                value.name = "hhs")

dwtcost.htaz_tpurp_inc <- dtcost.htaz_tpurp_inc %>%
  left_join(dtrips.htaz_tpurp_inc) %>%
  left_join(dhhs.inc) %>%
  mutate(tcost.wt = tcost * trips / hhs,
         tcost.wt = ifelse(is.infinite(tcost.wt), NA, tcost.wt)
         )

require(magrittr)
dwtcost.htaz_tpurp_inc %<>% 
  mutate(inc.level = factor(inc.level, levels=Ic, labels=c("Low Inc", "Mid Inc", "High Inc")),
         TripPurpose = factor(TripPurpose, levels=Pr, labels=c("HBW", "HB Shopping", "HB Recreation", "HB Other")))

dwtcost.htaz_inc <- dwtcost.htaz_tpurp_inc %>%
  group_by(htaz, inc.level) %>%
  summarize(tcost.wt=sum(tcost * trips) / mean(hhs),
            hhs = first(hhs)) %>%
  as.data.frame() %>%
  mutate(tcost.wt = ifelse(is.infinite(tcost.wt), NA, tcost.wt))

dwtcost.htaz_tpurp <- dwtcost.htaz_tpurp_inc %>%
  group_by(htaz, TripPurpose) %>%
  summarize(tcost.wt=mean(tcost.wt)) %>%
  as.data.frame() %>%
  mutate(tcost.wt = ifelse(is.infinite(tcost.wt), NA, tcost.wt))

dwtcost.htaz <- dwtcost.htaz_inc %>%
  group_by(htaz) %>%
  summarize(tcost.wt=wt.mean(tcost.wt, hhs)) %>%
  as.data.frame() %>%
  mutate(tcost.wt = ifelse(is.infinite(tcost.wt), NA, tcost.wt))

# prepare data for plotting
pden.inc <- ggplot(dwtcost.htaz_inc, aes(x = tcost.wt, colour=inc.level, group=inc.level)) +
  geom_density(fill=NA, size=1) + labs(x="Travel Costs (minutes)") + xlim(0, 480) +
  scale_colour_grey(name = 'Income Level') + theme_bw()
pden.inc
output_file = file.path(OUTPUT_DIR, "density_tcost.hh_by_inc.png")
ggsave(pden.inc, file=output_file, type="cairo-png")

pden.tpurp <- ggplot(dwtcost.htaz_tpurp, aes(x = tcost.wt, colour=TripPurpose, group=TripPurpose)) +
  geom_density(fill=NA, size=1) + labs(x="Travel Costs (minutes)") + xlim(0, 180) +
  scale_colour_grey(name = 'Income Level') + theme_bw()
pden.tpurp
output_file = file.path(OUTPUT_DIR, "density_tcost.hh_by_tpurp.png")
ggsave(pden.tpurp, file=output_file, type="cairo-png")

boxp.tpurp_inc <- ggplot(dwtcost.htaz_tpurp_inc, aes(x=TripPurpose, y=tcost.wt, fill=inc.level)) +
  geom_boxplot() + labs(y="Generalized Travel Costs (minutes)") + xlab("Trip Purpose") + ylim(0, 250)  +
  scale_fill_grey(name = 'Income Level')  + theme_bw() 
boxp.tpurp_inc
output_file = file.path(OUTPUT_DIR, "boxplot_tcost.hh_by_tpurp.inc.png")
ggsave(boxp.tpurp_inc, file=output_file, type="cairo-png")


dwtcost.htaz_inc %<>% select(-hhs)

#transform data for plotting maps
tcost.htaz_all <- dwtcost.htaz_tpurp_inc %>%
  select(-c(trips, hhs, tcost)) %>%
  mutate(inc.level=as.character(inc.level),
         TripPurpose=as.character(TripPurpose)
  )  %>%
  union(mutate(ungroup(dwtcost.htaz_tpurp), inc.level="All Households", TripPurpose=as.character(TripPurpose))) %>%
  union(mutate(ungroup(dwtcost.htaz_inc), inc.level=as.character(inc.level), TripPurpose="All Trips")) %>%
  union(mutate(ungroup(dwtcost.htaz), inc.level="All Households", TripPurpose="All Trips"))  #%>%
  #right_join(expand.grid(htaz=1:max.taz_id, TripPurpose=c('All', Pr), inc.level=c(Ic, 'All'), stringsAsFactors = F))

#prepare data for plotting
tcost.htaz_all %<>% 
  mutate(id=as.character(htaz), 
         value = tcost.wt,
         inc.level = factor(inc.level, levels=c("Low Inc", "Mid Inc", "High Inc", 'All Households')), #labels=c(labels(), "All Households")),
         TripPurpose = factor(TripPurpose, levels=c('All Trips', "HBW", "HB Shopping", "HB Recreation", "HB Other") )#, labels=c("All Trips", "HBW", "HB Shopping", "HB Recreation", "HB Other"))         
         )
         
# tcost.htaz_all %<>% 
#   mutate(id=as.character(htaz), 
#          value = tcost.wt,
#          inc.level = factor(inc.level, levels=c(levels(inc.level), 'All '), labels=c(labels(), "All Households")),
#          TripPurpose = factor(TripPurpose, levels=c('All', Pr), labels=c("All Trips", "HBW", "HB Shopping", "HB Recreation", "HB Other"))
#   )

require(ggmap)
require(scales)

taz <- readOGR(dsn = file.path(INPUT_DIR, "shp"), layer = "TAZ")
taz <- fortify(taz, region="newtaz")

#taz.data <- tcost.distr %>% mutate(id = as.character(district.id),
#                                         value = tcost.wtavg)
#taz <- left_join(taz, taz.data)

plot_map <- function(plot.Data) {
  p <- ggplot() +
    geom_polygon(data = plot.Data, aes(x = long, y = lat, group = group, fill = value), 
                 color = NA, size = 0.1) +
    scale_fill_distiller(palette = "YlOrRd", breaks = pretty_breaks(n = 10), limits = c(0, 600), 
                         name = "Travel Costs\n(Minutes)", na.value = "grey80") +
    guides(fill = guide_legend(reverse = TRUE)) +
    theme_nothing(legend = TRUE)
}

plot.data <- full_join(taz, tcost.htaz_all)
maps <- plot_map(plot.data) + facet_grid(TripPurpose~inc.level)
maps
output_file = file.path(OUTPUT_DIR, "map_taz_all.png")
ggsave(maps, file = output_file, width = 8.5, height = 11, type = "cairo-png")

if (!exists(paste(Pr, "ci", sep="")))
  load(file.path(INTERMEDIATE_DIR, "centers.RData"))
  
for (pr in Pr) {
  CentersObjName <- paste(pr, "ci", sep="")
  Centers <- get(CentersObjName); rm(CentersObjName)
  Centers$TripPurpose <- pr
}  

plot_map2 <- function(plot.Data) {
  p <- ggplot() +
    geom_polygon(data = plot.Data, aes(x = long, y = lat, group = group, fill = value), 
                 color = "black", size = 0.1) +
    #scale_fill_distiller(palette = "YlOrRd", na.value = "gray80") + 
    scale_fill_identity(na.value = "gray80") +
    #guides(fill = guide_legend(reverse = TRUE)) +
    theme_nothing(legend = FALSE)
}

centers <- mutate(hbwci, TripPurpose="HBW") %>%
  union(mutate(hbsci, TripPurpose="HB Shopping")) %>%
  union(mutate(hbrci, TripPurpose="HB Recreation")) %>%
  union(mutate(hboci, TripPurpose="HB Other")) %>%
  mutate(id = as.character(TAZ),
         is.center = 1, 
         value = is.center)

taz <- taz %>%
  mutate(TAZ = as.integer(id),
         seqid=1:n())

#taz$TAZ <- as.integer(taz$id)
#taz$seq <- 1:nrow(taz)

plot.centers <- left_join(taz, hbwci) %>% mutate(TripPurpose="HBW") %>%
  union( left_join(taz, hbsci) %>% mutate(TripPurpose="HB Shopping") ) %>%
  union( left_join(taz, hbrci) %>% mutate(TripPurpose="HB Recreation") ) %>%
  union( left_join(taz, hboci) %>% mutate(TripPurpose="HB Other") ) %>%
  mutate(TripPurpose = factor(TripPurpose, levels=c("HBW", "HB Shopping", "HB Recreation", "HB Other")),
         is.center = as.integer(!is.na(center.id))
         ) %>%
  arrange(TripPurpose, seqid)

#plot.centers <- full_join(taz, centers)

plot_map2 <- function(plot.Data) {
  p <- ggplot() +
  geom_polygon(data = plot.Data, aes(x = long, y = lat, group = group, fill = center.id),
  color = "grey70", size = 0.25) +
  #scale_fill_manual(values=cbPalette) +
  scale_fill_identity(na.value = "grey70") +
  #guides(fill = guide_legend(reverse = TRUE)) +
  theme_nothing(legend = TRUE)
}

map_centers <- plot_map2(plot.centers) + facet_wrap(~TripPurpose, ncol=2, scales="free") #facet_grid(TripPurpose~inc.level)
map_centers
output_file = file.path(OUTPUT_DIR, "map_centers.png")
ggsave(map_centers, file = output_file, type = "cairo-png")

ggplot(nmmaps, aes(date,temp))+geom_point(color="chartreuse4")+
  facet_wrap(~year, ncol=2, scales="free")