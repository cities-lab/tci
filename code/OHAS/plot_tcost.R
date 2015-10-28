# plot maps and descriptives of travel time cost

# load saveGraph function
source("code/thirdparty/openGraphSaveGraph.R")

#load(file.path(OUTPUT_DIR, "tcost.RData"))

# plot travel costs distribution by income group
require(ggplot2)
require(ggmap)

# pden.inc <- ggplot(tcost.hh, aes(x = tcost, colour=inc.level, group=inc.level)) +
#   geom_density(fill=NA, size=1) + labs(x="Travel Costs (minutes)") + xlim(0, 360) +
#   scale_colour_discrete(name = 'Income Level') + 
#   ggtitle("Household-level trip cost by trip purposes income groups") +
#   theme(plot.title = element_text(face="bold", size=12, vjust=1))

pden.inc <- pden.inc.f(plot.data=tcost.hh, unit.name=unit.name)
output_file = file.path(OUTPUT_DIR, "density_tcost.hh_by_inc.png")
ggsave(pden.inc, file=output_file, type="cairo-png")

# tcost.hh <- tcost.hh %>% 
#   mutate(hhsiz.cat=cut(HHSIZ,
#                        breaks=c(1, 2, 3, 4, 9),
#                        labels=c("1", "2", "3", "4+"),   #allow alternative household grouping
#                        include.lowest=T, right=F
#   ))
# pden.hhsiz <- ggplot(tcost.hh, aes(x = tcost, colour=hhsiz.cat, group=hhsiz.cat)) +
#   geom_density(fill=NA, size=1) + labs(x="Travel Costs (minutes)") + xlim(0, 360) +
#   scale_colour_discrete(name = 'Household Size')+ 
#   ggtitle("Household-level travel cost by household size") +
#   theme(plot.title = element_text(face="bold", size=12, vjust=1))

pden.hhsiz  <- pden.hhsiz.f(plot.data=tcost.hh, unit.name=unit.name)
pden.hhsiz
output_file = file.path(OUTPUT_DIR, "density_tcost.hh_by_hhsiz.png")
ggsave(pden.hhsiz, file=output_file, type="cairo-png")

# tcost.hh.tpurp <- tcost.hh.tpurp %>% 
#   mutate(inc.level = factor(inc.level, levels=Ic, labels=c("Low Inc", "Mid Inc", "High Inc")),
#          TripPurpose = factor(TripPurpose, levels=Pr, labels=c("HBW", "HB Shopping", "HB Recreation", "HB Other"))
#   )
# 
# boxp.tpurp_inc <- ggplot(tcost.hh.tpurp, aes(x=TripPurpose, y=tcost, fill=inc.level)) +
#   geom_boxplot() + labs(y="Generalized Travel Costs (minutes)") + xlab("Trip Purpose") + ylim(0, 250)  +
#   scale_fill_discrete(name = 'Income Level') + 
#   ggtitle("Household-level travel cost by trip purposes income groups") +
#   theme(plot.title = element_text(face="bold", size=12, vjust=1))

boxp.tpurp_inc <- boxp.tpurp_inc.f(plot.data=tcost.hh.tpurp, unit.name=unit.name)
boxp.tpurp_inc
output_file = file.path(OUTPUT_DIR, "boxplot_tcost.hh_by_tpurp.inc.png")
ggsave(file=output_file, type="cairo-png")

m.sp <- ggplot(tcost.hh, aes(x = INCOME, y = tcost, colour=inc.level, group=inc.level))
m.sp + geom_point(fill=NA, size=2, position = "jitter") + labs(y="Travel Costs (minutes)") + ylim(0, 250)

m.bp <- ggplot(tcost.hh, aes(inc.level, tcost, fill=inc.level))
m.bp + geom_boxplot() + labs(y="Generalized Travel Costs (minutes)") + ylim(0, 250)


# t.lc <- ggplot(tcost.tpurp.inc, aes(x = inc.level, y = tcost.wtavg, colour=TripPurpose, group=TripPurpose))
# t.lc + geom_line(fill=NA, size=1) + labs(x="Income Level") + labs(y="Travel Costs (minutes)") + ylim(0, 120) +
#   ggtitle("Trip-level travel cost by trip purposes income groups") +
#   theme(plot.title = element_text(face="bold", size=12, vjust=1))

linep.tpurp.inc<- linep.tpurp.inc.f(plot.data=tcost.tpurp.inc, unit.name=unit.name)
linep.tpurp.inc
output_file = file.path(OUTPUT_DIR, "linechart_tcost.hh_by_tpurp.inc.png")
ggsave(file=output_file, type="cairo-png")
#saveGraph(filename=output_file, type="pdf")

t.bp <- ggplot(tcost.hh, aes(factor(INCOME), tcost, fill=inc.level))
t.bp + geom_boxplot() + labs(y="Generalized Travel Costs (minutes)") + ylim(0, 250)

#districts.data <- tcost.distr %>% mutate(id = as.character(district.id),
#                                         value = tcost.wtavg)
#districts <- left_join(districts, districts.data)

# plot_map <- function(plot.Data) {
#   p <- ggplot() +
#        geom_polygon(data = plot.Data, aes(x = long, y = lat, group = group, fill = value), 
#                     color = NA, size = 0.1) +
#        scale_fill_distiller(palette = "YlOrRd", breaks = pretty_breaks(n = 10), limits = c(0, 200), 
#                             name = "Travel Costs\n(Minutes)", na.value = "grey80") +
#        guides(fill = guide_legend(reverse = TRUE)) +
#        theme_nothing(legend = TRUE)
# }

#transform data for plotting
#  no applicable method for 'right_join' applied to an object of class "list"
# tcost.distr_all <- tcost.distr.tpurp.inc %>% 
#   ungroup() %>%
#   mutate(inc.level=as.character(inc.level),
#          TripPurpose=as.character(TripPurpose)
#   )  %>%
#   union(mutate(ungroup(tcost.distr.tpurp), inc.level="All", TripPurpose=as.character(TripPurpose))) %>%
#   union(mutate(ungroup(tcost.distr.inc), inc.level=as.character(inc.level), TripPurpose="All")) %>%
#   union(mutate(ungroup(tcost.distr), inc.level="All", TripPurpose="All")) %>%
#   right_join(expand.grid(district.id=1:20, TripPurpose=c('All', Pr), inc.level=c(Ic, 'All'), stringsAsFactors = F))

tcost.distr_tpurp.inc <- expand.grid(district.id=1:20, TripPurpose=c(Pr), inc.level=c(Ic), stringsAsFactors = F) %>% 
                         left_join(tcost.distr.tpurp.inc %>% ungroup() %>% mutate(inc.level=as.character(inc.level), TripPurpose=as.character(TripPurpose))) 

tcost.distr_tpurp.all <- expand.grid(district.id=1:20, TripPurpose=c(Pr), inc.level=c("All"), stringsAsFactors = F) %>%
                         left_join(mutate(ungroup(tcost.distr.tpurp), inc.level="All", TripPurpose=as.character(TripPurpose))) 

tcost.distr_all.inc <- expand.grid(district.id=1:20, TripPurpose=c("All"), inc.level=c(Ic), stringsAsFactors = F) %>%
                       left_join(mutate(ungroup(tcost.distr.inc), inc.level=as.character(inc.level), TripPurpose="All")) 

tcost.distr_all.all <- expand.grid(district.id=1:20, TripPurpose=c("All"), inc.level=c("All"), stringsAsFactors = F) %>%
                       left_join(mutate(ungroup(tcost.distr), inc.level="All", TripPurpose="All")) 

tcost.distr_all <- rbind(tcost.distr_tpurp.inc, tcost.distr_tpurp.all, tcost.distr_all.inc, tcost.distr_all.all) %>%
                   right_join(expand.grid(district.id=1:20, TripPurpose=c('All', Pr), inc.level=c(Ic, 'All'), stringsAsFactors = F))


#prepare data for plotting
tcost.distr_all <- tcost.distr_all %>% 
  mutate(id=as.character(district.id), 
         value = tcost.wtavg,
         inc.level = factor(inc.level, levels=c(Ic, 'All'), labels=c("Low Inc", "Mid Inc", "High Inc", "All Households")),
         TripPurpose = factor(TripPurpose, levels=c('All', Pr), labels=c("All Trips", "HBW", "HB Shopping", "HB Recreation", "HB Other"))
  )

plot.data <- full_join(districts, tcost.distr_all)
maps <- plot_map(plot.data) + facet_grid(TripPurpose~inc.level)

output_file = file.path(OUTPUT_DIR, "map_districts_all.png")
ggsave(maps, file = output_file, width = 8.5, height = 11, type = "cairo-png")

#p.all <- plot_map(districts)

#library(gridExtra)
#grid.arrange(p1, p2, ncol=2, main = "Main title")

# maps <- list()
# i <- 1 #keep track rows
# for (tpurp in c('All', Pr)) {
#   j <- 1 #keep track columns
#   for (inc in c(Ic, 'All')) {
#     attrs <- tcost.distr_all %>% 
#       filter_(interp(~TripPurpose==tpurp, tpurp=tpurp), 
#               interp(~inc.level==inc, inc=inc))
#     plot.Data <- left_join(districts, attrs)
#     map <- plot_map(plot.Data)
#     if (i==1) { #add header for the first row
#       map <- map + labs(title = inc, fill = "")
#     }
#     if (j==1) { #add header for the first col
#       map <- map +  annotate("text", x = min(districts$long) - 6000, y = median(districts$lat), label = tpurp, angle=90)
#     }
#     maps[[(i-1)*4+j]] <- map
#     j <- j + 1
#   }
#   i <- i + 1
# }
# grid.arrange(maps, ncol=j-1)
# 
# output_file = file.path(OUTPUT_DIR, "map_districts_all.png")
# ggsave(p1, file = output_file, width = 7.5, height = 4.5, type = "cairo-png")

