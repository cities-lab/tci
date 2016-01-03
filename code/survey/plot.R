# plot descriptives and maps of travel time cost

require(ggplot2)
require(ggmap)

## density plot
  pden.ic <- plot_density(plot.data=tcost.hh, x="tcost", xlab="Travel Costs", 
                          group="inc.level", legend.title="Income Level", 
                          unit.name=unit.name)
  #pden.inc <- pden.inc.f(plot.data=tcost.hh, unit.name=unit.name)
  output_file = file.path(OUTPUT_DIR, "density_tcost.hh_by_inc.png")
  ggsave(pden.ic, file=output_file, type="cairo-png")
  
  pden.pp.ic <- plot_density(plot.data=tcost.pp, x="tcost", xlab="Travel Costs", 
                          group="inc.level", legend.title="Income Level", 
                          unit.name=unit.name)
  #pden.inc <- pden.inc.f(plot.data=tcost.hh, unit.name=unit.name)
  output_file = file.path(OUTPUT_DIR, "density_tcost.pp_by_inc.png")
  ggsave(pden.pp.ic, file=output_file, type="cairo-png")
  
  pden.hh.child <- plot_density(plot.data=tcost.hh, x="tcost", xlab="Travel Costs", 
                          group="has.child", legend.title="Presence of Children", 
                          unit.name=unit.name)
  #pden.inc <- pden.inc.f(plot.data=tcost.hh, unit.name=unit.name)
  output_file = file.path(OUTPUT_DIR, "density_tcost.hh_by_child.png")
  ggsave(pden.hh.child, file=output_file, type="cairo-png")
  
  pden.pp.child <- plot_density(plot.data=tcost.pp, x="tcost", xlab="Travel Costs", 
                             group="has.child", legend.title="Presence of Children", 
                             unit.name=unit.name)
  #pden.inc <- pden.inc.f(plot.data=tcost.hh, unit.name=unit.name)
  output_file = file.path(OUTPUT_DIR, "density_tcost.pp_by_child.png")
  ggsave(pden.pp.child, file=output_file, type="cairo-png")  
  
  tcost.hh <- tcost.hh %>% 
    mutate(hhsiz.cat=cut(HHSIZ,
                         breaks=c(1, 2, 3, 4, max(HHSIZ)),
                         labels=c("1", "2", "3", "4+"),   #allow alternative household grouping
                         include.lowest=T, right=F))
  
  pden.hhsize <- plot_density(plot.data=tcost.hh, x="tcost", xlab="Travel Costs", 
                          group="hhsiz.cat", legend.title="Household Size", 
                          unit.name=unit.name) 
  
  #pden.hhsiz  <- pden.hhsiz.f(plot.data=tcost.hh, unit.name=unit.name)
  pden.hhsize
  output_file = file.path(OUTPUT_DIR, "density_tcost.hh_by_hhsize.png")
  ggsave(pden.hhsize, file=output_file, type="cairo-png")

  
  
  
## boxplot
  tcost.hh.tpurp <- tcost.hh.tpurp %>% 
    mutate(inc.level = factor(inc.level, levels=Ic, labels=c("Low Inc", "Mid Inc", "High Inc")),
           TripPurpose = factor(TripPurpose, levels=Pr, labels=c("HBW", "HB Shopping", "HB Recreation", "HB Other"))
    )
  
  pbox.pr.ic <- plot_boxplot(plot.data=tcost.hh.tpurp, 
                             x="TripPurpose", xlab="Trip Purpose",
                             y="tcost", ylab="Travel Costs",
                             fill="inc.level", legend.title="Income Level",
                             unit.name=unit.name) 
  
  #boxp.tpurp.inc <- boxp.tpurp.inc.f(plot.data=tcost.hh.tpurp, unit.name=unit.name)
  pbox.pr.ic
  output_file = file.path(OUTPUT_DIR, "boxplot_tcost.hh_by_pr.ic.png")
  ggsave(pbox.pr.ic, file=output_file, type="cairo-png")
  
  #m.sp <- ggplot(tcost.hh, aes(x = INCOME, y = tcost, colour=inc.level, group=inc.level))
  #m.sp + geom_point(fill=NA, size=2, position = "jitter") + labs(y="Travel Costs (minutes)") + ylim(0, 250)
  
  #m.bp <- ggplot(tcost.hh, aes(inc.level, tcost, fill=inc.level))
  #m.bp + geom_boxplot() + labs(y="Generalized Travel Costs (minutes)") + ylim(0, 250)
  
  #t.bp <- ggplot(tcost.hh, aes(factor(INCOME), tcost, fill=inc.level))
  #t.bp + geom_boxplot() + labs(y="Generalized Travel Costs (minutes)") + ylim(0, 250)
  
  pbox.ic <- plot_boxplot(plot.data=tcost.hh, 
                          x="inc.level", xlab="Income Level",
                          y="tcost", ylab="Travel Costs",
                          unit.name=unit.name)
  
  pbox.income <- plot_boxplot(plot.data=tcost.hh, 
                          x="INCOME", xlab="Income",
                          y="tcost", ylab="Travel Costs",
                          unit.name=unit.name)

  
## line chart
  pline.pr.ic <- plot_line(plot.data=tcost.tpurp.inc, 
                           x="inc.level", xlab="Income",
                           y="tcost.wtavg", ylab="Travel Cost",
                           group="TripPurpose",
                           unit.name=unit.name) 
  
  #linep.tpurp.inc<- linep.tpurp.inc.f(plot.data=tcost.tpurp.inc, unit.name=unit.name)
  pline.pr.ic
  output_file = file.path(OUTPUT_DIR, "linechart_tcost.hh_by_pr.ic.png")
  ggsave(pline.pr.ic, file=output_file, type="cairo-png")


## plot district maps
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
  #maps <- plot_map(plot.data) + facet_grid(TripPurpose~inc.level)
  maps <- plot_map(plot.data, name="Travel Cost", 
                   group="group", fill="value", 
                   unit.name=unit.name, limits.max = 350) + facet_grid(TripPurpose~inc.level) 
  maps
  output_file = file.path(OUTPUT_DIR, "map_districts_all.png")
  ggsave(maps, file = output_file, width = 8.5, height = 11, type = "cairo-png")
