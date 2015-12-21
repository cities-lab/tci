#plot results from cluster-based approach

# plot charts
  pden.htaz.ic <- plot_density(plot.data=tcost.htaz.ic, x="tcost", xlab="Travel Cost", 
                                group="ic", legend.title="Income Level", 
                                unit.name=unit.name)
  pden.htaz.ic
  output_file = file.path(OUTPUT_DIR, "density_tcost_by_ic.png")
  ggsave(pden.htaz.ic, file=output_file, type="cairo-png")
  
  
  ## this is trip level, as there is no way to aggregate trips to households
  pden.htaz.pr <- plot_density(tcost.htaz.pr, x="tcost", xlab="Travel Cost", 
                                group="pr", legend.title="Trip Purpose", 
                                unit.name=unit.name)
  output_file = file.path(OUTPUT_DIR, "density_tcost_by_pr.png")
  ggsave(pden.htaz.pr, file=output_file, type="cairo-png")
  
  ## 
  pbox.htaz.pr.ic <- plot_boxplot(tcost.htaz.pr.ic, x="pr", xlab="Trip Purpose",
                                  y="tcost", ylab="Travel Cost", ylim.max=100,
                                  fill="ic", legend.title="Income Level",
                                  unit.name=unit.name)
  output_file = file.path(OUTPUT_DIR, "boxplot_tcost_by_pr.png")
  ggsave(pbox.htaz.pr.ic, file=output_file, type="cairo-png")

#plot maps
  tcost.htaz.mapdata <- tcost.htaz.pr.ic %>%
    #select(-c(hhs, tcost.sum)) %>%
    ungroup() %>%
    mutate(ic=as.character(ic),
           pr=as.character(pr))  %>%
    dplyr::union(mutate(ungroup(tcost.htaz.pr), ic="All Households", pr=as.character(pr))) %>%
    dplyr::union(mutate(ungroup(tcost.htaz.ic), ic=as.character(ic), pr="All Trips")) %>%
    dplyr::union(mutate(ungroup(tcost.htaz), ic="All Households", pr="All Trips"))

  tcost.htaz.mapdata <- tcost.htaz.mapdata %>% 
    mutate(id=as.character(htaz), 
           value = tcost,
           ic = factor(ic, levels=c("Low Inc", "Mid Inc", "High Inc", 'All Households')), #labels=c(labels(), "All Households")),
           pr = factor(pr, levels=c('All Trips', "HBW", "HB Shopping", "HB Recreation", "HB Other") )#, labels=c("All Trips", "HBW", "HB Shopping", "HB Recreation", "HB Other"))         
    )
  
  plot.data <- full_join(taz, tcost.htaz.mapdata)
  
  maps.taz <- plot_map(plot.data=plot.data,
                   name="Travel Cost",
                   group="group",
                   fill="value",
                   unit.name=unit.name)
  
  maps.taz <- maps.taz + facet_grid(pr~ic)
  maps.taz
  output_file = file.path(OUTPUT_DIR, "map_taz_by_ic_pr.png")
  ggsave(maps.taz, file = output_file, width = 8.5, height = 11, type = "cairo-png")

  if (exists("tcost.distr.pr.ic")) {
    tcost.distr.mapdata <- tcost.distr.pr.ic %>%
      ungroup() %>%
      mutate(ic = as.character(ic),
             pr = as.character(pr))  %>%
      dplyr::union(mutate(ungroup(tcost.distr.pr), ic = "All Households", pr =
                            as.character(pr))) %>%
      dplyr::union(mutate(ungroup(tcost.distr.ic), ic = as.character(ic), pr =
                            "All Trips")) %>%
      dplyr::union(mutate(ungroup(tcost.distr), ic = "All Households", pr =
                            "All Trips"))
    
    tcost.distr.mapdata <- tcost.distr.mapdata %>%
      mutate(
        id = as.character(DISTRICT),
        value = tcost,
        ic = factor(ic, levels = c(
          "Low Inc", "Mid Inc", "High Inc", 'All Households'
        )), #labels=c(labels(), "All Households")),
        pr = factor(
          pr, levels = c('All Trips', "HBW", "HB Shopping", "HB Recreation", "HB Other")
        )#, labels=c("All Trips", "HBW", "HB Shopping", "HB Recreation", "HB Other"))
      )
    
    plot.data <- full_join(districts.geo, tcost.distr.mapdata)
    
    maps.distr <- plot_map(
      plot.data = plot.data,
      name = "Travel Cost",
      group = "group",
      fill = "value",
      unit.name = unit.name
    )
    
    maps.distr <- maps.distr + facet_grid(pr ~ ic)
    maps.distr
    output_file = file.path(OUTPUT_DIR, "map_district_by_ic_pr.png")
    ggsave(
      maps.distr, file = output_file, width = 8.5, height = 11, type = "cairo-png"
    )
  }