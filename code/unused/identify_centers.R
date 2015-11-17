## This script aggregates LEHD employment from block level into TAZ level,
## calculates employment and sizeterms density for each TAZ. The calculation 
## results are appended to taz shapefile, then centers are identified
## based on clustering

# identify hbw tazs of centers
#hbwci <- identify_centers(TAZPloyNoNA, "totemp.den", cutoff.val=cutoff['cutoff.val', 'hbw'], dist=1.0, sum.col="tot.emp", sum.cutoff.val=cutoff['sum.cutoff.val', 'hbw'])
hbwci <- identify_centers(TAZPloyNoNA, "totemp.den", dist=1.0, sum.col="tot.emp", cutoffs=split(cutoffs[, 'hbw'], rownames(cutoffs)))
hbwci <- hbwci %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

# identify hbs tazs of centers 
hbsci <- identify_centers(TAZPloyNoNA, "st.hbs.den", dist=1.0, sum.col="st.hbs", cutoffs=split(cutoffs[, 'hbs'], rownames(cutoffs)))
hbsci <- hbsci %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

# identify hbr tazs of centers 
hbrci <- identify_centers(TAZPloyNoNA, "st.hbr.den", dist=1.0, sum.col="st.hbr", cutoffs=split(cutoffs[, 'hbr'], rownames(cutoffs)))
hbrci <- hbrci %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

# identify hbo tazs of centers 
hboci <- identify_centers(TAZPloyNoNA, "st.hbo.den", dist=1.0, sum.col="st.hbo", cutoffs=split(cutoffs[, 'hbo'], rownames(cutoffs)))
hboci <- hboci %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

if (SAVE.INTERMEDIARIES) {
  intm.file <- file.path(INTERMEDIATE_DIR, "centers.RData")
  save(hbwci, hbsci, hbrci, hboci, file=intm.file)
}


# Explore LQ for use in determining center identification threshold 

  # identify hbw tazs of centers based on LQ
  #lq.hbwci <- identify_centers(TAZPloyNoNA, "lq.hbw", cutoff.val=1, dist=1.0, sum.col="tot.emp", sum.cutoff.val=0.1)
  #lq.hbwci <- lq.hbwci %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

  # identify hbs tazs of centers based on LQ
  #lq.hbsci <- identify_centers(TAZPloyNoNA, "lq.hbs", cutoff.val=1, dist=1.0, sum.col="st.hbs", sum.cutoff.val=0.1)
  #lq.hbsci <- lq.hbsci %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

  # identify hbr tazs of centers based on LQ
  #lq.hbrci <- identify_centers(TAZPloyNoNA, "lq.hbr", cutoff.val=1, dist=1.0, sum.col="st.hbr", sum.cutoff.val=0.1)
  #lq.hbrci <- lq.hbrci %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

  # identify hbo tazs of centers based on LQ
  #lq.hboci <- identify_centers(TAZPloyNoNA, "lq.hbo", cutoff.val=1, dist=1.0, sum.col="st.hbo", sum.cutoff.val=0.1)
  #lq.hboci <- lq.hboci %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

  #if (SAVE.INTERMEDIARIES) {
  #  intm.file <- file.path(INTERMEDIATE_DIR, "lq.centers.RData")
  #  save(lq.hbwci, lq.hbsci, lq.hbrci, lq.hboci, file=intm.file)
  #}


## plot centers
# 
# if (!exists(paste(Pr, "ci", sep="")))
#   load(file.path(INTERMEDIATE_DIR, "centers.RData"))
#   
# for (pr in Pr) {
#   CentersObjName <- paste(pr, "ci", sep="")
#   Centers <- get(CentersObjName); rm(CentersObjName)
#   Centers$TripPurpose <- pr
# }  
# 
# plot_map2 <- function(plot.Data) {
#   p <- ggplot() +
#     geom_polygon(data = plot.Data, aes(x = long, y = lat, group = group, fill = value), 
#                  color = "black", size = 0.1) +
#     #scale_fill_distiller(palette = "YlOrRd", na.value = "gray80") + 
#     scale_fill_identity(na.value = "gray80") +
#     #guides(fill = guide_legend(reverse = TRUE)) +
#     theme_nothing(legend = FALSE)
# }
# 
# centers <- mutate(hbwci, TripPurpose="HBW") %>%
#   union(mutate(hbsci, TripPurpose="HB Shopping")) %>%
#   union(mutate(hbrci, TripPurpose="HB Recreation")) %>%
#   union(mutate(hboci, TripPurpose="HB Other")) %>%
#   mutate(id = as.character(TAZ),
#          is.center = 1, 
#          value = is.center)
# 
# taz <- taz %>%
#   mutate(TAZ = as.integer(id),
#          seqid=1:n())
# 
# #taz$TAZ <- as.integer(taz$id)
# #taz$seq <- 1:nrow(taz)
# 
# plot.centers <- left_join(taz, hbwci) %>% mutate(TripPurpose="HBW") %>%
#   union( left_join(taz, hbsci) %>% mutate(TripPurpose="HB Shopping") ) %>%
#   union( left_join(taz, hbrci) %>% mutate(TripPurpose="HB Recreation") ) %>%
#   union( left_join(taz, hboci) %>% mutate(TripPurpose="HB Other") ) %>%
#   mutate(TripPurpose = factor(TripPurpose, levels=c("HBW", "HB Shopping", "HB Recreation", "HB Other")),
#          is.center = as.integer(!is.na(center.id))
#          ) %>%
#   arrange(TripPurpose, seqid)
# 
# #plot.centers <- full_join(taz, centers)
# 
# plot_map2 <- function(plot.Data) {
#   p <- ggplot() +
#   geom_polygon(data = plot.Data, aes(x = long, y = lat, group = group, fill = center.id),
#   color = "grey70", size = 0.25) +
#   #scale_fill_manual(values=cbPalette) +
#   scale_fill_identity(na.value = "grey70") +
#   #guides(fill = guide_legend(reverse = TRUE)) +
#   theme_nothing(legend = TRUE)
# }
# 
# map_centers <- plot_map2(plot.centers) + facet_wrap(~TripPurpose, ncol=2, scales="free") #facet_grid(TripPurpose~inc.level)
# map_centers
# output_file = file.path(OUTPUT_DIR, "map_centers.png")
# ggsave(map_centers, file = output_file, type = "cairo-png")
# 
# ggplot(nmmaps, aes(date,temp))+geom_point(color="chartreuse4")+
#  facet_wrap(~year, ncol=2, scales="free")
