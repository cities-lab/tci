
str(TAZPloyNoNA@data)

# Identify clusters for HBW
quantile(TAZPloyNoNA@data[,"totemp.den"], c(0.5,0.6,0.7,0.8,0.9,0.95))
data.df <- TAZPloyNoNA@data
hbwfilter170 <- data.df$totemp.den >= 170
hbwcluster170 <- identify_clusters(TAZPloyNoNA, filter=hbwfilter170, dist=1.0)

hbwfilter316 <- data.df$totemp.den >= 316
hbwcluster316 <- identify_clusters(TAZPloyNoNA, filter=hbwfilter316, dist=1.0)

hbwfilter529 <- data.df$totemp.den >= 529
hbwcluster529 <- identify_clusters(TAZPloyNoNA, filter=hbwfilter529, dist=1.0)

hbwfilter1010 <- data.df$totemp.den >= 1010
hbwcluster1010 <- identify_clusters(TAZPloyNoNA, filter=hbwfilter1010, dist=1.0)

hbwfilter2249 <- data.df$totemp.den >= 2249
hbwcluster2249 <- identify_clusters(TAZPloyNoNA, filter=hbwfilter2249, dist=1.0)

hbwfilter4138 <- data.df$totemp.den >= 4138
hbwcluster4138 <- identify_clusters(TAZPloyNoNA, filter=hbwfilter4138, dist=1.0)

if (SAVE.INTERMEDIARIES) {
  intm.file <- file.path(INTERMEDIATE_DIR, "hbwclusters.RData")
  save(hbwcluster170, hbwcluster316, hbwcluster1010, hbwcluster529, hbwfilter2249, hbwcluster4138, file=intm.file)
}

# IDentify clusters for HBS
quantile(TAZPloyNoNA@data[,"st.hbs.den"], c(0.5,0.6,0.7,0.8,0.9,0.95))
data.df <- TAZPloyNoNA@data
hbsfilter30 <- data.df$st.hbs.den >= 30
hbscluster30 <- identify_clusters(TAZPloyNoNA, filter=hbsfilter30, dist=1.0)

hbsfilter54 <- data.df$st.hbs.den >= 54
hbscluster54 <- identify_clusters(TAZPloyNoNA, filter=hbsfilter54, dist=1.0)

hbsfilter102 <- data.df$st.hbs.den >= 102
hbscluster102 <- identify_clusters(TAZPloyNoNA, filter=hbsfilter102, dist=1.0)

hbsfilter218 <- data.df$st.hbs.den >= 218
hbscluster218 <- identify_clusters(TAZPloyNoNA, filter=hbsfilter218, dist=1.0)

hbsfilter545 <- data.df$st.hbs.den >= 545
hbscluster545 <- identify_clusters(TAZPloyNoNA, filter=hbsfilter545, dist=1.0)

hbsfilter1060 <- data.df$st.hbs.den >= 1060
hbscluster1060 <- identify_clusters(TAZPloyNoNA, filter=hbsfilter1060, dist=1.0)

exists("hbscluster54")

if (SAVE.INTERMEDIARIES) {
  intm.file <- file.path(INTERMEDIATE_DIR, "hbsclusters.RData")
  save(hbscluster30, hbscluster54, hbscluster102,hbscluster218,hbscluster545,hbscluster1060, file=intm.file)
}



# Identify clusters for HBR 
quantile(TAZPloyNoNA@data[,"st.hbr.den"], c(0.5,0.6,0.7,0.8,0.9,0.95))
data.df <- TAZPloyNoNA@data
hbrfilter1085 <- data.df$st.hbr.den >= 1085
hbrcluster1085 <- identify_clusters(TAZPloyNoNA, filter=hbrfilter1085, dist=1.0)

hbrfilter1375 <- data.df$st.hbr.den >= 1375
hbrcluster1375 <- identify_clusters(TAZPloyNoNA, filter=hbrfilter1375, dist=1.0)

hbrfilter1761 <- data.df$st.hbr.den >= 1761
hbrcluster1761 <- identify_clusters(TAZPloyNoNA, filter=hbrfilter1761, dist=1.0)

hbrfilter2238 <- data.df$st.hbr.den >= 2238
hbrcluster2238 <- identify_clusters(TAZPloyNoNA, filter=hbrfilter2238, dist=1.0)

hbrfilter3372 <- data.df$st.hbr.den >= 3372
hbrcluster3372 <- identify_clusters(TAZPloyNoNA, filter=hbrfilter3372, dist=1.0)

hbrfilter5494 <- data.df$st.hbr.den >= 5494
hbrcluster5494 <- identify_clusters(TAZPloyNoNA, filter=hbrfilter5494, dist=1.0)


if (SAVE.INTERMEDIARIES) {
  intm.file <- file.path(INTERMEDIATE_DIR, "hbrclusters.RData")
  save(hbrcluster1085, hbrcluster1375, hbrcluster1761, hbrcluster2238,hbrcluster3372, hbrcluster5494, file=intm.file)
}


# Identify clusters for HBO`
quantile(TAZPloyNoNA@data[,"st.hbo.den"], c(0.5,0.6,0.7,0.8,0.9,0.95))
data.df <- TAZPloyNoNA@data
hbofilter251 <- data.df$st.hbo.den >= 251
hbocluster251 <- identify_clusters(TAZPloyNoNA, filter=hbofilter251, dist=1.0)

hbofilter353 <- data.df$st.hbo.den >= 353
hbocluster353 <- identify_clusters(TAZPloyNoNA, filter=hbofilter353, dist=1.0)

hbofilter496 <- data.df$st.hbo.den >= 496
hbocluster496 <- identify_clusters(TAZPloyNoNA, filter=hbofilter496, dist=1.0)

hbofilter766 <- data.df$st.hbo.den >= 766
hbocluster766 <- identify_clusters(TAZPloyNoNA, filter=hbofilter766, dist=1.0)

hbofilter1473 <- data.df$st.hbo.den >= 1473
hbocluster1473 <- identify_clusters(TAZPloyNoNA, filter=hbofilter1473, dist=1.0)

hbofilter2553 <- data.df$st.hbo.den >= 2553
hbocluster2553 <- identify_clusters(TAZPloyNoNA, filter=hbofilter2553, dist=1.0)



if (SAVE.INTERMEDIARIES) {
  intm.file <- file.path(INTERMEDIATE_DIR, "hboclusters.RData")
  save(hbocluster251, hbocluster353, hbocluster496, hbocluster766,hbocluster1473, hbocluster2553, file=intm.file)
}


