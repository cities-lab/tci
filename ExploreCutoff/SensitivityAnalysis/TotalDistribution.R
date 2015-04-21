## hbs
# hbs.den 50% (30) percentile 30
# Get distribution of sum of sizeterms and calculate percentiles of sum of sizeterms 
hbscluster30data <- hbscluster30@data

hbscluster30datasum <- hbscluster30data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbs), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1)  %>%
  arrange(cluster.sum,cluster.count)

str(hbscluster30datasum)

as.data.frame(hbscluster30datasum)

if (SAVE.INTERMEDIARIES) {
  intm.file <- file.path(INTERMEDIATE_DIR, "hbscluster30datasum.txt")
  write.table(hbscluster30datasum,intm.file,sep="\t")
}

quantile(hbscluster30datasum$cluster.sum, c(0.25,0.5,0.75))

hbscluster30datasumdistribution <- data.frame(cluster.sum=rep(hbscluster30datasum$cluster.sum, hbscluster30datasum $cluster.count))
quantile(hbscluster30datasumdistribution$cluster.sum, c(0.25,0.5,0.75))


# identify TAZs indexs 
# Unweighted percentiles
# 0.25 unweighted percentile 

hbscluster30_600valid.cluster <- hbscluster30data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbs), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 600)  %>%
  dplyr::select(cluster.id)

hbscenters30_600 <- inner_join(hbscluster30data, hbscluster30_600valid.cluster)

hbsci30_600 <- hbscenters30_600 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hbsci30_600)

# 0.5 unweighted percentile 
hbscluster30_1100valid.cluster <- hbscluster30data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbs), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 1100)  %>%
  dplyr::select(cluster.id)

hbscenters30_1100 <- inner_join(hbscluster30data, hbscluster30_1100valid.cluster)

hbsci30_1100 <- hbscenters30_1100 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hbsci30_1100)

# 0.75 unweighted percentile 

hbscluster30_1594valid.cluster <- hbscluster30data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbs), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 1594)  %>%
  dplyr::select(cluster.id)

hbscenters30_1594 <- inner_join(hbscluster30data, hbscluster30_1594valid.cluster)

hbsci30_1594 <- hbscenters30_1594 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hbsci30_1594)


# weighted percentiles
# 0.25 weighted percentile 
hbscluster30_24009valid.cluster <- hbscluster30data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbs), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 24009)  %>%
  dplyr::select(cluster.id)

hbscenters30_24009 <- inner_join(hbscluster30data, hbscluster30_24009valid.cluster)

hbsci30_24009 <- hbscenters30_24009 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hbsci30_24009)

# 0.5 weighted percentile 
hbscluster30_68817valid.cluster <- hbscluster30data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbs), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 68817)  %>%
  dplyr::select(cluster.id)

hbscenters30_68817 <- inner_join(hbscluster30data, hbscluster30_68817valid.cluster)

hbsci30_68817 <- hbscenters30_68817 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hbsci30_68817)

# 0.75 weighted percentile 

hbscluster30_77073valid.cluster <- hbscluster30data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbs), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 77073)  %>%
  dplyr::select(cluster.id)

hbscenters30_77073 <- inner_join(hbscluster30data, hbscluster30_77073valid.cluster)

hbsci30_77073 <- hbscenters30_77073 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hbsci30_77073)

# hbs.den 60% (54) percentile 54
# Get distribution of sum of sizeterms and calculate percentiles of sum of sizeterms 
hbscluster54data <- hbscluster54@data

hbscluster54datasum <- hbscluster54data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbs), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1)  %>%
  arrange(cluster.sum,cluster.count)

str(hbscluster54datasum)

as.data.frame(hbscluster54datasum)

if (SAVE.INTERMEDIARIES) {
  intm.file <- file.path(INTERMEDIATE_DIR, "hbscluster54datasum.txt")
  write.table(hbscluster54datasum,intm.file,sep="\t")
}

quantile(hbscluster54datasum$cluster.sum, c(0.25,0.5,0.75))

hbscluster54datasumdistribution <- data.frame(cluster.sum=rep(hbscluster54datasum$cluster.sum, hbscluster54datasum $cluster.count))
quantile(hbscluster54datasumdistribution$cluster.sum, c(0.25,0.5,0.75))


# identify TAZs indexs 
# Unweighted percentiles
# 0.25 unweighted percentile 

hbscluster54_322valid.cluster <- hbscluster54data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbs), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 322)  %>%
  dplyr::select(cluster.id)

hbscenters54_322 <- inner_join(hbscluster54data, hbscluster54_322valid.cluster)

hbsci54_322 <- hbscenters54_322 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hbsci54_322)

# 0.5 unweighted percentile 
hbscluster54_815valid.cluster <- hbscluster54data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbs), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 815)  %>%
  dplyr::select(cluster.id)

hbscenters54_815 <- inner_join(hbscluster54data, hbscluster54_815valid.cluster)

hbsci54_815 <- hbscenters54_815 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hbsci54_815)

# 0.75 unweighted percentile 

hbscluster54_1660valid.cluster <- hbscluster54data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbs), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 1660)  %>%
  dplyr::select(cluster.id)

hbscenters54_1660 <- inner_join(hbscluster54data, hbscluster54_1660valid.cluster)

hbsci54_1660 <- hbscenters54_1660 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hbsci54_1660)


# weighted percentiles
# 0.25 weighted percentile 
hbscluster54_8080valid.cluster <- hbscluster54data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbs), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 8080)  %>%
  dplyr::select(cluster.id)

hbscenters54_8080 <- inner_join(hbscluster54data, hbscluster54_8080valid.cluster)

hbsci54_8080 <- hbscenters54_8080 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hbsci54_8080)

# 0.5 weighted percentile 
hbscluster54_64595valid.cluster <- hbscluster54data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbs), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 64595)  %>%
  dplyr::select(cluster.id)

hbscenters54_64595 <- inner_join(hbscluster54data, hbscluster54_64595valid.cluster)

hbsci54_64595 <- hbscenters54_64595 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hbsci54_64595)

# 0.75 weighted percentile 

hbscluster54_74519valid.cluster <- hbscluster54data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbs), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 74519)  %>%
  dplyr::select(cluster.id)

hbscenters54_74519 <- inner_join(hbscluster54data, hbscluster54_74519valid.cluster)

hbsci54_74519 <- hbscenters54_74519 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hbsci54_74519)



# hbs 70%(102) percentile 102
# Get distribution of sum of sizeterms and calculate percentiles of sum of sizeterms 
hbscluster102data <- hbscluster102@data

hbscluster102datasum <- hbscluster102data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbs), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1)  %>%
  arrange(cluster.sum,cluster.count)

str(hbscluster102datasum)

as.data.frame(hbscluster102datasum)

if (SAVE.INTERMEDIARIES) {
  intm.file <- file.path(INTERMEDIATE_DIR, "hbscluster102datasum.txt")
  write.table(hbscluster102datasum,intm.file,sep="\t")
}

quantile(hbscluster102datasum$cluster.sum, c(0.25,0.5,0.75))

hbscluster102datasumdistribution <- data.frame(cluster.sum=rep(hbscluster102datasum$cluster.sum, hbscluster102datasum $cluster.count))
quantile(hbscluster102datasumdistribution$cluster.sum, c(0.25,0.5,0.75))

test <- hbscluster102datasumdistribution %>%
  group_by(cluster.sum) %>%
  summarise(cluster.count=n())

# identify TAZs indexs 
# Unweighted percentiles
# 0.25 unweighted percentile 
hbscluster102data <- hbscluster102@data

hbscluster102_458valid.cluster <- hbscluster102data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbs), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 458)  %>%
  dplyr::select(cluster.id)

hbscenters102_458 <- inner_join(hbscluster102data, hbscluster102_458valid.cluster)

hbsci102_458 <- hbscenters102_458 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hbsci102_458)

# 0.5 unweighted percentile 
hbscluster102_1134valid.cluster <- hbscluster102data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbs), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 1134)  %>%
  dplyr::select(cluster.id)

hbscenters102_1134 <- inner_join(hbscluster102data, hbscluster102_1134valid.cluster)

hbsci102_1134 <- hbscenters102_1134 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hbsci102_1134)

# 0.75 unweighted percentile 

hbscluster102_3240valid.cluster <- hbscluster102data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbs), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 3240)  %>%
  dplyr::select(cluster.id)

hbscenters102_3240 <- inner_join(hbscluster102data, hbscluster102_3240valid.cluster)

hbsci102_3240 <- hbscenters102_3240 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hbsci102_3240)


# weighted percentiles
# 0.25 weighted percentile 
hbscluster102_3608valid.cluster <- hbscluster102data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbs), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 3608)  %>%
  dplyr::select(cluster.id)

hbscenters102_3608 <- inner_join(hbscluster102data, hbscluster102_3608valid.cluster)

hbsci102_3608 <- hbscenters102_3608 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hbsci102_3608)

# 0.5 weighted percentile 
hbscluster102_23238valid.cluster <- hbscluster102data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbs), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 23238)  %>%
  dplyr::select(cluster.id)

hbscenters102_23238 <- inner_join(hbscluster102data, hbscluster102_23238valid.cluster)

hbsci102_23238 <- hbscenters102_23238 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hbsci102_23238)

# 0.75 weighted percentile 

hbscluster102_45027valid.cluster <- hbscluster102data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbs), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 45027)  %>%
  dplyr::select(cluster.id)

hbscenters102_45027 <- inner_join(hbscluster102data, hbscluster102_45027valid.cluster)

hbsci102_45027 <- hbscenters102_45027 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hbsci102_45027)

# hbs.den 80% (218) percentile 218
# Get distribution of sum of sizeterms and calculate percentiles of sum of sizeterms 
hbscluster218data <- hbscluster218@data

hbscluster218datasum <- hbscluster218data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbs), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1)  %>%
  arrange(cluster.sum,cluster.count)

str(hbscluster218datasum)

as.data.frame(hbscluster218datasum)

if (SAVE.INTERMEDIARIES) {
  intm.file <- file.path(INTERMEDIATE_DIR, "hbscluster218datasum.txt")
  write.table(hbscluster218datasum,intm.file,sep="\t")
}

quantile(hbscluster218datasum$cluster.sum, c(0.25,0.5,0.75))

hbscluster218datasumdistribution <- data.frame(cluster.sum=rep(hbscluster218datasum$cluster.sum, hbscluster218datasum $cluster.count))
quantile(hbscluster218datasumdistribution$cluster.sum, c(0.25,0.5,0.75))


# identify TAZs indexs 
# Unweighted percentiles
# 0.25 unweighted percentile 

hbscluster218_593valid.cluster <- hbscluster218data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbs), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 593)  %>%
  dplyr::select(cluster.id)

hbscenters218_593 <- inner_join(hbscluster218data, hbscluster218_593valid.cluster)

hbsci218_593 <- hbscenters218_593 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hbsci218_593)

# 0.5 unweighted percentile 
hbscluster218_1030valid.cluster <- hbscluster218data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbs), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 1030)  %>%
  dplyr::select(cluster.id)

hbscenters218_1030 <- inner_join(hbscluster218data, hbscluster218_1030valid.cluster)

hbsci218_1030 <- hbscenters218_1030 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hbsci218_1030)

# 0.75 unweighted percentile 

hbscluster218_2616valid.cluster <- hbscluster218data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbs), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 2616)  %>%
  dplyr::select(cluster.id)

hbscenters218_2616 <- inner_join(hbscluster218data, hbscluster218_2616valid.cluster)

hbsci218_2616 <- hbscenters218_2616 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hbsci218_2616)


# weighted percentiles
# 0.25 weighted percentile 
hbscluster218_1146valid.cluster <- hbscluster218data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbs), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 1146)  %>%
  dplyr::select(cluster.id)

hbscenters218_1146 <- inner_join(hbscluster218data, hbscluster218_1146valid.cluster)

hbsci218_1146 <- hbscenters218_1146 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hbsci218_1146)

# 0.5 weighted percentile 
hbscluster218_4467valid.cluster <- hbscluster218data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbs), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 4467)  %>%
  dplyr::select(cluster.id)

hbscenters218_4467 <- inner_join(hbscluster218data, hbscluster218_4467valid.cluster)

hbsci218_4467 <- hbscenters218_4467 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hbsci218_4467)

# 0.75 weighted percentile 

hbscluster218_8583valid.cluster <- hbscluster218data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbs), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 8583)  %>%
  dplyr::select(cluster.id)

hbscenters218_8583 <- inner_join(hbscluster218data, hbscluster218_8583valid.cluster)

hbsci218_8583 <- hbscenters218_8583 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hbsci218_8583)



# hbs.den 90% (545) percentile 545
# Get distribution of sum of sizeterms and calculate percentiles of sum of sizeterms 
hbscluster545data <- hbscluster545@data

hbscluster545datasum <- hbscluster545data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbs), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1)  %>%
  arrange(cluster.sum,cluster.count)

str(hbscluster545datasum)

as.data.frame(hbscluster545datasum)

if (SAVE.INTERMEDIARIES) {
  intm.file <- file.path(INTERMEDIATE_DIR, "hbscluster545datasum.txt")
  write.table(hbscluster545datasum,intm.file,sep="\t")
}

quantile(hbscluster545datasum$cluster.sum, c(0.25,0.5,0.75))

hbscluster545datasumdistribution <- data.frame(cluster.sum=rep(hbscluster545datasum$cluster.sum, hbscluster545datasum $cluster.count))
quantile(hbscluster545datasumdistribution$cluster.sum, c(0.25,0.5,0.75))


# identify TAZs indexs 
# Unweighted percentiles
# 0.25 unweighted percentile 

hbscluster545_955valid.cluster <- hbscluster545data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbs), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 955)  %>%
  dplyr::select(cluster.id)

hbscenters545_955 <- inner_join(hbscluster545data, hbscluster545_955valid.cluster)

hbsci545_955 <- hbscenters545_955 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hbsci545_955)

# 0.5 unweighted percentile 
hbscluster545_1845valid.cluster <- hbscluster545data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbs), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 1845)  %>%
  dplyr::select(cluster.id)

hbscenters545_1845 <- inner_join(hbscluster545data, hbscluster545_1845valid.cluster)

hbsci545_1845 <- hbscenters545_1845 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hbsci545_1845)

# 0.75 unweighted percentile 

hbscluster545_3828valid.cluster <- hbscluster545data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbs), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 3828)  %>%
  dplyr::select(cluster.id)

hbscenters545_3828 <- inner_join(hbscluster545data, hbscluster545_3828valid.cluster)

hbsci545_3828 <- hbscenters545_3828 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hbsci545_3828)


# weighted percentiles
# 0.25 weighted percentile 
hbscluster545_1424valid.cluster <- hbscluster545data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbs), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 1424)  %>%
  dplyr::select(cluster.id)

hbscenters545_1424 <- inner_join(hbscluster545data, hbscluster545_1424valid.cluster)

hbsci545_1424 <- hbscenters545_1424 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hbsci545_1424)

# 0.5 weighted percentile 
hbscluster545_4535valid.cluster <- hbscluster545data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbs), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 4535)  %>%
  dplyr::select(cluster.id)

hbscenters545_4535 <- inner_join(hbscluster545data, hbscluster545_4535valid.cluster)

hbsci545_4535 <- hbscenters545_4535 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hbsci545_4535)

# 0.75 weighted percentile 

hbscluster545_10949valid.cluster <- hbscluster545data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbs), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 10949)  %>%
  dplyr::select(cluster.id)

hbscenters545_10949 <- inner_join(hbscluster545data, hbscluster545_10949valid.cluster)

hbsci545_10949 <- hbscenters545_10949 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hbsci545_10949)


# hbs.den 95% (1060) percentile 1060
# Get distribution of sum of sizeterms and calculate percentiles of sum of sizeterms 
hbscluster1060data <- hbscluster1060@data

hbscluster1060datasum <- hbscluster1060data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbs), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1)  %>%
  arrange(cluster.sum,cluster.count)

str(hbscluster1060datasum)

as.data.frame(hbscluster1060datasum)

if (SAVE.INTERMEDIARIES) {
  intm.file <- file.path(INTERMEDIATE_DIR, "hbscluster1060datasum.txt")
  write.table(hbscluster1060datasum,intm.file,sep="\t")
}

quantile(hbscluster1060datasum$cluster.sum, c(0.25,0.5,0.75))

hbscluster1060datasumdistribution <- data.frame(cluster.sum=rep(hbscluster1060datasum$cluster.sum, hbscluster1060datasum $cluster.count))
quantile(hbscluster1060datasumdistribution$cluster.sum, c(0.25,0.5,0.75))


# identify TAZs indexs 
# Unweighted percentiles
# 0.25 unweighted percentile 

hbscluster1060_694valid.cluster <- hbscluster1060data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbs), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 694)  %>%
  dplyr::select(cluster.id)

hbscenters1060_694 <- inner_join(hbscluster1060data, hbscluster1060_694valid.cluster)

hbsci1060_694 <- hbscenters1060_694 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hbsci1060_694)

# 0.5 unweighted percentile 
hbscluster1060_1725valid.cluster <- hbscluster1060data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbs), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 1725)  %>%
  dplyr::select(cluster.id)

hbscenters1060_1725 <- inner_join(hbscluster1060data, hbscluster1060_1725valid.cluster)

hbsci1060_1725 <- hbscenters1060_1725 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hbsci1060_1725)

# 0.75 unweighted percentile 

hbscluster1060_3124valid.cluster <- hbscluster1060data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbs), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 3124)  %>%
  dplyr::select(cluster.id)

hbscenters1060_3124 <- inner_join(hbscluster1060data, hbscluster1060_3124valid.cluster)

hbsci1060_3124 <- hbscenters1060_3124 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hbsci1060_3124)


# weighted percentiles
# 0.25 weighted percentile 
hbscluster1060_1324valid.cluster <- hbscluster1060data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbs), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 1324)  %>%
  dplyr::select(cluster.id)

hbscenters1060_1324 <- inner_join(hbscluster1060data, hbscluster1060_1324valid.cluster)

hbsci1060_1324 <- hbscenters1060_1324 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hbsci1060_1324)

# 0.5 weighted percentile 
hbscluster1060_4294valid.cluster <- hbscluster1060data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbs), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 4294)  %>%
  dplyr::select(cluster.id)

hbscenters1060_4294 <- inner_join(hbscluster1060data, hbscluster1060_4294valid.cluster)

hbsci1060_4294 <- hbscenters1060_4294 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hbsci1060_4294)

# 0.75 weighted percentile 

hbscluster1060_17534valid.cluster <- hbscluster1060data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbs), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 17534)  %>%
  dplyr::select(cluster.id)

hbscenters1060_17534 <- inner_join(hbscluster1060data, hbscluster1060_17534valid.cluster)

hbsci1060_17534 <- hbscenters1060_17534 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hbsci1060_17534)


## HBR 
# hbr.den 50% (1085) percentile 1085
# Get distribution of sum of sizeterms and calculate percentiles of sum of sizeterms 
hbrcluster1085data <- hbrcluster1085@data

hbrcluster1085datasum <- hbrcluster1085data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbr), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1)  %>%
  arrange(cluster.sum,cluster.count)

str(hbrcluster1085datasum)

as.data.frame(hbrcluster1085datasum)

if (SAVE.INTERMEDIARIES) {
  intm.file <- file.path(INTERMEDIATE_DIR, "hbrcluster1085datasum.txt")
  write.table(hbrcluster1085datasum,intm.file,sep="\t")
}

quantile(hbrcluster1085datasum$cluster.sum, c(0.25,0.5,0.75))

hbrcluster1085datasumdistribution <- data.frame(cluster.sum=rep(hbrcluster1085datasum$cluster.sum, hbrcluster1085datasum $cluster.count))
quantile(hbrcluster1085datasumdistribution$cluster.sum, c(0.25,0.5,0.75))


# identify TAZs indexs 
# Unweighted percentiles
# 0.25 unweighted percentile 

hbrcluster1085_4657valid.cluster <- hbrcluster1085data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbr), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 4657)  %>%
  dplyr::select(cluster.id)

hbrcenters1085_4657 <- inner_join(hbrcluster1085data, hbrcluster1085_4657valid.cluster)

hbrci1085_4657 <- hbrcenters1085_4657 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hbrci1085_4657)

# 0.5 unweighted percentile 
hbrcluster1085_7723valid.cluster <- hbrcluster1085data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbr), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 7723)  %>%
  dplyr::select(cluster.id)

hbrcenters1085_7723 <- inner_join(hbrcluster1085data, hbrcluster1085_7723valid.cluster)

hbrci1085_7723 <- hbrcenters1085_7723 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hbrci1085_7723)

# 0.75 unweighted percentile 

hbrcluster1085_22201valid.cluster <- hbrcluster1085data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbr), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 22201)  %>%
  dplyr::select(cluster.id)

hbrcenters1085_22201 <- inner_join(hbrcluster1085data, hbrcluster1085_22201valid.cluster)

hbrci1085_22201 <- hbrcenters1085_22201 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hbrci1085_22201)


# weighted percentiles
# 0.25 weighted percentile 
hbrcluster1085_626499valid.cluster <- hbrcluster1085data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbr), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 626499)  %>%
  dplyr::select(cluster.id)

hbrcenters1085_626499 <- inner_join(hbrcluster1085data, hbrcluster1085_626499valid.cluster)

hbrci1085_626499 <- hbrcenters1085_626499 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hbrci1085_626499)

# 0.5 weighted percentile 
hbrcluster1085_626499valid.cluster <- hbrcluster1085data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbr), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 626499)  %>%
  dplyr::select(cluster.id)

hbrcenters1085_626499 <- inner_join(hbrcluster1085data, hbrcluster1085_626499valid.cluster)

hbrci1085_626499 <- hbrcenters1085_626499 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hbrci1085_626499)

# 0.75 weighted percentile 

hbrcluster1085_630932valid.cluster <- hbrcluster1085data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbr), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 630932)  %>%
  dplyr::select(cluster.id)

hbrcenters1085_630932 <- inner_join(hbrcluster1085data, hbrcluster1085_630932valid.cluster)

hbrci1085_630932 <- hbrcenters1085_630932 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hbrci1085_630932)

# hbr.den 60% (1375) percentile 1375
# Get distribution of sum of sizeterms and calculate percentiles of sum of sizeterms 
hbrcluster1375data <- hbrcluster1375@data

hbrcluster1375datasum <- hbrcluster1375data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbr), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1)  %>%
  arrange(cluster.sum,cluster.count)

str(hbrcluster1375datasum)

as.data.frame(hbrcluster1375datasum)

if (SAVE.INTERMEDIARIES) {
  intm.file <- file.path(INTERMEDIATE_DIR, "hbrcluster1375datasum.txt")
  write.table(hbrcluster1375datasum,intm.file,sep="\t")
}

quantile(hbrcluster1375datasum$cluster.sum, c(0.25,0.5,0.75))

hbrcluster1375datasumdistribution <- data.frame(cluster.sum=rep(hbrcluster1375datasum$cluster.sum, hbrcluster1375datasum $cluster.count))
quantile(hbrcluster1375datasumdistribution$cluster.sum, c(0.25,0.5,0.75))


# identify TAZs indexs 
# Unweighted percentiles
# 0.25 unweighted percentile 

hbrcluster1375_3777valid.cluster <- hbrcluster1375data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbr), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 3777)  %>%
  dplyr::select(cluster.id)

hbrcenters1375_3777 <- inner_join(hbrcluster1375data, hbrcluster1375_3777valid.cluster)

hbrci1375_3777 <- hbrcenters1375_3777 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hbrci1375_3777)

# 0.5 unweighted percentile 
hbrcluster1375_7306valid.cluster <- hbrcluster1375data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbr), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 7306)  %>%
  dplyr::select(cluster.id)

hbrcenters1375_7306 <- inner_join(hbrcluster1375data, hbrcluster1375_7306valid.cluster)

hbrci1375_7306 <- hbrcenters1375_7306 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hbrci1375_7306)

# 0.75 unweighted percentile 

hbrcluster1375_13519valid.cluster <- hbrcluster1375data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbr), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 13519)  %>%
  dplyr::select(cluster.id)

hbrcenters1375_13519 <- inner_join(hbrcluster1375data, hbrcluster1375_13519valid.cluster)

hbrci1375_13519 <- hbrcenters1375_13519 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hbrci1375_13519)


# weighted percentiles
# 0.25 weighted percentile 
hbrcluster1375_182333valid.cluster <- hbrcluster1375data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbr), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 182333)  %>%
  dplyr::select(cluster.id)

hbrcenters1375_182333 <- inner_join(hbrcluster1375data, hbrcluster1375_182333valid.cluster)

hbrci1375_182333 <- hbrcenters1375_182333 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hbrci1375_182333)

# 0.5 weighted percentile 
hbrcluster1375_330929valid.cluster <- hbrcluster1375data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbr), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 330929)  %>%
  dplyr::select(cluster.id)

hbrcenters1375_330929 <- inner_join(hbrcluster1375data, hbrcluster1375_330929valid.cluster)

hbrci1375_330929 <- hbrcenters1375_330929 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hbrci1375_330929)

# 0.75 weighted percentile 

hbrcluster1375_555713valid.cluster <- hbrcluster1375data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbr), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 555713)  %>%
  dplyr::select(cluster.id)

hbrcenters1375_555713 <- inner_join(hbrcluster1375data, hbrcluster1375_555713valid.cluster)

hbrci1375_555713 <- hbrcenters1375_555713 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hbrci1375_555713)


# hbr.den 70% (1761) percentile 1761
# Get distribution of sum of sizeterms and calculate percentiles of sum of sizeterms 
hbrcluster1761data <- hbrcluster1761@data

hbrcluster1761datasum <- hbrcluster1761data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbr), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1)  %>%
  arrange(cluster.sum,cluster.count)

str(hbrcluster1761datasum)

as.data.frame(hbrcluster1761datasum)

if (SAVE.INTERMEDIARIES) {
  intm.file <- file.path(INTERMEDIATE_DIR, "hbrcluster1761datasum.txt")
  write.table(hbrcluster1761datasum,intm.file,sep="\t")
}

quantile(hbrcluster1761datasum$cluster.sum, c(0.25,0.5,0.75))

hbrcluster1761datasumdistribution <- data.frame(cluster.sum=rep(hbrcluster1761datasum$cluster.sum, hbrcluster1761datasum $cluster.count))
quantile(hbrcluster1761datasumdistribution$cluster.sum, c(0.25,0.5,0.75))


# identify TAZs indexs 
# Unweighted percentiles
# 0.25 unweighted percentile 

hbrcluster1761_3239valid.cluster <- hbrcluster1761data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbr), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 3239)  %>%
  dplyr::select(cluster.id)

hbrcenters1761_3239 <- inner_join(hbrcluster1761data, hbrcluster1761_3239valid.cluster)

hbrci1761_3239 <- hbrcenters1761_3239 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hbrci1761_3239)

# 0.5 unweighted percentile 
hbrcluster1761_6483valid.cluster <- hbrcluster1761data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbr), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 6483)  %>%
  dplyr::select(cluster.id)

hbrcenters1761_6483 <- inner_join(hbrcluster1761data, hbrcluster1761_6483valid.cluster)

hbrci1761_6483 <- hbrcenters1761_6483 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hbrci1761_6483)

# 0.75 unweighted percentile 

hbrcluster1761_13077valid.cluster <- hbrcluster1761data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbr), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 13077)  %>%
  dplyr::select(cluster.id)

hbrcenters1761_13077 <- inner_join(hbrcluster1761data, hbrcluster1761_13077valid.cluster)

hbrci1761_13077 <- hbrcenters1761_13077 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hbrci1761_13077)


# weighted percentiles
# 0.25 weighted percentile 
hbrcluster1761_23100valid.cluster <- hbrcluster1761data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbr), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 23100)  %>%
  dplyr::select(cluster.id)

hbrcenters1761_23100 <- inner_join(hbrcluster1761data, hbrcluster1761_23100valid.cluster)

hbrci1761_23100 <- hbrcenters1761_23100 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hbrci1761_23100)

# 0.5 weighted percentile 
hbrcluster1761_245452valid.cluster <- hbrcluster1761data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbr), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 245452)  %>%
  dplyr::select(cluster.id)

hbrcenters1761_245452 <- inner_join(hbrcluster1761data, hbrcluster1761_245452valid.cluster)

hbrci1761_245452 <- hbrcenters1761_245452 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hbrci1761_245452)

# 0.75 weighted percentile 

hbrcluster1761_374459valid.cluster <- hbrcluster1761data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbr), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 374459)  %>%
  dplyr::select(cluster.id)

hbrcenters1761_374459 <- inner_join(hbrcluster1761data, hbrcluster1761_374459valid.cluster)

hbrci1761_374459 <- hbrcenters1761_374459 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hbrci1761_374459)


# hbr.den 80% (2238) percentile 2238
# Get distribution of sum of sizeterms and calculate percentiles of sum of sizeterms 
hbrcluster2238data <- hbrcluster2238@data

hbrcluster2238datasum <- hbrcluster2238data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbr), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1)  %>%
  arrange(cluster.sum,cluster.count)

str(hbrcluster2238datasum)

as.data.frame(hbrcluster2238datasum)

if (SAVE.INTERMEDIARIES) {
  intm.file <- file.path(INTERMEDIATE_DIR, "hbrcluster2238datasum.txt")
  write.table(hbrcluster2238datasum,intm.file,sep="\t")
}

quantile(hbrcluster2238datasum$cluster.sum, c(0.25,0.5,0.75))

hbrcluster2238datasumdistribution <- data.frame(cluster.sum=rep(hbrcluster2238datasum$cluster.sum, hbrcluster2238datasum $cluster.count))
quantile(hbrcluster2238datasumdistribution$cluster.sum, c(0.25,0.5,0.75))


# identify TAZs indexs 
# Unweighted percentiles
# 0.25 unweighted percentile 

hbrcluster2238_2850valid.cluster <- hbrcluster2238data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbr), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 2850)  %>%
  dplyr::select(cluster.id)

hbrcenters2238_2850 <- inner_join(hbrcluster2238data, hbrcluster2238_2850valid.cluster)

hbrci2238_2850 <- hbrcenters2238_2850 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hbrci2238_2850)

# 0.5 unweighted percentile 
hbrcluster2238_6128valid.cluster <- hbrcluster2238data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbr), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 6128)  %>%
  dplyr::select(cluster.id)

hbrcenters2238_6128 <- inner_join(hbrcluster2238data, hbrcluster2238_6128valid.cluster)

hbrci2238_6128 <- hbrcenters2238_6128 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hbrci2238_6128)

# 0.75 unweighted percentile 

hbrcluster2238_11056valid.cluster <- hbrcluster2238data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbr), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 11056)  %>%
  dplyr::select(cluster.id)

hbrcenters2238_11056 <- inner_join(hbrcluster2238data, hbrcluster2238_11056valid.cluster)

hbrci2238_11056 <- hbrcenters2238_11056 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hbrci2238_11056)


# weighted percentiles
# 0.25 weighted percentile 
hbrcluster2238_8319valid.cluster <- hbrcluster2238data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbr), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 8319)  %>%
  dplyr::select(cluster.id)

hbrcenters2238_8319 <- inner_join(hbrcluster2238data, hbrcluster2238_8319valid.cluster)

hbrci2238_8319 <- hbrcenters2238_8319 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hbrci2238_8319)

# 0.5 weighted percentile 
hbrcluster2238_28529valid.cluster <- hbrcluster2238data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbr), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 28529)  %>%
  dplyr::select(cluster.id)

hbrcenters2238_28529 <- inner_join(hbrcluster2238data, hbrcluster2238_28529valid.cluster)

hbrci2238_28529 <- hbrcenters2238_28529 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hbrci2238_28529)

# 0.75 weighted percentile 

hbrcluster2238_176324valid.cluster <- hbrcluster2238data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbr), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 176324)  %>%
  dplyr::select(cluster.id)

hbrcenters2238_176324 <- inner_join(hbrcluster2238data, hbrcluster2238_176324valid.cluster)

hbrci2238_176324 <- hbrcenters2238_176324 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hbrci2238_176324)


# hbr.den 90% (3372) percentile 3372
# Get distribution of sum of sizeterms and calculate percentiles of sum of sizeterms 
hbrcluster3372data <- hbrcluster3372@data

hbrcluster3372datasum <- hbrcluster3372data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbr), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1)  %>%
  arrange(cluster.sum,cluster.count)

str(hbrcluster3372datasum)

as.data.frame(hbrcluster3372datasum)

if (SAVE.INTERMEDIARIES) {
  intm.file <- file.path(INTERMEDIATE_DIR, "hbrcluster3372datasum.txt")
  write.table(hbrcluster3372datasum,intm.file,sep="\t")
}

quantile(hbrcluster3372datasum$cluster.sum, c(0.25,0.5,0.75))

hbrcluster3372datasumdistribution <- data.frame(cluster.sum=rep(hbrcluster3372datasum$cluster.sum, hbrcluster3372datasum $cluster.count))
quantile(hbrcluster3372datasumdistribution$cluster.sum, c(0.25,0.5,0.75))


# identify TAZs indexs 
# Unweighted percentiles
# 0.25 unweighted percentile 

hbrcluster3372_2746valid.cluster <- hbrcluster3372data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbr), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 2746)  %>%
  dplyr::select(cluster.id)

hbrcenters3372_2746 <- inner_join(hbrcluster3372data, hbrcluster3372_2746valid.cluster)

hbrci3372_2746 <- hbrcenters3372_2746 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hbrci3372_2746)

# 0.5 unweighted percentile 
hbrcluster3372_6278valid.cluster <- hbrcluster3372data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbr), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 6278)  %>%
  dplyr::select(cluster.id)

hbrcenters3372_6278 <- inner_join(hbrcluster3372data, hbrcluster3372_6278valid.cluster)

hbrci3372_6278 <- hbrcenters3372_6278 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hbrci3372_6278)

# 0.75 unweighted percentile 

hbrcluster3372_13772valid.cluster <- hbrcluster3372data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbr), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 13772)  %>%
  dplyr::select(cluster.id)

hbrcenters3372_13772 <- inner_join(hbrcluster3372data, hbrcluster3372_13772valid.cluster)

hbrci3372_13772 <- hbrcenters3372_13772 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hbrci3372_13772)


# weighted percentiles
# 0.25 weighted percentile 
hbrcluster3372_10558valid.cluster <- hbrcluster3372data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbr), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 10558)  %>%
  dplyr::select(cluster.id)

hbrcenters3372_10558 <- inner_join(hbrcluster3372data, hbrcluster3372_10558valid.cluster)

hbrci3372_10558 <- hbrcenters3372_10558 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hbrci3372_10558)

# 0.5 weighted percentile 
hbrcluster3372_22094valid.cluster <- hbrcluster3372data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbr), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 22094)  %>%
  dplyr::select(cluster.id)

hbrcenters3372_22094 <- inner_join(hbrcluster3372data, hbrcluster3372_22094valid.cluster)

hbrci3372_22094 <- hbrcenters3372_22094 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hbrci3372_22094)

# 0.75 weighted percentile 

hbrcluster3372_99156valid.cluster <- hbrcluster3372data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbr), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 99156)  %>%
  dplyr::select(cluster.id)

hbrcenters3372_99156 <- inner_join(hbrcluster3372data, hbrcluster3372_99156valid.cluster)

hbrci3372_99156 <- hbrcenters3372_99156 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hbrci3372_99156)


# hbr.den 95% (5494) percentile 5494
# Get distribution of sum of sizeterms and calculate percentiles of sum of sizeterms 
hbrcluster5494data <- hbrcluster5494@data

hbrcluster5494datasum <- hbrcluster5494data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbr), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1)  %>%
  arrange(cluster.sum,cluster.count)

str(hbrcluster5494datasum)

as.data.frame(hbrcluster5494datasum)

if (SAVE.INTERMEDIARIES) {
  intm.file <- file.path(INTERMEDIATE_DIR, "hbrcluster5494datasum.txt")
  write.table(hbrcluster5494datasum,intm.file,sep="\t")
}

quantile(hbrcluster5494datasum$cluster.sum, c(0.25,0.5,0.75))

hbrcluster5494datasumdistribution <- data.frame(cluster.sum=rep(hbrcluster5494datasum$cluster.sum, hbrcluster5494datasum $cluster.count))
quantile(hbrcluster5494datasumdistribution$cluster.sum, c(0.25,0.5,0.75))


# identify TAZs indexs 
# Unweighted percentiles
# 0.25 unweighted percentile 

hbrcluster5494_6848valid.cluster <- hbrcluster5494data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbr), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 6848)  %>%
  dplyr::select(cluster.id)

hbrcenters5494_6848 <- inner_join(hbrcluster5494data, hbrcluster5494_6848valid.cluster)

hbrci5494_6848 <- hbrcenters5494_6848 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hbrci5494_6848)

# 0.5 unweighted percentile 
hbrcluster5494_9089valid.cluster <- hbrcluster5494data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbr), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 9089)  %>%
  dplyr::select(cluster.id)

hbrcenters5494_9089 <- inner_join(hbrcluster5494data, hbrcluster5494_9089valid.cluster)

hbrci5494_9089 <- hbrcenters5494_9089 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hbrci5494_9089)

# 0.75 unweighted percentile 

hbrcluster5494_12932valid.cluster <- hbrcluster5494data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbr), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 12932)  %>%
  dplyr::select(cluster.id)

hbrcenters5494_12932 <- inner_join(hbrcluster5494data, hbrcluster5494_12932valid.cluster)

hbrci5494_12932 <- hbrcenters5494_12932 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hbrci5494_12932)


# weighted percentiles
# 0.25 weighted percentile 
hbrcluster5494_9089valid.cluster <- hbrcluster5494data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbr), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 9089)  %>%
  dplyr::select(cluster.id)

hbrcenters5494_9089 <- inner_join(hbrcluster5494data, hbrcluster5494_9089valid.cluster)

hbrci5494_9089 <- hbrcenters5494_9089 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hbrci5494_9089)

# 0.5 weighted percentile 
hbrcluster5494_48501valid.cluster <- hbrcluster5494data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbr), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 48501)  %>%
  dplyr::select(cluster.id)

hbrcenters5494_48501 <- inner_join(hbrcluster5494data, hbrcluster5494_48501valid.cluster)

hbrci5494_48501 <- hbrcenters5494_48501 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hbrci5494_48501)

# 0.75 weighted percentile 

hbrcluster5494_146273valid.cluster <- hbrcluster5494data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbr), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 146273)  %>%
  dplyr::select(cluster.id)

hbrcenters5494_146273 <- inner_join(hbrcluster5494data, hbrcluster5494_146273valid.cluster)

hbrci5494_146273 <- hbrcenters5494_146273 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hbrci5494_146273)


## HBO 
# hbo.den 50% (251) percentile 251
# Get distribution of sum of sizeterms and calculate percentiles of sum of sizeterms 
hbocluster251data <- hbocluster251@data

hbocluster251datasum <- hbocluster251data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbo), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1)  %>%
  arrange(cluster.sum,cluster.count)

str(hbocluster251datasum)

as.data.frame(hbocluster251datasum)

if (SAVE.INTERMEDIARIES) {
  intm.file <- file.path(INTERMEDIATE_DIR, "hbocluster251datasum.txt")
  write.table(hbocluster251datasum,intm.file,sep="\t")
}

quantile(hbocluster251datasum$cluster.sum, c(0.25,0.5,0.75))

hbocluster251datasumdistribution <- data.frame(cluster.sum=rep(hbocluster251datasum$cluster.sum, hbocluster251datasum $cluster.count))
quantile(hbocluster251datasumdistribution$cluster.sum, c(0.25,0.5,0.75))


# identify TAZs indexs 
# Unweighted percentiles
# 0.25 unweighted percentile 

hbocluster251_1433valid.cluster <- hbocluster251data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbo), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 1433)  %>%
  dplyr::select(cluster.id)

hbocenters251_1433 <- inner_join(hbocluster251data, hbocluster251_1433valid.cluster)

hboci251_1433 <- hbocenters251_1433 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hboci251_1433)

# 0.5 unweighted percentile 
hbocluster251_2209valid.cluster <- hbocluster251data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbo), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 2209)  %>%
  dplyr::select(cluster.id)

hbocenters251_2209 <- inner_join(hbocluster251data, hbocluster251_2209valid.cluster)

hboci251_2209 <- hbocenters251_2209 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hboci251_2209)

# 0.75 unweighted percentile 

hbocluster251_7268valid.cluster <- hbocluster251data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbo), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 7268)  %>%
  dplyr::select(cluster.id)

hbocenters251_7268 <- inner_join(hbocluster251data, hbocluster251_7268valid.cluster)

hboci251_7268 <- hbocenters251_7268 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hboci251_7268)


# weighted percentiles
# 0.25 weighted percentile 
hbocluster251_223078valid.cluster <- hbocluster251data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbo), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 223078)  %>%
  dplyr::select(cluster.id)

hbocenters251_223078 <- inner_join(hbocluster251data, hbocluster251_223078valid.cluster)

hboci251_223078 <- hbocenters251_223078 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hboci251_223078)

# 0.5 weighted percentile 
hbocluster251_223078valid.cluster <- hbocluster251data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbo), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 223078)  %>%
  dplyr::select(cluster.id)

hbocenters251_223078 <- inner_join(hbocluster251data, hbocluster251_223078valid.cluster)

hboci251_223078 <- hbocenters251_223078 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hboci251_223078)

# 0.75 weighted percentile 

hbocluster251_231831valid.cluster <- hbocluster251data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbo), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 231831)  %>%
  dplyr::select(cluster.id)

hbocenters251_231831 <- inner_join(hbocluster251data, hbocluster251_231831valid.cluster)

hboci251_231831 <- hbocenters251_231831 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hboci251_231831)


# hbo.den 60% (353) percentile 353
# Get distribution of sum of sizeterms and calculate percentiles of sum of sizeterms 
hbocluster353data <- hbocluster353@data

hbocluster353datasum <- hbocluster353data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbo), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1)  %>%
  arrange(cluster.sum,cluster.count)

str(hbocluster353datasum)

as.data.frame(hbocluster353datasum)

if (SAVE.INTERMEDIARIES) {
  intm.file <- file.path(INTERMEDIATE_DIR, "hbocluster353datasum.txt")
  write.table(hbocluster353datasum,intm.file,sep="\t")
}

quantile(hbocluster353datasum$cluster.sum, c(0.25,0.5,0.75))

hbocluster353datasumdistribution <- data.frame(cluster.sum=rep(hbocluster353datasum$cluster.sum, hbocluster353datasum $cluster.count))
quantile(hbocluster353datasumdistribution$cluster.sum, c(0.25,0.5,0.75))


# identify TAZs indexs 
# Unweighted percentiles
# 0.25 unweighted percentile 

hbocluster353_1395valid.cluster <- hbocluster353data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbo), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 1395)  %>%
  dplyr::select(cluster.id)

hbocenters353_1395 <- inner_join(hbocluster353data, hbocluster353_1395valid.cluster)

hboci353_1395 <- hbocenters353_1395 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hboci353_1395)

# 0.5 unweighted percentile 
hbocluster353_2439valid.cluster <- hbocluster353data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbo), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 2439)  %>%
  dplyr::select(cluster.id)

hbocenters353_2439 <- inner_join(hbocluster353data, hbocluster353_2439valid.cluster)

hboci353_2439 <- hbocenters353_2439 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hboci353_2439)

# 0.75 unweighted percentile 

hbocluster353_6592valid.cluster <- hbocluster353data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbo), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 6592)  %>%
  dplyr::select(cluster.id)

hbocenters353_6592 <- inner_join(hbocluster353data, hbocluster353_6592valid.cluster)

hboci353_6592 <- hbocenters353_6592 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hboci353_6592)


# weighted percentiles
# 0.25 weighted percentile 
hbocluster353_27553valid.cluster <- hbocluster353data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbo), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 27553)  %>%
  dplyr::select(cluster.id)

hbocenters353_27553 <- inner_join(hbocluster353data, hbocluster353_27553valid.cluster)

hboci353_27553 <- hbocenters353_27553 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hboci353_27553)

# 0.5 weighted percentile 
hbocluster353_124105valid.cluster <- hbocluster353data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbo), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 124105)  %>%
  dplyr::select(cluster.id)

hbocenters353_124105 <- inner_join(hbocluster353data, hbocluster353_124105valid.cluster)

hboci353_124105 <- hbocenters353_124105 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hboci353_124105)

# 0.75 weighted percentile 

hbocluster353_192102valid.cluster <- hbocluster353data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbo), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 192102)  %>%
  dplyr::select(cluster.id)

hbocenters353_192102 <- inner_join(hbocluster353data, hbocluster353_192102valid.cluster)

hboci353_192102 <- hbocenters353_192102 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hboci353_192102)


# hbo.den 70% (496) percentile 496
# Get distribution of sum of sizeterms and calculate percentiles of sum of sizeterms 
hbocluster496data <- hbocluster496@data

hbocluster496datasum <- hbocluster496data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbo), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1)  %>%
  arrange(cluster.sum,cluster.count)

str(hbocluster496datasum)

as.data.frame(hbocluster496datasum)

if (SAVE.INTERMEDIARIES) {
  intm.file <- file.path(INTERMEDIATE_DIR, "hbocluster496datasum.txt")
  write.table(hbocluster496datasum,intm.file,sep="\t")
}

quantile(hbocluster496datasum$cluster.sum, c(0.25,0.5,0.75))

hbocluster496datasumdistribution <- data.frame(cluster.sum=rep(hbocluster496datasum$cluster.sum, hbocluster496datasum $cluster.count))
quantile(hbocluster496datasumdistribution$cluster.sum, c(0.25,0.5,0.75))


# identify TAZs indexs 
# Unweighted percentiles
# 0.25 unweighted percentile 

hbocluster496_1874valid.cluster <- hbocluster496data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbo), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 1874)  %>%
  dplyr::select(cluster.id)

hbocenters496_1874 <- inner_join(hbocluster496data, hbocluster496_1874valid.cluster)

hboci496_1874 <- hbocenters496_1874 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hboci496_1874)

# 0.5 unweighted percentile 
hbocluster496_3532valid.cluster <- hbocluster496data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbo), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 3532)  %>%
  dplyr::select(cluster.id)

hbocenters496_3532 <- inner_join(hbocluster496data, hbocluster496_3532valid.cluster)

hboci496_3532 <- hbocenters496_3532 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hboci496_3532)

# 0.75 unweighted percentile 

hbocluster496_9055valid.cluster <- hbocluster496data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbo), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 9055)  %>%
  dplyr::select(cluster.id)

hbocenters496_9055 <- inner_join(hbocluster496data, hbocluster496_9055valid.cluster)

hboci496_9055 <- hbocenters496_9055 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hboci496_9055)


# weighted percentiles
# 0.25 weighted percentile 
hbocluster496_10398valid.cluster <- hbocluster496data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbo), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 10398)  %>%
  dplyr::select(cluster.id)

hbocenters496_10398 <- inner_join(hbocluster496data, hbocluster496_10398valid.cluster)

hboci496_10398 <- hbocenters496_10398 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hboci496_10398)

# 0.5 weighted percentile 
hbocluster496_45275valid.cluster <- hbocluster496data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbo), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 45275)  %>%
  dplyr::select(cluster.id)

hbocenters496_45275 <- inner_join(hbocluster496data, hbocluster496_45275valid.cluster)

hboci496_45275 <- hbocenters496_45275 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hboci496_45275)

# 0.75 weighted percentile 

hbocluster496_119948valid.cluster <- hbocluster496data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbo), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 119948)  %>%
  dplyr::select(cluster.id)

hbocenters496_119948 <- inner_join(hbocluster496data, hbocluster496_119948valid.cluster)

hboci496_119948 <- hbocenters496_119948 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hboci496_119948)


# hbo.den 80% (766) percentile 766
# Get distribution of sum of sizeterms and calculate percentiles of sum of sizeterms 
hbocluster766data <- hbocluster766@data

hbocluster766datasum <- hbocluster766data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbo), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1)  %>%
  arrange(cluster.sum,cluster.count)

str(hbocluster766datasum)

as.data.frame(hbocluster766datasum)

if (SAVE.INTERMEDIARIES) {
  intm.file <- file.path(INTERMEDIATE_DIR, "hbocluster766datasum.txt")
  write.table(hbocluster766datasum,intm.file,sep="\t")
}

quantile(hbocluster766datasum$cluster.sum, c(0.25,0.5,0.75))

hbocluster766datasumdistribution <- data.frame(cluster.sum=rep(hbocluster766datasum$cluster.sum, hbocluster766datasum $cluster.count))
quantile(hbocluster766datasumdistribution$cluster.sum, c(0.25,0.5,0.75))


# identify TAZs indexs 
# Unweighted percentiles
# 0.25 unweighted percentile 

hbocluster766_1348valid.cluster <- hbocluster766data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbo), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 1348)  %>%
  dplyr::select(cluster.id)

hbocenters766_1348 <- inner_join(hbocluster766data, hbocluster766_1348valid.cluster)

hboci766_1348 <- hbocenters766_1348 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hboci766_1348)

# 0.5 unweighted percentile 
hbocluster766_3399valid.cluster <- hbocluster766data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbo), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 3399)  %>%
  dplyr::select(cluster.id)

hbocenters766_3399 <- inner_join(hbocluster766data, hbocluster766_3399valid.cluster)

hboci766_3399 <- hbocenters766_3399 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hboci766_3399)

# 0.75 unweighted percentile 

hbocluster766_7554valid.cluster <- hbocluster766data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbo), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 7554)  %>%
  dplyr::select(cluster.id)

hbocenters766_7554 <- inner_join(hbocluster766data, hbocluster766_7554valid.cluster)

hboci766_7554 <- hbocenters766_7554 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hboci766_7554)


# weighted percentiles
# 0.25 weighted percentile 
hbocluster766_5321valid.cluster <- hbocluster766data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbo), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 5321)  %>%
  dplyr::select(cluster.id)

hbocenters766_5321 <- inner_join(hbocluster766data, hbocluster766_5321valid.cluster)

hboci766_5321 <- hbocenters766_5321 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hboci766_5321)

# 0.5 weighted percentile 
hbocluster766_15052valid.cluster <- hbocluster766data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbo), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 15052)  %>%
  dplyr::select(cluster.id)

hbocenters766_15052 <- inner_join(hbocluster766data, hbocluster766_15052valid.cluster)

hboci766_15052 <- hbocenters766_15052 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hboci766_15052)

# 0.75 weighted percentile 

hbocluster766_61945valid.cluster <- hbocluster766data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbo), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 61945)  %>%
  dplyr::select(cluster.id)

hbocenters766_61945 <- inner_join(hbocluster766data, hbocluster766_61945valid.cluster)

hboci766_61945 <- hbocenters766_61945 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hboci766_61945)


# hbo.den 90% (1473) percentile 1473
# Get distribution of sum of sizeterms and calculate percentiles of sum of sizeterms 
hbocluster1473data <- hbocluster1473@data

hbocluster1473datasum <- hbocluster1473data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbo), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1)  %>%
  arrange(cluster.sum,cluster.count)

str(hbocluster1473datasum)

as.data.frame(hbocluster1473datasum)

if (SAVE.INTERMEDIARIES) {
  intm.file <- file.path(INTERMEDIATE_DIR, "hbocluster1473datasum.txt")
  write.table(hbocluster1473datasum,intm.file,sep="\t")
}

quantile(hbocluster1473datasum$cluster.sum, c(0.25,0.5,0.75))

hbocluster1473datasumdistribution <- data.frame(cluster.sum=rep(hbocluster1473datasum$cluster.sum, hbocluster1473datasum $cluster.count))
quantile(hbocluster1473datasumdistribution$cluster.sum, c(0.25,0.5,0.75))


# identify TAZs indexs 
# Unweighted percentiles
# 0.25 unweighted percentile 

hbocluster1473_2089valid.cluster <- hbocluster1473data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbo), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 2089)  %>%
  dplyr::select(cluster.id)

hbocenters1473_2089 <- inner_join(hbocluster1473data, hbocluster1473_2089valid.cluster)

hboci1473_2089 <- hbocenters1473_2089 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hboci1473_2089)

# 0.5 unweighted percentile 
hbocluster1473_3390valid.cluster <- hbocluster1473data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbo), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 3390)  %>%
  dplyr::select(cluster.id)

hbocenters1473_3390 <- inner_join(hbocluster1473data, hbocluster1473_3390valid.cluster)

hboci1473_3390 <- hbocenters1473_3390 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hboci1473_3390)

# 0.75 unweighted percentile 

hbocluster1473_6886valid.cluster <- hbocluster1473data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbo), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 6886)  %>%
  dplyr::select(cluster.id)

hbocenters1473_6886 <- inner_join(hbocluster1473data, hbocluster1473_6886valid.cluster)

hboci1473_6886 <- hbocenters1473_6886 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hboci1473_6886)


# weighted percentiles
# 0.25 weighted percentile 
hbocluster1473_3630valid.cluster <- hbocluster1473data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbo), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 3630)  %>%
  dplyr::select(cluster.id)

hbocenters1473_3630 <- inner_join(hbocluster1473data, hbocluster1473_3630valid.cluster)

hboci1473_3630 <- hbocenters1473_3630 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hboci1473_3630)

# 0.5 weighted percentile 
hbocluster1473_7801valid.cluster <- hbocluster1473data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbo), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 7801)  %>%
  dplyr::select(cluster.id)

hbocenters1473_7801 <- inner_join(hbocluster1473data, hbocluster1473_7801valid.cluster)

hboci1473_7801 <- hbocenters1473_7801 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hboci1473_7801)

# 0.75 weighted percentile 

hbocluster1473_41579valid.cluster <- hbocluster1473data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbo), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 41579)  %>%
  dplyr::select(cluster.id)

hbocenters1473_41579 <- inner_join(hbocluster1473data, hbocluster1473_41579valid.cluster)

hboci1473_41579 <- hbocenters1473_41579 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hboci1473_41579)


# hbo.den 95% (2553) percentile 2553
# Get distribution of sum of sizeterms and calculate percentiles of sum of sizeterms 
hbocluster2553data <- hbocluster2553@data

hbocluster2553datasum <- hbocluster2553data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbo), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1)  %>%
  arrange(cluster.sum,cluster.count)

str(hbocluster2553datasum)

as.data.frame(hbocluster2553datasum)

if (SAVE.INTERMEDIARIES) {
  intm.file <- file.path(INTERMEDIATE_DIR, "hbocluster2553datasum.txt")
  write.table(hbocluster2553datasum,intm.file,sep="\t")
}

quantile(hbocluster2553datasum$cluster.sum, c(0.25,0.5,0.75))

hbocluster2553datasumdistribution <- data.frame(cluster.sum=rep(hbocluster2553datasum$cluster.sum, hbocluster2553datasum $cluster.count))
quantile(hbocluster2553datasumdistribution$cluster.sum, c(0.25,0.5,0.75))


# identify TAZs indexs 
# Unweighted percentiles
# 0.25 unweighted percentile 

hbocluster2553_2768valid.cluster <- hbocluster2553data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbo), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 2768)  %>%
  dplyr::select(cluster.id)

hbocenters2553_2768 <- inner_join(hbocluster2553data, hbocluster2553_2768valid.cluster)

hboci2553_2768 <- hbocenters2553_2768 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hboci2553_2768)

# 0.5 unweighted percentile 
hbocluster2553_3684valid.cluster <- hbocluster2553data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbo), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 3684)  %>%
  dplyr::select(cluster.id)

hbocenters2553_3684 <- inner_join(hbocluster2553data, hbocluster2553_3684valid.cluster)

hboci2553_3684 <- hbocenters2553_3684 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hboci2553_3684)

# 0.75 unweighted percentile 

hbocluster2553_6601valid.cluster <- hbocluster2553data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbo), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 6601)  %>%
  dplyr::select(cluster.id)

hbocenters2553_6601 <- inner_join(hbocluster2553data, hbocluster2553_6601valid.cluster)

hboci2553_6601 <- hbocenters2553_6601 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hboci2553_6601)


# weighted percentiles
# 0.25 weighted percentile 
hbocluster2553_3816valid.cluster <- hbocluster2553data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbo), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 3816)  %>%
  dplyr::select(cluster.id)

hbocenters2553_3816 <- inner_join(hbocluster2553data, hbocluster2553_3816valid.cluster)

hboci2553_3816 <- hbocenters2553_3816 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hboci2553_3816)

# 0.5 weighted percentile 
hbocluster2553_18509valid.cluster <- hbocluster2553data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbo), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 18509)  %>%
  dplyr::select(cluster.id)

hbocenters2553_18509 <- inner_join(hbocluster2553data, hbocluster2553_18509valid.cluster)

hboci2553_18509 <- hbocenters2553_18509 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hboci2553_18509)

# 0.75 weighted percentile 

hbocluster2553_69812valid.cluster <- hbocluster2553data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(st.hbo), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 69812)  %>%
  dplyr::select(cluster.id)

hbocenters2553_69812 <- inner_join(hbocluster2553data, hbocluster2553_69812valid.cluster)

hboci2553_69812 <- hbocenters2553_69812 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hboci2553_69812)


## HBW

# forget to save scripts for hbw.den 50% (170) 

# hbw.den 60% (316) percentile 316
# Get distribution of sum of sizeterms and calculate percentiles of sum of sizeterms 
hbwcluster316data <- hbwcluster316@data

hbwcluster316datasum <- hbwcluster316data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(tot.emp), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1)  %>%
  arrange(cluster.sum,cluster.count)

str(hbwcluster316datasum)

as.data.frame(hbwcluster316datasum)

if (SAVE.INTERMEDIARIES) {
  intm.file <- file.path(INTERMEDIATE_DIR, "hbwcluster316datasum.txt")
  write.table(hbwcluster316datasum,intm.file,sep="\t")
}

quantile(hbwcluster316datasum$cluster.sum, c(0.25,0.5,0.75))

hbwcluster316datasumdistribution <- data.frame(cluster.sum=rep(hbwcluster316datasum$cluster.sum, hbwcluster316datasum $cluster.count))
quantile(hbwcluster316datasumdistribution$cluster.sum, c(0.25,0.5,0.75))


# identify TAZs indexs 
# Unweighted percentiles
# 0.25 unweighted percentile 

hbwcluster316_1550valid.cluster <- hbwcluster316data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(tot.emp), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 1550)  %>%
  dplyr::select(cluster.id)

hbwcenters316_1550 <- inner_join(hbwcluster316data, hbwcluster316_1550valid.cluster)

hbwci316_1550 <- hbwcenters316_1550 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hbwci316_1550)

# 0.5 unweighted percentile 
hbwcluster316_3903valid.cluster <- hbwcluster316data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(tot.emp), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 3903)  %>%
  dplyr::select(cluster.id)

hbwcenters316_3903 <- inner_join(hbwcluster316data, hbwcluster316_3903valid.cluster)

hbwci316_3903 <- hbwcenters316_3903 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hbwci316_3903)

# 0.75 unweighted percentile 

hbwcluster316_14188valid.cluster <- hbwcluster316data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(tot.emp), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 14188)  %>%
  dplyr::select(cluster.id)

hbwcenters316_14188 <- inner_join(hbwcluster316data, hbwcluster316_14188valid.cluster)

hbwci316_14188 <- hbwcenters316_14188 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hbwci316_14188)


# weighted percentiles
# 0.25 weighted percentile 
hbwcluster316_26487valid.cluster <- hbwcluster316data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(tot.emp), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 26487)  %>%
  dplyr::select(cluster.id)

hbwcenters316_26487 <- inner_join(hbwcluster316data, hbwcluster316_26487valid.cluster)

hbwci316_26487 <- hbwcenters316_26487 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hbwci316_26487)

# 0.5 weighted percentile 
hbwcluster316_208432valid.cluster <- hbwcluster316data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(tot.emp), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 208432)  %>%
  dplyr::select(cluster.id)

hbwcenters316_208432 <- inner_join(hbwcluster316data, hbwcluster316_208432valid.cluster)

hbwci316_208432 <- hbwcenters316_208432 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hbwci316_208432)

# 0.75 weighted percentile 

hbwcluster316_290861valid.cluster <- hbwcluster316data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(tot.emp), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 290861)  %>%
  dplyr::select(cluster.id)

hbwcenters316_290861 <- inner_join(hbwcluster316data, hbwcluster316_290861valid.cluster)

hbwci316_290861 <- hbwcenters316_290861 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hbwci316_290861)

# hbw.den 70% (529) percentile 
# Get distribution of sum of sizeterms and calculate percentiles of sum of sizeterms 
hbwcluster529data <- hbwcluster529@data

hbwcluster529datasum <- hbwcluster529data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(tot.emp), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1)  %>%
  arrange(cluster.sum,cluster.count)

str(hbwcluster529datasum)

as.data.frame(hbwcluster529datasum)

if (SAVE.INTERMEDIARIES) {
  intm.file <- file.path(INTERMEDIATE_DIR, "hbwcluster529datasum.txt")
  write.table(hbwcluster529datasum,intm.file,sep="\t")
}

quantile(hbwcluster529datasum$cluster.sum, c(0.25,0.5,0.75))

hbwcluster529datasumdistribution <- data.frame(cluster.sum=rep(hbwcluster529datasum$cluster.sum, hbwcluster529datasum $cluster.count))
quantile(hbwcluster529datasumdistribution$cluster.sum, c(0.25,0.5,0.75))


# identify TAZs indexs 
# Unweighted percentiles
# 0.25 unweighted percentile 

hbwcluster529_2534valid.cluster <- hbwcluster529data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(tot.emp), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 2534)  %>%
  dplyr::select(cluster.id)

hbwcenters529_2534 <- inner_join(hbwcluster529data, hbwcluster529_2534valid.cluster)

hbwci529_2534 <- hbwcenters529_2534 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hbwci529_2534)

# 0.5 unweighted percentile 
hbwcluster529_4819valid.cluster <- hbwcluster529data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(tot.emp), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 4819)  %>%
  dplyr::select(cluster.id)

hbwcenters529_4819 <- inner_join(hbwcluster529data, hbwcluster529_4819valid.cluster)

hbwci529_4819 <- hbwcenters529_4819 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hbwci529_4819)

# 0.75 unweighted percentile 

hbwcluster529_9672valid.cluster <- hbwcluster529data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(tot.emp), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 9672)  %>%
  dplyr::select(cluster.id)

hbwcenters529_9672 <- inner_join(hbwcluster529data, hbwcluster529_9672valid.cluster)

hbwci529_9672 <- hbwcenters529_9672 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hbwci529_9672)


# weighted percentiles
# 0.25 weighted percentile 
hbwcluster529_11913valid.cluster <- hbwcluster529data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(tot.emp), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 11913)  %>%
  dplyr::select(cluster.id)

hbwcenters529_11913 <- inner_join(hbwcluster529data, hbwcluster529_11913valid.cluster)

hbwci529_11913 <- hbwcenters529_11913 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hbwci529_11913)

# 0.5 weighted percentile 
hbwcluster529_139756valid.cluster <- hbwcluster529data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(tot.emp), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 139756)  %>%
  dplyr::select(cluster.id)

hbwcenters529_139756 <- inner_join(hbwcluster529data, hbwcluster529_139756valid.cluster)

hbwci529_139756 <- hbwcenters529_139756 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hbwci529_139756)

# 0.75 weighted percentile 

hbwcluster529_186005valid.cluster <- hbwcluster529data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(tot.emp), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 186005)  %>%
  dplyr::select(cluster.id)

hbwcenters529_186005 <- inner_join(hbwcluster529data, hbwcluster529_186005valid.cluster)

hbwci529_186005 <- hbwcenters529_186005 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hbwci529_186005)



# hbw.den 80% (1010) percentile 
# Get distribution of sum of sizeterms and calculate percentiles of sum of sizeterms 
hbwcluster1010data <- hbwcluster1010@data

hbwcluster1010datasum <- hbwcluster1010data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(tot.emp), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1)  %>%
  arrange(cluster.sum,cluster.count)

str(hbwcluster1010datasum)

as.data.frame(hbwcluster1010datasum)

if (SAVE.INTERMEDIARIES) {
  intm.file <- file.path(INTERMEDIATE_DIR, "hbwcluster1010datasum.txt")
  write.table(hbwcluster1010datasum,intm.file,sep="\t")
}

quantile(hbwcluster1010datasum$cluster.sum, c(0.25,0.5,0.75))

hbwcluster1010datasumdistribution <- data.frame(cluster.sum=rep(hbwcluster1010datasum$cluster.sum, hbwcluster1010datasum $cluster.count))
quantile(hbwcluster1010datasumdistribution$cluster.sum, c(0.25,0.5,0.75))


# identify TAZs indexs 
# Unweighted percentiles
# 0.25 unweighted percentile 

hbwcluster1010_2840valid.cluster <- hbwcluster1010data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(tot.emp), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 2840)  %>%
  dplyr::select(cluster.id)

hbwcenters1010_2840 <- inner_join(hbwcluster1010data, hbwcluster1010_2840valid.cluster)

hbwci1010_2840 <- hbwcenters1010_2840 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hbwci1010_2840)

# 0.5 unweighted percentile 
hbwcluster1010_5165valid.cluster <- hbwcluster1010data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(tot.emp), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 5165)  %>%
  dplyr::select(cluster.id)

hbwcenters1010_5165 <- inner_join(hbwcluster1010data, hbwcluster1010_5165valid.cluster)

hbwci1010_5165 <- hbwcenters1010_5165 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hbwci1010_5165)

# 0.75 unweighted percentile 

hbwcluster1010_12083valid.cluster <- hbwcluster1010data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(tot.emp), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 12083)  %>%
  dplyr::select(cluster.id)

hbwcenters1010_12083 <- inner_join(hbwcluster1010data, hbwcluster1010_12083valid.cluster)

hbwci1010_12083 <- hbwcenters1010_12083 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hbwci1010_12083)


# weighted percentiles
# 0.25 weighted percentile 
hbwcluster1010_8816valid.cluster <- hbwcluster1010data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(tot.emp), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 8816)  %>%
  dplyr::select(cluster.id)

hbwcenters1010_8816 <- inner_join(hbwcluster1010data, hbwcluster1010_8816valid.cluster)

hbwci1010_8816 <- hbwcenters1010_8816 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hbwci1010_8816)

# 0.5 weighted percentile 
hbwcluster1010_42071valid.cluster <- hbwcluster1010data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(tot.emp), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 42071)  %>%
  dplyr::select(cluster.id)

hbwcenters1010_42071 <- inner_join(hbwcluster1010data, hbwcluster1010_42071valid.cluster)

hbwci1010_42071 <- hbwcenters1010_42071 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hbwci1010_42071)

# 0.75 weighted percentile 

hbwcluster1010_98691valid.cluster <- hbwcluster1010data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(tot.emp), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 98691)  %>%
  dplyr::select(cluster.id)

hbwcenters1010_98691 <- inner_join(hbwcluster1010data, hbwcluster1010_98691valid.cluster)

hbwci1010_98691 <- hbwcenters1010_98691 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hbwci1010_98691)


# hbw.den 95% (4138) percentile 
# Get distribution of sum of sizeterms and calculate percentiles of sum of sizeterms 
hbwcluster4138data <- hbwcluster4138@data

hbwcluster4138datasum <- hbwcluster4138data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(tot.emp), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1)  %>%
  arrange(cluster.sum,cluster.count)

str(hbwcluster4138datasum)

as.data.frame(hbwcluster4138datasum)

if (SAVE.INTERMEDIARIES) {
  intm.file <- file.path(INTERMEDIATE_DIR, "hbwcluster4138datasum.txt")
  write.table(hbwcluster4138datasum,intm.file,sep="\t")
}

quantile(hbwcluster4138datasum$cluster.sum, c(0.25,0.5,0.75))

hbwcluster4138datasumdistribution <- data.frame(cluster.sum=rep(hbwcluster4138datasum$cluster.sum, hbwcluster4138datasum $cluster.count))
quantile(hbwcluster4138datasumdistribution$cluster.sum, c(0.25,0.5,0.75))


# identify TAZs indexs 
# Unweighted percentiles
# 0.25 unweighted percentile 

hbwcluster4138_5171valid.cluster <- hbwcluster4138data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(tot.emp), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 5171)  %>%
  dplyr::select(cluster.id)

hbwcenters4138_5171 <- inner_join(hbwcluster4138data, hbwcluster4138_5171valid.cluster)

hbwci4138_5171 <- hbwcenters4138_5171 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hbwci4138_5171)

# 0.5 unweighted percentile 
hbwcluster4138_7805valid.cluster <- hbwcluster4138data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(tot.emp), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 7805)  %>%
  dplyr::select(cluster.id)

hbwcenters4138_7805 <- inner_join(hbwcluster4138data, hbwcluster4138_7805valid.cluster)

hbwci4138_7805 <- hbwcenters4138_7805 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hbwci4138_7805)

# 0.75 unweighted percentile 

hbwcluster4138_9899valid.cluster <- hbwcluster4138data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(tot.emp), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 9899)  %>%
  dplyr::select(cluster.id)

hbwcenters4138_9899 <- inner_join(hbwcluster4138data, hbwcluster4138_9899valid.cluster)

hbwci4138_9899 <- hbwcenters4138_9899 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hbwci4138_9899)


# weighted percentiles
# 0.25 weighted percentile 
hbwcluster4138_7250valid.cluster <- hbwcluster4138data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(tot.emp), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 7250)  %>%
  dplyr::select(cluster.id)

hbwcenters4138_7250 <- inner_join(hbwcluster4138data, hbwcluster4138_7250valid.cluster)

hbwci4138_7250 <- hbwcenters4138_7250 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hbwci4138_7250)

# 0.5 weighted percentile 
hbwcluster4138_40491valid.cluster <- hbwcluster4138data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(tot.emp), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 40491)  %>%
  dplyr::select(cluster.id)

hbwcenters4138_40491 <- inner_join(hbwcluster4138data, hbwcluster4138_40491valid.cluster)

hbwci4138_40491 <- hbwcenters4138_40491 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hbwci4138_40491)

# 0.75 weighted percentile 

hbwcluster4138_117180valid.cluster <- hbwcluster4138data %>% 
  group_by(cluster.id) %>%
  summarise(cluster.sum=sum(tot.emp), cluster.count=n()) %>%
  filter(cluster.id!=0 & cluster.count > 1&cluster.sum >= 117180)  %>%
  dplyr::select(cluster.id)

hbwcenters4138_117180 <- inner_join(hbwcluster4138data, hbwcluster4138_117180valid.cluster)

hbwci4138_117180 <- hbwcenters4138_117180 %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)

nrow(hbwci4138_117180)


