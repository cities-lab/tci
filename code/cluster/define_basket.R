dummy_basket <- function(args) {
  #a dummy basket that includes all TAZs
  Zi <- args$Zi
  Zi.n <- length(Zi)
  basket <- matrix(TRUE, nrow = Zi.n, ncol = Zi.n, dimnames = list(Zi, Zi))
  basket
}

cluster_basket <- function(args) {
  taz.shp <- args$taz 
  pr <- args$pr
  cutoffs <- args$cutoffs
  Zi <- args$Zi
  dist <- ifelse(exists('dist', where=args), args$dist, 1.0)
  print("Use cluster-based basket")
  
  ci <- identify_centers(taz.shp, paste0("den.", pr),
                         dist=dist, sum.col=paste0("st.", pr), 
                         cutoffs=split(cutoffs[, pr], rownames(cutoffs)))
  ci <- ci %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)
  ci.ch <- as.character(ci[, "TAZ"])
  Zi <- as.character(taz.shp$TAZ)
  Zi.n <- length(Zi)
  basket <- matrix(FALSE, nrow = Zi.n, ncol = Zi.n, dimnames = list(Zi, Zi))
  basket[, ci.ch] <- TRUE
  basket
}

load_cluster_basket <- function(args) {
  centers.file <- args$centers.file
  pr <- args$pr
  Zi <- args$Zi
  
  print(paste("Load cluster-based basket from", centers.file))
  obj.name <- paste0(pr, 'ci')
  if (!exists(obj.name)) load(centers.file)
  obj.data <- get(obj.name)
  ci <- obj.data[, "TAZ"]
  ci.ch <- as.character(ci)
  Zi.n <- length(Zi)
  basket <- matrix(FALSE, nrow = Zi.n, ncol = Zi.n, dimnames = list(Zi, Zi))
  basket[, ci.ch] <- TRUE
  basket
}
