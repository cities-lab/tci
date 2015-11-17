dummy_basket <- function(Zi) {
  Zi.n <- length(Zi)
  basket <- matrix(TRUE, nrow = Zi.n, ncol = Zi.n, dimnames = list(Zi, Zi))
  basket
}

cluster_basket <- function(TAZ.shp, pr, cutoffs, dist=1.0) {
  ci <- identify_centers(TAZ.shp, "sizeterm.density", 
                         dist=1.0, sum.col="sizeterm.total", 
                         cutoffs=split(cutoffs[, pr], rownames(cutoffs)))
  ci <- ci %>% dplyr::select(TAZ, center.id=cluster.id) %>% arrange(TAZ)
  Zi <- as.character(TAZ.shp$TAZ)
  Zi.n <- length(Zi)
  basket <- matrix(FALSE, nrow = Zi.n, ncol = Zi.n, dimnames = list(Zi, Zi))
  basket[, ci] <- TRUE
  basket
}
