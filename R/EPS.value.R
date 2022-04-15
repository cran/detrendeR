#' @title  Computes general statistics on ring-width series.
#' @description Compute general statistics on a data.frame of ring-width series.
#' @details This function is a changed version of the Andrew Bunn's rwi.stats function from the dplR package (Bunn, 2008).
#' @usage EPS.value(rwl, stc = c(5,2,1))
#' @param rwl a data.frame with ring-width values
#' @param stc a vector of three integers giving the Site-Tree-Core mask. Defaults to c(5,2,1) and these three values should sum to eigth. The stc[1] gives the number of characters used to define the site code, stc[2] defines the tree mask and the core ids is given by the last element of the vector stc[3].
#' @returns A \code{"data.frame"} with the following values:
#'   \item{start}{the first year}
#'   \item{end}{the last year}
#'   \item{tree}{the number of trees}
#'   \item{core}{the number of cores}
#'   \item{n.tot}{the number of unique combinations of the input series (i.e., 1/2*n*[n-1])}
#'   \item{n.wt}{the total number of unique combinations of the within-tree series}
#'   \item{n.bt}{the total number of unique combinations of the between-tree series}
#'   \item{r.tot}{the mean of all correlation between different cores}
#'   \item{r.wt}{the mean of correlations between series from the same tree over all trees}
#'   \item{r.bt}{the mean interseries correlation between all series from different trees}
#'   \item{c.eff}{the effective number of cores}
#'   \item{r.eff}{the effective signal calculated as [rbar.bt / (rbar.wt + (1-rbar.wt) / c.eff)]}
#'   \item{eps}{the expressed population signal}
#'
#' @references
#' Bunn, A. 2008. A dendrochronology program library in R (dplR). Dendrochronologia 26:115-124.
#'
#' Cook, E.R., Kairiukstis, L.A. 1990. Methods of Dendrochronology: applications in the environmental sciences.  Kluwer Academic Publishers.
#'
#' @examples
#' \dontshow{
#'  set.seed(1)
#'  rwl <- matrix(sample(1:1000,1000), ncol = 10, dimnames = list(c(1:100),1:10))
#'  rwl[49,] <- 100
#'  rwl[50,] <- 1
#'  rwl[51,] <- 100
#'  rwl <- as.rwl(rwl)
#'  EPS.value(rwl, stc = c(0, 8, 0))
#'  }
#' \dontrun{
#' data(co021, package = "dplR")
#' years <- as.integer(rownames(co021))
#' co021.subset <- subset(co021, subset = years >= 1900 & years <= 1950)
#' ## remove the following series "645232","646107" and "646118"
#' co021.subset <- co021.subset[, !colnames(co021) %in% c("645232", "646107", "646118")]
#' EPS.value(co021.subset, stc = c(0, 8, 0))
#' }
#' @export
EPS.value <- function(rwl, stc = c(5, 2, 1)) {
  if (sum(stc) != 8) {
    stop("Site-Tree-Core mask does not sum to 8")
  }
  tree.mask <- 8 - stc[3]
  colnames(rwl) <- substr(colnames(rwl), 1, tree.mask)
  tree.names <- colnames(rwl)
  n.cores <- dim(rwl)[2]
  n.trees <- length(unique(colnames(rwl)))
  n.cores.tree <- data.frame(table(tree.names))$Freq
  r.mat <- cor(rwl)
  n.tot <- 0.5 * n.cores * (n.cores - 1)
  rbar.tot <- mean(r.mat[upper.tri(r.mat)], na.rm = TRUE)
  within.tree <-
    matrix(rep(tree.names, n.cores), ncol = n.cores) ==
      matrix(rep(tree.names, n.cores),
        ncol = n.cores,
        byrow = T
      )
  n.wt <- sum(within.tree[upper.tri(within.tree)], na.rm = TRUE)
  {
    if (n.wt == 0) {
      rbar.wt <- 0
    } else {
      within.tree.r <- within.tree * r.mat
      within.tree.r <- replace(within.tree.r, within.tree ==
        FALSE, NA)
      rbar.wt <- mean(within.tree.r[upper.tri(within.tree.r)],
        na.rm = TRUE
      )
    }
  }
  n.bt <- n.tot - n.wt
  rbar.bt <- 1 / n.bt * (rbar.tot * n.tot - rbar.wt * n.wt)
  c.eff <- (1 / n.trees * sum(1 / n.cores.tree))^-1
  rbar.eff <- rbar.bt / (rbar.wt + (1 - rbar.wt) / c.eff)
  n <- n.trees
  eps <- (n * rbar.eff) / ((n * rbar.eff) + (1 - rbar.eff))
  start <- FirstYear(rwl)
  end <- LastYear(rwl)
  compos.stats <- data.frame(
    start,
    end,
    n.trees,
    n.cores,
    n.tot,
    n.wt,
    n.bt,
    rbar.tot,
    rbar.wt,
    rbar.bt,
    c.eff,
    rbar.eff,
    eps
  )
  compos.stats <- round(compos.stats, 3)
  return(compos.stats)
}
