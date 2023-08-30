#' Plot basic heatmap of adjacency matrix (based on package 'pheatmap')
#'
#' Only useful for limited number of features (p < 50)
#'
#' @param x result from get_adj_mat()
#'
#' @importFrom grDevices colorRampPalette
#'
#' @export
#'
plot_heat <- function(x, covtocor = FALSE){

  cov_to_cor <- function(x){
    p <- ncol(x)
    A <- sqrt((diag(0, p) + diag(x)))
    r <- -x/A/t(A)
    return(r)
  }

  x <- as.matrix(x)

  if(covtocor){
    x <- cov_to_cor(-x)
    diag(x) <- -diag(x)
  }

  x <- round(x, 3)
  rg <- max(abs(x))
  breaks0 = seq(-rg, rg, length.out = 1000)
  heat <- pheatmap::pheatmap(x, breaks = breaks0,
                             color = colorRampPalette(c("#e70000", "white", "#0074E8"))(1000),
                             cluster_rows = F, cluster_cols = F, na_col = "grey50",
                             fontsize_col = 8, # 10 default
                             fontsize_row = 8, # 10 default
                             border_color = NA,
                             legend = F,
                             angle_col = 315,
                             display_numbers = x,
                             # number_format = "%.2f", #"%.1e"
                             # main = p_title,
                             silent = F)
  return(heat)
}