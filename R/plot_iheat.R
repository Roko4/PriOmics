#' Plot interactive heatmap of adjacency matrix (based on package'heatmaply')
#'
#' For large matrices zoom into the plot 
#'
#' @param x result from get_adj_mat()
#' @param upper_only logical, If TRUE, only the upper half of the matrix is
#' plotted. (default = FALSE)
#' @param dark_colors logical, If TRUE, a darker color palette
#' is used.  (default = FALSE)
#' @param save_file logical, If TRUE, the heatmap is saved as html file at
#' current working directory (default = FALSE); no visible output
#' Saving (and opening) files, may take a while:
#' p=100 -> ~5mb (5s)
#' p=500 -> ~25mb (30s)
#' p=1000 -> ~90mb (2min)
#' p=2000 -> ~330mb (7min) not recommended! (R crashes, but file will be stored
#' anyway)
#' 
#' @importFrom grDevices colorRampPalette
#'
#' @export
#'
plot_iheat <- function(x, upper_only = FALSE,
                       dark_colors = FALSE, save_file = FALSE){
  
  if (dark_colors){
    heat_colors <- c(colorRampPalette(c("#700007", "#ffcccc"))(10000),
                      colorRampPalette(c("#b3d9ff", "#001370"))(10000))

  } else {
    heat_colors <- c(colorRampPalette(c("#e70000", "#ffeeee"))(10000),
                     colorRampPalette(c("#F3F9FF", "#0074e8"))(10000))
  }
  
  x_mat <- x

  # upper_only
  if(upper_only == TRUE){x_mat[lower.tri(x_mat)] <- 0}
  
  x_mat[x_mat == 0] <- NA
  
  if (save_file == FALSE){
    heat <- heatmaply::heatmaply(x_mat,
                                 Rowv = F,
                                 Colv = F,
                                 cexRow = 0.75,
                                 cexCol = 0.75,
                                 limits = c(-max(abs(c(min(x_mat, na.rm = T), max(x_mat, na.rm = T)))),
                                            max(abs(c(min(x_mat, na.rm = T), max(x_mat, na.rm = T))))),
                                 seriate = "none",
                                 colors = heat_colors,
                                 grid_color = "grey95",
                                 grid_size = 0.001,
                                 na.value = "white",
                                 margins = c(0, 0, 25, 0)
                                 
    )
    return(heat)
    
  } else {
    heat <- heatmaply::heatmaply(x_mat,
                                 Rowv = F,
                                 Colv = F,
                                 cexRow = 0.75,
                                 cexCol = 0.75,
                                 limits = c(-max(abs(c(min(x_mat, na.rm = T), max(x_mat, na.rm = T)))),
                                            max(abs(c(min(x_mat, na.rm = T), max(x_mat, na.rm = T))))),
                                 seriate = "none",
                                 colors = heat_colors,
                                 grid_color = "grey95",
                                 grid_size = 0.001,
                                 na.value = "white",
                                 margins = c(0, 0, 25, 0),
                                 file = "./heatmap.html"
                                 
    )
  }
}