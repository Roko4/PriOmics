#' Plot basic heatmap of selected features (based on package 'pheatmap')
#'
#' @param adj result from get_adj_mat()
#' @param sel selected features, separated by "|" (e.g, "Feat_X|Feat_Y|Feat_Z")
#' or use regex to selected features from feature list
#'
#' @importFrom grDevices colorRampPalette
#'
#' @export
#'
plot_heat_selection <- function(adj, sel){

  lookup_ID <- grep(sel, colnames(adj))
  
  adj_sub <- adj[, lookup_ID, drop = FALSE]
  dim(adj_sub)
  
  remain_list <- list()
  for (i in 1:ncol(adj_sub)){
    # i = 1
    remain_list[[i]] <- which(sapply(adj_sub[, i], function(x) x != 0))
  }
  remain_list <- c(unlist(remain_list), lookup_ID)
  remain_list <- remain_list[!duplicated(remain_list)]
  
  adj_sub <- adj[remain_list, remain_list]
  dim(adj_sub)
  
  mat <- adj[remain_list, lookup_ID, drop = FALSE]
  mat <- mat[, order(colnames(mat)), drop = FALSE]
  
  right_order <- rownames(adj)[which(rownames(adj) %in% rownames(mat))]
  mat <- mat[right_order, , drop = FALSE]
  
  mat <- round(mat, 3)
  
  rg <- max(abs(mat))
  breaks0  <- seq(-rg, rg, length.out = 1000)
  
  heat <- pheatmap::pheatmap(mat, breaks = breaks0,
                             color = colorRampPalette(c("#e70000", "white", "#0074E8"))(1000),
                             cluster_rows = F, cluster_cols = F, na_col = "grey50",
                             fontsize_col = 8, # 10 default
                             fontsize_row = 8, # 10 default
                             border_color = NA,
                             legend = F,
                             angle_col = 315,
                             display_numbers = round(mat, 3),
                             # number_format = "%.2f", #"%.1e"
                             # main = p_title,
                             silent = F)
  return(heat)
}
