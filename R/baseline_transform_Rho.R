#' Perform baseline transformation on Rho
#'
#' @param Rho ...
#' @param Y_levels ...
#'
#' @export
baseline_transform_Rho <- function(Rho, Y_levels){

  Rho0 <- Rho
  q <- sum(Y_levels)
  levels_cumsum2 <- cumsum(Y_levels)
  levels_cumsum1 <- (cumsum(Y_levels)-Y_levels)+1

  for (i in 1:length(levels_cumsum1)){
    Rho_sub <- Rho[levels_cumsum1[i]:levels_cumsum2[i], ]
    Rho_sub0 <- Rho_sub[1, ]

    for (j in 1:nrow(Rho_sub)){
      Rho_sub[j, ] <- Rho_sub[j, ] - Rho_sub0
    }
    Rho0[levels_cumsum1[i]:levels_cumsum2[i], ] <- Rho_sub
  }
  return(Rho0)
}

################################################################################
# def baseline_transform_Rho(Rho, levels_Y):
#   Rho0 = Rho.copy()
#   q = np.sum(levels_Y)
#   levels_cumsum = np.cumsum([0] + levels_Y)
#
#   for i in range(len(levels_cumsum)-1):
#     Rho_sub = Rho[levels_cumsum[i]:levels_cumsum[i+1],:]
#     Rho_sub0 = Rho_sub[0,:].copy()
#     for j in range(Rho_sub.shape[0]):
#       Rho_sub[j,:] = Rho_sub[j,:] - Rho_sub0
#     Rho0[levels_cumsum[i]:levels_cumsum[i+1],:] = Rho_sub
#   return(Rho0)
