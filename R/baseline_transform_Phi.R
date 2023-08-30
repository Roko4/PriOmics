#' Perform baseline transformation on Phi
#'
#' @param Phi ...
#' @param Y_levels ...
#'
#' @export
baseline_transform_Phi <- function(Phi, Y_levels){

  Phi0 <- Phi
  q <- sum(Y_levels)
  levels_cumsum2 <- cumsum(Y_levels)
  levels_cumsum1 <- (cumsum(Y_levels)-Y_levels)+1

  for (i in 1:length(levels_cumsum1)){
    for (j in i:length(levels_cumsum1)){
      Phi_sub <- Phi[levels_cumsum1[i]:levels_cumsum2[i],
                     levels_cumsum1[j]:levels_cumsum2[j]]
      Phi_sub0 <- Phi_sub[1, ]
      for (k in 1:nrow(Phi_sub)){
        Phi_sub[k, ] <- Phi_sub[k, ] - Phi_sub0
      }
      Phi_sub1 <- Phi_sub[, 1]
      for (l in 1:ncol(Phi_sub)){
        Phi_sub[, l] <- Phi_sub[, l] - Phi_sub1
      }
      Phi0[levels_cumsum1[i]:levels_cumsum2[i], levels_cumsum1[j]:levels_cumsum2[j]] <- Phi_sub
      Phi0[levels_cumsum1[j]:levels_cumsum2[j], levels_cumsum1[i]:levels_cumsum2[i]] <- t(Phi_sub)
    }
  }
  return(Phi0)
}
