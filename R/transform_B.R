#' Internal function
#'
#' @param B ...
#' @param groups_X ...
#' @param prior_X ...
transform_B <- function(B, groups_X, prior_X){
  p <- dim(B)[2]
  for(i in 1:length(groups_X)){
    IDs_B <- groups_X[[i]]
    prior <- prior_X[[i]]
    if (prior == "A_exp" || prior == "A"){
      B[IDs_B, IDs_B] <- 0
    }
  }
  return(B)
}
