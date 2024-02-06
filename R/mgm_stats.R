#' Get information about model fits
#'
#' @param fit calculated with fit_MGM() or fit_MGM_par()
#'
#' @export
mgm_stats <- function(fit){

  res <- as.data.frame(matrix(unlist(fit$EBIC), ncol = 4, byrow = TRUE))
  colnames(res) <- names(fit$EBIC[[1]])
  res$lamba <-  fit$lambda
  res$iter <- fit$steps

  res$BIC_min <- rep(FALSE, nrow(res))
  res$BIC_min[which.min(res$BIC)] <- TRUE

  res$EBIC_min <- rep(FALSE, nrow(res))
  res$EBIC_min[which.min(res$EBIC)] <- TRUE

  # if (!is.null(nrow(res))){
  #   if (which.min(res[, 2]) == which.min(res[, 1])){
  #     warning("Lowest given Lambda value is found to be minimum. You may try to
  #             select a lower Lambda for better results")
  #   }
  # }

  BIC_min <- which.min(res$BIC)
  EBIC_min <- which.min(res$EBIC)

  return(list("summary" = res,
              "BIC_min" = BIC_min,
              "EBIC_min" = EBIC_min
              )
  )
}
