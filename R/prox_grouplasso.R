#' Internal function for proximal operator of group lasso penalty
#'
#' @param x ...
#' @param t ...
#' @param opts ...
#' @references adapted from github.com/jpvert/apg
#'
prox_grouplasso <- function(x, t, opts=list(groups=as.list(seq(length(xc))))) {
  norm_vec <- function(x) sqrt(sum(x^2))
  if (!exists("groups",where=opts))
    stop("No list of groups provided for the group lasso.")
  ngroups <- length(opts$groups)

  if (!exists("groupweights",where=opts)) {
    w <- rep(t, ngroups)
  } else {
    if (length(opts[["groupweights"]]) == ngroups) {
      w <- t*opts[["groupweights"]]
    } else {
      w <- t*rep(opts[["groupweights"]][1], ngroups)
    }
  }

  u <- x
  for (i in seq(ngroups)) {
    g <- opts[["groups"]][[i]]
    u[g] <- max(0, 1 - w[i] / norm_vec(x[g]) ) * x[g]
  }
  return(u)
}
