#' Internal function
#'
#' @param grad_f ...
#' @param prox_g ...
#' @param xtemp ...
#' @param groupIDs ...
#' @param max_iters ...
#' @param eps ...
#' @param alpha ...
#' @param beta ...
#'
#' @importFrom utils setTxtProgressBar txtProgressBar
#'
solve_mgm <- function(grad_f, prox_g, xtemp, groupIDs = group_IDs,
                      max_iters=2,
                      eps=1e-6,
                      alpha=1.01,
                      beta=0.5){ # 0.5

  norm_vec <- function(x) sqrt(sum(x^2))

  x <- xtemp # = x_init
  y <- xtemp
  g <- grad_f(y)

  theta = 1

  # barzilai-borwein step-size initialization:
  t = 1/norm_vec(g)
  x_hat = x - t * g
  g_hat = grad_f(x_hat)
  t = abs(sum((x - x_hat) * (g - g_hat)) / (norm_vec(g - g_hat)^2))

  ### Start
  k = 1
  err1 = NA

  pb <- txtProgressBar(min = 0, max = max_iters, style = 3) #progress bar

  for (k in 1:max_iters) {
    setTxtProgressBar(pb, k) #progress bar

    x_old = x
    y_old = y
    x = y - t * g
    x = prox_g(x, t, groupIDs)
    err1 = norm_vec((y - x)) / (1 + norm_vec(x)) / t

    if (err1 < eps) {
      close(pb) #progress bar
      print(paste("optimum found at step =", k))
      break
    }
    theta = 2. / (1 + sqrt(1 + 4 / (theta^2)))

    if (sum((y - x) * (x - x_old)) > 0) {
      x = x_old
      y = x
      theta = 1.
    } else {
      y = x + (1 - theta) * (x - x_old)
    }

    g_old = g
    g = grad_f(y)

    # tfocs-style backtracking:
    t_hat = 0.5 * (norm_vec(y - y_old)^2) / abs(as.vector(y - y_old) %*% as.vector(g_old - g))
    t = min(alpha * t, max(beta * t, t_hat))
  }
  solver_res <- list("x" = x, "final_iter" = k)
  return(solver_res)
}
