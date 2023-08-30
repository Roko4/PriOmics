#' Calculate a mixed graphical model with or without additional priors (PriOmics algorithm)
#'
#' @param X continuous data (n x p matrix or data.frame)
#' @param Y  discrete/categorical data (n x p matrix or data.frame)
#' @param groups_X list of groups for prior assumption (for continuous data)
#' @param lambda_seq list of tuning parameter lambda (model selection)
#' @param iterations maximum number of iterations, until optimization is stopped
#' @param eps precision parameter
#' @param prior_X vector of prior assumption ("A", "B" or "A_exp" (not recommended)) for each group specified in "groups_X"
#'
#' @importFrom stats sd
#'
#' @return list of resulting models according to lambda value
#'
#' @examples
#' \dontrun{
#' ################################################################################
#' ### fit MGM without prior assumptions
#' ################################################################################
#' # load sample data
#' data(X_n500)
#' data(Y_n500)
#' data(adjmat_truth)
#'
#' # 1a) fit MGM
#' fit1 <- fit_MGM(X = X_n500,
#'                 Y = Y_n500,
#'                 lambda_seq = c(1.3^c(-2:-17)),
#'                 iterations = 500
#' )
#'
#' # 1b) fit MGM in parallel mode
#' fit1 <- fit_MGM_par(X = X_n500,
#'                     Y = Y_n500,
#'                     lambda_seq = c(1.3^c(-2:-17)),
#'                     iterations = 500,
#'                     n_cor = 6
#' )
#'
#' ################################################################################
#' # 2) Model selection:
#' # inspection of model parameter (edges, iterations, BIC, EBIC)
#' plot_BIC(fit1)
#' # manually select 9th lambda value in sequence and generate adjacency matrix
#' adj <- get_adj_mat(fit1, 9)
#' # or automatically
#' adj <- get_adj_mat(fit1, mgm_stats(fit1)[[2]]) # EBIC/BIC optimum
#'
#' ################################################################################
#' # 3) plot heatmap of adjacency matrix and compare it to original data matrix
#' plot_heat(adj)
#' plot_heat(adjmat_truth)
#' plot_heat_selection(adj, "D|F|X_1") # plot subset of adj
#'
#' ################################################################################
#' # 4) interactive Plots
#' # a) interactive networks
#' plot_net(adj, plot_style = "circle")
#' plot_net(adj, plot_style = "igraph")
#'
#' # save network as html
#' net <- plot_net(adj, plot_style = "circle")
#' visNetwork::visSave(net, file = ".../path/.../net.html")
#'
#' # b) interactive heatmaps (for larger matrices)
#' plot_iheat(adj)
#' # save heatmap as html
#' plot_iheat(adj, save_file = T)
#'
#'
#' ################################################################################
#' ### fit MGM with prior A (redundant feature groups, i.e. copies)
#' ################################################################################
#'
#' # add known structure to model
#' X_groups <- list(1, 2, c(3, 4), c(5, 6), c(7, 8, 9), c(10, 11, 12))
#' prior <- rep("A", length(X_groups))
#'
#' fit2 <- fit_MGM(X = X_n500,
#'                 Y = Y_n500,
#'                 groups_X = X_groups,
#'                 prior_X = prior,
#'                 lambda_seq = c(1.3^c(-2:-17)),
#'                 iterations = 500
#'
#' )
#'
#' plot_BIC(fit2)
#' adj2 <- get_adj_mat(fit2, 6)
#'
#' plot_heat(adj2)
#' plot_heat(adjmat_truth)
#'
#' ################################################################################
#' ### fit MGM with prior B (similar feature groups, but no copies)
#' ################################################################################
#'
#' # add known structure to model
#' X_groups <- list(1, 2, c(3, 4), c(5, 6), c(7, 8, 9), c(10, 11, 12))
#' prior <- rep("B", length(X_groups))
#'
#' fit3 <- fit_MGM(X = X_n500,
#'                 Y = Y_n500,
#'                 groups_X = X_groups,
#'                 prior_X = prior,
#'                 lambda_seq = c(1.3^c(-2:-17)),
#'                 iterations = 500
#' )
#'
#' plot_BIC(fit3)
#' adj3 <- get_adj_mat(fit3, 9)
#'
#' plot_heat(adj3)
#' plot_heat(adjmat_truth)
#' }
#'
#' @export
fit_MGM <- function(X, Y, groups_X = FALSE, lambda_seq, iterations = 100, eps = 1e-6, prior_X = FALSE){
  # temp:
  # X <- t(X_sub)
  # Y <- Y_sub
  # groups_X <- Sim$X_groups
  # lambda_seq <- lambda_seq_list
  # prior_X <- rep("C", length(Sim$X_groups))
  # grad = "R"


  pheno_wide_format <- function(Y, Y_levels){
    pheno_wide <- NULL
    for (i in 1:ncol(Y)){
      # i = 11
      if(Y_levels[i] == 2){
        Y_lev <- sort(as.character(unique(Y[, i])))
        Y_first <- as.character(Y[, i])
        Y_second <- rep(NA, length(Y_first))
        Y_second[Y_first == Y_lev[1]] <- 0
        Y_second[Y_first == Y_lev[2]] <- 1
        Y_first <- abs(Y_second-1)
        Y_out <- cbind(Y_first, Y_second)
        colnames(Y_out) <- c(paste0(colnames(Y)[i], "_", Y_lev[1]),
                             paste0(colnames(Y)[i], "_", Y_lev[2]))
      } else {
        Y0 <- Y[, i]
        Y_uniq <- sort(unique(Y0))
        Y_out <- NULL
        for (j in 1:length(Y_uniq)){
          # j = 2
          Y_new <- matrix(0, nrow = nrow(Y), ncol = 1)
          Y_new[which(Y_uniq[j] == Y0)] <- 1
          colnames(Y_new) <- paste(colnames(Y)[i], Y_uniq[j], sep = "_")
          Y_out <- cbind(Y_out, Y_new)
        }
      }
      pheno_wide <- cbind(pheno_wide, Y_out)
    }
    pheno_wide <- apply(pheno_wide, 2, as.integer)
    return(pheno_wide)
  }

  Y_levels <- apply(Y, 2, function(x) length(unique(x)))
  names(Y_levels) <- NULL

  Y_feature_names <- colnames(Y)
  Y <- pheno_wide_format(Y, Y_levels)

  if (isFALSE(groups_X) | isFALSE(prior_X)){
    groups_X = as.list(1:ncol(X))
    prior_X = rep("B", ncol(X))
    use_prior = FALSE
    # message("\nNo groups or no priors are assigned to continuous features. A standard MGM will be calculated!")
  } else {
    use_prior = TRUE
  }

  # if (missing(groups_X) | missing(prior_X)){
  #   groups_X = as.list(1:ncol(X))
  #   prior_X = rep("B", ncol(X))
  #   use_prior = FALSE
  #   # message("\nNo groups or no priors are assigned to continuous features. A standard MGM will be calculated!")
  # } else {
  #   use_prior = TRUE
  # }

  ### old
  # if (missing(prior_X)){
  #   use_prior = FALSE
  #   prior_X = rep("B", ncol(X))
  #   groups_X = as.list(1:ncol(X))
  # } else if (all(is.na(prior_X))){
  #   use_prior = FALSE
  # } else {
  #   use_prior = TRUE
  # }

  input_par <- list("X" = X,
                    "X_feature_names" = colnames(X),
                    "Y" = Y,
                    "Y_feature_names" = Y_feature_names,
                    "Y_levels" = Y_levels,
                    "groups_X" = groups_X,
                    "Sample_names" = rownames(X),
                    "lambda_seq" = lambda_seq,
                    "iterations" = iterations,
                    "eps" = eps,
                    "prior_X" = prior_X
                    #"grad" = grad
  )

  ### Algorithm starts here
  # grad <<- grad

  X <- t((t(X) - colMeans(X)) / (apply(X, 2, sd)))
  p <- ncol(X)
  q <- ncol(Y)

  id1 <- cumsum(Y_levels)
  id2 <- id1-(Y_levels-1)
  groups_Y <- list()
  for (i in seq_along(Y_levels)){
    groups_Y[[i]] <- c(id2[i]:id1[i])
  }

  start <- make_starting_parameters(X, Y)
  start <- B_Rho_Phi_alphap_alphaq(start)

  grad_f <- function(x){
    return(grad_f_temp(x, X, Y, Y_levels, p, q, groups_X, prior_X, use_prior))
  }

  # fun <- function(x){return(neglogli_plain(x, X, Y, Y_levels, p, q, groups_X, prior_X, use_prior))}

  group_IDs <- get_group_IDs_and_weights(groups_X, groups_Y, X, Y, Y_levels)

  x_list <- list(rep(NA, length(lambda_seq)))
  final_iter_list <- list(rep(NA, length(lambda_seq)))
  EBIC_list <- list(rep(NA, length(lambda_seq)))
  group_IDs_list <- list(rep(NA, length(lambda_seq)))
  xtemp <- start

  for (i in seq_along(lambda_seq)){
    # i = 1
    l_l1 <- lambda_seq[[i]]
    print(paste0("lambda = ", l_l1))

    prox_g <- function(x, l, groupIDs){
      return(prox_grouplasso(x, t = l_l1*l, opts = list(groups = group_IDs$groups,
                                                        groupweights = unlist(group_IDs$weights))))
    }

    fit <- solve_mgm(grad_f = grad_f, prox_g = prox_g, xtemp = xtemp,
                     max_iters = iterations, eps = eps, groupIDs = group_IDs)
    x_list[[i]] = Inv_B_Rho_Phi_alphap_alphaq(fit[[1]], p, q)
    final_iter_list[[i]] <- fit[[2]]
    EBIC_list[[i]] <- BIC_function(x_list[[i]], X, Y, Y_levels, groups_X, prior_X, use_prior)
    group_IDs_list[[i]] <- group_IDs
    # xtemp = fit[[i]] #warm start?
  }

  loss_list <- list()
  k = 0
  for (j in 1:length(x_list)){
    k = k+1
    loss_vec <- neglogli(x_list[[j]], X, Y, Y_levels, groups_X, prior_X, use_prior)
    loss_list[[j]] <- loss_vec
  }

  res <- list("fit" = x_list, "loss" = loss_list, "steps" = final_iter_list, "EBIC" = EBIC_list,
              "lambda" = lambda_seq, "input_par" = input_par) # "group_IDs" = group_IDs_list,

  return(res)
}
