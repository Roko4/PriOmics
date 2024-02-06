#' Plot overview of PriOmics models (Lambda against BIC and EBIC)
#'
#' @param x list of fits calculated with fit_MGM() or fit_MGM_par()
#'
#' @importFrom graphics axis legend mtext par points text
#'
#' @export
plot_BIC <- function (x, mar = c(10, 7, 4, 7), cex.bic.text = 0.7, cex.it.text = 0.8,
                      leg.position = "top", legend.text.size = 0.7, cex.axis = 1.5,
                      cex.lambda.text = 1.5, cex.bic.axis = 1.5, cex.ebic.axis = 1.5)
{
  lambda_seq <- x$lambda
  eBIC <- sapply(x$EBIC, function(y) y[[1]])
  active_par <- sapply(x$EBIC, function(y) y[[3]])
  BIC <- sapply(x$EBIC, function(y) y[[4]])
  col_vec1 <- rep("black", length(lambda_seq))
  col_vec2 <- rep("black", length(lambda_seq))
  eBIC_min <- which.min(eBIC)
  BIC_min <- which.min(BIC)
  col_vec1[BIC_min] <- "green4"
  col_vec2[eBIC_min] <- "green4"
  iter = as.vector(unlist(x$steps))

  par(mar = mar, cex.axis = cex.axis)
  plot(BIC, xaxt = "n", xlab = "", col = col_vec1, pch = 16,
       cex.lab = 1.2, las = 2, ylab = "")
  axis(1, format(lambda_seq, digits = 4),
       las = 2, at = 1:length(lambda_seq))
  text(1:length(lambda_seq), BIC, active_par, col = "red",
       cex = cex.bic.text, pos = 3)
  graphics::abline(v = eBIC_min, col = "grey60", lty = 3)
  graphics::abline(v = BIC_min, col = "grey60", lty = 3)

  par(new = TRUE)
  plot(eBIC, col = col_vec2, pch = 17, axes = FALSE, xlab = "",
       ylab = "")
  axis(side = 4, at = pretty(range(eBIC)), las = 2)
  mtext("BIC", side = 2, line = 4.5, cex = cex.bic.axis)
  mtext("EBIC", side = 4, line = 5, cex = cex.ebic.axis)
  mtext(iter, side = 3, at = seq_along(iter), col = "blue",
        cex = cex.it.text, las = 2)
  mtext("Iterations:", side = 3, line = 1.5, at = 0.4, col = "blue",
        cex = cex.it.text, adj = 0)
  mtext("Lambda", side = 1, line = 7,
        #at = ifelse(length(lambda_seq)%%2 == 0, length(lambda_seq)/2 + 0.5, ceiling(length(lambda_seq))),
        col = "black", cex = cex.lambda.text)
  points(eBIC, col = col_vec2, pch = 17)
  legend(leg.position, legend = c("BIC", "EBIC", "# Edges",
                                  "Optimum"), pch = c(16, 17, 3, 3), text.col = c("black",
                                                                                  "black", "red", "green4"), cex = legend.text.size)
}
