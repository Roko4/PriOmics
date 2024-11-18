## Overview
An implementation for calculating Mixed Graphical models based on the approach by Lee & Hastie (2012). Additionally, prior group knowledge about features can be added to increase model performance.

For a python version of the algorithm, see: https://github.com/Roko4/PriOmics-Py

## Installation
``` r
# Download and install the PriOmics R package with the following steps:
require("devtools")
install_github("roko4/PriOmics")
library(PriOmics)

```

## Usage

### Standard MGM
Employing the standard MGM algorithm (without PriOmics extension) requires at minimum 3 inputs:
- X:	Continuous data as matrix or data.frame with samples in rows and features in columns (n x p)
- Y:	Discrete/categorical data as matrix or data.frame with samples in rows and features in columns (n x p)
- lambda_seq: Vector of tuning parameter lambda used for model selection. 

### PriOmics MGM
In addition to the inputs of the standard MGM, we need 2 more arguments:
- groups_X: List of groups for prior assumption (for continuous data)
- prior_X: Vector of the prior assumptions, either "A" or "B". If no prior should be assigned to set the corresponding feature to a group size of one. The prior will then be inored. Applying both priors to different feature groups in the same model is possible.

## Example

``` r
## Not run: 
################################################################################
### Method 1: fit MGM without prior assumptions
################################################################################
library(PriOmics)

# load sample data
data(X_n500)
data(Y_n500)
data(adjmat_truth)

# 1a) fit MGM
fit1 <- fit_MGM(X = X_n500,
                Y = Y_n500,
                lambda_seq = c(1.3^c(-2:-17)),
                iterations = 500
)

# 1b) fit MGM in parallel mode
fit1 <- fit_MGM_par(X = X_n500,
                    Y = Y_n500,
                    lambda_seq = c(1.3^c(-2:-17)),
                    iterations = 500,
                    n_cor = 6
)

################################################################################
# 2) Model selection:
# inspection of model parameter (edges, iterations, BIC, EBIC)
plot_BIC(fit1)
# manually select 9th lambda value in sequence and generate adjacency matrix
adj <- get_adj_mat(fit1, 9)
# or automatically
adj <- get_adj_mat(fit1, mgm_stats(fit1)[[2]]) # EBIC/BIC optimum

################################################################################
# 3) plot heatmap of adjacency matrix and compare it to original data matrix
plot_heat(adj)
plot_heat(adjmat_truth)
plot_heat_selection(adj, "D|F|X_1") # plot subset of adj

################################################################################
# 4) interactive Plots
# a) interactive networks
plot_net(adj, plot_style = "circle")
plot_net(adj, plot_style = "igraph")

# save network as html
net <- plot_net(adj, plot_style = "circle")
visNetwork::visSave(net, file = ".../path/.../net.html")

# b) interactive heatmaps (for larger matrices)
plot_iheat(adj)
# save heatmap as html
plot_iheat(adj, save_file = T)


################################################################################
### Method 2: fit PriOmics-MGM with prior A, i.e., CIG - conditionally independent (grouped)
################################################################################

# add known structure to model
X_groups <- list(1, 2, c(3, 4), c(5, 6), c(7, 8, 9), c(10, 11, 12))
prior <- rep("A", length(X_groups))

fit2 <- fit_MGM(X = X_n500,
                Y = Y_n500,
                groups_X = X_groups,
                prior_X = prior,
                lambda_seq = c(1.3^c(-2:-17)),
                iterations = 500

)

plot_BIC(fit2)
adj2 <- get_adj_mat(fit2, 6)

plot_heat(adj2)
plot_heat(adjmat_truth)

################################################################################
### Method 3: fit PriOmics-MGM with prior B, i.e., non-CIG - non-conditionally independent (grouped)
################################################################################

# add known structure to model
X_groups <- list(1, 2, c(3, 4), c(5, 6), c(7, 8, 9), c(10, 11, 12))
prior <- rep("B", length(X_groups))

fit3 <- fit_MGM(X = X_n500,
                Y = Y_n500,
                groups_X = X_groups,
                prior_X = prior,
                lambda_seq = c(1.3^c(-2:-17)),
                iterations = 500
)

plot_BIC(fit3)
adj3 <- get_adj_mat(fit3, 9)

plot_heat(adj3)
plot_heat(adjmat_truth)

## End(Not run)
```

## Dependencies
matrixStats
pheatmap
qgraph
snowfall
visNetwork

## License
MIT License
Copyright (c) 2024 Robin Kosch and Michael Altenbuchinger:
Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:  The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software. 
THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

## Citation
Kosch, R., Limm, K., Staiger, A. M., Kurz, N. S., Seifert, N., Oláh, B., ... & Altenbuchinger, M. (2023). PriOmics: integration of high-throughput proteomic data with complementary omics layers using mixed graphical modeling with group priors. bioRxiv, 2023-11.

## Additional Resources
Altenbuchinger, M., Weihs, A., Quackenbush, J., Grabe, H. J., & Zacharias, H. U. (2020). Gaussian and Mixed Graphical Models as (multi-) omics data analysis tools. Biochimica et Biophysica Acta (BBA)-Gene Regulatory Mechanisms, 1863(6), 194418.

Altenbuchinger, M., Zacharias, H. U., Solbrig, S., Schäfer, A., Büyüközkan, M., Schultheiß, U. T., ... & Gronwald, W. (2019). A multi-source data integration approach reveals novel associations between metabolites and renal outcomes in the German Chronic Kidney Disease study. Scientific reports, 9(1), 13954.

## News/Updates
- Version 1.3.1 (corrected plot_BIC(); added function for automated group detection get_groups())
- Version 1.2.2 (added function plot_net_selection() for first order neighborhoods)
- Version 1.1 Minor bug fixes
- Version 1.0 Original release

## Getting help
Use the in-build R helper function for further help, e.g.:
``` r
?fit_MGM()
?plot_BIC()
?plot_heat()
```