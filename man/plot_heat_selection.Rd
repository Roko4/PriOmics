% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_heat_selection.R
\name{plot_heat_selection}
\alias{plot_heat_selection}
\title{Plot basic heatmap of selected features (based on package 'pheatmap')}
\usage{
plot_heat_selection(adj, sel)
}
\arguments{
\item{adj}{result from get_adj_mat()}

\item{sel}{selected features, separated by "|" (e.g, "Feat_X|Feat_Y|Feat_Z")
or use regex to selected features from feature list}
}
\description{
Plot basic heatmap of selected features (based on package 'pheatmap')
}
