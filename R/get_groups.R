#' Extract group information from column names in continuous data (i.e., X). 
#' Group names should be indicated by an underscore, e.g. Group_Feature or
#' Protein_Peptide.
#'
#' @param x matrix or data.frame with continuous data
#'
#' @export

get_groups <- function(x){
  x <- colnames(x)
  
  if(any(!grepl("^[^_]*_[^_]*$", x) == TRUE)){
    stop("Function not suitable for your input data. Colnames of X should be separated by an underscore, e.g., Group_Feature. Only one underscore is allowed in the colnames")
  } else {
    print("ok")
    x_groups <- sapply(x, function(y) strsplit(y, "_")[[1]][[1]])
    names(x_groups) <- NULL
    x_groups <- lapply(unique(x_groups), function(y) which(x_groups == y))
  }
  
  return(x_groups)
}