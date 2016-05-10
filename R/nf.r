#' Separate a table based on two groups of columns. 
#' 
#' This function will return two tables in a list from one, keeping all columns in one table or the other. 
#' 
#' https://en.wikipedia.org/wiki/Database_normalization
#' @param data 
#' @param ... attributes to separate from the rest as "groups"
#'
#' @return
#' @export
#'
#' @examples
db  <- function (data, ...) {
  data <- as_data_frame(data)
  g_cols <- unname(dplyr::select_vars(colnames(data), ...))
  out <- db_(data, g_cols)
  class(out) <- c("dblist", "list")
  out
}

db_ <- function(x, ...) UseMethod("db_")

print.dblist <- function(x, ...) {
  lapply(x, function(x) {print(paste(dim(x), collapse = ", ")); print(as.data.frame(x[1,])); cat("\n", sep = "")})
  invisible(NULL)
}
#' @examples 
#' library(dplyr)
#' db(quakes, -long, -lat)
db_.data.frame <- function(data, g_cols = character()) {
  data_cols <- setdiff(names(data), g_cols)
  # split into data and groups based on user input
  datadata = select_(data, .dots = data_cols)
  groupdata = select_(data, .dots = g_cols)
  ## remove and badge duplicates
  groupdata$id_ <- seq(nrow(groupdata))
  #fun <- function(...) paste(..., sep = "_")
  #datadata$id_ <- as.integer(factor(do.call(fun, as.list(datadata))))
  
  list(data = datadata, groups = groupdata)
}


## once the tables have been separated, perform a  normalization
nlizetail <- function(data) {
  aXb <- dplyr::select_(tail(data, 1), "id_")
  
}
