library(spbabel)
## two built-in data sets
semap ## the geometry in spbabel/fortify form
seatt ## the data, linked to semap by object_

library(tidyr)
## nesting on object_ (so we match seatt for rows)
obj <- semap  %>% nest(-object_) %>% inner_join(seatt) %>% select(object_, NAME, data)

## we now have all our geometry in one table, object attributes in the other
## but the link is gone
geom <- unnest(x %>% select(data))


## what I want is this in one operation
## (sorry for inexpert NSE . . .)
myfun <- function(data, ..., .key = data) {
  dname <- tidyr:::col_name(substitute(.key))
  nest_cols <- unname(dplyr::select_vars(colnames(data), ...))
  out <- list()
  out[[dname]] <- select(data, ...)
  out[["obj"]] <- distinct_(select_(data, .dots = setdiff(names(data), names(out[[dname]]))))
  out
}
semap %>% myfun(-object_)



## another example