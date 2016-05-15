

nfi <- function(map, v_atts = c("x_", "y_")) {
  dat1<- as_data_frame(as.data.frame(map)) %>% mutate(object_ = row_number())
  ## please note, we add unique vertex label here - but this is tidy the overall process, it's not used until COMBO 3
  map1 <- sptable(map) 
  map1 <- map1 %>% 
    mutate(vertex_  = as.integer(factor(do.call(paste, select_(map1, .dots = v_atts)))))
  
  ## three tables (dat1, map2, map3)
  map2 <- map1 %>% distinct(object_, branch_, island_) %>% dplyr::select(-x_, -y_, -vertex_)
  map3 <- map1 %>% dplyr::select(-object_, -island_)
  
  ## four tables (dat1, map2, map4, map5)
  map4 <- map3 %>% select_(.dots = c("branch_", "vertex_", "order_")) #dplyr::select(branch_, vertex_, order_)
  v_atts <- c(v_atts, "vertex_")
  map5 <- map3 %>% distinct_(.dots = v_atts) %>% select_(.dots = v_atts) 
  #map4 <- map4 %>% select_(.dots = c("branch_", "vertex_", "order_"))
  list(data = dat1, branch = map2, brXvert = map4, vertex = map5)
}


library(maptools)
data(wrld_simpl)
x <- wrld_simpl[c(1), ]
nf <- nfi(wrld_simpl[c(8, 9), ])

path2edge <- function(x) {
  lapply(split(x$vertex_, x$branch_), gris:::prs)
}
## is it better to go to edge-based model from sptable or from db-list?
# branch does not change
# brXvert
