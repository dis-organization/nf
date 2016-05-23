
gris2 <- function(x) {
  ## two tables (dat1, map1 - same as ggplot2 fortify)
  #map <- subset(x, NAME %in% c("Botswana", "Lesotho", "Namibia","South Africa", "Swaziland"))
  map <- x
  dat1<- as.data.frame(map) %>% mutate(object_ = row_number())
  ## please note, we add unique vertex label here - but this is tidy the overall process, it's not used until COMBO 3
  map1 <- sptable(map) 
  map1 <- map1 %>% 
    mutate(vertex_  = as.integer(factor(do.call(paste, select_(map1, .dots = v_atts)))))
  map2 <- map1 %>% distinct(object_, branch_, island_) %>% dplyr::select(object_, branch_, island_)
  map3 <- map1 %>% dplyr::select(-object_, -island_)
  
  ## four tables (dat1, map2, map4, map5)
  map4 <- map3 %>% dplyr::select(branch_, vertex_, order_)
  map5 <- map3 %>% distinct(x_, y_, vertex_) %>% dplyr::select(x_, y_, vertex_)
  
  list(o = dat1, b = map2, bXv = map4, v = map5)
}
pairs0 <- function (x) {
  data_frame(s0 = head(x, -1), s1 = tail(x, -1))
}


grist <- function(x) {
  br <- vector("list", nrow(x$o))
  for (i in seq(nrow(x$o))) {
    obj <- x$o %>% select(object_) %>% slice(i)  %>% inner_join(x$b, "object_") %>% inner_join(x$bXv, "branch_") %>% inner_join(x$v, "vertex_")
    br[[i]] <- bind_rows(lapply(split(obj, obj$branch), function(a) bind_cols(pairs0(a$vertex_), data_frame(branch_ = head(a$branch, -1)))))
  }
  bind_rows(br)
} 

seg2struct <- function(x) {
  v <- x$v
  v$i <- seq(nrow(v))
  s <- x$s
  s %>% inner_join(v, c("s0" = "vertex_")) %>% 
    transmute(s1, i0 = i) %>% 
    inner_join(v, c("s1" = "vertex_")) %>% 
    transmute(i0, i1 = i) 
}


