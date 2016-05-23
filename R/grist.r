
## we expect that these attributes, taken together are "unique vertices" potentially shared by neighbours
v_atts <- c("x_", "y_")

library(spbabel) ## devtools::install_github("mdsumner/spbabel")

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




library(maptools)
data(wrld_simpl)
library(dplyr)
x <- gris2(wrld_simpl)
verts <- with(x, o %>% filter(NAME == "Japan") %>% select(object_) %>% inner_join(b) %>% inner_join(bXv) %>% inner_join(v))
library(ggplot2)
ggplot(verts) + aes(x = x_, y = y_, fill = branch_, group = branch_) + geom_polygon()

pairs0 <- function (x) {
  data_frame(s0 = head(x, -1), s1 = tail(x, -1))
}
# x$s <- bind_rows(
#    with(x,lapply(split(bXv, bXv$branch_), function(a) pairs0(a$vertex_))), 
#    .id = "branch_")


grist <- function(x) {
  br <- vector("list", nrow(x$o))
  for (i in seq(nrow(x$o))) {
    obj <- x$o %>% select(object_) %>% slice(i)  %>% inner_join(x$b, "object_") %>% inner_join(x$bXv, "branch_") %>% inner_join(x$v, "vertex_")
    br[[i]] <- bind_rows(lapply(split(obj, obj$branch), function(a) bind_cols(pairs0(a$vertex_), data_frame(branch_ = head(a$branch, -1)))))
  }
  bind_rows(br)
} 


## say we add the segments, then subset
x$s <- grist(x)
xa <- list(o =x$o %>% filter(NAME == "Japan"))
xa$b <- inner_join(x$b, xa$o %>% select(object_))
xa$bXv <- inner_join(x$bXv, xa$b %>% select(branch_))
xa$v <- inner_join(x$v, xa$bXv %>% select(vertex_)) %>% distinct()
xa$s <- inner_join(x$s, xa$b %>% select(branch_))

verts <- with(xa, o %>% select(object_) %>% inner_join(b) %>% inner_join(bXv) %>% inner_join(v))
library(ggplot2)
ggplot(verts) + aes(x = x_, y = y_, fill = branch_, group = branch_) + geom_polygon()

seg2struct <- function(x) {
  v <- x$v
  v$i <- seq(nrow(v))
  s <- x$s
  s %>% inner_join(v, c("s0" = "vertex_")) %>% 
    transmute(s1, i0 = i) %>% 
    inner_join(v, c("s1" = "vertex_")) %>% 
    transmute(i0, i1 = i) 
}


#p <- RTriangle::pslg(xa$v %>% dplyr::select(x_, y_) %>% as.matrix())
#S = seg2struct(xa) %>% as.matrix()


tr <- RTriangle::triangulate(p = RTriangle::pslg(xa$v %>% dplyr::select(x_, y_) %>% as.matrix(), 
                                                 S = seg2struct(xa) %>% as.matrix()), 
                                                 a = 0.1)
plot(tr$P, pch = ".")
apply(tr$T, 1, function(a) lines(tr$P[a, ]))




library(RTriangle)
tr <- RTriangle::triangulate(p = RTriangle::pslg(x$v %>% dplyr::select(x_, y_) %>% as.matrix()), 
                             S = cbind(br$s0, br$s1))

#S = split(br, br$branch_)[[1]] %>% dplyr::select(s0, s1) %>%  as.matrix())




split(x$s, x$s$branch_)[[1]]
library(RTriangle)

RTriangle::triangulate(p = pslg(x$v %>% dplyr::select(x_, y_) %>% as.matrix()), 
                       S = x$s %>% as.matrix())
