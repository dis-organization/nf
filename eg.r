library(maptools)
data(wrld_simpl)

library(spbabel) ## devtools::install_github("mdsumner/spbabel")
## two tables
map <- subset(wrld_simpl, NAME %in% c("Botswana", "Lesotho", "Namibia","South Africa", "Swaziland"))
dattable <- as.data.frame(map) %>% mutate(object_ = row_number())
maptable <- sptable(map)

pryr::object_size(dattable, maptable)

## three tables
dattable1 <- dattable
brtable1 <- maptable %>% distinct(object_, branch_, island_) 
maptable1 <- maptable %>% dplyr::select(-object_, -island_)
pryr::object_size(dattable1, brtable1, maptable1)

## four tables
dattable2 <- dattable1
brtable2 <- brtable1
vertex <- maptable1 %>% mutate(vertex_ = row_number()) %>% dplyr::select(-branch_, -order_)
br_vertex <- maptable1 %>% dplyr::select(branch_, order_) %>% mutate(vertex_ = row_number())

br_vertex$vertex_ <- vertex$vertex_ <- as.integer(factor(do.call(paste, dplyr::select(vertex, -vertex_))))
pryr::object_size(dattable2, brtable2, vertex, br_vertex)

vertex <- distinct(vertex)
pryr::object_size(dattable2, brtable2, vertex, br_vertex)

dattable2 %>% dplyr::select(NAME, object_) %>% inner_join(brtable2) %>% inner_join(br_vertex) %>% inner_join(vertex)
