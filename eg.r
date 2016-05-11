library(maptools)
data(wrld_simpl)




# COMBO 1. 
# Create coordinate-first table and link to object table
# 
# COMBO 2. 
# Badge unique vertices with new identifier .vx0
# Split sptable into distinct Branch table and Coordinates table 
# - optionally Branch has attributes on branch, as well as object, island status
# - Coordinates table does not require object_ or island anymore island_, still needs order
# 
# COMBO 3. 
# Split Coordinate table into branch-link-vertices and **unique** vertices
# 




## we expect that these attributes, taken together are "unique vertices" potentially shared by neighbours
v_atts <- c("x_", "y_")

pryr::object_size(wrld_simpl)
library(spbabel) ## devtools::install_github("mdsumner/spbabel")
## two tables (dat1, map1 - same as ggplot2 fortify)
map <- subset(wrld_simpl, NAME %in% c("Botswana", "Lesotho", "Namibia","South Africa", "Swaziland"))
#map <- wrld_simpl
dat1<- as.data.frame(map) %>% mutate(object_ = row_number())
## please note, we add unique vertex label here - but this is tidy the overall process, it's not used until COMBO 3
map1 <- sptable(map) 
map1 <- map1 %>% 
  mutate(vertex_  = as.integer(factor(do.call(paste, select_(map1, .dots = v_atts)))))
pryr::object_size(dat1, map1)

## three tables (dat1, map2, map3)
map2 <- map1 %>% distinct(object_, branch_, island_) 
map3 <- map1 %>% dplyr::select(-object_, -island_)
pryr::object_size(dat1, map2, map3)

## four tables (dat1, map2, map4, map5)
map4 <- map3 %>% dplyr::select(branch_, vertex_)
map5 <- map5 %>% distinct(x_, y_, vertex_)

pryr::object_size(dat1, map2, map4, map5)

## we can drive ggplot2 or sp with any of these combos
library(ggplot2)
combo1 <- dat1 %>% select(NAME, object_) %>% inner_join(map1)
ggplot(combo1) + 
  aes(x_, y_, group = branch_, fill = factor(NAME)) +   geom_polygon() 
spplot(spFromTable(combo1))

combo2 <- dat1 %>% select(NAME, object_) %>% inner_join(map2) %>% inner_join(map3)
ggplot(combo2) + 
  aes(x_, y_, group = branch_, fill = factor(NAME)) +  geom_polygon()
spplot(spFromTable(combo2))

combo3 <- dat1 %>% select(NAME, object_) %>% inner_join(map2) %>% inner_join(map4) %>% inner_join(map5)
ggplot(combo3) + 
  aes(x_, y_, group = branch_, fill = factor(NAME)) +  geom_polygon()
spplot(spFromTable(combo2))






