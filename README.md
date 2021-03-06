
<!-- README.md is generated from README.Rmd. Please edit that file -->
New approach for gris.

``` r
## we expect that these attributes, taken together are "unique vertices" potentially shared by neighbours
v_atts <- c("x_", "y_")

library(spbabel) ## devtools::install_github("mdsumner/spbabel")
#> Loading required package: dplyr
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
source("R/grist.r")
#library(maptools)
#data(wrld_simpl)
#install.packages("rworldxtra")
library(rworldxtra)
#> Loading required package: sp
data(countriesHigh)
library(dplyr)
#x <- gris2(wrld_simpl)
x <- gris2(countriesHigh)
verts <- with(x, o %>% filter(NAME == "Japan") %>% select(object_) %>% inner_join(b) %>% inner_join(bXv) %>% inner_join(v))
#> Joining by: "object_"
#> Joining by: "branch_"
#> Joining by: "vertex_"
library(ggplot2)
ggplot(verts) + aes(x = x_, y = y_, fill = branch_, group = branch_) + geom_polygon()
```

![](README-unnamed-chunk-2-1.png)<!-- -->

``` r



## say we add the segments, then subset
x$s <- grist(x)
xa <- list(o =x$o %>% filter(NAME == "Japan"))
xa$b <- inner_join(x$b, xa$o %>% select(object_))
#> Joining by: "object_"
xa$bXv <- inner_join(x$bXv, xa$b %>% select(branch_))
#> Joining by: "branch_"
xa$v <- inner_join(x$v, xa$bXv %>% select(vertex_)) %>% distinct()
#> Joining by: "vertex_"
xa$s <- inner_join(x$s, xa$b %>% select(branch_))
#> Joining by: "branch_"

verts <- with(xa, o %>% select(object_) %>% inner_join(b) %>% inner_join(bXv) %>% inner_join(v))
#> Joining by: "object_"
#> Joining by: "branch_"
#> Joining by: "vertex_"
library(ggplot2)
ggplot(verts) + aes(x = x_, y = y_, fill = branch_, group = branch_) + geom_polygon()
```

![](README-unnamed-chunk-2-2.png)<!-- -->

``` r





tr <- RTriangle::triangulate(p = RTriangle::pslg(xa$v %>% dplyr::select(x_, y_) %>% as.matrix(), 
                                                 S = seg2struct(xa) %>% as.matrix()),                                               a = 0.1)

plot(tr$P, pch = ".")
apply(tr$T, 1, function(a) lines(tr$P[a, ]))
```

![](README-unnamed-chunk-2-3.png)<!-- -->

    #> NULL
