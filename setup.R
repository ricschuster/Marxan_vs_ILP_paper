library(raster)
library(prioritizr)

setwd("data/")

fls <- list.files(pattern = "*.tif$")
remove <- c("Cost.tif", "Protected.tif")

cost <- raster("Cost.tif")
prot <- raster("Protected.tif")
features <- stack(fls[!fls %in% remove])

pp <- problem(cost, features = features) %>% 
  add_min_set_objective() %>%
  add_relative_targets(0.2) %>%
  add_binary_decisions()
  
ss <- solve(pp)
