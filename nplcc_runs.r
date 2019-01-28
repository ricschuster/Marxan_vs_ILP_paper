library(raster)
library(prioritizr)
library(marxan)
library(foreach)
library(doParallel)
library(tidyverse)
select <- dplyr::select

# load nplcc data ----

# species list
download.file("https://cornell.box.com/s/zi83x5wbyi1tfrjhzc9oiy59dvw76y7h")



species <- read_csv("data/nplcc_species.csv")
sp_lookup <- deframe(species %>% select(id, species_code))

# cost
r_cost <- raster("data/nplcc_cost.tif") %>% 
  setNames("cost")
n_cell <- cellStats(!is.na(r_cost), sum)

# species occupancy
r_occ <- list.files("data/species/", full.names = TRUE) %>% 
  stack()
names(r_occ) <- names(r_occ) %>% 
  str_extract("(?<=nplcc_occ_)[a-z]+") %>%
  {sp_lookup[.]}
cost_occ <- stack(r_cost, r_occ) %>% 
  rasterToPoints()


# setup runs ----

# define run matrix
runs <- expand.grid(
  target = c(0.5, seq(0.1, 0.9, by = 0.1), 0.95),
  n_features = c(30, 40, 50, 60, 72),
  n_pu = round(n_cell / 10^(0:4)),
  marxan_iterations = c(1e5, 1e6, 1e7, 1e8, 1e9)
)

# fixed run parameters
ilp_solvers <- c("marxan", "gurobi", "rsymphony")
ilp_gap <- 0.1
marxan_replicates <- 100
