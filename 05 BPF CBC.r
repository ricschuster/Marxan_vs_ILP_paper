library(raster)
library(prioritizr)
library(marxan)
library(foreach)
library(doParallel)
library(uuid)
library(here)
library(tidyverse)

pkg_list <- c("raster", "prioritizr", "marxan", "uuid",  "here", "tidyverse")
select <- dplyr::select
walk(list.files("R", full.names = TRUE), source)
prioritizr_timed <- add_timer(prioritizr::solve)
# parallelization
n_cores <- 12
cl <- makeCluster(n_cores)
registerDoParallel(cl)

# load nplcc data ----

# species list
species <- here("data", "nplcc_species.csv") %>% 
  read_csv() %>% 
  mutate(id = as.integer(id))

# cost and occupancy
nplcc_file <- here("data", "nplcc_cost_occupancy.zip")
if (!file.exists(nplcc_file)) {
  "https://s3.amazonaws.com/marxan-vs-ilp/nplcc_cost_occupancy.zip" %>% 
    download.file(destfile = nplcc_file)
}
cost_occ <- read_csv(nplcc_file, 
                     col_types = cols(.default = col_double(),
                                      pu = col_integer()))

# split out cost and occupancy
cost <- select(cost_occ, id = pu, cost) %>% 
  arrange(id)
occ <- select(cost_occ, -cost) %>% 
  gather("species_code", "amount", -pu) %>% 
  inner_join(species %>% select(species_code, species = id), 
             by = "species_code") %>% 
  select(pu, species, amount, name = species_code) %>% 
  arrange(pu, species)
rm(cost_occ)

# planning unit raster
pus <- here("data", "nplcc_planning-units.tif") %>% 
  raster() %>% 
  # create an empty template
  raster()

# setup runs ----

# define run matrix
# marxan_runs <- expand.grid(
#   marxan_iterations = 1e8,
#   spf = 25
# )
runs <- expand.grid(target = seq(0.1, 0.9, by = 0.1),
                    n_features = 72,
                    n_pu = 50625,
                    blm = c(0.1, 1, 10, 100, 1000))# %>%
  # add marxan specific parameters
  # mutate(marxan = list(marxan_runs),
  #        run_id = 300 + row_number()) %>%
  # select(run_id, everything())

# fixed run parameters
ilp_gap <- 0.01
# marxan_reps <- 10
random_subset <- TRUE
# sysname <- tolower(Sys.info()[["sysname"]])
# marxan_path <- switch(sysname, 
#                       windows = here("marxan", "Marxan_x64.exe"), 
#                       darwin = here("marxan", "MarOpt_v243_Mac64"), 
#                       linux = here("marxan", "MarOpt_v243_Linux64")
# )
# stopifnot(file.exists(marxan_path))

# iterate over runs ----

# clean up old files
gurobi_dir <- here("output_blm_cbc", "gurobi")
unlink(gurobi_dir, recursive = TRUE)
dir.create(gurobi_dir)
cbc_dir <- here("output_blm_cbc", "cbc")
unlink(cbc_dir, recursive = TRUE)
dir.create(cbc_dir)
runs_dir <- here("output_blm_cbc", "runs")
unlink(runs_dir, recursive = TRUE)
dir.create(runs_dir)

# convert vector of selected units to raster using template
solution_to_raster <- function(x, y) {
  x <- filter(x, solution_1 == 1) %>% 
    pull(id)
  y[x] <- 1
  return(y)
}

set.seed(1)


tt <- here("data", "nplcc_planning-units.tif") %>% 
  raster()

tt[] <- 1:ncell(tt)

e <- extent(560000, 560000 + 22500, 5300000 - 22500, 5300000)
tmp.r <- crop(tt, e)

cost_ss <- cost[cost$id %in% tmp.r[], ] %>% 
  arrange(id)

bnd_mat <- boundary_matrix(tmp.r)
smm_mat <- summary(bnd_mat)
# df <- as.data.frame(as.matrix(bnd_mat))

bnd_df <- data.frame(id1 = tmp.r[][smm_mat$i],
                     id2 = tmp.r[][smm_mat$j],
                     amount = round(smm_mat$x,0))

runs <- foreach(run = seq_len(nrow(runs)), .combine = bind_rows) %do% {
  r <- runs[run, ]
  str_glue_data(r, "Run ", run, 
                ": Target {target}; Features {n_features}; PUs {n_pu}; BLM {blm}") %>% 
    message()
  
  
  
  # sample species and planning units
  features <- species %>% 
    select(id, name = species_code) %>% 
    as.data.frame(stringsAsFactors = FALSE)
  
  r$species <- paste(features$name, collapse = ",")
  pu_ss <- pus
  pu_ss[cost_ss$id] <- 0
  rij <- filter(occ, species %in% features$id, pu %in% cost_ss$id) %>% 
    arrange(pu, species)
  targets <- group_by(rij, species) %>% 
    summarize(tot_amount = sum(amount, na.rm = TRUE)) %>% 
    ungroup() %>% 
    transmute(id = species, amount = r$target * tot_amount)
  features <- inner_join(features, targets, by = "id")
  
  # ilp 
  p <- problem(cost_ss, 
               features = features %>% select(id, name), 
               rij = rij, 
               cost_column = "cost") %>% 
    add_min_set_objective() %>%
    add_relative_targets(r$target) %>%
    add_binary_decisions() %>%
    add_boundary_penalties(penalty = r$blm, edge_factor = 0.5, data = bnd_mat)
  # gurobi
  s_gur <- p %>% 
    add_gurobi_solver(gap = ilp_gap, threads = 40) %>%
    prioritizr_timed(force = TRUE)
  # solution summary
  cost_gurobi <- attr(s_gur$result, "objective")
  r$gurobi <- list(tibble(n_solutions = 1,
                          cost = cost_gurobi, 
                          time = s_gur$time[["elapsed"]]))
  # save solution
  s_gur <- "gurobi_target-{target}_features-{n_features}_pu-{n_pu}_blm-{blm}.tif" %>% 
    str_glue_data(r, .) %>% 
    file.path(gurobi_dir, .) %>% 
    writeRaster(solution_to_raster(s_gur$result, pus), overwrite = TRUE, .)
  rm(s_gur)
  
  # cbc
  s_cpl <- p %>% 
    add_cbc_solver(gap = ilp_gap, threads = 40, time_limit = 1000) %>% 
    prioritizr_timed(force = TRUE)
  # solution summary
  cost_cbc <- attr(s_cpl$result, "objective")
  r$cbc <- list(tibble(n_solutions = 1,
                         cost = cost_cbc, 
                         time = s_cpl$time[["elapsed"]]))
  # save solution
  s_cpl <- "cbc_target-{target}_features-{n_features}_pu-{n_pu}_blm-{blm}.tif" %>% 
    str_glue_data(r, .) %>% 
    file.path(cbc_dir, .) %>% 
    writeRaster(solution_to_raster(s_cpl$result, pus), .)
  rm(s_cpl)
  
  # save this iteration in case of crashing
  str_glue_data(r, "run-", run,
                "_target-{target}_features-{n_features}_pu-{n_pu}_blm-{blm}.rds") %>%
    file.path(runs_dir, .) %>%
    saveRDS(r, .)
  r
}

# unnest
runs_g <- runs %>% 
  mutate(solver = "gurobi") %>% 
  select(solver, target, n_features, n_pu, blm, species, gurobi) %>% 
  unnest()
runs_c <- runs %>% 
  mutate(solver = "cbc") %>% 
  select(solver, target, n_features, n_pu, blm, species, cbc) %>% 
  unnest()
# runs_m <- runs %>% 
#   mutate(solver = "marxan") %>% 
#   select(run_id, solver, target, n_features, n_pu, species, marxan) %>% 
#   unnest()
runs_long <- bind_rows(runs_g, runs_c)
# runs_long <- bind_rows(runs_g, runs_s, runs_m)
write_csv(runs_long, here("output_blm_cbc", "ilp-comparison-runs_no_marx.csv"))

# clean up
stopCluster(cl)



