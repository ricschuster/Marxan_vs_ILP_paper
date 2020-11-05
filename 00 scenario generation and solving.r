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
n_cores <- 24
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
#   marxan_iterations = c(1e4, 1e5, 1e6, 1e7, 1e8),
#   spf = 5^(0:3)
# )
runs <- expand.grid(target = seq(0.1, 0.9, by = 0.1),
                     n_features = round(seq(10, 72, length.out = 5)),
                     n_pu = round(nrow(cost) / 4^(4:2)))# %>%
  # add marxan specific parameters
  # mutate(marxan = list(marxan_runs),
  #        run_id = row_number()) %>%
  # select(run_id, everything())

# # for testing
# marxan_runs <- expand.grid(marxan_iterations = c(1e5, 1e6), spf = c(5, 25))
# runs <- expand.grid(target = c(0.25, 0.5),
#                     n_features = c(10, nrow(species)),
#                     n_pu = round(nrow(cost) / 4^c(4, 3))) %>%
#   # add marxan specific parameters
#   mutate(marxan = list(marxan_runs),
#          run_id = row_number()) %>%
#   select(run_id, everything())

# fixed run parameters
ilp_gap <- 0.001
# marxan_reps <- 10
random_subset <- TRUE
sysname <- tolower(Sys.info()[["sysname"]])
# marxan_path <- switch(sysname, 
#                       windows = here("marxan", "Marxan_x64.exe"), 
#                       darwin = here("marxan", "MarOpt_v243_Mac64"), 
#                       linux = here("marxan", "MarOpt_v243_Linux64")
# )
# stopifnot(file.exists(marxan_path))

# iterate over runs ----

# clean up old files
gurobi_dir <- here("output2", "gurobi")
#unlink(gurobi_dir, recursive = TRUE)
dir.create(gurobi_dir)
rsymphony_dir <- here("output2", "rsymphony")
#unlink(rsymphony_dir, recursive = TRUE)
dir.create(rsymphony_dir)
cplex_dir <- here("output2", "cplex")
#unlink(rsymphony_dir, recursive = TRUE)
dir.create(cplex_dir)
# marxan_dir <- here("output2", "marxan")
# #unlink(marxan_dir, recursive = TRUE)
# dir.create(marxan_dir)
runs_dir <- here("output2", "runs")
#unlink(runs_dir, recursive = TRUE)
dir.create(runs_dir)

# convert vector of selected units to raster using template
solution_to_raster <- function(x, y) {
  x <- filter(x, solution_1 == 1) %>% 
    pull(id)
  y[x] <- 1
  return(y)
}

set.seed(1)
runs <- foreach(run = seq_len(nrow(runs)), .combine = bind_rows) %do% {
  r <- runs[run, ]
  str_glue_data(r, "Run ", run, 
                ": Target {target}; Features {n_features}; PUs {n_pu}") %>% 
    message()
  
  # sample species and planning units
  if (random_subset) {
    features <- species %>% 
      select(id, name = species_code) %>% 
      sample_n(size = r$n_features, replace = FALSE) %>% 
      arrange(id) %>% 
      as.data.frame(stringsAsFactors = FALSE)
    cost_ss <- cost %>% 
      sample_n(size = r$n_pu, replace = FALSE) %>% 
      arrange(id)
  } else {
    features <- species %>% 
      select(id, name = species_code) %>% 
      slice(seq_len(r$n_features)) %>% 
      arrange(id) %>% 
      as.data.frame(stringsAsFactors = FALSE)
    cost_ss <- cost %>% 
      slice(seq_len(r$n_pu)) %>% 
      arrange(id)
  }
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
    add_binary_decisions()
  # gurobi
  s_gur <- p %>% 
    add_gurobi_solver(gap = ilp_gap) %>% 
    prioritizr_timed(force = TRUE)
  # solution summary
  cost_gurobi <- attr(s_gur$result, "objective")
  r$gurobi <- list(tibble(n_solutions = 1,
                          cost = cost_gurobi, 
                          time = s_gur$time[["elapsed"]]))
  # save solution
  s_gur <- "gurobi_target-{target}_features-{n_features}_pu-{n_pu}.tif" %>% 
    str_glue_data(r, .) %>% 
    file.path(gurobi_dir, .) %>% 
    writeRaster(solution_to_raster(s_gur$result, pus), .)
  rm(s_gur)
  
  # cplex
  s_cpl <- p %>% 
    add_cplex_solver(gap = ilp_gap) %>% 
    prioritizr_timed(force = TRUE)
  # solution summary
  cost_cplex <- attr(s_cpl$result, "objective")
  r$cplex <- list(tibble(n_solutions = 1,
                          cost = cost_cplex, 
                          time = s_cpl$time[["elapsed"]]))
  # save solution
  s_cpl <- "cplex_target-{target}_features-{n_features}_pu-{n_pu}.tif" %>% 
    str_glue_data(r, .) %>% 
    file.path(cplex_dir, .) %>% 
    writeRaster(solution_to_raster(s_cpl$result, pus), .)
  rm(s_cpl)
  
  # # symphony
  # s_sym <- p %>% 
  #   add_rsymphony_solver(gap = ilp_gap * cost_gurobi) %>% 
  #   prioritizr_timed(force = TRUE)
  # # solution summary
  # r$rsymphony <- list(tibble(n_solutions = 1,
  #                            cost = attr(s_sym$result, "objective"), 
  #                            time = s_sym$time[["elapsed"]]))
  # # save solution
  # s_sym <- "rsymphony_target-{target}_features-{n_features}_pu-{n_pu}.tif" %>% 
  #   str_glue_data(r, .) %>% 
  #   file.path(rsymphony_dir, .) %>% 
  #   writeRaster(solution_to_raster(s_sym$result, pus), .)
  # rm(s_sym)
  # 
 # save this iteration in case of crashing
  str_glue_data(r, "run-", run,
                "_target-{target}_features-{n_features}_pu-{n_pu}.rds") %>%
    file.path(runs_dir, .) %>%
    saveRDS(r, .)
   r
}

# unnest
runs_g <- runs %>% 
  mutate(solver = "gurobi") %>% 
  select(run_id, solver, target, n_features, n_pu, species, gurobi) %>% 
  unnest()
runs_s <- runs %>% 
  mutate(solver = "rsymphony") %>% 
  select(run_id, solver, target, n_features, n_pu, species, rsymphony) %>% 
  unnest()
runs_c <- runs %>% 
  mutate(solver = "cplex") %>% 
  select(run_id, solver, target, n_features, n_pu, species, marxan) %>% 
  unnest()
runs_long <- bind_rows(runs_g, runs_s, runs_c)
write_csv(runs_long, here("output2", "ilp-comparison-runs2.csv"))

# clean up
stopCluster(cl)


