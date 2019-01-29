library(raster)
library(prioritizr)
library(marxan)
library(foreach)
library(here)
library(tidyverse)
select <- dplyr::select
walk(list.files("R", full.names = TRUE), source)
prioritizr_timed <- add_timer(prioritizr::solve)

# load nplcc data ----

# species list
species <- here("data", "nplcc_species.csv") %>% 
  read_csv() %>% 
  mutate(id = as.integer(id))

# cost and occupancy
nplcc_file <- here("data", "nplcc_cost_occupancy.rds")
if (!file.exists(nplcc_file)) {
  "https://s3.amazonaws.com/marxan-vs-ilp/nplcc_cost_occupancy.rds" %>% 
    download.file(destfile = nplcc_file)
}
cost_occ <- readRDS(nplcc_file) %>% 
  mutate(pu = row_number()) %>% 
  select(pu, everything())
# split out cost and occupancy
cost <- select(cost_occ, id = pu, cost) %>% 
  arrange(id)
occ <- select(cost_occ, -cost) %>% 
  gather("species_code", "amount", -pu) %>% 
  inner_join(species %>% select(species_code, species = id), 
             by = "species_code") %>% 
  select(pu, species, amount)
rm(cost_occ)


# setup runs ----

# define run matrix
# runs <- expand.grid(
#   target = seq(0.1, 0.9, by = 0.1),
#   n_features = round(seq(10, 72, length.out = 5)),
#   n_pu = round(nrow(cost) / 4^(4:0)),
#   marxan_iterations = c(1e5, 1e6, 1e7, 1e8, 1e9)) %>% 
#   # nest on marxan iterations since this doesn't affect ilp runs
#   nest(marxan_iterations) %>% 
#   rename(marxan = data) %>% 
#   mutate(run_id = row_number()) %>% 
#   select(run_id, everything())

# for testing
runs <- expand.grid(
  target = c(0.25, 0.75),
  n_features = c(30, 72),
  n_pu = round(nrow(cost) / 10^(4:3)),
  marxan_iterations = c(1e5, 1e6)) %>% 
  # nest on marxan iterations since this doesn't affect ilp runs
  nest(marxan_iterations) %>% 
  rename(marxan = data) %>% 
  mutate(run_id = row_number()) %>% 
  select(run_id, everything())

# fixed run parameters
ilp_gap <- 0.1
marxan_reps <- 100
marxan_spf <- 10
marxan_path <- here("marxan", "MarOpt_v243_Mac64")


# iterate over runs ----

# clean up old files
gurobi_dir <- here("output", "gurobi")
unlink(gurobi_dir, recursive = TRUE)
dir.create(gurobi_dir)
rsymphony_dir <- here("output", "rsymphony")
unlink(rsymphony_dir, recursive = TRUE)
dir.create(rsymphony_dir)
marxan_dir <- here("output", "marxan")
unlink(marxan_dir, recursive = TRUE)
dir.create(marxan_dir)
runs_dir <- here("output", "runs")
unlink(runs_dir, recursive = TRUE)
dir.create(runs_dir)

set.seed(1)
runs <- foreach(run = seq_len(nrow(runs)), .combine = bind_rows) %do% {
  r <- runs[run, ]
  str_glue_data(r, "Run ", run, 
                ": Target {target}; Features {n_features}; PUs {n_pu}") %>% 
    message()
  
  # sample species and planning units
  features <- species %>% 
    select(id, name = species_code) %>% 
    sample_n(size = r$n_features) %>% 
    arrange(id) %>% 
    as.data.frame(stringsAsFactors = FALSE)
  cost_ss <- cost %>% 
    sample_n(size = r$n_pu, replace = FALSE) %>% 
    arrange(id)
  rij <- filter(occ, species %in% features$id, pu %in% cost_ss$id) %>% 
    arrange(pu, species)
  targets <- group_by(rij, species) %>% 
    summarize(amount = sum(amount, na.rm = TRUE)) %>% 
    ungroup() %>% 
    transmute(id = species, amount = r$target * amount)
  features <- inner_join(features, targets, by = "id")
  
  # ilp 
  p <- problem(cost_ss, features = features, rij = rij, cost_column = "cost") %>% 
    add_min_set_objective() %>%
    add_relative_targets(r$target) %>%
    add_binary_decisions()
  # gurobi
  s_gur <- p %>% 
    add_gurobi_solver(gap = ilp_gap) %>% 
    prioritizr_timed()
  # save
  "gurobi_target-{target}_features-{n_features}_pu-{n_pu}.rds" %>% 
    str_glue_data(r, .) %>% 
    file.path(gurobi_dir, .) %>% 
    saveRDS(s_gur, .)
  cost_gurobi <- attr(s_gur$result, "objective")
  r$gurobi <- list(tibble(n_solutions = 1,
                          cost = cost_gurobi, 
                          time = s_gur$time[["elapsed"]]))
  rm(s_gur)
  
  # symphony
  s_sym <- p %>% 
    add_rsymphony_solver(gap = ilp_gap * cost_gurobi) %>% 
    prioritizr_timed()
  # save
  "rsymphony_target-{target}_features-{n_features}_pu-{n_pu}.rds" %>% 
    str_glue_data(r, .) %>% 
    file.path(rsymphony_dir, .) %>% 
    saveRDS(s_sym, .)
  cost_gurobi <- attr(s_sym$result, "objective")
  r$rsymphony <- list(tibble(n_solutions = 1,
                             cost = cost_gurobi, 
                             time = s_sym$time[["elapsed"]]))
  rm(s_sym)
  
  # marxan
  # data
  m_data <- MarxanData(pu = mutate(cost_ss, status = 0L), 
                       species = transmute(features, id = id, 
                                           target = amount, 
                                           spf = marxan_spf), 
                       puvspecies = rij, 
                       boundary = NULL)
  # loop over marxan iterations
  r_marxan <- foreach(i_marxan = seq_len(nrow(r$marxan[[1]])), 
                      .combine = bind_rows) %do% {
    r_marxan <- r$marxan[[1]][i_marxan, , drop = FALSE]
    message(paste("  Marxan:", r_marxan$marxan_iterations, "iterations"))
    # options
    m_opts <- MarxanOpts(BLM = 0, NCORES = 1L, VERBOSITY = 3L)
    m_opts@NUMREPS <- as.integer(marxan_reps)
    m_opts@NUMITNS <- as.integer(r_marxan$marxan_iterations)
    m_opts@NUMTEMP <- as.integer(ceiling(m_opts@NUMITNS * 0.2))
    m_unsolved <- MarxanUnsolved(opts = m_opts, data = m_data)
    # solve
    td <- file.path(tempdir(), "marxan-run")
    dir.create(td, recursive = TRUE, showWarnings = FALSE)
    write.MarxanData(m_unsolved@data, td)
    # need to write twice to avoid segfault, no idea why???
    write.MarxanOpts(m_unsolved@opts, td, seed = 1)
    write.MarxanOpts(m_unsolved@opts, td, seed = 1)
    file.copy(marxan_path, td)
    system(paste0("chmod +x ", file.path(td, basename(marxan_path))))
    cmd <- paste0("cd ", td, "; ./", basename(marxan_path), " input.dat -s")
    m_time <- system.time(safely(system)(cmd))
    # save
    f_marxan <- file.path(td, "output_sum.csv")
    if (file.exists(f_marxan)) {
      "marxan_target-{target}_features-{n_features}_pu-{n_pu}_iters-" %>% 
        str_glue_data(cbind(r, r_marxan), .) %>%
        paste0(i_marxan, ".csv") %>% 
        file.path(marxan_dir, .) %>% 
        file.copy(f_marxan, .)
      s_marxan <- read.csv(file.path(td, "output_sum.csv"), 
                           stringsAsFactors = FALSE)
      r_marxan$n_solutions <- sum(s_marxan$Shortfall == 0)
      if (r_marxan$n_solutions > 0) {
        r_marxan$cost <- filter(s_marxan, Shortfall == 0) %>% 
          pull(Cost) %>% 
          min()
      } else {
        r_marxan$cost <- NA_real_
      }
      r_marxan$time <- m_time["elapsed"]
    } else {
      r_marxan$n_solutions <- NA_integer_
      r_marxan$cost <- NA_real_
      r_marxan$time <- NA_real_
    }
    unlink(td, recursive = TRUE)
    r_marxan
  }
  r$marxan <- list(r_marxan)
  # save this iteration in case of crashing
  "target-{target}_features-{n_features}_pu-{n_pu}.rds" %>% 
    str_glue_data(r, .) %>% 
    paste0("run-", i, .) %>% 
    file.path(runs_dir, .) %>% 
    saveRDS(r, .)
  r
}

# unnest
runs_g <- runs %>% 
  mutate(solver = "gurobi") %>% 
  select(run_id, solver, target, n_features, n_pu, gurobi) %>% 
  unnest()
runs_s <- runs %>% 
  mutate(solver = "rsymphony") %>% 
  select(run_id, solver, target, n_features, n_pu, rsymphony) %>% 
  unnest()
runs_m <- runs %>% 
  mutate(solver = "marxan") %>% 
  select(run_id, solver, target, n_features, n_pu, marxan) %>% 
  unnest()
runs_long <- bind_rows(runs_g, runs_s, runs_m)
write_csv(runs_long, here("output", "ilp-comparison-runs.csv"))
