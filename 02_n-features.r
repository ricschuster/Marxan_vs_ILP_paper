library(raster)
library(prioritizr)
library(marxan)
library(foreach)
library(doParallel)
library(timethis) # devtools::install_github("mstrimas/timethis)
library(tidyverse)
registerDoParallel(4)
walk(list.files("R", full.names = TRUE), source)
select <- dplyr::select
prioritizr_timed <- add_timer(prioritizr::solve)

mm <- raster::rasterOptions()$maxmemory
raster::rasterOptions(maxmemory = 1e7)

# parameters ----

target <- 0.2
ilp_gap <- 0.1
marxan_reps <- 10L
marxan_iterations <- 1e8
n_features <- c(1, 5, 10, 15, 20, 25, 30, 35, 40, 45)
solvers <- c("marxan", "gurobi", "rsymphony")
scenarios <- expand.grid(solver = solvers, n_features = n_features, 
                         stringsAsFactors = FALSE)

# load data ----

cost <- raster("data/Cost.tif")
features <- list.files("data/species", full.names = TRUE) %>% 
  # randomly order
  sample() %>% 
  stack()
# rasters are slightly misaligned
cost <- projectRaster(cost, features, method = "ngb")
# mask out any cells that are NA in another layer
na_mask <- calc(is.na(stack(cost, features)), sum) == 0
cost <- mask(cost, na_mask, maskvalue = 0)
features <- mask(features, na_mask, maskvalue = 0)


# exact solutions ----

s_exact <- foreach (n = n_features, .combine = bind_rows) %dopar% {
  s <- problem(cost, features = features[[seq.int(n)]]) %>% 
    add_min_set_objective() %>%
    add_relative_targets(target) %>%
    add_binary_decisions() %>% 
    add_gurobi_solver(gap = 0.0001) %>% 
    prioritizr_timed()
  data_frame(n_features = n,
             time_exact = s$time["elapsed"],
             cost_optimal = attr(s$result, "objective"))
}

sss1 <- problem(cost, features = features[[seq.int(5)]]) %>% 
  add_min_set_objective() %>%
  add_relative_targets(target) %>%
  add_binary_decisions() %>% 
  add_gurobi_solver(0.0001) %>% 
  solve()
sss2 <- problem(cost, features = features[[seq.int(5)]]) %>% 
  add_min_set_objective() %>%
  add_relative_targets(target) %>%
  add_binary_decisions() %>% 
  add_gurobi_solver(0.1) %>% 
  solve()

# solve scenarios ----
scenario_cost_time <- foreach(i = seq_len(nrow(scenarios)), 
                              .combine = bind_rows) %dopar% {
  r <- scenarios[i, ]
  f_subset <- features[[seq.int(r$n_features)]]
  if (r$solver %in% c("gurobi", "rsymphony")) {
    p <- problem(cost, features = f_subset) %>% 
      add_min_set_objective() %>%
      add_relative_targets(target) %>%
      add_binary_decisions()
    if (r$solver == "gurobi") {
      p <- add_gurobi_solver(p, gap = ilp_gap)
    } else {
      cost_optimal <- s_exact %>% 
        filter(n_features == r$n_features) %>% 
        pull(cost_optimal)
      p <- add_rsymphony_solver(p, gap = ilp_gap * cost_optimal)
    }
    s <- prioritizr_timed(p)
    r$selected <- list(Which(s$result == 1, cells = TRUE))
    r$n_solutions <- 1
    r$cost <- unname(attr(s$result, "objective"))
    r$time <- s$time["elapsed"]
  } else if (r$solver == "marxan") {
    # options
    m_opts <- MarxanOpts(BLM = 0, NCORES = 1L)
    m_opts@NUMREPS <- as.integer(marxan_reps)
    m_opts@NUMITNS <- as.integer(marxan_iterations)
    m_opts@NUMTEMP <- as.integer(ceiling(m_opts@NUMITNS * 0.2))  
    # data
    m_data <- prepare_marxan_data(cost, f_subset, target = target, spf = 10)
    m_unsolved <- MarxanUnsolved(opts = m_opts, data = m_data)
    # solve
    s <- time_this(solve(m_unsolved))
    # lowest cost
    idx <- as.integer(gsub("[a-zA-Z]+", "", 
                           colnames(s$result@results@selections)))
    sel <- s$result@results@selections[s$result@results@best, , drop = TRUE]
    r$selected <- list(idx[sel])
    r$n_solutions <- sum(s$result@results@summary$Shortfall == 0)
    r$cost <- filter(s$result@results@summary, Shortfall == 0) %>% 
      pull(Cost) %>% 
      min()
    r$time <- s$time["elapsed"]
  } else {
    stop("unknown solver")
  }
  r
}
# clean up to free memory
f <- "output/02_n-features_all-scenarios.rds"
if (!file.exists(f)) {
  saveRDS(scenario_cost_time, f)
}
scenario_cost_time <- select(scenario_cost_time, -selected)  %>% 
  inner_join(s_exact, by = "n_features") %>% 
  mutate(pct_above_optimal = cost / cost_optimal - 1) %>% 
  select(solver, n_features, n_solutions, cost, time, pct_above_optimal)
write_csv(scenario_cost_time, "output/02_time-vs-n-features.csv")
raster::removeTmpFiles(h = 0)
gc()

# plots ----

# num features vs. time
g <- ggplot(scenario_cost_time) +
  aes(x = n_features, y = time / 60, color = solver) +
  geom_line() +
  geom_point() +
  labs(x = "# features",
       y = "Run time (minutes)",
       color = "Solver") +
  theme(legend.position = "bottom")
ggsave("figures/02_time-vs-n-features.png", g, width = 6, height = 6)

# num features vs. cost
g <- ggplot(scenario_cost_time) +
  aes(x = n_features, y = pct_above_optimal, color = solver) +
  geom_line() +
  geom_point() +
  scale_y_continuous(label = scales::percent) +
  labs(x = "# features",
       y = "% above optimal cost",
       color = "Solver") +
  theme(legend.position = "bottom")
ggsave("figures/02_time-vs-cost.png", g, width = 6, height = 6)
