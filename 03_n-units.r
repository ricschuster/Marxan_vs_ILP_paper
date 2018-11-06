library(raster)
library(prioritizr)
library(marxan)
library(foreach)
library(doParallel)
library(tidyverse)
registerDoParallel(4)
walk(list.files("R", full.names = TRUE), source)
select <- dplyr::select
prioritizr_timed <- add_timer(prioritizr::solve)


# parameters ----

target <- 0.2
ilp_gap <- 0.1
marxan_reps <- 10L
marxan_iterations <- 1e8
agg_fact <- 2^(0:4)
solvers <- c("gurobi", "rsymphony", "marxan")
scenarios <- expand.grid(solver = solvers, agg_factor = agg_fact, 
                         stringsAsFactors = FALSE)


# load data ----

cost <- raster("data/Cost.tif")
features <- stack(c("data/OF.tif", "data/SAV.tif"))
# mask out any cells that are NA in another layer
na_mask <- calc(is.na(stack(cost, features)), sum) == 0
cost <- mask(cost, na_mask, maskvalue = 0)
features <- mask(features, na_mask, maskvalue = 0)


# exact solutions ----

s_exact <- foreach (af = agg_fact, .combine = bind_rows) %dopar% {
  c_agg <- raster::aggregate(cost, fact = af, fun = sum)
  f_agg <- raster::aggregate(features, fact = af, fun = mean)
  s <- problem(c_agg, features = f_agg) %>% 
    add_min_set_objective() %>%
    add_relative_targets(target) %>%
    add_binary_decisions() %>% 
    add_gurobi_solver(gap = 0.0001) %>% 
    prioritizr_timed()
  data_frame(agg_factor = af,
             n_units = sum(!is.na(c_agg[])),
             time_exact = s$time["elapsed"],
             cost_optimal = attr(s$result, "objective"))
}


# solve scenarios ----

scenario_cost_time <- foreach(i = seq_len(nrow(scenarios)), 
                              .combine = bind_rows) %dopar% {
  r <- scenarios[i, ]
  # aggregate to reduce number of planning units
  c_agg <- raster::aggregate(cost, fact = r$agg_fact, fun = sum)
  f_agg <- raster::aggregate(features, fact = r$agg_fact, fun = mean)
  if (r$solver %in% c("gurobi", "rsymphony")) {
    p <- problem(c_agg, features = f_agg) %>% 
      add_min_set_objective() %>%
      add_relative_targets(target) %>%
      add_binary_decisions()
    if (r$solver == "gurobi") {
      p <- add_gurobi_solver(p, gap = ilp_gap)
    } else {
      cost_optimal <- s_exact %>% 
        filter(agg_factor == r$agg_factor) %>% 
        pull(cost_optimal)
      p <- add_rsymphony_solver(p, gap = ilp_gap * cost_optimal)
    }
    s <- prioritizr_timed(p)
    #r$selected <- list(Which(s$result == 1, cells = TRUE))
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
    m_data <- prepare_marxan_data(c_agg, f_agg, target = target, spf = 10)
    m_unsolved <- MarxanUnsolved(opts = m_opts, data = m_data)
    # solve
    s <- time_this(solve(m_unsolved))
    # lowest cost
    idx <- as.integer(gsub("[a-zA-Z]+", "", 
                           colnames(s$result@results@selections)))
    sel <- s$result@results@selections[s$result@results@best, , drop = TRUE]
    #r$selected <- list(idx[sel])
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
# save results
scenario_cost_time <- scenario_cost_time  %>% 
  inner_join(s_exact %>% select(-time_exact), by = "agg_factor") %>% 
  mutate(pct_above_optimal = cost / cost_optimal - 1) %>% 
  select(solver, agg_factor, n_units, time, n_solutions, cost, pct_above_optimal)
write_csv(scenario_cost_time, "output/03_time-vs-n-units.csv")


# plots ----

# num planning units vs. time
g <- ggplot(scenario_cost_time) +
  aes(x = n_units, y = time / 60, color = solver) +
  geom_line() +
  geom_point() +
  scale_x_log10(labels = sci_notation) +
  scale_color_brewer(palette = "Set1") +
  labs(x = "# planning units",
       y = "Run time (minutes)",
       color = "Solver") +
  theme(legend.position = "bottom", 
        plot.margin = unit(c(0.1, 0.2, 0.1, 0.1), units = "in"))
ggsave("figures/03_time-vs-n-units.png", g, width = 8, height = 6)

# num features vs. cost
g <- ggplot(scenario_cost_time) +
  aes(x = n_units, y = pct_above_optimal, color = solver) +
  geom_line() +
  geom_point() +
  scale_x_log10(labels = sci_notation) +
  scale_y_continuous(label = scales::percent) +
  scale_color_brewer(palette = "Set1") +
  labs(x = "# planning units",
       y = "% above optimal cost",
       color = "Solver") +
  theme(legend.position = "bottom", 
        plot.margin = unit(c(0.1, 0.2, 0.1, 0.1), units = "in"))
ggsave("figures/03_cost-vs-n-units.png", g, width = 8, height = 6)

raster::removeTmpFiles(h = 0)
gc()