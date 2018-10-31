library(raster)
library(prioritizr)
library(marxan)
library(timethis) # devtools::install_github("mstrimas/timethis)
library(tidyverse)
walk(list.files("R", full.names = TRUE), source)
select <- dplyr::select
prioritizr_timed <- add_timer(prioritizr::solve)

# parameters ----

target <- 0.2
ilp_gap <- 0.1
marxan_reps <- 10L
marxan_iterations <- c(1e5, 1e6, 1e7, 1e8, 1e9)

# load data ----

cost <- raster("data/Cost.tif")
features <- stack(c("data/OF.tif", "data/SAV.tif"))
# mask out any cells that are NA in another layer
na_mask <- calc(is.na(stack(cost, features)), sum) == 0
cost <- mask(cost, na_mask, maskvalue = 0)
features <- mask(features, na_mask, maskvalue = 0)

# ilp ----

# exact
p <- problem(cost, features = features) %>% 
  add_min_set_objective() %>%
  add_relative_targets(target) %>%
  add_binary_decisions()
s_exact <- p  %>% 
  add_gurobi_solver(gap = 0.0001) %>% 
  solve()
cost_optimal <- attr(s_exact, "objective")
# gurobi
s_gurboi <- p  %>% 
  add_gurobi_solver(gap = ilp_gap) %>% 
  prioritizr_timed()
# rsymphony
s_rsymphony <- p  %>% 
  add_rsymphony_solver(gap = ilp_gap * cost_optimal) %>% 
  prioritizr_timed()
# times
ilp_times <- data_frame(
  solver = c("gurobi", "rsymphony"),
  time = c(s_gurboi$time["elapsed"], s_rsymphony$time["elapsed"]) / 60
)

# marxan ----

# options
m_opts <- MarxanOpts(BLM = 0, NCORES = 1L)
m_opts@NUMREPS <- as.integer(marxan_reps)
# data
m_data <- prepare_marxan_data(cost, features, target = target, spf = 10)
m_unsolved <- MarxanUnsolved(opts = m_opts, data = m_data)
# solve
solve_marxan <- function(x, problem) {
  problem@opts@NUMITNS <- as.integer(x)
  problem@opts@NUMTEMP <- as.integer(ceiling(problem@opts@NUMITNS * 0.2))  
  s <- time_this(solve(problem))
  # only keep solutions meeting all targets
  s$result <- filter(s$result@results@summary, Shortfall == 0)
  return(s)
}
s_marxan <- data_frame(n_iterations = marxan_iterations,
                       solver = "marxan") %>% 
  mutate(solution = map(n_iterations, solve_marxan, problem = m_unsolved)) %>% 
  mutate(n_solutions = map_dbl(solution, ~ nrow(.$result)),
         cost_min = map_dbl(solution, ~ min(.$result$Cost)),
         pct_above_optimal = cost_min / cost_optimal - 1,
         time = map_dbl(solution, ~ .$time["elapsed"]))
saveRDS(s_marxan, "output/01_marxan-runs_time-vs-iterations.rds")


# plots ----

# plot cost
g <- ggplot(s_marxan) +
  aes(x = n_iterations, y = pct_above_optimal) +
  geom_point() +
  geom_line() +
  scale_x_log10(labels = sci_notation) +
  scale_y_continuous(limits = c(0, max(s_marxan$pct_above_optimal)), 
                     labels = scales::percent) +
  labs(x = "# Marxan iterations",
       y = paste0("Cost (% above optimal cost = ", 
                  scales::dollar(cost_optimal), ")"))
ggsave("figures/01_cost-vs-iterations.png", g, width = 6, height = 6)
# plot execution time
g <- ggplot(s_marxan) +
  aes(x = n_iterations, y = time / 60) +
  geom_text(x = 1e6, y = 2, label = "cows") +
  geom_hline(data = ilp_times,
             aes(yintercept = time, color = solver)) +
  geom_line() +
  geom_point(aes(fill = pct_above_optimal), shape = 21, size = 2, 
             color = "transparent") +
  scale_x_log10(labels = sci_notation) +
  scale_color_brewer(palette = "Set1", direction = -1) +
  scale_fill_viridis_c() +
  labs(x = "# Marxan iterations",
       y = "Run time (minutes)",
       color = "Solver",
       fill = "% above optimal cost") +
  theme(legend.position = "bottom")
ggsave("figures/01_time-vs-iterations.png", g, width = 6, height = 6)
