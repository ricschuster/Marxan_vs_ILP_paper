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
occ <- select(cost_occ, -cost) 
#rm(cost_occ)

# planning unit raster
pus <- here("data", "nplcc_planning-units.tif") %>% 
  raster() %>% 
  # create an empty template
  raster()

# setup runs ----

# define run matrix
marxan_runs <- expand.grid(
  marxan_iterations = c(1e4, 1e5, 1e6, 1e7, 1e8),
  spf = 5^(0:3)
)
runs <- expand.grid(target = seq(0.1, 0.9, by = 0.1),
                    n_features = round(seq(10, 72, length.out = 5)),
                    n_pu = round(nrow(cost) / 4^(4:2))) %>%
  # add marxan specific parameters
  mutate(marxan = list(marxan_runs),
         run_id = row_number()) %>%
  select(run_id, everything())

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
ilp_gap <- 0.1
marxan_reps <- 10
random_subset <- TRUE
sysname <- tolower(Sys.info()[["sysname"]])
marxan_path <- switch(sysname, 
                      windows = here("marxan", "Marxan_x64.exe"), 
                      darwin = here("marxan", "MarOpt_v243_Mac64"), 
                      linux = here("marxan", "MarOpt_v243_Linux64")
)
stopifnot(file.exists(marxan_path))


# convert vector of selected units to raster using template
solution_to_raster <- function(x, y) {
  x <- filter(x, solution_1 == 1) %>% 
    pull(id)
  y[x] <- 1
  return(y)
}

set.seed(1)


cost_ss <- cost %>% 
  sample_n(size = r$n_pu, replace = FALSE) %>% 
  arrange(id)


# Post-processing
runs_long <- read_csv(here("output", "ilp-comparison-runs.csv"))

rl_filt <- runs_long %>% filter(n_features == 72 & n_pu == 148510) %>% group_by(solver, target) %>% 
  summarise(time = mean(time, na.rm = T),
            cost = median(cost, na.rm = T)) %>%
  mutate(deltaC = (cost - filter(rl_filt, solver == 'gurobi')$cost)/filter(rl_filt, solver == 'gurobi')$cost * 100)
  

p1 <- ggplot(data=rl_filt, aes(x = target, y = time, group = solver)) +
  ggtitle("Marxan - ILP: # features = 72; # pu's = 148510; mean time + meadian cost for Marxan") +
  ylab("Mean processing time [sec]") +
  geom_line(aes(color=solver))+
  geom_point(aes(color=solver))

ggsave(here("figures","p1.png"), p1)

p2 <- ggplot(data=rl_filt, aes(x = target, y = cost, group = solver)) +
  ggtitle("Marxan - ILP: # features = 72; # pu's = 148510; mean time + meadian cost for Marxan") +
  ylab("Solution cost [$]") +
  geom_line(aes(color=solver))+
  geom_point(aes(color=solver))

ggsave(here("figures","p2.png"), p2)

p3 <- ggplot(data=rl_filt, aes(x = target, y = deltaC, group = solver)) +
  ggtitle("Marxan - ILP: # features = 72; # pu's = 148510; mean time + meadian cost for Marxan") +
  ylab("Delta cost [%] with optimal cost as baseline") +
  geom_line(aes(color=solver))+
  geom_point(aes(color=solver))

ggsave(here("figures","p3.png"), p3)

