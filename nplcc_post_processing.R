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


# Post-processing
runs_long <- read_csv(here("output", "ilp-comparison-runs.csv"))

rl_filt <- runs_long %>% filter(n_features == 72 & n_pu == 148510) %>% group_by(solver, target) %>% 
  summarise(time = mean(time, na.rm = T),
            cost = mean(cost, na.rm = T))

ggplot(data=rl_filt, aes(x = target, y = time, group = solver)) +
  geom_line(aes(color=solver))+
  geom_point(aes(color=solver))

ggplot(data=rl_filt, aes(x = target, y = cost, group = solver)) +
  geom_line(aes(color=solver))+
  geom_point(aes(color=solver))
