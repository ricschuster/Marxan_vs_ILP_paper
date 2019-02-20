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

