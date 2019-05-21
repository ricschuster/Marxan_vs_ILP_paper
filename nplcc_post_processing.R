library(raster)
library(foreach)
library(doParallel)
library(uuid)
library(here)
library(tidyverse)
library(prioritizr)
library(marxan)
pkg_list <- c("raster", "prioritizr", "marxan", "uuid",  "here", "tidyverse")
select <- dplyr::select
walk(list.files("R", full.names = TRUE), source)
prioritizr_timed <- add_timer(prioritizr::solve)

# # load nplcc data ----
# 
# # species list
# species <- here("data", "nplcc_species.csv") %>% 
#   read_csv() %>% 
#   mutate(id = as.integer(id))
# 
# # cost and occupancy
# nplcc_file <- here("data", "nplcc_cost_occupancy.zip")
# if (!file.exists(nplcc_file)) {
#   "https://s3.amazonaws.com/marxan-vs-ilp/nplcc_cost_occupancy.zip" %>% 
#     download.file(destfile = nplcc_file)
# }
# cost_occ <- read_csv(nplcc_file, 
#                      col_types = cols(.default = col_double(),
#                                       pu = col_integer()))
# 
# # split out cost and occupancy
# cost <- select(cost_occ, id = pu, cost) %>% 
#   arrange(id)
# occ <- select(cost_occ, -cost) 
# #rm(cost_occ)
# 
# # planning unit raster
# pus <- here("data", "nplcc_planning-units.tif") %>% 
#   raster() %>% 
#   # create an empty template
#   raster()
# 
# # setup runs ----
# 
# # define run matrix
# marxan_runs <- expand.grid(
#   marxan_iterations = c(1e4, 1e5, 1e6, 1e7, 1e8),
#   spf = 5^(0:3)
# )
# runs <- expand.grid(target = seq(0.1, 0.9, by = 0.1),
#                     n_features = round(seq(10, 72, length.out = 5)),
#                     n_pu = round(nrow(cost) / 4^(4:2))) %>%
#   # add marxan specific parameters
#   mutate(marxan = list(marxan_runs),
#          run_id = row_number()) %>%
#   select(run_id, everything())
# 
# # # for testing
# # marxan_runs <- expand.grid(marxan_iterations = c(1e5, 1e6), spf = c(5, 25))
# # runs <- expand.grid(target = c(0.25, 0.5),
# #                     n_features = c(10, nrow(species)),
# #                     n_pu = round(nrow(cost) / 4^c(4, 3))) %>%
# #   # add marxan specific parameters
# #   mutate(marxan = list(marxan_runs),
# #          run_id = row_number()) %>%
# #   select(run_id, everything())
# 
# # fixed run parameters
# ilp_gap <- 0.1
# marxan_reps <- 10
# random_subset <- TRUE
# sysname <- tolower(Sys.info()[["sysname"]])
# marxan_path <- switch(sysname, 
#                       windows = here("marxan", "Marxan_x64.exe"), 
#                       darwin = here("marxan", "MarOpt_v243_Mac64"), 
#                       linux = here("marxan", "MarOpt_v243_Linux64")
# )
# stopifnot(file.exists(marxan_path))
# 
# 
# # convert vector of selected units to raster using template
# solution_to_raster <- function(x, y) {
#   x <- filter(x, solution_1 == 1) %>% 
#     pull(id)
#   y[x] <- 1
#   return(y)
# }
# 
# set.seed(1)
# 
# 
# cost_ss <- cost %>% 
#   sample_n(size = r$n_pu, replace = FALSE) %>% 
#   arrange(id)
# 

# Post-processing
runs_long <- read_csv(here("output", "ilp-comparison-runs.csv"))
runs_long <- runs_long %>% mutate(solv_it = ifelse(!is.na(marxan_iterations), paste(solver, marxan_iterations, sep="_"), solver))

rl_filt <- runs_long %>% filter(n_features == 72 & n_pu == 148510 & 
                                  (solver != 'marxan' | (marxan_iterations > 1E+07 & spf > 1))
) %>% group_by(solver, target) %>% 
  summarise(time = mean(time, na.rm = T),
            cost = mean(cost, na.rm = T))

rl_filt <- rl_filt %>%
  mutate(deltaC = (cost - filter(rl_filt, solver == 'gurobi')$cost)/filter(rl_filt, solver == 'gurobi')$cost * 100,
         deltaT = cost - filter(rl_filt, solver == 'gurobi')$cost,
         deltaTM = (time - filter(rl_filt, solver == 'gurobi')$time)/filter(rl_filt, solver == 'gurobi')$time * 100,
         deltaTT = time - filter(rl_filt, solver == 'gurobi')$time
         )


(p1 <- ggplot(data=rl_filt, aes(x = target, y = time, group = solver)) +
    ggtitle("Marxan - ILP: # features = 72; # pu's = 148510; # iterations = 1E+08 \n mean time + mean cost for Marxan") +
    ylab("Mean processing time [sec]") +
    geom_line(aes(color=solver))+
    geom_point(aes(color=solver)) +
    scale_x_continuous("Target [%]", labels = as.character(rl_filt$target * 100), breaks = rl_filt$target)    
)

(p2 <- ggplot(data=rl_filt, aes(x = target, y = deltaTM, group = solver)) +
    ggtitle("Marxan - ILP: # features = 72; # pu's = 148510; # iterations = 1E+08 \n mean time + mean cost for Marxan") +
    #ylab("Mean processing time [sec]") +
    geom_line(aes(color=solver))+
    geom_point(aes(color=solver)) +
    geom_text(aes(label = ifelse(solver == "gurobi", "",as.character(paste0(round(deltaTM/100,2),""))), hjust = 0.5, vjust = -0.7)) +
    
    scale_x_continuous("Target [%]", labels = as.character(rl_filt$target * 100), breaks = rl_filt$target) +
    scale_y_continuous("Differnce to fastest solver [multiplier of best time]", labels = as.character(c(0, 200, 400, 600)), breaks = c(0, 20000, 40000, 60000))
)


(p3 <- ggplot(data=rl_filt, aes(x = target, y = cost, group = solver)) +
    ggtitle("Marxan - ILP: # features = 72; # pu's = 148510; # iterations = 1E+08 \n mean time + mean cost for Marxan") +
    ylab("Solution cost [$]") +
    geom_line(aes(color=solver))+
    geom_point(aes(color=solver))+
    scale_x_continuous("Target [%]", labels = as.character(rl_filt$target * 100), breaks = rl_filt$target)
)

(p4 <- ggplot(data=rl_filt, aes(x = target, y = deltaC, group = solver)) +
    ggtitle("Marxan - ILP: # features = 72; # pu's = 148510; # iterations = 1E+08 \n mean time + mean cost for Marxan") +
    ylab("Delta cost [%] with optimal cost as baseline") +
    geom_line(aes(color=solver))+
    geom_point(aes(color=solver)) +
    geom_text(aes(label = ifelse(deltaT > 1000000,
                                 as.character(paste0("$",round(deltaT/1000000,0),"M")),
                                 ifelse(solver == "gurobi", paste0("$",round(cost/1000000,0),"M"),""))), hjust = 0.5, vjust = -0.7) +
    scale_x_continuous("Target [%]", labels = as.character(rl_filt$target * 100), breaks = rl_filt$target)
  
)



rl_filt <- runs_long %>% filter(n_features == 72 & n_pu == 148510 & 
                                  (solver != 'marxan' | (marxan_iterations > 10000 & spf > 1))) %>% 
  group_by(solv_it, target) %>% 
  summarise(time = mean(time, na.rm = T),
            cost = mean(cost, na.rm = T))

rl_filt <- rl_filt %>%
  mutate(deltaC = (cost - filter(rl_filt, solv_it == 'gurobi')$cost)/filter(rl_filt, solv_it == 'gurobi')$cost * 100,
         deltaT = cost - filter(rl_filt, solv_it == 'gurobi')$cost,
         deltaTM = (time - filter(rl_filt, solv_it == 'gurobi')$time)/filter(rl_filt, solv_it == 'gurobi')$time * 100,
         deltaTT = time - filter(rl_filt, solv_it == 'gurobi')$time
  )


(p5 <- ggplot(data=rl_filt, aes(x = target, y = time, group = solv_it)) +
    ggtitle("Marxan - ILP: # features = 72; # pu's = 148510; # iterations > 10k \n mean time + mean cost for Marxan") +    ylab("Mean processing time [sec]") +
    geom_line(aes(color=solv_it))+
    geom_point(aes(color=solv_it)) +
    scale_x_continuous("Target [%]", labels = as.character(rl_filt$target * 100), breaks = rl_filt$target)    
)

(p6 <- ggplot(data=rl_filt, aes(x = target, y = deltaTM, group = solv_it)) +
    ggtitle("Marxan - ILP: # features = 72; # pu's = 148510; # iterations = 1E+08 \n mean time + mean cost for Marxan") +
    #ylab("Mean processing time [sec]") +
    geom_line(aes(color=solv_it))+
    geom_point(aes(color=solv_it)) +
    geom_text(aes(label = ifelse(solv_it == "gurobi", "",as.character(paste0(round(deltaTM/100,2),""))), hjust = 0.5, vjust = -0.7)) +
    
    scale_x_continuous("Target [%]", labels = as.character(rl_filt$target * 100), breaks = rl_filt$target) +
    scale_y_continuous("Differnce to fastest solver [multiplier of best time]", labels = as.character(c(0, 200, 400, 600)), breaks = c(0, 20000, 40000, 60000))
)


(p7 <- ggplot(data=rl_filt, aes(x = target, y = cost, group = solv_it)) +
    ggtitle("Marxan - ILP: # features = 72; # pu's = 148510; # iterations > 10k \n mean time + mean cost for Marxan") +    ylab("Solution cost [$]") +
    geom_line(aes(color=solv_it))+
    geom_point(aes(color=solv_it))+
    scale_x_continuous("Target [%]", labels = as.character(rl_filt$target * 100), breaks = rl_filt$target)
)

(p8 <- ggplot(data=rl_filt, aes(x = target, y = deltaC, group = solv_it)) +
    ggtitle("Marxan - ILP: # features = 72; # pu's = 148510; # iterations > 10k \n mean time + mean cost for Marxan") +
    ylab("Delta cost [%] with optimal cost as baseline") +
    geom_line(aes(color=solv_it))+
    geom_point(aes(color=solv_it)) +
    scale_x_continuous("Target [%]", labels = as.character(rl_filt$target * 100), breaks = rl_filt$target)
  
)



rl_filt <- runs_long %>% filter(n_features == 72 & n_pu == 594040 & 
                                  (solver != 'marxan' | (marxan_iterations > 1E+07 & spf > 1))
) %>% group_by(solver, target) %>% 
  summarise(time = mean(time, na.rm = T),
            cost = mean(cost, na.rm = T))

rl_filt <- rl_filt %>%
  mutate(deltaC = (cost - filter(rl_filt, solver == 'gurobi')$cost)/filter(rl_filt, solver == 'gurobi')$cost * 100,
         deltaT = cost - filter(rl_filt, solver == 'gurobi')$cost,
         deltaTM = (time - filter(rl_filt, solver == 'gurobi')$time)/filter(rl_filt, solver == 'gurobi')$time * 100,
         deltaTT = time - filter(rl_filt, solver == 'gurobi')$time
  )


(p9 <- ggplot(data=rl_filt, aes(x = target, y = time, group = solver)) +
    ggtitle("Marxan - ILP: # features = 72; # pu's = 594040; # iterations = 1E+08 \n mean time + mean cost for Marxan") +
    ylab("Mean processing time [sec]") +
    geom_line(aes(color=solver))+
    geom_point(aes(color=solver)) +
    scale_x_continuous("Target [%]", labels = as.character(rl_filt$target * 100), breaks = rl_filt$target)    
)

(p10 <- ggplot(data=rl_filt, aes(x = target, y = deltaTM, group = solver)) +
    ggtitle("Marxan - ILP: # features = 72; # pu's = 594040; # iterations = 1E+08 \n mean time + mean cost for Marxan") +
    #ylab("Mean processing time [sec]") +
    geom_line(aes(color=solver))+
    geom_point(aes(color=solver)) +
    geom_text(aes(label = ifelse(solver == "gurobi", "",as.character(paste0(round(deltaTM/100,2),""))), hjust = 0.5, vjust = -0.7)) +
    
    scale_x_continuous("Target [%]", labels = as.character(rl_filt$target * 100), breaks = rl_filt$target) +
    scale_y_continuous("Differnce to fastest solver [multiplier of best time]", labels = as.character(c(0, 200, 400, 600)), breaks = c(0, 20000, 40000, 60000))
)


(p11 <- ggplot(data=rl_filt, aes(x = target, y = cost, group = solver)) +
    ggtitle("Marxan - ILP: # features = 72; # pu's = 594040; # iterations = 1E+08 \n mean time + mean cost for Marxan") +
    ylab("Solution cost [$]") +
    geom_line(aes(color=solver))+
    geom_point(aes(color=solver))+
    scale_x_continuous("Target [%]", labels = as.character(rl_filt$target * 100), breaks = rl_filt$target)
)

(p12 <- ggplot(data=rl_filt, aes(x = target, y = deltaC, group = solver)) +
    ggtitle("Marxan - ILP: # features = 72; # pu's = 594040; # iterations = 1E+08 \n mean time + mean cost for Marxan") +
    ylab("Delta cost [%] with optimal cost as baseline") +
    geom_line(aes(color=solver))+
    geom_point(aes(color=solver)) +
    geom_text(aes(label = ifelse(deltaT > 1000000,
                                 as.character(paste0("$",round(deltaT/1000000,0),"M")),
                                 ifelse(solver == "gurobi", paste0("$",round(cost/1000000,0),"M"),""))), hjust = 0.5, vjust = -0.7) +
    scale_x_continuous("Target [%]", labels = as.character(rl_filt$target * 100), breaks = rl_filt$target)
  
)



ggsave(here("figures","p1.png"), p1)
ggsave(here("figures","p2.png"), p2)
ggsave(here("figures","p3.png"), p3)
ggsave(here("figures","p4.png"), p4)
ggsave(here("figures","p5.png"), p5)
ggsave(here("figures","p6.png"), p6)
ggsave(here("figures","p7.png"), p7)
ggsave(here("figures","p8.png"), p8)
ggsave(here("figures","p9.png"), p9)
ggsave(here("figures","p10.png"), p10)
ggsave(here("figures","p11.png"), p11)
ggsave(here("figures","p12.png"), p12)


(deltas <- runs_long %>% mutate(deltaTM = (time - filter(runs_long, solver == 'gurobi')$time)/filter(runs_long, solver == 'gurobi')$time * 100,
                             deltaTT = time - filter(runs_long, solver == 'gurobi')$time,
                             deltaC = (cost - filter(runs_long, solver == 'gurobi')$cost)/filter(runs_long, solver == 'gurobi')$cost * 100
                             ) %>% group_by(solver) %>% summarise(avg_time = mean(deltaTM)/100,
                                                                  max_time = max(deltaTM)/100,
                                                                  min_time = min(deltaTM)/100,
                                                                  avg_cost = mean(deltaC, na.rm = T),
                                                                  max_cost = max(deltaC, na.rm = T),
                                                                  min_cost = min(deltaC, na.rm = T))
) 



# Explore budget
g1 <- raster(here("output/gurobi/", "gurobi_target-0.3_features-72_pu-148510.tif"))

runs_long <- read_csv(here("output", "ilp-comparison-runs.csv"))
runs_long <- runs_long %>% mutate(solv_it = ifelse(!is.na(marxan_iterations), paste(solver, marxan_iterations, sep="_"), solver))

rl_filt <- runs_long %>% filter(n_features == 72 & n_pu == 148510 & 
                                  (solver != 'marxan' | (marxan_iterations > 1E+07 & spf > 1))
) %>% group_by(solver, target) %>% 
  summarise(time = mean(time, na.rm = T),
            cost = mean(cost, na.rm = T))

rl_filt <- rl_filt %>%
  mutate(deltaC = (cost - filter(rl_filt, solver == 'gurobi')$cost)/filter(rl_filt, solver == 'gurobi')$cost * 100,
         deltaT = cost - filter(rl_filt, solver == 'gurobi')$cost,
         deltaTM = (time - filter(rl_filt, solver == 'gurobi')$time)/filter(rl_filt, solver == 'gurobi')$time * 100,
         deltaTT = time - filter(rl_filt, solver == 'gurobi')$time
  )

budget <- filter(rl_filt, solver == "marxan" & target == "0.3")$cost


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
ilp_gap <- 0.001
target = 0.3
n_features = 72
n_pu = 148510

# convert vector of selected units to raster using template
solution_to_raster <- function(x, y) {
  x <- filter(x, solution_1 == 1) %>% 
    pull(id)
  y[x] <- 1
  return(y)
}

set.seed(1)

# sample species and planning units
features <- species %>% 
  select(id, name = species_code) %>% 
  sample_n(size = n_features, replace = FALSE) %>% 
  arrange(id) %>% 
  as.data.frame(stringsAsFactors = FALSE)
cost_ss <- cost %>% 
  sample_n(size = n_pu, replace = FALSE) %>% 
  arrange(id)
#r$species <- paste(features$name, collapse = ",")
pu_ss <- pus
pu_ss[cost_ss$id] <- 0
rij <- filter(occ, species %in% features$id, pu %in% cost_ss$id) %>% 
  arrange(pu, species)
targets <- group_by(rij, species) %>% 
  summarize(tot_amount = sum(amount, na.rm = TRUE)) %>% 
  ungroup() %>% 
  transmute(id = species, amount = target * tot_amount)
features <- inner_join(features, targets, by = "id")

# ilp 
p1 <- problem(cost_ss, 
             features = features %>% select(id, name), 
             rij = rij, 
             cost_column = "cost") %>% 
  add_min_set_objective() %>%
  add_relative_targets(0.3) %>%
  add_binary_decisions()
# gurobi
s_gur1 <- p1 %>% 
  add_gurobi_solver(gap = ilp_gap) %>% 
  prioritizr_timed()

g1 <- solution_to_raster(s_gur1$result, pus)

(cost_gurobi1 <- attr(s_gur1$result, "objective"))


p <- problem(cost_ss, 
             features = features %>% select(id, name), 
             rij = rij, 
             cost_column = "cost") %>% 
  add_min_set_objective() %>%
  add_relative_targets(0.31350) %>%
  add_binary_decisions()
# gurobi
s_gur <- p %>% 
  add_gurobi_solver(gap = ilp_gap) %>% 
  prioritizr_timed()

g2 <- solution_to_raster(s_gur$result, pus)

(cost_gurobi <- attr(s_gur$result, "objective"))

# out.df <- data.frame(target = numeric(), cost = numeric())
# for(ii in 1:20){
#   t.tmp <- target + 0.01 + 0.001 *ii/4
#   p1 <- p %>%
#     add_relative_targets(t.tmp)   
#   
#   s_gur <- p1 %>% 
#     add_gurobi_solver(gap = ilp_gap) %>% 
#     prioritizr_timed()
#   
#   out.df[ii,] <- c(t.tmp, attr(s_gur$result, "objective"))
# }
c1 <- table(g1[])
c2 <- table(g2[])

(delta <- table(g2[]) - table(g1[]))

tb1 <- tibble(pu = s_gur$result$id, g1 = s_gur1$result$solution_1, g2 = s_gur$result$solution_1)
tb1$g1 <- g1[tb1$pu]


tot_amount <- group_by(rij, species) %>% 
  summarize(tot_amount = sum(amount, na.rm = TRUE))

rij_j <- inner_join(rij, tb1)


amount_g1 <- rij_j %>% filter(g1 == 1) %>% group_by(species) %>% 
  summarize(g1_amount = sum(amount, na.rm = TRUE))

amount_g2 <- rij_j %>% filter(g2 == 1) %>% group_by(species) %>% 
  summarize(g2_amount = sum(amount, na.rm = TRUE))

out_tb <- tibble(species = tot_amount$species, tot_amount = tot_amount$tot_amount, 
                 g1 = amount_g1$g1_amount, g2 = amount_g2$g2_amount) %>%
                  mutate(g1_perc = g1/tot_amount*100, g2_perc = g2/tot_amount*100, incr = g2_perc - g1_perc)
  




##############
## run times
##############

runs_complete <- runs_long %>% filter(run_id <200)
(time_sum <- runs_complete %>% group_by(solver) %>% summarise(min_t = min(time),
                                                              mean_t = mean(time),
                                                              max_t = max(time)))


rr <- runs_complete %>% filter(solver == "gurobi") 
ss <- rr %>% group_by(n_features, n_pu)

ggplot(runs_complete %>% filter(solver == "gurobi"), aes(target, time, colour = as.factor(n_pu), group = interaction(n_features, n_pu))) +
  geom_line()
