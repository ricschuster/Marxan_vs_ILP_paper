library(raster)
library(foreach)
library(doParallel)
library(uuid)
library(here)
library(tidyverse)
library(prioritizr)
pkg_list <- c("raster", "prioritizr", "marxan", "uuid",  "here", "tidyverse")
select <- dplyr::select
walk(list.files("R", full.names = TRUE), source)
prioritizr_timed <- add_timer(prioritizr::solve)


#plot function
pp <- function(x, title = "", y.var) {
  #x.var <- enquo(x.var)
  y.var <- enquo(y.var)
  ggplot(x, 
         aes(x = target, y = !! y.var, colour = as.factor(n_pu) , shape = as.factor(n_features), 
             group = interaction(n_features, n_pu))) +
    scale_colour_discrete(name  ="Planning units") +
    scale_shape_discrete(name  ="Features") + 
    geom_line() +
    geom_point() + 
    ggtitle(title) +
    theme_bw()
}



# Post-processing
runs_long <- read_csv(here("output", "ilp-comparison-runs.csv"))
runs_long <- runs_long %>% mutate(solv_it = ifelse(!is.na(marxan_iterations), paste(solver, marxan_iterations, sep="_"), solver)) %>%
  filter(run_id < 200)

runs_gur <- runs_long %>% filter(solver == 'gurobi') %>% mutate(cost_gur = cost, time_gur = time) %>% select(run_id, cost_gur, time_gur)

runs_long <- inner_join(runs_long, runs_gur, by = "run_id")


rl_filt <- runs_long %>% filter(n_features == 72 & n_pu == 148510 & 
                                  (solver != 'marxan' | (marxan_iterations > 1E+07 & spf > 1))
) %>% group_by(solver, target) %>% 
  summarise(time = mean(time, na.rm = T),
            cost = mean(cost, na.rm = T),
            time_gur = mean(time_gur, na.rm = T),
            cost_gur = mean(cost_gur, na.rm = T))

rl_filt <- rl_filt %>%
  mutate(deltaC = (cost - cost_gur)/cost_gur * 100,
         deltaT = cost - cost_gur,
         deltaTM = (time - time_gur)/time_gur * 100,
         deltaTT = time - time_gur
  )


(fig1 <- ggplot(data=rl_filt, aes(x = target, y = deltaC, group = solver)) +
    ggtitle("Marxan - ILP: # features = 72; # pu's = 148510; # iterations = 1E+08 \n mean time + mean cost for Marxan") +
    ylab("Delta cost [%] with optimal cost as baseline") +
    geom_line(aes(color=solver))+
    geom_point(aes(color=solver)) +
    geom_text(aes(label = ifelse(deltaT > 1000000,
                                 as.character(paste0("$",round(deltaT/1000000,0),"M")),
                                 ifelse(solver == "gurobi", paste0("$",round(cost/1000000,0),"M"),""))), hjust = 0.5, vjust = -0.7) +
    scale_x_continuous("Target [%]", labels = as.character(rl_filt$target * 100), breaks = rl_filt$target) +
    theme_bw()
  
)


(fig2 <- ggplot(data=rl_filt, aes(x = target, y = deltaTM, group = solver)) +
    ggtitle("Marxan - ILP: # features = 72; # pu's = 148510; # iterations = 1E+08 \n mean time + mean cost for Marxan") +
    #ylab("Mean processing time [sec]") +
    geom_line(aes(color=solver))+
    geom_point(aes(color=solver)) +
    geom_text(aes(label = ifelse(solver == "gurobi", "",as.character(paste0(round(deltaTM/100,2),""))), hjust = 0.5, vjust = -0.7)) +
    
    scale_x_continuous("Target [%]", labels = as.character(rl_filt$target * 100), breaks = rl_filt$target) +
    scale_y_continuous("Differnce to fastest solver [multiplier of best time]", labels = as.character(c(0, 200, 400, 600)), breaks = c(0, 20000, 40000, 60000)) +
    theme_bw()
)


ggsave(here("figures","Figure 1.png"), fig1)
ggsave(here("figures","Figure 2.png"), fig2)


(deltas <- runs_long %>% mutate(deltaTM = (time - time_gur)/time_gur * 100,
                                deltaTT = time - time_gur,
                                deltaC = (cost - cost_gur)/cost_gur * 100
) %>% group_by(solver) %>% summarise(avg_time = mean(deltaTM)/100,
                                     max_time = max(deltaTM)/100,
                                     min_time = min(deltaTM)/100,
                                     avg_cost = mean(deltaC, na.rm = T),
                                     max_cost = max(deltaC, na.rm = T),
                                     min_cost = min(deltaC, na.rm = T))
) 


# Range of savings and time
runs_red <- runs_long %>% filter(run_id <200)

rr_marxan_compl <- runs_red %>% filter(solver == "gurobi" | solver == "rsymphony" | (solver == "marxan" & n_solutions == 10))
rr_marxan_compl <- rr_marxan_compl %>% group_by(solver)

rr_marxan_compl <- rr_marxan_compl %>% mutate(time_perc = (time - time_gur)/time_gur * 100,
                    cost_perc = (cost - cost_gur)/cost_gur * 100)

mm <- rr_marxan_compl %>% filter(solver == "marxan")
summary(mm$cost_perc)

mm <- rr_marxan_compl %>% filter(solver == "marxan" & marxan_iterations > 1E+07 & spf > 1 & spf < 125)
summary(mm$cost_perc)

pp(rr_marxan_compl %>% filter(solver == "marxan" & marxan_iterations > 1E+07 & spf > 1 & spf < 125), 
   "Marxan vs Gurobi; #iterations > 100,000; SPF = 5 or 25", cost_perc)

# pp(rr_marxan_compl %>% filter(solver == "rsymphony"), 
#    "RSYMPHONY vs Gurobi", cost_perc * 100)


#cost profiles
pp(rr_marxan_compl %>% filter(solver == "gurobi"), "Gurobi", cost)

pp(rr_marxan_compl %>% filter(solver == "rsymphony"), "SYMPHONY", cost)

pp(rr_marxan_compl %>% filter(solver == "marxan") %>% group_by(target, n_features, n_pu) %>% 
     summarise(cost = mean(cost)), "Marxan", cost)


#time

# mm <- rr_marxan_compl %>% filter(solver == "gurobi")
# summary(mm$time_perc/100)

mm <- rr_marxan_compl %>% filter(solver == "rsymphony")
summary(mm$time_perc/100)

mm <- rr_marxan_compl %>% filter(solver == "marxan")
summary(mm$time_perc/100)

mm <- rr_marxan_compl %>% filter(solver == "marxan" & marxan_iterations > 1E+07 & spf > 1 & spf < 125)
summary(mm$time_perc/100)

pp(rr_marxan_compl %>% filter(solver == "rsymphony"), "SYMPHONY", time_perc / 100)

pp(rr_marxan_compl %>% filter(solver == "marxan") %>% group_by(target, n_features, n_pu) %>% 
     summarise(time_perc = mean(time_perc)), "Marxan", time_perc / 100)



#time profiles
pp(rr_marxan_compl %>% filter(solver == "gurobi"), "Gurobi", time)

pp(rr_marxan_compl %>% filter(solver == "rsymphony"), "SYMPHONY", time)

pp(rr_marxan_compl %>% filter(solver == "marxan") %>% group_by(target, n_features, n_pu) %>% 
     summarise(time = mean(time)), "Marxan", time)




# Explore budget
# g1 <- raster(here("output/gurobi/", "gurobi_target-0.3_features-72_pu-148510.tif"))
# 
# runs_long <- read_csv(here("output", "ilp-comparison-runs.csv"))
# runs_long <- runs_long %>% mutate(solv_it = ifelse(!is.na(marxan_iterations), paste(solver, marxan_iterations, sep="_"), solver))
# 
# rl_filt <- runs_long %>% filter(n_features == 72 & n_pu == 148510 & 
#                                   (solver != 'marxan' | (marxan_iterations > 1E+07 & spf > 1))
# ) %>% group_by(solver, target) %>% 
#   summarise(time = mean(time, na.rm = T),
#             cost = mean(cost, na.rm = T))
# 
# rl_filt <- rl_filt %>%
#   mutate(deltaC = (cost - filter(rl_filt, solver == 'gurobi')$cost)/filter(rl_filt, solver == 'gurobi')$cost * 100,
#          deltaT = cost - filter(rl_filt, solver == 'gurobi')$cost,
#          deltaTM = (time - filter(rl_filt, solver == 'gurobi')$time)/filter(rl_filt, solver == 'gurobi')$time * 100,
#          deltaTT = time - filter(rl_filt, solver == 'gurobi')$time
#   )
# 
# budget <- filter(rl_filt, solver == "marxan" & target == "0.3")$cost
# 
# 
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
# occ <- select(cost_occ, -cost) %>% 
#   gather("species_code", "amount", -pu) %>% 
#   inner_join(species %>% select(species_code, species = id), 
#              by = "species_code") %>% 
#   select(pu, species, amount, name = species_code) %>% 
#   arrange(pu, species)
# rm(cost_occ)
# 
# # planning unit raster
# pus <- here("data", "nplcc_planning-units.tif") %>% 
#   raster() %>% 
#   # create an empty template
#   raster()
# 
# # setup runs ----
# ilp_gap <- 0.001
# target = 0.3
# n_features = 72
# n_pu = 148510
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
# # sample species and planning units
# features <- species %>% 
#   select(id, name = species_code) %>% 
#   sample_n(size = n_features, replace = FALSE) %>% 
#   arrange(id) %>% 
#   as.data.frame(stringsAsFactors = FALSE)
# cost_ss <- cost %>% 
#   sample_n(size = n_pu, replace = FALSE) %>% 
#   arrange(id)
# #r$species <- paste(features$name, collapse = ",")
# pu_ss <- pus
# pu_ss[cost_ss$id] <- 0
# rij <- filter(occ, species %in% features$id, pu %in% cost_ss$id) %>% 
#   arrange(pu, species)
# targets <- group_by(rij, species) %>% 
#   summarize(tot_amount = sum(amount, na.rm = TRUE)) %>% 
#   ungroup() %>% 
#   transmute(id = species, amount = target * tot_amount)
# features <- inner_join(features, targets, by = "id")
# 
# # ilp 
# p1 <- problem(cost_ss, 
#               features = features %>% select(id, name), 
#               rij = rij, 
#               cost_column = "cost") %>% 
#   add_min_set_objective() %>%
#   add_relative_targets(0.3) %>%
#   add_binary_decisions()
# # gurobi
# s_gur1 <- p1 %>% 
#   add_gurobi_solver(gap = ilp_gap) %>% 
#   prioritizr_timed()
# 
# g1 <- solution_to_raster(s_gur1$result, pus)
# 
# (cost_gurobi1 <- attr(s_gur1$result, "objective"))
# 
# 
# p <- problem(cost_ss, 
#              features = features %>% select(id, name), 
#              rij = rij, 
#              cost_column = "cost") %>% 
#   add_min_set_objective() %>%
#   add_relative_targets(0.31350) %>%
#   add_binary_decisions()
# # gurobi
# s_gur <- p %>% 
#   add_gurobi_solver(gap = ilp_gap) %>% 
#   prioritizr_timed()
# 
# g2 <- solution_to_raster(s_gur$result, pus)
# 
# (cost_gurobi <- attr(s_gur$result, "objective"))
# 
# # out.df <- data.frame(target = numeric(), cost = numeric())
# # for(ii in 1:20){
# #   t.tmp <- target + 0.01 + 0.001 *ii/4
# #   p1 <- p %>%
# #     add_relative_targets(t.tmp)   
# #   
# #   s_gur <- p1 %>% 
# #     add_gurobi_solver(gap = ilp_gap) %>% 
# #     prioritizr_timed()
# #   
# #   out.df[ii,] <- c(t.tmp, attr(s_gur$result, "objective"))
# # }
# c1 <- table(g1[])
# c2 <- table(g2[])
# 
# (delta <- table(g2[]) - table(g1[]))
# 
# tb1 <- tibble(pu = s_gur$result$id, g1 = s_gur1$result$solution_1, g2 = s_gur$result$solution_1)
# tb1$g1 <- g1[tb1$pu]
# 
# 
# tot_amount <- group_by(rij, species) %>% 
#   summarize(tot_amount = sum(amount, na.rm = TRUE))
# 
# rij_j <- inner_join(rij, tb1)
# 
# 
# amount_g1 <- rij_j %>% filter(g1 == 1) %>% group_by(species) %>% 
#   summarize(g1_amount = sum(amount, na.rm = TRUE))
# 
# amount_g2 <- rij_j %>% filter(g2 == 1) %>% group_by(species) %>% 
#   summarize(g2_amount = sum(amount, na.rm = TRUE))
# 
# out_tb <- tibble(species = tot_amount$species, tot_amount = tot_amount$tot_amount, 
#                  g1 = amount_g1$g1_amount, g2 = amount_g2$g2_amount) %>%
#   mutate(g1_perc = g1/tot_amount*100, g2_perc = g2/tot_amount*100, incr = g2_perc - g1_perc)


