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
    # ggtitle("Marxan - ILP: # features = 72; # pu's = 148510; # iterations = 1E+08 \n mean time + mean cost for Marxan") +
    ylab("Delta cost [%] with optimal cost as baseline") +
    geom_line(aes(color=solver))+
    geom_point(aes(color=solver)) +
    geom_text(aes(label = ifelse(deltaT > 1000000,
                                 as.character(format(round(deltaT/1000000,0), big.mark=",")),
                                 ifelse(solver == "gurobi", format(round(cost/1000000,0), big.mark=","),""))), hjust = 0.5, vjust = -0.7) +
    scale_x_continuous("Target [%]", labels = as.character(rl_filt$target * 100), breaks = rl_filt$target) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    theme(legend.position = c(0.1, 0.85)) +
    theme(legend.background = element_rect(fill="white",
                                           size=0.5, linetype="solid", 
                                           colour ="black"))
  
)


(fig2 <- ggplot(data=rl_filt, aes(x = target, y = deltaTM, group = solver)) +
    # ggtitle("Marxan - ILP: # features = 72; # pu's = 148510; # iterations = 1E+08 \n mean time + mean cost for Marxan") +
    #ylab("Mean processing time [sec]") +
    geom_line(aes(color=solver))+
    geom_point(aes(color=solver)) +
    geom_text(aes(label = ifelse(solver == "gurobi", "",as.character(paste0(round(deltaTM/100,0),""))), hjust = 0.5, vjust = -0.7)) +
    
    scale_x_continuous("Target [%]", labels = as.character(rl_filt$target * 100), breaks = rl_filt$target) +
    scale_y_continuous("Differnce to fastest solver [multiplier of best time]", labels = as.character(c(0, 200, 400, 600)), breaks = c(0, 20000, 40000, 60000)) +
    theme_bw()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    theme(legend.position = c(0.1, 0.85)) +
    theme(legend.background = element_rect(fill="white",
                                           size=0.5, linetype="solid", 
                                           colour ="black"))
  
)


ggsave(here("figures","Figure 1.png"), fig1)
ggsave(here("figures","Figure 2.png"), fig2)


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



