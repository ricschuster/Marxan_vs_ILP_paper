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


# Post-processing
runs_long <- read_csv(here("output2", "ilp-comparison-runs2.csv"))

runs <- runs_long %>% filter(solver == 'gurobi') %>% mutate(cost_gur = cost, time_gur = time) %>% 
  select(target, n_features, n_pu, cost_gur, time_gur) %>%
  mutate(cost_cpl = runs_long %>% filter(solver == 'cplex') %>% pull(cost),
         time_cpl = runs_long %>% filter(solver == 'cplex') %>% pull(time)) %>%
  mutate(delta_cost = (cost_cpl - cost_gur) / cost_gur * 100,
         delta_time = (time_cpl - time_gur) / time_gur * 100)

(fig1 <- ggplot(data = runs, aes(x = target, y = delta_time, color = as.factor(n_pu), shape = as.factor(n_features))) +
    ggtitle("Gurobi - CPLEX comparison no boundary") +
    ylab("Deviation from Gurobi time [%] (< 0 == faster)") +
    geom_line(aes(color=as.factor(n_pu)))+
    geom_point(aes(color=as.factor(n_pu)), size = 3) +
    scale_shape_discrete(name  ="n features") + 
    # geom_text(aes(label = ifelse(deltaT > 1000000,
    #                              as.character(format(round(deltaT/1000000,0), big.mark=",")),
    #                              ifelse(solver == "gurobi", format(round(cost/1000000,0), big.mark=","),""))), hjust = 0.5, vjust = -0.7) +
    scale_x_continuous("Target [%]", labels = as.character(runs$target), breaks = runs$target) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    theme(legend.position = c(0.1, 0.6)) +
    theme(legend.background = element_rect(fill="white",
                                           size=0.5, linetype="solid", 
                                           colour ="black")) +
    geom_hline(yintercept=0, linetype="dashed")

)

ggsave(here("figures","Benchmark_figure1.png"), fig1)


runs_blm <- read_csv(here("output_blm_c", "ilp-comparison-runs_no_marx.csv"))
runsb <- runs_blm %>% filter(solver == 'gurobi') %>% mutate(cost_gur = cost, time_gur = time) %>% 
  select(target, n_features, n_pu, blm, cost_gur, time_gur) %>%
  mutate(cost_cpl = runs_blm %>% filter(solver == 'cplex') %>% pull(cost),
         time_cpl = runs_blm %>% filter(solver == 'cplex') %>% pull(time)) %>%
  mutate(delta_cost = (cost_cpl - cost_gur) / cost_gur * 100,
         delta_time = (time_cpl - time_gur) / time_gur * 100)


(fig2 <- ggplot(data = runsb, aes(x = target, y = delta_time, color = as.factor(blm))) +
    ggtitle("Gurobi - CPLEX comparison no boundary") +
    ylab("Deviation from Gurobi time [%] (< 0 == faster)") +
    geom_line(aes(color=as.factor(blm)))+
    # geom_text(aes(label = ifelse(deltaT > 1000000,
    #                              as.character(format(round(deltaT/1000000,0), big.mark=",")),
    #                              ifelse(solver == "gurobi", format(round(cost/1000000,0), big.mark=","),""))), hjust = 0.5, vjust = -0.7) +
    scale_x_continuous("Target [%]", labels = as.character(runs$target), breaks = runs$target) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    theme(legend.position = c(0.9, 0.8)) +
    theme(legend.background = element_rect(fill="white",
                                           size=0.5, linetype="solid", 
                                           colour ="black")) +
    geom_hline(yintercept=0, linetype="dashed")
  
)

ggsave(here("figures","Benchmark_figure2.png"), fig2)


#############################################################################
# BLM comparison figure

# natural earth political boundaries
# natural earth political boundaries
ne_land <- read_sf("data/ne-land.gpkg") %>% st_geometry()
ne_country_lines <- read_sf("data/ne-country-lines.gpkg") %>% st_geometry()
ne_state_lines <- read_sf("data/ne-state-lines.gpkg") %>% st_geometry()

gr_rast <- stack(list.files(here("output_blm_c/gurobi/"), full.names = TRUE))
cp_rast <- stack(list.files(here("output_blm_c/cplex/"), full.names = TRUE))

land <- st_transform(ne_land, crs = proj4string(gr_rast))
country <- st_transform(ne_country_lines, crs = proj4string(gr_rast))
state <- st_transform(ne_state_lines, crs = proj4string(gr_rast))

e <- extent(560000, 560000 + 22500, 5300000 - 22500, 5300000)
gr_rast <- crop(gr_rast, e)
cp_rast <- crop(cp_rast, e)

mybb <- cbind(x=c(560000, 560000 + 22500, 560000 + 22500, 560000), 
              y=c(5300000 - 22500, 5300000 - 22500, 5300000, 5300000))
mybb <- SpatialPolygons(list(Polygons(list(Polygon(mybb)),"1")), proj4string=CRS(proj4string(gr_rast)))


here("figures", paste0("Benchmark_figure3", ".png")) %>%
  png(width = 2000, height = 3000, res = 300)

ll <- list()
for(ii in 1:5){
  ll <- c(ll, gr_rast[[ii]], cp_rast[[ii]])
  
}
out_r <- stack(ll)

par(mfrow=c(5,2))
par(mar = c(0.1, 0.1, 0.1, 0.1), oma = c(0,3.5,1.5,0), bg = "white")

# titl <- c("Gurobi", "Symphony", "Marxan",
#           rep("", 12))
# ylab <- c(0.1, "", "",
#           1, "", "",
#           10, "", "",
#           100, "", "",
#           1000, "", "")


for (ii in 1:nlayers(out_r)) {
  # print map
  plot(land, col = "white", border = NA, xlim = e[1:2], ylim = e[3:4])
  #title(titl[ii], cex.main = 1.5)
  palette <- c("Greens", "Blues", "YlOrRd", "Reds")
  pal <- brewer.pal(9, palette[1])[2:9]
  pal <- colorRampPalette(pal)
  #pal <- colorQuantile(pal, values(abd_plot[[f1[ii]]]), n = 8, #probs = seq(0, 1, length.out = n + 1),
  #              na.color = "#808080", alpha = FALSE, reverse = FALSE)
  plot(out_r[[ii]], add = TRUE, col = pal(256), legend = FALSE, 
       maxpixels = ncell(out_r))
  # if(lh[[ii]]){
  #   add_legend("", pal, legend_offsets[3], low_high = lh[ii],
  #              text_col = text_col)
  # }
  
  # boundaries
  plot(mybb, lwd = 1, add = TRUE)
  plot(state, col = "black", lwd = 0.5, lty = 1, add = TRUE)
  plot(country, col = "black", lwd = 1, add = TRUE)
  
  # # title
  # # plot bounds
  usr <- par("usr")
  xwidth <- usr[2] - usr[1]
  yheight <- usr[4] - usr[3]
  # labels
  # text(x = usr[1] - 0.1 * xwidth, y = usr[3] + 0.5 * yheight,
  # labels = ylab[ii], pos = 4, font = 1, cex = 1.5 , col = "black")
  # 
  # text(x = usr[1] + 0.1 * xwidth, y = usr[3] + 0.4 * yheight,
  #      labels = pll[ii], pos = 4, font = 1, cex = 1.5 * scl, col = text_col)
  # 
  #rasterImage(logo,usr[1] + 0.01 * xwidth, usr[3] + 0.03 * yheight,
  #            usr[1] + 0.38 * xwidth, usr[3] + 0.09 * yheight)
  
}

mtext("Gurobi", side=3, at = 0.25, cex=1, col="black", outer=TRUE) 
mtext("CPLEX", side=3, at = 0.75, cex=1, col="black", outer=TRUE) 

mtext("0.1", side=2, at = 0.9, cex=1, col="black", outer=TRUE, las = 1) 
mtext("1", side=2, at = 0.7, cex=1, col="black", outer=TRUE, las = 1) 
mtext("10", side=2, at = 0.5, cex=1, col="black", outer=TRUE, las = 1) 
mtext("100", side=2, at = 0.3, cex=1, col="black", outer=TRUE, las = 1) 
mtext("1,000", side=2, at = 0.1, cex=1, col="black", outer=TRUE, las = 1) 

dev.off()
############################################################################# 







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


# Post-processing BLM
runs_long <- read_csv(here("output_blm/", "ilp-comparison-runs.csv"))
runs_long <- runs_long %>% mutate(solv_it = ifelse(!is.na(marxan_iterations), paste(solver, marxan_iterations, sep="_"), solver)) 

runs_gur <- runs_long %>% filter(solver == 'gurobi') %>% mutate(cost_gur = cost, time_gur = time) %>% select(run_id, cost_gur, time_gur)

runs_long <- inner_join(runs_long, runs_gur, by = "run_id")


# rl_filt <- runs_long %>% group_by(solver, target, blm) %>% 
#  summarise(time = mean(time, na.rm = T),
#            cost = mean(cost, na.rm = T),
#            time_gur = mean(time_gur, na.rm = T),
#            cost_gur = mean(cost_gur, na.rm = T))

rl_filt <- runs_long %>%
 mutate(deltaC = (cost - cost_gur)/cost_gur * 100,
        deltaT = cost - cost_gur,
        deltaTM = (time - time_gur)/time_gur * 100,
        deltaTT = time - time_gur
 )


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


# Explore budget
g1 <- raster(here("output/gurobi/", "gurobi_target-0.3_features-72_pu-37128.tif"))

runs_long <- read_csv(here("output", "ilp-comparison-runs.csv"))
runs_long <- runs_long %>% mutate(solv_it = ifelse(!is.na(marxan_iterations), paste(solver, marxan_iterations, sep="_"), solver))

rl_filt <- runs_long %>% filter(n_features == 72 & n_pu == 37128 & 
                                  (solver != 'marxan' | (marxan_iterations == 1E+07 & spf > 1))
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
n_pu = 37128

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
  prioritizr_timed(force = TRUE)

g1 <- solution_to_raster(s_gur1$result, pus)

(cost_gurobi1 <- attr(s_gur1$result, "objective"))


p <- problem(cost_ss, 
             features = features %>% select(id, name), 
             rij = rij, 
             cost_column = "cost") %>% 
  add_min_set_objective() %>%
  add_relative_targets(0.31908) %>%
  add_binary_decisions()
# gurobi
s_gur <- p %>% 
  add_gurobi_solver(gap = ilp_gap) %>% 
  prioritizr_timed(force = TRUE)

g2 <- solution_to_raster(s_gur$result, pus)

(cost_gurobi <- attr(s_gur$result, "objective")) - budget

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




#####################
#### BLM with cost, but best solution only

nn <- cost_occ
nn[cost_occ$pu] <- cost_occ$cost

setwd(here("output_blm/gurobi/"))
gr <- stack(list.files())
gr_cst <- gr * nn
gr_df <- as.data.frame(gr_cst)
gr_cs <- colSums(gr_df, na.rm = TRUE)
names(gr_cs) <- names(gr)

gr_out_df <- data.frame(solver = str_split(names(gr_cs), pattern = "_", simplify = TRUE)[,1],
                     target = as.numeric(str_split(str_split(names(gr_cs), pattern = "target.", simplify = TRUE)[,2],
                                                   pattern = "_featu", simplify = TRUE)[,1]),
                     blm = as.numeric(str_split(names(gr_cs), pattern = "blm.", simplify = TRUE)[,2]),
                     dollar_cost = as.vector(gr_cs))




setwd(here("output_blm/rsymphony"))
sy <- stack(list.files())
sy_cst <- sy * nn
sy_df <- as.data.frame(sy_cst)
sy_cs <- colSums(sy_df, na.rm = TRUE)
names(sy_cs) <- names(sy)
sy_out_df <- data.frame(solver = str_split(names(sy_cs), pattern = "_", simplify = TRUE)[,1],
                        target = as.numeric(str_split(str_split(names(sy_cs), pattern = "target.", simplify = TRUE)[,2],
                                                      pattern = "_featu", simplify = TRUE)[,1]),
                        blm = as.numeric(str_split(names(sy_cs), pattern = "blm.", simplify = TRUE)[,2]),
                        dollar_cost = as.vector(sy_cs))



setwd(here("output_blm2/marxan"))
ma <- stack(list.files(pattern = ".tif$"))
ma_cst <- ma * nn
ma_df <- as.data.frame(ma_cst)
ma_cs <- colSums(ma_df, na.rm = TRUE)
names(ma_cs) <- names(ma)

ma_out_df <- data.frame(solver = str_split(names(ma_cs), pattern = "_", simplify = TRUE)[,1],
                        target = as.numeric(str_split(str_split(names(ma_cs), pattern = "target.", simplify = TRUE)[,2],
                                                      pattern = "_featu", simplify = TRUE)[,1]),
                        blm = as.numeric(str_split(str_split(names(ma_cs), pattern = "blm.", simplify = TRUE)[,2], 
                                                   pattern = "_spf", simplify = TRUE)[,1]),
                        dollar_cost = as.vector(ma_cs))


out_df <- rbind(gr_out_df, sy_out_df, ma_out_df)
out_df %>% arrange(solver, blm, solver) %>% write_csv(here("output_blm", "out_df_dollar_cost.csv"))
