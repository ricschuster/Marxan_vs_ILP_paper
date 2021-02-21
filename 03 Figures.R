library(raster)
library(foreach)
library(doParallel)
library(uuid)
library(here)
library(tidyverse)
library(prioritizr)
library(sf)
library(RColorBrewer)
library(gridExtra)

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
runs_long <- read_csv(here("output_cbc/", "ilp-comparison-runs2.csv")) %>%
  mutate(run_id = rep(1:(length(solver)/2), 2))

# runs_long <- runs_long %>% mutate(solv_it = ifelse(!is.na(marxan_iterations), paste(solver, marxan_iterations, sep="_"), solver)) %>%
#   filter(run_id < 200)

runs_cbc <- runs_long %>% filter(solver == 'cbc') %>% mutate(cost_cbc = cost, time_cbc = time) %>% 
  select(run_id, cost_cbc, time_cbc)

runs_long <- inner_join(runs_long, runs_cbc, by = "run_id")


rl_filt <- runs_long %>% filter(n_features == 72 & n_pu == 148510) %>% group_by(solver, target) %>% 
  summarise(time = mean(time, na.rm = T),
            cost = mean(cost, na.rm = T),
            time_cbc = mean(time_cbc, na.rm = T),
            cost_cbc = mean(cost_cbc, na.rm = T))

rl_filt <- rl_filt %>%
  mutate(deltaC = (cost - cost_cbc)/cost_cbc * 100,
         deltaT = cost - cost_cbc,
         deltaTM = (time - time_cbc)/time_cbc * 100,
         deltaTT = time - time_cbc
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
                                           colour ="black")) +
    geom_text(x=0.9, y=20, label="a)", size = 6) +
    ylim(0,55)
  
)


(fig2 <- ggplot(data=rl_filt, aes(x = target, y = deltaTM, group = solver)) +
    # ggtitle("Marxan - ILP: # features = 72; # pu's = 148510; # iterations = 1E+08 \n mean time + mean cost for Marxan") +
    #ylab("Mean processing time [sec]") +
    geom_line(aes(color=solver))+
    geom_point(aes(color=solver)) +
    # geom_text(aes(label = ifelse(solver == "gurobi", "",as.character(paste0(round(deltaTM/100,0),""))), 
    #               hjust = 0.5, vjust = -0.7)) +
    # 
    scale_x_continuous("Target [%]", labels = as.character(rl_filt$target * 100), breaks = rl_filt$target) +
    scale_y_continuous("Differnce to fastest solver [% difference to CBC]") +#, 
                       # labels = as.character(c(0, 100, 200)), breaks = c(0, 10000, 20000)) +
    theme_bw()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())# +
    # theme(legend.position = 'none') + #c(0.1, 0.85)) +
    # # theme(legend.background = element_rect(fill="white",
    # #                                        size=0.5, linetype="solid", 
    # #                                        colour ="black")) +
    # geom_text(x=0.9, y=6000, label="b)", size = 6)# +
    # ylim(0, 20000)
  
)

ff <-grid.arrange(fig1, fig2, nrow = 2, padding = 0)

ggsave(here("figures","Figure 1.png"), fig1)
ggsave(here("figures","Figure 2.png"), fig2)

ggsave(here("figures","Figure 1c.png"), ff)

# Range of savings and time
runs_red <- runs_long %>% filter(run_id <200)

rr_marxan_compl <- runs_red %>% filter(solver == "gurobi" | solver == "rsymphony" | (solver == "marxan" & n_solutions == 10))
rr_marxan_compl <- rr_marxan_compl %>% group_by(solver)

rr_marxan_compl <- rr_marxan_compl %>% mutate(time_perc = (time - time_cbc)/time_cbc * 100,
                    cost_perc = (cost - cost_cbc)/cost_cbc * 100)

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


### BLM

# Post-processing BLM results
runs_long <- read_csv(here("output_blm/", "ilp-comparison-runs.csv"))
runs_long <- runs_long %>% mutate(solv_it = ifelse(!is.na(marxan_iterations), paste(solver, marxan_iterations, sep="_"), solver)) 

runs_cbc <- runs_long %>% filter(solver == 'gurobi') %>% mutate(cost_cbc = dollar_cost, time_cbc = time) %>% select(run_id, cost_cbc, time_cbc)

runs_long <- inner_join(runs_long, runs_cbc, by = "run_id")


# rl_filt <- runs_long %>% group_by(solver, target, blm) %>% 
#  summarise(time = mean(time, na.rm = T),
#            cost = mean(cost, na.rm = T),
#            time_cbc = mean(time_cbc, na.rm = T),
#            cost_cbc = mean(cost_cbc, na.rm = T))

rl_filt <- runs_long %>%
 mutate(deltaC = (dollar_cost - cost_cbc)/cost_cbc * 100,
        deltaT = dollar_cost - cost_cbc,
        deltaTM = (time - time_cbc)/time_cbc * 100,
        deltaTT = time - time_cbc
 )


(fig3 <- ggplot(data=rl_filt, aes(x = target, y = deltaC, color = solver, shape = as.factor(blm))) +
   # ggtitle("Marxan - ILP: # features = 72; # pu's = 148510; # iterations = 1E+08 \n mean time + mean cost for Marxan") +
   ylab("Deviation from lowest objective function [%]") +
   geom_line(aes(color=solver))+
   geom_point(aes(color=solver)) +
   scale_shape_discrete(name  ="BLM") + 
   # geom_text(aes(label = ifelse(deltaT > 1000000,
   #                              as.character(format(round(deltaT/1000000,0), big.mark=",")),
   #                              ifelse(solver == "gurobi", format(round(cost/1000000,0), big.mark=","),""))), hjust = 0.5, vjust = -0.7) +
   scale_x_continuous("Target [%]", labels = as.character(rl_filt$target * 100), breaks = rl_filt$target) +
   theme_bw() +
   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
   theme(legend.position = c(0.1, 0.6)) +
   theme(legend.background = element_rect(fill="white",
                                          size=0.5, linetype="solid", 
                                          colour ="black")) +
    geom_text(x=0.9, y=80, label="a)", size = 6, color = "black")
 
)


(fig4 <- ggplot(data=rl_filt, aes(x = target, y = deltaTM, color = solver, shape = as.factor(blm))) +
   # ggtitle("Marxan - ILP: # features = 72; # pu's = 148510; # iterations = 1E+08 \n mean time + mean cost for Marxan") +
   #ylab("Mean processing time [sec]") +
   geom_line(aes(color=solver))+
   geom_point(aes(color=solver)) +
   # geom_text(aes(label = ifelse(solver == "gurobi", "",as.character(paste0(round(deltaTM/100,0),""))), hjust = 0.5, vjust = -0.7)) +
   
   scale_x_continuous("Target [%]", labels = as.character(rl_filt$target * 100), breaks = rl_filt$target) +
   scale_y_continuous("Differnce to fastest solver [multiplier of best time]", labels = as.character(c(0, 50, 100, 150, 200)), breaks = c(0, 5000, 10000, 15000, 20000)) +
   theme_bw()+
   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
   theme(legend.position = "none") + #c(0.1, 0.8)) +
   # theme(legend.background = element_rect(fill="white",
   #                                        size=0.5, linetype="solid", 
   #                                        colour ="black"))
   geom_text(x=0.9, y=12000, label="b)", size = 6, color = "black")
)

ff2 <-grid.arrange(fig3, fig4, nrow = 2)

ggsave(here("figures","Figure 3.png"), fig3)
ggsave(here("figures","Figure 4.png"), fig4)

ggsave(here("figures","Figure 2c.png"), ff2)


# natural earth political boundaries
# natural earth political boundaries
ne_land <- read_sf("data/ne-land.gpkg") %>% st_geometry()
ne_country_lines <- read_sf("data/ne-country-lines.gpkg") %>% st_geometry()
ne_state_lines <- read_sf("data/ne-state-lines.gpkg") %>% st_geometry()

gr_rast <- stack(list.files(here("output_blm/gurobi/"), full.names = TRUE))
sy_rast <- stack(list.files(here("output_blm/rsymphony/"), full.names = TRUE))
ma_rast <- stack(list.files(here("output_blm/marxan//"), pattern = "*.tif", full.names = TRUE))


land <- st_transform(ne_land, crs = proj4string(gr_rast))
country <- st_transform(ne_country_lines, crs = proj4string(gr_rast))
state <- st_transform(ne_state_lines, crs = proj4string(gr_rast))

e <- extent(560000, 560000 + 22500, 5300000 - 22500, 5300000)
gr_rast <- crop(gr_rast, e)
sy_rast <- crop(sy_rast, e)
ma_rast <- crop(ma_rast, e)

mybb <- cbind(x=c(560000, 560000 + 22500, 560000 + 22500, 560000), 
              y=c(5300000 - 22500, 5300000 - 22500, 5300000, 5300000))
mybb <- SpatialPolygons(list(Polygons(list(Polygon(mybb)),"1")), proj4string=CRS(proj4string(gr_rast)))

# BLM comparison figure
here("Figures", paste0("Figure S10", ".png")) %>%
  png(width = 2000, height = 3000, res = 300)

ll <- list()
for(ii in 1:5){
  ll <- c(ll, gr_rast[[ii]], sy_rast[[ii]], ma_rast[[ii]])
  
}
out_r <- stack(ll)

par(mfrow=c(5,3))
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

mtext("Gurobi", side=3, at = 0.16, cex=1, col="black", outer=TRUE) 
mtext("Symphony", side=3, at = 0.5, cex=1, col="black", outer=TRUE) 
mtext("Marxan", side=3, at = 0.825, cex=1, col="black", outer=TRUE) 

mtext("0.1", side=2, at = 0.9, cex=1, col="black", outer=TRUE, las = 1) 
mtext("1", side=2, at = 0.7, cex=1, col="black", outer=TRUE, las = 1) 
mtext("10", side=2, at = 0.5, cex=1, col="black", outer=TRUE, las = 1) 
mtext("100", side=2, at = 0.3, cex=1, col="black", outer=TRUE, las = 1) 
mtext("1,000", side=2, at = 0.1, cex=1, col="black", outer=TRUE, las = 1) 

dev.off()
# 

# ##### 
# Study area
# ##### 
CA_land <- readRDS("data/gadm36_CAN_1_sf.rds")
CA_land_red <- CA_land %>% filter(NAME_1 == "British Columbia")

US_land <- readRDS("data/gadm36_USA_1_sf.rds")
US_land_red <- US_land %>% filter(NAME_1 == "Washington" | NAME_1 == "Oregon")

ne_land <- rbind(CA_land_red, US_land_red) %>% st_geometry()
ne_lines <- st_cast(ne_land, "MULTILINESTRING")
# ne_country_lines <- read_sf("data/ne-country-lines.gpkg") %>% st_geometry()
# ne_state_lines <- read_sf("data/ne-state-lines.gpkg") %>% st_geometry()

study_area <- raster("data/study_area.tif")
 
crs <- crs(study_area)

e <- extent(study_area)
#e <- extent(abd_plot)
text_col <- "black"

# prepare vector layers
land <- st_transform(ne_land, crs = crs)
plot_line <- st_transform(ne_lines, crs = crs)
country <- st_transform(ne_country_lines, crs = crs)
state <- st_transform(ne_state_lines, crs = crs)

palette <- c("Greens", "Blues", "YlOrRd", "Reds")



##Plotting
here("figures/", paste0("Figure 0", ".png")) %>% 
  png(width = 3000, height = 3000, res = 300)


pal <- brewer.pal(5, "Set2")[1:5]
pal <- colorRampPalette(pal)
par(mar = c(0.1, 0.1, 0.1, 0.1), oma = c(0,0,0,0), bg = "light blue")
plot(land, col = "grey85", border = NA, xlim = e[1:2], ylim = e[3:4])

plot(study_area, add = TRUE, col = "green4", legend = FALSE, 
     maxpixels = ncell(study_area))

# boundaries
plot(plot_line, col = "black", lwd = 1, add = TRUE)
# plot(state, col = "black", lwd = 0.5, lty = 1, add = TRUE)
# plot(country, col = "black", lwd = 1, add = TRUE)

# title
# plot bounds
usr <- par("usr")
xwidth <- usr[2] - usr[1]
yheight <- usr[4] - usr[3]
# labels

# text(x = usr[1] + 0.6 * xwidth, y = usr[3] + 0.2 * yheight,
#      labels = "Study area", pos = 4, font = 1, cex = 1.2, col = text_col)

text(x = usr[1] + 0.75 * xwidth, y = usr[3] + 0.9 * yheight,
     labels = "British Columbia", pos = 4, font = 1, cex = 1.2, col = text_col)

text(x = usr[1] + 0.75 * xwidth, y = usr[3] + 0.4 * yheight,
     labels = "Washington", pos = 4, font = 1, cex = 1.2, col = text_col)

text(x = usr[1] + 0.75 * xwidth, y = usr[3] + 0.08 * yheight,
     labels = "Oregon", pos = 4, font = 1, cex = 1.2, col = text_col)

dev.off()

