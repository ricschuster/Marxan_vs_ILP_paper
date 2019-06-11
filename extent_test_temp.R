tt2 <- here("data", "nplcc_planning-units.tif") %>% 
  raster()


tt <- here("data", "nplcc_planning-units.tif") %>% 
  raster()

tt[] <- 1:ncell(tt)

e <- extent(560000, 560000 + 22500, 5300000 - 22500, 5300000)
tmp.r <- crop(tt, e)
