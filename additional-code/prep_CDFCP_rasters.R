library(raster)

wd <- getwd()
inwd <- paste0(wd, "/data/CDFCP/")
outwd <- paste0(wd, "/data/CDFCP_tif/")
outwd2 <- paste0(wd, "/data/CDFCP_tif2/")

setwd(inwd)


fls <- gsub("./", "", list.dirs())[-1]
fls <- fls[!fls %in% "info"]


for(ii in fls){
  tmp <- raster(ii)
  writeRaster(tmp, filename=paste0(outwd, ii, ".tif"), format="GTiff", overwrite=TRUE)
  rm(tmp)
}


setwd(outwd)
files <- list.files(pattern = ".tif$")
setwd(outwd2)
file.old <- list.files(pattern = ".tif$")
file.rename(file.old,files)

for(ii in 1:length(files)){
  
  print(files[ii])
  flush.console()
  
  system(paste("gdalwarp -r near"
               ,paste0(outwd,files[ii])
               ,paste0(outwd2,files[ii]),sep=" "))
}
  