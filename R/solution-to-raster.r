solution_to_raster <- function(x, y, ...) {
  x <- filter(x, solution_1 == 1) %>% 
    pull(id)
  y[x] <- 1
  return(y)
}