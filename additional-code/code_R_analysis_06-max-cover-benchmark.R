## restore session
session::restore.session(session_path("01"))
raster::rasterOptions(maxmemory = 1e+11, chunksize = 1e+10)

## benchmark parameters
max_cov_parameters <- "code/parameters/max-cov-benchmark.toml" %>%
                      RcppTOML::parseTOML() %>%
                      `[[`(MODE)

## load functions
source("code/R/functions/random_raster.R")

## simulate data
max_cov_data <- plyr::llply(
  seq_along(max_cov_parameters$number_planning_units),
  function(i) {
    # raster memory settings
    raster::rasterOptions(maxmemory = 1e+11, chunksize=1e+10)
    # create planning units
    curr_ncell <- as.integer(max_cov_parameters$number_planning_units[i])
    curr_pu <- random_raster(curr_ncell, fun = function(n) rep(1, n))
    raster::extent(curr_pu) <- c(0, 1, 0, 1)
    # simulate features with binary distributions
    curr_features <- prioritizr::simulate_species(curr_pu,
                       n = max_cov_parameters$number_features[i],
                       transform = function(x) round(stats::plogis(x)))
    # if some features have no occupied planning units then randomly assign
    # them to a single planning unit
    absent_pos <- which(raster::cellStats(curr_features, "max") == 0)
    for (j in absent_pos)
      curr_features[[j]][sample.int(curr_ncell, size = 1)] <- 1
    # add padding NAs around raster (see page 92 of Zonation manual)
    curr_pu %<>% raster::extend(c(1, 1))
    curr_features %<>% raster::extend(c(1, 1))
    # set extent so that cell resolutions are 1 x 1
    raster::extent(curr_pu) <- c(1, raster::ncol(curr_pu) + 1, 1,
                                 raster::nrow(curr_pu) + 1)
    raster::extent(curr_features) <- c(1, raster::ncol(curr_pu) + 1, 1,
                                       raster::nrow(curr_pu) + 1)
    # make CRS missing
    curr_pu@crs <- sp::CRS()
    raster::crs(curr_features) <- sp::CRS()
    # return list with data
    list(planning_units = curr_pu, features = curr_features)
})

## delete previous runs if present
if (file.exists("data/intermediate/benchmark/max-cov"))
  unlink("data/intermediate/benchmark/max-cov", recursive = TRUE)

## max cover problems
# prioritizr
max_cov_prioritizr_results <- plyr::ldply(seq_along(max_cov_data),
                                          function(i) {
  plyr::ldply(c("gurobi", "rsymphony", "lpsymphony"), function(j) {
    # build problem
    p <- prioritizr::problem(max_cov_data[[i]][[1]],
                             max_cov_data[[i]][[2]]) %>%
         prioritizr::add_max_cover_objective(max_cov_parameters$budget) %>%
         prioritizr::add_binary_decisions()
    # add solver
    if (j == "gurobi")
      p <- p %>% prioritizr::add_gurobi_solver(gap = 0, verbose = FALSE,
                                               threads =
                                                 general_parameters$threads)
    if (j == "rsymphony")
      p <- p %>% prioritizr::add_rsymphony_solver(gap = 0, verbose = FALSE)
    if (j == "lpsymphony")
      p <- p %>% prioritizr::add_lpsymphony_solver(gap = 0, verbose = FALSE)
    # solve problem
    result <- system.time(s <- prioritizr::solve(p))
    obj <- sum(raster::cellStats(s * max_cov_data[[i]][[2]], "sum") >= 1)
    # extract results
    data.frame(software = "prioritizr", method = j,
               number_planning_units =
                 max_cov_parameters$number_planning_units[i],
               number_features =
                 max_cov_parameters$number_features[i],
               time = result[[3]], obj = obj)
  })
})

# zonation
max_cov_zonation_results <- plyr::ldply(seq_along(max_cov_data), function(i) {
  # initialization
  curr_zdir <- paste0("data/intermediate/benchmark/max-cov/zonation/",
                      i, "/project")
  curr_ddir <- paste0("data/intermediate/benchmark/max-cov/zonation/",
                      i, "/data")
  dir.create(curr_zdir, showWarnings = FALSE, recursive = TRUE)
  dir.create(curr_ddir, showWarnings = FALSE, recursive = TRUE)
  # save data
  for (k in seq_len(raster::nlayers(max_cov_data[[i]][[2]])))
    raster::writeRaster(max_cov_data[[i]][[2]][[k]],
                        paste0(curr_ddir, "/spp_", k, ".tif"),
                        NAflag = -9999, overwrite = TRUE)
  # create zonation project
  curr_proj <- zonator::create_zproject(
    name = basename(curr_zdir), dir = dirname(curr_zdir),
    weight = 1, alpha = 1, bqp = 0, bqp_p = 0,
    variants = "maxcov", spp_template_dir = curr_ddir, overwrite = TRUE)
  # configure zonation settings
  zparam_file <- dir(curr_zdir, "^.*\\.dat$", full.names = TRUE,
                     recursive = TRUE)
  cat(paste0("[Settings]\nremoval rule = 1\nwarp factor = 1\n",
             "edge removal = 0\nannotate name = 0\n",
             "use boundary quality penalty = 0\nBLP = 0\n",
             "BQP mode = 1\n"),
      file = zparam_file)
  # run zonation
  result <- system.time(system(
    paste0(normalizePath(Sys.which("zig4")), " -r ",
           zparam_file, " ", raster::extension(zparam_file, ".spp"), " ",
           raster::extension(zparam_file, ""), "_out/01.txt 0.0 0 1.0 1 ",
           "--removal-rule=1 --warp-factor=1 ",
           "--grid-output-formats=compressed-tif --image-output-formats=png")))
  # load zonation solution
  s_raw <- raster::raster(paste0(raster::extension(zparam_file, ""),
                                 "_out/01.rank.compressed.tif"))
  # convert to binary solution using budget
  s <- sort(na.omit(raster::values(s_raw)))
  s <- s[length(s) - max_cov_parameters$budget]
  s <- raster::Which(s_raw >= s, na.rm = FALSE)
  # calculate solution objective
  obj <- sum(raster::cellStats(s * max_cov_data[[i]][[2]], "sum") >= 1)
  # extract results
  data.frame(software = "Zonation", method = "",
             number_planning_units =
               max_cov_parameters$number_planning_units[i],
             number_features =
               max_cov_parameters$number_features[i],
            time = result[[3]], obj = obj)
})

## save session
session::save.session(session_path("06"), compress = "xz")
