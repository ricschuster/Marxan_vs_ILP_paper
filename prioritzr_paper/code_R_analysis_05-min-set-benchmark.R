## restore session
session::restore.session(session_path("01"))

## benchmark parameters
min_set_parameters <- "code/parameters/min-set-benchmark.toml" %>%
                      RcppTOML::parseTOML() %>%
                      `[[`(MODE)

## load functions
source("code/R/functions/random_raster.R")

## simulate data
min_set_data <- plyr::llply(
  seq_along(min_set_parameters$number_planning_units),
  function(i) {
    # create planning units
    curr_ncell <- as.integer(min_set_parameters$number_planning_units[i])
    curr_pu <- random_raster(curr_ncell, fun = function(n) rep(1, n))
    raster::extent(curr_pu) <- c(0, 1, 0, 1)
    # simulate features
    curr_features <- prioritizr::simulate_species(curr_pu,
                       n = min_set_parameters$number_features[i])
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
if (file.exists("data/intermediate/benchmark/min-set"))
  unlink("data/intermediate/benchmark/min-set", recursive = TRUE)

## minimum set problems
# prioritizr
min_set_prioritizr_results <- plyr::ldply(seq_along(min_set_data),
                                          function(i) {
  plyr::ldply(c("gurobi", "rsymphony", "lpsymphony"), function(j) {
    # build problem
    p <- prioritizr::problem(min_set_data[[i]][[1]],
                             min_set_data[[i]][[2]]) %>%
         prioritizr::add_min_set_objective() %>%
         prioritizr::add_relative_targets(min_set_parameters$targets) %>%
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
    # extract results
    data.frame(software = "prioritizr", method = j,
               number_planning_units =
                 min_set_parameters$number_planning_units[i],
               number_features =
                 min_set_parameters$number_features[i],
               time = result[[3]], obj = attr(s, "objective"))
  })
})

# marxan
if (general_parameters$threads > 1) {
  clust <- parallel::makeCluster(general_parameters$threads, "PSOCK")
  res <- parallel::clusterEvalQ(clust, library(magrittr))
  res <- parallel::clusterExport(clust, c("min_set_data", "min_set_parameters"),
                                 envir = environment())
  doParallel::registerDoParallel(clust)
}

min_set_marxan_results <- plyr::ddply(
  .data = expand.grid(i = seq_along(min_set_data),
                      j = seq_along(min_set_parameters$marxan_iterations),
                      k = seq_along(min_set_parameters$marxan_spf)),
  .variables = c("i", "j", "k"),
  .parallel = isTRUE(general_parameters$threads > 1),
  .fun = function(x) {
    # initialization
    i <- x$i[[1]]
    j <- x$j[[1]]
    k <- x$k[[1]]
    # make directory
    curr_dir <- paste0("data/intermediate/benchmark/min-set/marxan/",
                       i,  "/", j, "/", k)
    dir.create(curr_dir, showWarnings = FALSE, recursive = TRUE)
    # prepare marxan options
    curr_opts <- marxan::MarxanOpts(NUMREPS = 10L, NUMITNS = 10L, BLM = 0,
                                    NCORES = 1L)
    curr_opts@NUMREPS <- as.integer(min_set_parameters$marxan_replicates[j])
    curr_opts@NUMITNS <- as.integer(min_set_parameters$marxan_iterations[j])
    if (curr_opts@NUMTEMP > curr_opts@NUMITNS)
      curr_opts@s <- as.integer(max(floor(curr_opts@NUMITNS * 0.2), 1))
    # prepare marxan data
    curr_pu <- data.frame(
      id = seq_len(min_set_parameters$number_planning_units[i]),
      cost = 1,
      status = 0L)
    curr_spp <- data.frame(
      id = seq_len(min_set_parameters$number_features[i]),
      target = unname(raster::cellStats(min_set_data[[i]][[2]], "sum") *
                      min_set_parameters$targets),
      spf = as.numeric(min_set_parameters$marxan_spf[k]),
      name = paste0("spp_", seq_len(min_set_parameters$number_features[i])),
      stringsAsFactors = FALSE)
    curr_rij <- data.frame(
      species = rep(seq_len(min_set_parameters$number_features[i]),
                    each = min_set_parameters$number_planning_units[i]),
      pu = rep(curr_pu[[1]], min_set_parameters$number_features[i]),
      amount = min_set_data[[i]][[2]] %>%
               raster::trim() %>%
               raster::as.data.frame(na.rm = TRUE) %>%
               tidyr::gather(species, amount) %>%
               `[[`(2)) %>%
      dplyr::mutate(species = as.integer(species), pu = as.integer(pu))
    curr_data <- marxan::MarxanData(pu = curr_pu, species = curr_spp,
                                    puvspecies = curr_rij,
                                    boundary = NULL)
    # write marxan files to disk
    options(scipen = 1000)
    dir.create(curr_dir, showWarnings = FALSE, recursive = TRUE)
    marxan::write.MarxanData(curr_data, dir = curr_dir)
    marxan::write.MarxanOpts(curr_opts, normalizePath(curr_dir),
                             normalizePath(curr_dir),
                             seed = sample.int(n = 10000L, size = 1L))
    options(scipen = 0)
    # copy marxan executable
    marxan_path <- system.file("bin/MarOpt_v243_Linux64", package = "marxan")
    file.copy(marxan_path, paste0(curr_dir, "/", basename(marxan_path)))
    system(paste0("chmod +x ", file.path(curr_dir, "/", basename(marxan_path))))
    # solve problem
    result <- system.time(withr::with_dir(
      curr_dir,
      out <- system(paste0("./", basename(marxan_path), " input.dat -s"))))
    # load results
    # extract results
    obj <- file.path(curr_dir, "output_sum.csv") %>%
           data.table::fread(data.table = FALSE) %>%
           dplyr::filter(Shortfall == 0) %$%
           min(Cost)
    # if no solution found with zero shortfall return NA
    if (is.null(obj) || !is.finite(obj))
      obj <- NA_real_
    data.frame(software = "Marxan",
               method = paste0(formatC(min_set_parameters$marxan_iterations[j],
                                       format = "d", big.mark = ","),
                               " iterations, ",
                               formatC(min_set_parameters$marxan_replicates[j],
                                       format = "d", big.mark = ","),
                               " replicates"),
               spf = min_set_parameters$marxan_spf[k],
               number_planning_units =
                 min_set_parameters$number_planning_units[i],
               number_features =
                 min_set_parameters$number_features[i],
               time = result[[3]], obj = obj)
}) %>% dplyr::select(-i, -j, -k)

if (general_parameters$threads > 1)
  parallel::stopCluster(clust)

# zonation
min_set_zonation_results <- plyr::ldply(seq_along(min_set_data), function(i) {
  # initialization
  curr_zdir <- paste0("data/intermediate/benchmark/min-set/zonation/",
                      i, "/project")
  curr_ddir <- paste0("data/intermediate/benchmark/min-set/zonation/",
                      i, "/data")
  dir.create(curr_zdir, showWarnings = FALSE, recursive = TRUE)
  dir.create(curr_ddir, showWarnings = FALSE, recursive = TRUE)
  # save data
  for (k in seq_len(raster::nlayers(min_set_data[[i]][[2]])))
    raster::writeRaster(min_set_data[[i]][[2]][[k]],
                        paste0(curr_ddir, "/spp_", k, ".tif"),
                        NAflag = -9999, overwrite = TRUE)
  # create zonation project
  curr_proj <- zonator::create_zproject(
    name = basename(curr_zdir), dir = dirname(curr_zdir),
    weight = 1, alpha = 1, bqp = 0, bqp_p = 0,
    cellrem = min_set_parameters$targets,
    variants = "target", spp_template_dir = curr_ddir, overwrite = TRUE)
  # configure zonation settings
  zparam_file <- dir(curr_zdir, "^.*\\.dat$", full.names = TRUE,
                     recursive = TRUE)
  cat(paste0("[Settings]\nremoval rule = 3\nwarp factor = 1\n",
             "edge removal = 0\nannotate name = 0\n",
             "use boundary quality penalty = 0\nBLP = 0\n",
             "BQP mode = 1\n"),
             file = zparam_file)
  # run zonation
  result <- system.time(system(
    paste0(normalizePath(Sys.which("zig4")), " -r ",
           zparam_file, " ", raster::extension(zparam_file, ".spp"), " ",
           raster::extension(zparam_file, ""), "_out/01.txt 0.0 0 1.0 1 ",
           "--removal-rule=3 --warp-factor=1 ",
           "--grid-output-formats=compressed-tif --image-output-formats=png")))
  # extract zonation rank where targets are violated
  obj <- readLines(paste0(raster::extension(zparam_file, ""),
                             "_out/01.run_info.txt"))
  obj <- grep("violated", obj, value = TRUE)[1]
  obj <- as.numeric(dplyr::last(strsplit(obj, " ", fixed = TRUE)[[1]]))
  # convert load zonation rank raster and convert to indices
  s_raw <- raster::raster(paste0(raster::extension(zparam_file, ""),
                                 "_out/01.rank.compressed.tif"))
  raster::values(s_raw) <- rank(raster::values(s_raw), na.last = "keep")
  # calculate number of planning units in solution at this rank
  obj <- max(floor(obj * min_set_parameters$number_planning_units[i]) - 1, 0)
  obj <- min_set_parameters$number_planning_units[i] - obj
  abs_targets <- raster::cellStats(min_set_parameters$targets *
                                   min_set_data[[i]][[2]], "sum", na.rm = TRUE)
  while(!all(raster::cellStats(raster::Which(s_raw >= obj) *
                               min_set_data[[i]][[2]], "sum") >=
             abs_targets)) {
    obj <- obj - 1
  }
  obj <- min_set_parameters$number_planning_units[i] - obj
  # extract results
  data.frame(software = "Zonation", method = "",
             number_planning_units =
               min_set_parameters$number_planning_units[i],
             number_features =
               min_set_parameters$number_features[i],
            time = result[[3]], obj = obj)
})

## save session
session::save.session(session_path("05"), compress = "xz")
