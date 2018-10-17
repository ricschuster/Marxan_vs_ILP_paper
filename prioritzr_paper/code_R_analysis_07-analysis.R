## restore session
session::restore.session(session_path("02"))
load(session_path("03"))
load(session_path("04"))
load(session_path("05"))
load(session_path("06"))

## compile marxan case-study results
# load data
marxan_solution_raster_data <- "data/intermediate/marxan_solution.tif" %>%
                               raster::raster()

marxan_pu_raster_data <- raster::raster("data/intermediate/marxan_pu.tif")

# prioritization
marxan_prioritization_area <- marxan_solution_raster_data %>%
                              raster::cellStats("sum") %>%
                              `*`(prod(raster::res(marxan_pu_raster_data))) %>%
                              units::set_units(m^2)

marxan_land_area <- marxan_pu_raster_data %>%
                    {x <- .; raster::Which(is.finite(x))} %>%
                    raster::cellStats("sum") %>%
                    `*`(prod(raster::res(marxan_pu_raster_data))) %>%
                    units::set_units(m^2)

marxan_prioritization_proportion <- as.numeric(marxan_prioritization_area) /
                                    as.numeric(marxan_land_area)

## compile connectivity case-study results

## compile agricultural case-study results
# Dirichlet model results
agr_model_results <- anova(agr_forecast_model, agr_null_model) %>%
                     {data.frame(df = .$df[[2]], statistic = .$Difference[[2]],
                                 p.value = .[["Pr(>Chi)"]][[2]])}
agr_model_results$rmse <- agr_forecast_model %>%
                          predict(type = "response") %>%
                          {`-`(agr_forecast_model$data[, colnames(.)])} %>%
                          `^`(2) %>%
                          mean() %>%
                          sqrt()

## compile benchmark results
# min set problems
min_set_results <- min_set_marxan_results %>%
                   dplyr::group_by(number_planning_units, number_features,
                                   method) %>%
                   dplyr::slice(which.min(obj)) %>%
                   dplyr::ungroup() %>%
                   dplyr::select(-spf) %>%
                   data.frame() %>%
                   rbind(min_set_prioritizr_results) %>%
                   rbind(min_set_zonation_results) %>%
                   dplyr::mutate(problem = "Minimum set") %>%
                   dplyr::arrange(problem, software, number_features,
                                  number_planning_units) %>%
                   dplyr::group_by(number_planning_units, number_features) %>%
                   dplyr::mutate(optimal_obj = min(obj)) %>%
                   dplyr::mutate(gap = (abs(obj - optimal_obj) / obj) * 100) %>%
                   dplyr::mutate(gap_absolute = abs(obj - optimal_obj)) %>%
                   dplyr::ungroup() %>%
                   data.frame()

# max cover problems
max_cov_results <- max_cov_prioritizr_results %>%
                   rbind(max_cov_zonation_results) %>%
                   dplyr::mutate(problem = "Maximum coverage") %>%
                   dplyr::arrange(problem, software, number_features,
                                  number_planning_units) %>%
                   dplyr::group_by(number_planning_units, number_features) %>%
                   dplyr::mutate(optimal_obj = max(obj)) %>%
                   dplyr::mutate(gap = (abs(obj - optimal_obj) / obj) * 100) %>%
                   dplyr::mutate(gap_absolute = abs(obj - optimal_obj)) %>%
                   dplyr::ungroup() %>%
                   data.frame()

# results
benchmark_results <- rbind(min_set_results, max_cov_results)

## save session
session::save.session(session_path("07"), compress = "xz")
