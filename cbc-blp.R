# Initialization
## load packages


## define functions
solve_with_lexo_boundary_penalties <- function(x, data = NULL, gap = 0.1) {
  # assert arguments are valid
  assertthat::assert_that(
    inherits(x, "ConservationProblem"),
    assertthat::is.number(gap),
    assertthat::noNA(gap),
    inherits(x$objective, "MinimumSetObjective"),
    inherits(data, c("NULL", "Matrix")),
    number_of_zones(x) == 1)
  # prepare boundary matrix data
  ## generate data if needed
  if (is.null(data)) {
    data <- boundary_matrix(x$data$cost)
  }
  assertthat::assert_that(
    nrow(data) == x$number_of_total_units(),
    ncol(data) == x$number_of_total_units())
  ## extract data for planning units
  ## rescale values to help avoid numerical issues
  data <- Matrix::drop0(data)
  data@x <- scales::rescale(data@x, to = c(0.01, 100))
  # solve problem and find optimal solution for main objective
  x$solver$parameters$set("gap", gap)
  x$solver$parameters$set("verbose", 0L)
  sol1 <- prioritizr::solve(x, force = TRUE)  # compute optimal objective value
  opt_obj <- sum(x$data$cost$cost * sol1$solution_1, na.rm = T)
  # create new problem with for generating boundary length problem
  ## copy problem
  x2 <- prioritizr::pproto(NULL, x)
  ## set cost values to zero
  if (inherits(x2$data$cost, "RasterLayer")) {
    x2$data$cost <- x2$data$cost * 0
  } else if (inherits(x2$data$cost, "data.frame")) {
    for (i in x$data$cost_column) {
      x2$data$cost[[i]] <- x2$data$cost[[i]] * 0
    }
  } else {
    stop("cost data not recognized")
  }
  ## generate new problem with boundary penalties
  x2 <- add_boundary_penalties(x2, penalty = 1, data = data)
  ## compile optimization problem
  o2 <- prioritizr::compile(x2)
  ## extract model
  model <- list(
    modelsense = o2$modelsense(),
    vtype = o2$vtype(),
    obj = o2$obj(),
    A = o2$A(),
    rhs = o2$rhs(),
    sense = o2$sense(),
    lb = o2$lb(),
    ub = o2$ub())
  ## add additional constraint for primary objective
  new_a <- rep(0, length(model$obj))
  new_a[seq_len(x$number_of_planning_units())] <-
    c(x$planning_unit_costs()[, 1])
  model$A <- rbind(model$A, new_a)
  model$A <- Matrix::drop0(model$A)
  model$rhs <- c(model$rhs, opt_obj * (1 + gap))
  model$sense <- c(model$sense, "<=")
  ## prepare for solving with CBC
  ## initialize CBC arguments
  row_lb <- numeric(length(model$rhs))
  row_ub <- numeric(length(model$rhs))
  ## set equality constraints
  idx <- which(model$sense == "=")
  row_lb[idx] <- model$rhs[idx] - 1e-5
  row_ub[idx] <- model$rhs[idx] + 1e-5
  ## set lte constraints
  idx <- which(model$sense == "<=")
  row_lb[idx] <- -Inf
  row_ub[idx] <- model$rhs[idx]
  ## set gte constraints
  idx <- which(model$sense == ">=")
  row_lb[idx] <- model$rhs[idx]
  row_ub[idx] <- Inf
  # create problem
  model2 <- list(
    max = identical(model$modelsense, "max"),
    obj = model$obj,
    is_integer = model$vtype == "B",
    mat = model$A,
    col_lb = model$lb,
    col_ub = model$ub,
    row_lb = row_lb,
    row_ub = row_ub)
  ## create parameters
  p <- list(
    LOG = "1",
    PRESOLVE = "On",
    RATIO = as.character(gap),
    SEC = as.character(.Machine$integer.max),
    TIMEM = "ELAPSED",
    THREADS = as.character(x$solver$parameters$get("threads")))
  ## solve problem
  x3 <- do.call(rcbc::cbc_solve, append(model2, list(cbc_args = p)))
  assertthat::assert_that(!x3$is_proven_infeasible)
  sol <- x3$column_solution[seq_len(x$number_of_planning_units())]
  # return data in correct format
  out <- x$data$cost
  if (inherits(out, "Raster")) {
    out[x$planning_unit_indices()] <- sol
  } else if (inherits(out, "data.frame")) {
    out$solution_1 <- sol
  } else {
    stop("planning unit data in format that has not been implemented")
  }
  out
}

# # Preliminary processing
# # import data
# data(sim_pu_raster, sim_features)
# 
# # Main processing
# ## define problem
# p <-
#   problem(sim_pu_raster, sim_features) %>%
#   add_min_set_objective() %>%
#   add_relative_targets(0.1) %>%
#   add_binary_decisions() %>%
#   add_cbc_solver(gap = 0.1, verbose = FALSE)
# 
# # generate solution without boundary penalties
# t1 <- system.time({
#   s1 <-
#     p %>%
#     solve()
# })
# 
# ## generate solution with standard boundary penalties
# t2 <- system.time({
#   s2 <-
#     p %>%
#     add_boundary_penalties(penalty = 20) %>%
#     solve()
# })
# 
# ## generate solution with lexicographic boundary penalties
# t3 <- system.time({
#   s3 <-
#     p %>%
#     solve_with_lexo_boundary_penalties(gap = 0.1)
# })
# 
# # Exports
# ## print times
# message("time with no penalties")
# print(t1)
# 
# message("time with standard penalties")
# print(t2)
# 
# message("time with hierarchical penalties")
# print(t3)
# 
# ## plot solutions
# plot(stack(s1, s2, s3))
