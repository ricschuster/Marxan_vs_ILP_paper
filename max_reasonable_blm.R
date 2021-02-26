# load package
library(prioritizr)
library(prioritizrdata)

# load data
data(salt_pu, salt_features)

# create boundary matrix
bm <- boundary_matrix(salt_pu)

# create baseline problem
p1 <-
  problem(salt_pu, salt_features) %>%
  add_min_set_objective() %>%
  add_relative_targets(0.1) %>%
  add_binary_decisions()

# max BLM value
m1 <- (sum(c(p1$planning_unit_costs())) / min(bm@x)) + 1e-5
m2 <- (sum(c(p1$planning_unit_costs())) / sum(bm@x)) + 1e-5

# create problem with boundaries
p2 <- p1 %>% add_boundary_penalties(m1, data = bm)
p3 <- p1 %>% add_boundary_penalties(m2, data = bm)

# solve problems
s1 <- solve(p1)
s2 <- solve(p2)
s3 <- solve(p3)

# compare solution costs
eval_cost_summary(p1, s1)
eval_cost_summary(p1, s2)
eval_cost_summary(p1, s3)

# compare feature representation
eval_feature_representation_summary(p1, s1)
eval_feature_representation_summary(p1, s2)
eval_feature_representation_summary(p1, s3)

# plot solutions
plot(stack(s1, s2, s3))