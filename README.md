# Marxan vs Integer Linear Progamming Paper

Comparison of simulated annealing (with Marxan) versus integer linear programming (ILP; with the R package `prioritizr`) for solving conservation prioritizaiton problems. For the ILP prioritizations, we use both Gurobi and Symphony, to provide examples of closed- and open-source ILP solvers, respectively.

The main analsyis occurs in the following scripts:

- `01_time-vs-iterations.r`: increasing the number of simulated annealing iterations when running Marxan leads to lower cost solutions. In this script, we vary the number of iterations and see how it affects both cost (relative to the true optimum found with Gurobi) and execution time.
- `02_n-features.r`: in general, problems with more features are more challening to solve, leading to longer execution times. This script explores how cost and execution time are influenced by the number of features, for the different solvers.