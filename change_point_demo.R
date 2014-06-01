# Test for changepoint in means

cp_mean_sim <- c(rnorm(100, 0, 1), rnorm(100, 2, 1))

plot(cp_mean_sim)

cp_mean_results <- cp_mean_lhr(cp_mean_sim)

plot(cp_mean_results)

# Test for changepoint in variance

cp_var_sim <- c(rnorm(100, 0, 1), rnorm(100, 50, 3))

plot(cp_var_sim)

cp_var_results <- cp_var_lhr(cp_var_sim)

plot(cp_var_results)
