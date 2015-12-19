library(ggplot2)

theme_set(theme_minimal())

source("load-data.R")
source("process.R")
source("db.R")
source("user-data.R")

# v <- list()
# v$user_data <- init_user_data()
# v$user_data$responses[] <- sample(levels(v$user_data$responses), length(v$user_data$responses), TRUE)
# v$user_data$start_test <- Sys.time() - 720
# v$user_data$end_test <- Sys.time()
# v$user_data$resp_time[] <- seq(v$user_data$start_test, v$user_data$end_test, length.out = n)
# v$user_data <- get_scores(v$user_data)

# get_types(v$user_data)
# get_values(v$user_data)
# plot_types(v$user_data)
