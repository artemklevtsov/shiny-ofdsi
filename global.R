library(ggplot2)

theme_set(theme_minimal())

source("load-data.R")
source("process.R")

get_answers <- function(labels) {
    ids <- paste0("ans", seq_along(labels))
    div(
        lapply(seq_along(labels), function(i) {
            actionButton(ids[i], labels[i], class = "btn-block", style = "text-align: left;")
        })
    )
}

init_entry <- function() {
    structure(list(
            start_session = Sys.time(),
            end_session = .POSIXct(NA),
            start_test = .POSIXct(NA),
            end_test = .POSIXct(NA),
            gender = NA_character_,
            age = NA_integer_,
            hash = NA_character_,
            answers = structure(ordered(integer(n), levels = seq_along(unique(answers))), names = paste0("Q", seq_len(n))),
            resp_time = structure(.POSIXct(rep(NA, n)), names = paste0("Q", seq_len(n))),
            scales = structure(integer(nrow(scales)), names = scales$short),
            indexes = structure(integer(nrow(indexes)), names = indexes$short)),
        class = "UserEntry")
}

source("db.R")

# v <- list()
# v$user <- init_entry()
# v$user$answers[] <- sample(levels(user_data$answers), length(user_data$answers), TRUE)
# v$user$start_test <- Sys.time() - 720
# v$user$end_test <- Sys.time()
# v$user$resp_time[] <- seq(v$user$start_test, v$user$end_test, length.out = n)
# v$user <- get_scores(v$user, keys, formulas)

#get_types(v$user)
#plot_types(v$user)
