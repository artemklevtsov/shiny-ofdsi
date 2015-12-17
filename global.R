library(ggplot2)

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
            time = structure(.POSIXct(rep(NA, n)), names = paste0("Q", seq_len(n))),
            scales = structure(integer(nrow(scales)), names = scales$short),
            indexes = structure(integer(nrow(indexes)), names = indexes$short)),
        class = "UserEntry")
}

source("db.R")

#user_data <- init_entry()
#user_data$answers[] <- sample(levels(user_data$answers), length(user_data$answers), TRUE)

#user_data <- get_scores(user_data, keys, formulas)

#get_types(user_data)
#plot_types(user_data)
