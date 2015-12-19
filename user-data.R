init_user_data <- function() {
    structure(list(
        start_session = Sys.time(),
        end_session = .POSIXct(NA),
        start_test = .POSIXct(NA),
        end_test = .POSIXct(NA),
        gender = NA_character_,
        age = NA_integer_,
        hash = NA_character_,
        responses = structure(ordered(integer(n), levels = seq_along(unique(answers))), names = paste0("Q", seq_len(n))),
        resp_time = structure(.POSIXct(rep(NA, n)), names = paste0("Q", seq_len(n))),
        scales = structure(integer(nrow(scales)), names = scales$short),
        indexes = structure(integer(nrow(indexes)), names = indexes$short)),
        class = "UserEntry")
}

check_user_data <- function(data) {
    !is.na(data$start_test) && !is.na(data$end_test) && all(data$scales != 0) && all(data$indexes != 0)
}
