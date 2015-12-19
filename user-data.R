UserData <- R6::R6Class(
    classname = "UserData",
    cloneable = FALSE,
    class = FALSE,
    portable = FALSE,
    public = list(
        start_session = .POSIXct(NA),
        end_session = .POSIXct(NA),
        start_test = .POSIXct(NA),
        end_test = .POSIXct(NA),
        gender = NA_character_,
        age = NA_character_,
        responses = NA_integer_,
        resp_time = .POSIXct(NA),
        scales = NA_integer_,
        indexes = NA_integer_,
        initialize = function(n = 150, keys, formulas) {
            self$start_session <- Sys.time()
            self$responses <- structure(ordered(integer(n), levels = seq_along(unique(answers))),
                                        names = paste0("Q", seq_len(n)))
            self$resp_time <- structure(.POSIXct(rep(NA, n)), names = paste0("Q", seq_len(n)))
            self$scales <- structure(integer(length(keys)), names = names(keys))
            self$indexes = structure(integer(length(formulas)), names = names(formulas))
            private$keys <- keys
            private$formulas <- formulas
        },
        calc = function() {
            if (private$calculated)
                return(self)
            if (any(is.na(self$responses)) || any(self$responses == 0))
                stop("The answers are contained missing values.", call. = FALSE)
            levels(self$responses[attr(private$keys, "torev")]) <- rev(levels(self$responses))
            self$scales <- unlist(lapply(private$keys, function(i) sum(as.numeric(self$responses[i]))))
            for (i in seq_along(private$formulas)) {
                self$indexes[i] <- eval(parse(text = private$formulas[i]),
                                        envir = as.list(c(self$indexes, self$scales)))
            }
            private$calculated <- TRUE
        },
        check = function() {
            !is.na(self$start_test) && !is.na(self$end_test) && private$calculated
        }
    ),
    active = list(
        hash = function() {
            digest::digest(self)
        }
    ),
    private = list(
        keys = NULL,
        formulas = NA_character_,
        calculated = FALSE
    )
)
