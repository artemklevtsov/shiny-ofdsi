UserData <- R6::R6Class(
    classname = "UserData",
    cloneable = FALSE,
    public = list(
        start = .POSIXct(NA),
        end = .POSIXct(NA),
        gender = NA_character_,
        age = NA_character_,
        answers = NA_integer_,
        time = .POSIXct(NA),
        scales = NA_integer_,
        indexes = NA_integer_,
        initialize = function(n = 150, keys, formulas) {
            self$answers <- structure(ordered(integer(n), levels = seq_along(unique(answers))),
                                      names = paste0("Q", seq_len(n)))
            self$time <- structure(.POSIXct(rep(NA, n)),
                                   names = paste0("Q", seq_len(n)))
            self$scales <- structure(integer(length(keys)),
                                     names = names(keys))
            self$indexes = structure(integer(length(formulas)),
                                     names = names(formulas))
            private$keys <- keys
            private$formulas <- formulas
        },
        hash = function() {
            digest::digest(self)
        },
        calc_scores = function() {
            if (private$calculated)
                return(self)
            if (any(is.na(self$answers)) || any(self$answer == 0))
                stop("The answers are contained missing values.", call. = FALSE)
            levels(self$answers[attr(private$keys, "torev")]) <- rev(levels(self$answers))
            self$scales <- unlist(lapply(private$keys, function(i) sum(as.numeric(self$answers[i]))))
            for (i in seq_along(private$formulas)) {
                self$indexes[i] <- eval(parse(text = private$formulas[i]),
                                        envir = as.list(c(self$indexes, self$scales)))
            }
            private$calculated <- TRUE
        }
    ),
    private = list(
        keys = NULL,
        formulas = NA_character_,
        calculated = FALSE
    )
)

x <- UserData$new(150, keys, formulas)
x$answers[] <- sample(levels(x$answers), length(x$answers), TRUE)
x$calc_scores()
plot_types(x)
