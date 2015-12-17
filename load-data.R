get_keys <- function(file) {
    text <- readLines(file)
    nms <- unlist(lapply(strsplit(text, ":", fixed = TRUE), .subset2, 1L))
    vals <- unlist(lapply(strsplit(text, ": ", fixed = TRUE), .subset2, -1L))
    torev <- gsub("\\d+( |$)|\\*", "", paste(vals, collapse = " "))
    torev <- unlist(strsplit(torev, " ", fixed = TRUE))
    torev <- sort(unique(as.numeric(torev)))
    vals <- strsplit(gsub("*", "", vals, fixed = TRUE), " ", fixed = TRUE)
    vals <- lapply(vals, function(x) sort(as.numeric(x)))
    structure(vals, names = nms, torev = torev, class = "TestKeys")
}

get_formulas <- function(file) {
    text <- readLines(file)
    splitted <- strsplit(text, "=")
    nms <- unlist(lapply(splitted, .subset2, 1L))
    nms <- gsub("^\\s+|\\s+$", "", nms)
    fms <- unlist(lapply(splitted, .subset2, 2L))
    fms <- gsub("^\\s+|\\s+$", "", fms)
    structure(fms, names = nms, class = "TestIndexes")
}

quiz_data <- read.csv2("data/quiz.csv", header = FALSE, stringsAsFactors = FALSE)
questions <- .subset2(quiz_data, 1L)
answers <- quiz_data[, 2L:length(quiz_data)]
n <- length(questions)
indexes <- read.csv2("data/indexes.csv", stringsAsFactors = FALSE)
scales <- read.csv2("data/scales.csv", stringsAsFactors = FALSE)
types <- read.csv2("data/types.csv", stringsAsFactors = FALSE)
values <- read.csv2("data/interpretation.csv", stringsAsFactors = FALSE)
keys <- get_keys("data/keys.txt")
formulas <- get_formulas("data/formulax.txt")
