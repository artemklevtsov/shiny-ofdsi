library(DBI)
library(RSQLite)

init_db <- function(dbname = "db.sqlite") {
    db <- dbConnect(RSQLite::SQLite(), dbname = dbname)
    sql <- function(table, names, types) {
        fields <- c("id INTEGER PRIMARY KEY AUTOINCREMENT", paste(names, types))
        sprintf("CREATE TABLE %s (%s);",
                table, paste(fields, collapse = ", "))
    }
    if (!dbExistsTable(db, "answers"))
        dbGetQuery(db, sql("answers", paste0("Q", seq_len(n)), "INTEGER"))
    if (!dbExistsTable(db, "timings"))
        dbGetQuery(db, sql("timings", paste0("Q", seq_len(n)), "TEXT"))
    if (!dbExistsTable(db, "results"))
        dbGetQuery(db, sql("results", c(scales$short, indexes$short), "INTEGER"))
    if (!dbExistsTable(db, "users"))
        dbGetQuery(db, sql("users", c("start_session", "end_session", "start_test", "end_test", "age", "gender", "hash"),
                           c("TEXT", "TEXT", "TEXT", "TEXT", "INTEGER", "TEXT", "TEXT")))
    dbDisconnect(db)
}

write_data <- function(data, dbname = "db.sqlite") {
    db <- dbConnect(RSQLite::SQLite(), dbname = dbname)
    op <- options(digits.secs = 6)
    on.exit(options(op))
    sql <- function(table, data) {
        columns <- paste(names(data), collapse = ", ")
        if (is.data.frame(data))
            values <- paste0("(", apply(data, 1, function(x) paste0("'", x, "'", collapse = ", ")), ")", collapse = ", ")
        else
            values <- paste0("'", data, "'", collapse = ", ")
        sprintf("INSERT INTO %s (%s) VALUES (%s);",
                table, columns, values)
    }
    dbGetQuery(db, sql("answers", data$answers))
    dbGetQuery(db, sql("timings", format(data$resp_time, tz = "UTC")))
    dbGetQuery(db, sql("results", c(data$scales, data$indexes)))
    dbGetQuery(db, sql("users", c(start_session = format(data$start_test, tz = "UTC"),
                                  end_session = format(data$end_session, tz = "UTC"),
                                  start_test = format(data$start_test, tz = "UTC"),
                                  end_test = format(data$end_test, tz = "UTC"),
                                  age = data$age, gender = data$gender, hash = data$hash)))
    dbDisconnect(db)
}

init_db()
