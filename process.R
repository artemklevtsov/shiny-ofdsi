get_scores <- function(data) {
    levels(data$responses[attr(keys, "torev")]) <- rev(levels(data$responses))
    data$scales <- unlist(lapply(keys, function(i) sum(as.numeric(data$responses[i]))))
    for (i in seq_along(indexes))
        data$indexes[i] <- eval(parse(text = formulas[i]), envir = as.list(c(data$indexes, data$scales)))
    return(data)
}

get_levels <- function(value, mean, sd) {
    res <- structure(rep("Не установлен", length(value)), names = names(value))
    if (all(value == 0) || all(is.na(value)))
        return(res)
    res[value < mean - sd] <- "Низкий"
    res[value > mean + sd] <- "Высокий"
    res[value >= mean - sd & value <= mean + sd] <- "Средний"
    return(res)
}

get_values <- function(data) {
    levels <- get_levels(data$scales, scales$mean, scales$sd)
    structure(
        lapply(seq_along(levels), function(i) {
            values[values$scale == scales$short[i] & values$level == levels[i], "value"]
        }),
        names = scales$short)
}

get_types <- function(data) {
    levels <- get_levels(c(data$scales, data$indexes),
                         c(scales$mean, indexes$mean),
                         c(scales$sd, indexes$mean))
    structure(
        mapply(function(i, j) types[types$Активность == i & types$Эмоциональность == j, "Описание"],
               levels[names(levels) %in% c("ИПА", "ИИА", "ИКА", "ИОА")],
               levels[names(levels) %in% c("ЭМ", "ЭИ", "ЭК", "ИОЭ")],
               SIMPLIFY = FALSE, USE.NAMES = FALSE),
        names = c("Психомоторный", "Интеллектуальный", "Коммуникативный", "Общий")
    )
}
