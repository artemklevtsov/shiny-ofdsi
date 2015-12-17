get_scores <- function(data, kyes, indexes) {
    levels(data$answers[attr(keys, "torev")]) <- rev(levels(data$answers))
    data$scales <- unlist(lapply(keys, function(i) sum(as.numeric(data$answers[i]))))
    for (i in seq_along(indexes))
        data$indexes[i] <- eval(parse(text = indexes[i]), envir = as.list(c(data$indexes, data$scales)))
    return(data)
}

get_types <- function(data) {
    nm1 <- c("ЭМ", "ЭИ", "ЭК")
    nm2 <- c("ИПА", "ИИА", "ИКА")
    scales <- scales[scales$short %in% nm1, ]
    indexes <- indexes[indexes$short %in% nm2, ]
    user_scores <- data$indexes[names(data$indexes) %in% nm2]
    user_indexes <- data$scales[names(data$scales) %in% nm1]
    lvl <- function(x, mean, sd) {
        cut(x, c(mean - sd, mean + sd), rightmost.closed = TRUE, labels = c("Низкий", "Средний", "Высокий"))
    }
    scs <- sapply(seq_along(user_scores), function(i) lvl(user_scores[i], scales$mean[i], scales$sd[i]))
    idx <- sapply(seq_along(user_indexes), function(i) lvl(user_indexes[i], indexes$mean[i], indexes$sd[i]))
    res <- mapply(function(i, j) types[types$Активность == i & types$Эмоциональность == j, "Описание"], scs, idx)
    names(res) <- c("Психомоторная сфера", "Интеллектуальная сфера", "Коммуникативная сфера")
    return(res)
}

plot_types <- function(data) {
    nm1 <- c("ЭМ", "ЭИ", "ЭК")
    nm2 <- c("ИПА", "ИИА", "ИКА")
    scales <- scales[scales$short %in% nm1, ]
    indexes <- indexes[indexes$short %in% nm2, ]
    x <- data$indexes[names(data$indexes) %in% nm2]
    y <- data$scales[names(data$scales) %in% nm1]
    ylims <- c(min(scales$min), max(scales$max))
    xlims <- c(min(indexes$min), max(indexes$max))
    ratio <- max(xlims) / max(ylims)
    xmean <- indexes$mean[1L]
    ymean <- scales$mean[1L]
    ysd <- c(min(scales$mean - scales$sd), max(scales$mean + scales$sd))
    xsd <- c(min(indexes$mean - indexes$sd), max(indexes$mean + indexes$sd))
    text <- c("Ф", "С", "М", "Х")
    text_x <- c(xmean - diff(xlims) / 3, xmean + diff(xlims) / 3)
    text_y <- c(ymean - diff(ylims) / 3, ymean + diff(ylims) / 3)
    plot_data <- data.frame(x, y, z = c("Психомоторная сфера", "Интеллектуальная сфера", "Коммуникативная сфера"))
    ggplot(plot_data, aes(x = x, y = y, shape = z, color = z)) +
        geom_point(size = 5) +
        labs(x = "Активность", y = "Эмоциональность") +
        scale_x_continuous(limits = xlims) +
        scale_y_continuous(limits = ylims) +
        geom_vline(xintercept = xmean, alpha = 0.3) +
        geom_hline(yintercept = ymean, alpha = 0.3) +
        geom_vline(xintercept = xsd, linetype = "dashed", alpha = 0.3) +
        geom_hline(yintercept = ysd, linetype = "dashed", alpha = 0.3) +
        coord_fixed(ratio) + theme_minimal() +
        annotate("text", x = rep(text_x, 2), y = rep(text_y, each = 2), label = text, alpha = 0.5) +
        theme(legend.position = "bottom",
              legend.direction = "vertical",
              legend.title = element_blank())
}
