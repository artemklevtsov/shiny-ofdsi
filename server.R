library(shiny)
library(shinydashboard)
library(shinyjs)
library(DT)

shinyServer(function(input, output, session) {
    v <- reactiveValues(
        user_data = init_user_data(),
        counter = 0L
    )

    session$onSessionEnded(function() {
        isolate({
            v$user_data$end_session = Sys.time()
            is (check_user_data(v$user_data))
                write_data(v$user_data)
        })
    })

    set_values <- function(value) {
        v$user_data$resp_time[v$counter] <- Sys.time()
        v$user_data$responses[v$counter] <- value
        v$counter <- v$counter + 1
    }

    observe({
        toggleState("start", !is.na(input$age) && input$gender != "")
        toggleState("download", v$counter > n)
        if (v$counter > 0) {
            disable("gender")
            disable("age")
            disable("start")
            hide("help")
        }
        if (v$counter > n) {
            isolate({
                v$user_data$end_test <- unname(v$user_data$resp_time[n])
                v$user_data <- get_scores(v$user_data)
                #saveRDS(v$user_data, paste0("user-", v$user_data$hash, ".RDS"))
            })
        }
    })

    observeEvent(input$start, {
        v$user_data$start_test <- Sys.time()
        #v$counter <- 1L
        v$user_data$age <- input$age
        v$user_data$gender <- input$gender
        v$counter <- 150
        v$user_data$responses[] <- sample(levels(v$user_data$responses), length(v$user_data$responses), TRUE)
    })

    observeEvent(input$end, {
        updateTabItems(session, "tabs", "scales")
    })

    observeEvent(input$ans1, set_values(1L))
    observeEvent(input$ans2, set_values(2L))
    observeEvent(input$ans3, set_values(3L))
    observeEvent(input$ans4, set_values(4L))

    output$qbox <- renderUI({
        if (v$counter == 0)
            return(NULL)
        if (v$counter > 0 && v$counter <= n) {
            content <- p(class = "lead", questions[v$counter])
            footer <- answer_buttons(answers[v$counter, ])
            title <- sprintf("Вопрос %d из %d", v$counter, n)
        } else {
            content <- div(align = "center", actionButton("end", "Перейти к результатам"))
            footer <- NULL
            title <- "Спасибо за участие!"
        }
        box(title = title,
            width = NULL,
            status = "primary",
            content,
            footer = footer)
    })

    output$menu <- renderMenu({
        dropdownMenu(type = "tasks", badgeStatus = "success",
                     taskItem("Прогресс", value = v$counter / n * 100, color = "green"))
    })

    output$protocol <- renderPrint({
        print(v$counter)
        #print(ls.str(v$user_data))
    })

    output$scales <- renderDataTable({
        DF <- data.frame(
            Шкала = sprintf("%s (%s)", scales$long, scales$short),
            Балл = v$user_data$scales,
            Уровень = get_levels(v$user_data$scales, scales$mean, scales$sd),
            stringsAsFactors = FALSE
        )
        DT <- datatable(DF, rownames = FALSE, selection = "none", options = list(paging = FALSE, dom = 't'))
        formatStyle(DT, "Балл", fontWeight = "bold")
    })

    output$indexes <- renderDataTable({
        DF <- data.frame(
            Индекс = sprintf("%s (%s)", indexes$long, indexes$short),
            Балл = v$user_data$indexes,
            Уровень = get_levels(v$user_data$indexes, indexes$mean, indexes$sd),
            stringsAsFactors = FALSE
        )
        DT <- datatable(DF, rownames = FALSE, selection = "none", options = list(paging = FALSE, dom = 't'))
        formatStyle(DT, "Балл", fontWeight = "bold")
    })

    output$itext <- renderUI({
        vals <- get_values(v$user_data)
        tps <- get_types(v$user_data)
        tagList(
            box(title = "Психомоторная сфера",
                width = NULL,
                lapply(vals[grep("психомоторная", scales$long)], p),
                footer = span("Тип темперамента: ", strong(tps$Психомоторный))),
            box(title = "Интеллектуальная сфера",
                width = NULL,
                lapply(vals[grep("интеллектуальная", scales$long)], p),
                footer = span("Тип темперамента: ", strong(tps$Интеллектуальный))),
            box(title = "Коммуникативная сфера",
                width = NULL,
                lapply(vals[grep("коммуникативная", scales$long)], p),
                footer = span("Тип темперамента: ", strong(tps$Коммуникативный)))
        )
    })

    output$plot_types <- renderPlot({
        nm1 <- c("ЭМ", "ЭИ", "ЭК")
        nm2 <- c("ИПА", "ИИА", "ИКА")
        scales <- scales[scales$short %in% nm1, ]
        indexes <- indexes[indexes$short %in% nm2, ]
        x <- v$user_data$indexes[names(v$user_data$indexes) %in% nm2]
        y <- v$user_data$scales[names(v$user_data$scales) %in% nm1]
        ylims <- range(scales$min, scales$max)
        xlims <- range(indexes$min, indexes$max)
        xmean <- indexes$mean[1L]
        ymean <- scales$mean[1L]
        ysd <- range(scales$mean - scales$sd, scales$mean + scales$sd)
        xsd <- range(indexes$mean - indexes$sd, indexes$mean + indexes$sd)
        text <- c("Флегматик", "Сангвиник", "Меланхолик", "Холерик")
        text_x <- c(xmean - diff(xlims) / 3, xmean + diff(xlims) / 3)
        text_y <- c(ymean - diff(ylims) / 3, ymean + diff(ylims) / 3)
        plot_data <- data.frame(
            Активность = x, Эмоциональность = y,
            Сфера = c("Психомоторная", "Интеллектуальная", "Коммуникативная")
        )
        ggplot(plot_data, aes(x = Активность, y = Эмоциональность, shape = Сфера, color = Сфера)) +
            geom_point(size = 5) +
            scale_x_continuous(limits = xlims) +
            scale_y_continuous(limits = ylims) +
            geom_vline(xintercept = xmean, alpha = 0.3) +
            geom_hline(yintercept = ymean, alpha = 0.3) +
            geom_vline(xintercept = xsd, linetype = "dashed", alpha = 0.3) +
            geom_hline(yintercept = ysd, linetype = "dashed", alpha = 0.3) +
            coord_fixed(max(xlims) / max(ylims)) +
            annotate("text", x = rep(text_x, 2), y = rep(text_y, each = 2), label = text, alpha = 0.3, size = 5) +
            theme(legend.position = "bottom")
    }, width = 600, height = 600)

    output$plot_type <- renderPlot({
        indexes <- indexes[indexes$short %in% c("ИОА", "ИОЭ"), ]
        ind1 <- indexes[indexes$short  == "ИОА", ]
        ind2 <-indexes[indexes$short == "ИОЭ", ]
        x <- v$user_data$indexes["ИОА"]
        y <- v$user_data$indexes["ИОЭ"]
        xlims <- c(ind1$min, ind1$max)
        ylims <- c(ind2$min, ind2$max)
        xmean <- ind1$mean
        ymean <- ind2$mean
        xsd <- c(ind1$mean - ind1$sd, ind1$mean + ind1$sd)
        ysd <- c(ind2$mean - ind2$sd, ind2$mean + ind2$sd)
        text <- c("Флегматик", "Сангвиник", "Меланхолик", "Холерик")
        text_x <- c(xmean - diff(xlims) / 3, xmean + diff(xlims) / 3)
        text_y <- c(ymean - diff(ylims) / 3, ymean + diff(ylims) / 3)
        ggplot(data.frame(x, y), aes(x = x, y = y)) +
            geom_point(size = 5) +
            labs(x = "Активность", y = "Эмоциональность") +
            scale_x_continuous(limits = xlims) +
            scale_y_continuous(limits = ylims) +
            geom_vline(xintercept = xmean, alpha = 0.3) +
            geom_hline(yintercept = ymean, alpha = 0.3) +
            geom_vline(xintercept = xsd, linetype = "dashed", alpha = 0.3) +
            geom_hline(yintercept = ysd, linetype = "dashed", alpha = 0.3) +
            coord_fixed(max(xlims) / max(ylims)) +
            annotate("text", x = rep(text_x, 2), y = rep(text_y, each = 2), label = text, alpha = 0.3, size = 5)
    }, width = 600, height = 600)

    output$plot_resp_time <- renderPlot({
        if (!check_user_data(v$user_data) || any(is.na(v$user_data$resp_time)))
            return(NULL)
        time <- diff(c(v$user_data$start_test, v$user_data$resp_time), units = "ms")
        ggplot(NULL, aes(x = time)) +
            geom_density() +
            labs(x = "Время", y = "Плотность вероятности")
    })

    output$plot_resp_bar <- renderPlot({
        if (!check_user_data(v$user_data))
            return(NULL)
        responses <- factor(v$user_data$responses, levels = 1:4, labels = c("Не характерно", "Мало характерно", "Довольно характерно", "Характерно"))
        ggplot(NULL, aes(x = responses)) + geom_bar() +
            labs(x = "Ответы", y = "Частота")
    })

    output$plot_responses <- renderPlot({
        if (!check_user_data(v$user_data) || any(is.na(v$user_data$resp_time)))
            return(NULL)
        data <- data.frame(
            Номер = seq_len(n),
            Ответ = factor(v$user_data$responses, levels = 1:4, labels = c("Не характерно", "Мало характерно", "Довольно характерно", "Характерно")),
            Время = as.double(diff(c(v$user_data$start_test, v$user_data$resp_time), units = "ms")))
        ggplot(data, aes(x = Ответ, y = Номер, fill = Время)) +
            geom_tile(color = "black") +
            scale_y_reverse() +
            scale_fill_gradient(low = "steelblue4", high = "red")
    })

    output$download <- downloadHandler(
        filename = function() {
            paste0(Sys.Date(), "-ofdsi-report.pdf")
        },
        content = function(file) {
            src <- normalizePath("report.Rnw")
            owd <- setwd(tempdir())
            on.exit(setwd(owd))
            file.copy(src, "report.Rnw", overwrite = TRUE)
            out <- knitr::knit2pdf("report.Rnw")
            file.rename(out, file)
        }
    )
})
