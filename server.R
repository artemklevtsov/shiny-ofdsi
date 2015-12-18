library(shiny)

shinyServer(function(input, output, session) {
    v <- reactiveValues(
        counter = 0L,
        user = init_entry()
    )

    session$onSessionEnded(function() {
        isolate({
            v$user$end_session = Sys.time()
            write_data(v$user)

        })
    })

    set_values <- function(value) {
        v$user$time[v$counter] <- Sys.time()
        v$user$answers[v$counter] <- value
        v$counter <- v$counter + 1
    }

    observe({
        if (is.na(input$age) || input$gender == "") {
            shinyjs::disable("start")
        } else {
            shinyjs::enable("start")
            shinyjs::hide("help")
        }
        if (v$counter > 0) {
            shinyjs::disable("gender")
            shinyjs::disable("age")
            shinyjs::disable("start")
        }
        if (v$counter > n) {
            isolate({
                v$user$end_test <- unname(v$user$time[n])
                v$user <- get_scores(v$user, kyes, formulas)
                v$user$hash <- digest::digest(v$user)
                #saveRDS(v$user, paste0("user-", v$user$hash, ".RDS"))
            })
        }
    })

    observeEvent(input$start, {
        v$user$start_test <- Sys.time()
        #v$counter <- 1L
        v$user$age <- input$age
        v$user$gender <- input$gender
    })
    observeEvent(input$end, {
        updateTabItems(session, "tabs", "results")
    })

    observeEvent(input$ans1, {
        set_values(1L)
    })
    observeEvent(input$ans2, {
        set_values(2L)
    })
    observeEvent(input$ans3, {
        set_values(3L)
    })
    observeEvent(input$ans4, {
        set_values(4L)
    })

    output$qbox <- renderUI({
        if (v$counter == 0)
            return(NULL)
        if (v$counter > 0 && v$counter <= n) {
            content <- p(class = "lead", questions[v$counter])
            footer <- get_answers(answers[v$counter, ])
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
        print(v$user)
    })

    output$scales <- DT::renderDataTable({
        DF <- data.frame(
            Шкала = sprintf("%s (%s)", scales$long, scales$short),
            Балл = v$user$scales,
            Уровень = get_levels(v$user$scales, scales$mean, scales$sd)
        )
        DT <- DT::datatable(DF, selection = "none", options = list(paging = FALSE, dom = 't'))
        DT::formatStyle(DT, "Балл", fontWeight = "bold")
    })

    output$indexes <- DT::renderDataTable({
        DF <- data.frame(
            Индекс = sprintf("%s (%s)", indexes$long, indexes$short),
            Балл = v$user$indexes,
            Уровень = get_levels(v$user$indexes, indexes$mean, indexes$sd)
        )
        DT <- DT::datatable(DF, selection = "none", options = list(paging = FALSE, dom = 't'))
        DT::formatStyle(DT, "Балл", fontWeight = "bold")
    })

    output$plot_types <- renderPlot({
        plot_types(v$user)
    })

})
