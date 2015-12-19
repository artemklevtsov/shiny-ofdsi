library(shiny)
library(shinydashboard)
library(shinyjs)
library(DT)

shinyUI(dashboardPage(
    skin = "black",
    dashboardHeader(
        title = "ОФДСИ",
        titleWidth = 200,
        dropdownMenuOutput("menu")
    ),
    dashboardSidebar(
        sidebarMenu(
            id = "tabs",
            menuItem("Опросник", tabName = "quiz", icon = icon("pencil-square-o")),
            menuItem("Протокол", tabName = "protocol", icon = icon("bars")),
            menuItem("Показатели", tabName = "scales", icon = icon("table")),
            menuItem("Графики", tabName = "plots", icon = icon("bar-chart")),
            menuItem("Интерпретация", tabName = "interp", icon = icon("info-circle"))
        ),
        selectInput("gender", "Пол", c("", "Мужчина", "Женщина")),
        numericInput("age", "Возраст", NA, min = 16, step = 1),
        menuItem(downloadButton("download", strong("Скачать отчёт")))

    ),
    dashboardBody(
        useShinyjs(),
        tabItems(
            tabItem(
                tabName = "quiz",
                box(title = "Инструкция",
                    status = "info",
                    width = NULL,
                    collapsible = TRUE,
                    includeMarkdown("data/instructions.md"),
                    helpText(id = "help", class = " text-center",
                             "Перед началом тестирования, укажите, пожалуйста, Ваш пол и возраст."),
                    footer = div(align = "center", actionButton("start", strong("Начать тестирование")))
                ),
                uiOutput("qbox")
            ),
            tabItem(
                tabName = "protocol",
                box(title = "Протокол",
                    width = NULL,
                    verbatimTextOutput("protocol")
                )
            ),
            tabItem(
                tabName = "scales",
                tabBox(
                    title = tagList(icon("table"), "Показатели"),
                    width = 12,
                    tabPanel(title = "Шкалы",
                             dataTableOutput("scales")),
                    tabPanel(title = "Индексы",
                             dataTableOutput("indexes"))
                )
            ),
            tabItem(
                tabName = "plots",
                fluidRow(
                    tabBox(
                        title = "Тип темперамента",
                        width = 12,
                        tabPanel(
                            title = "По сферам",
                            plotOutput("plot_types", width = "100%", height = "100%")
                        ),
                        tabPanel(
                            title = "Общий",
                            plotOutput("plot_type", width = "100%", height = "100%")
                        )
                    )
                ),
                fluidRow(
                    tabBox(
                        title = "Ответы испытуемого",
                        width = 12,
                        tabPanel(title = "Время",
                                 plotOutput("plot_resp_time")
                        ),
                        tabPanel(title = "Распределение",
                                 plotOutput("plot_resp_bar")
                        ),
                        tabPanel(title = "Все ответы",
                                 plotOutput("plot_responses"))
                    )
                )
            ),
            tabItem(
                tabName = "interp",
                uiOutput("itext")
            )
        )
    )
))
