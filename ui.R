library(shiny)
library(shinydashboard)

shinyUI(dashboardPage(
    skin = "black",
    dashboardHeader(
        title = "ОФДСИ",
        titleWidth = 200,
        dropdownMenuOutput("menu")
    ),
    dashboardSidebar(
        sidebarMenu(id = "tabs",
                    menuItem("Опросник", tabName = "quiz", icon = icon("pencil-square-o")),
                    menuItem("Протокол", tabName = "protocol", icon = icon("bars")),
                    menuItem("Показатели", tabName = "scales", icon = icon("table")),
                    menuItem("Графики", tabName = "plots", icon = icon("bar-chart")),
                    hr(),
                    selectInput("gender", "Пол", c("", "Мужчина", "Женщина")),
                    numericInput("age", "Возраст", NA, min = 16, step = 1),
                    hr()
        )
    ),
    dashboardBody(
        shinyjs::useShinyjs(),
        tabItems(
            tabItem(tabName = "quiz",
                    box(title = "Инструкция",
                        status = "info",
                        width = NULL,
                        collapsible = TRUE,
                        includeMarkdown("data/instructions.md"),
                        helpText(id = "help", class = " text-center",
                                 "Перед началом тестирования, укажите, пожалуйста, Ваш пол и возраст."),
                        footer = div(align = "center", actionButton("start", "Начать тестирование"))
                    ),
                    uiOutput("qbox")
            ),
            tabItem(tabName = "protocol",
                    box(title = "Протокол",
                        width = NULL,
                        verbatimTextOutput("protocol")
                    )
            ),
            tabItem(tabName = "scales",
                    tabBox(
                        title = tagList(icon("table"), "Показатели"),
                        width = NULL,
                        tabPanel(
                            title = "Шкалы",
                            DT::dataTableOutput("scales")),
                        tabPanel(title = "Индексы",
                                 DT::dataTableOutput("indexes"))
                    )
            ),
            tabItem(tabName = "plots",
                    box(width = NULL,
                        height = "500px",
                        plotOutput("plot_types")
                    )
            )
        )
    )
))
