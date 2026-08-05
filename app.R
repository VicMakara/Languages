library(shiny)
library(bslib)
library(dplyr)
library(DT)
library(ggplot2)

source("functions.R")

# --- Данные ------------------------------------------------------------
# Поправьте путь под своё окружение. Для деплоя на shinyapps.io лучше
# держать tulos.rds в подпапке проекта (например, data/tulos.rds) —
# абсолютные Windows-пути там не будут работать.
DATA_PATH <- "C://R//tulos.rds"

tulos <- readRDS(DATA_PATH)

year_min <- min(tulos$Year, na.rm = TRUE)
year_max <- max(tulos$Year, na.rm = TRUE)
all_years <- sort(unique(tulos$Year))

# --- UI ------------------------------------------------------------------
ui <- page_navbar(
  title = "Languages in Finland",
  theme = bs_theme(version = 5),

  nav_panel(
    "Data table",
    layout_sidebar(
      sidebar = sidebar(
        sliderInput(
          "table_rank_cutoff", "Highlight top N languages (by population):",
          min = 5, max = 30, value = 15, step = 1
        ),
        helpText("Filters within columns (Language, Year, ...) are available directly in the table header.")
      ),
      div(
        style = "max-width: 900px; zoom: 1.15;",
        DTOutput("main_table")
      )
    )
  ),

  nav_panel(
    "Two-year comparison",
    layout_sidebar(
      sidebar = sidebar(
        selectInput("cmp_start_year", "Start year:", choices = all_years,
                    selected = all_years[1]),
        selectInput("cmp_end_year", "End year:", choices = all_years,
                    selected = all_years[length(all_years)]),
        sliderInput("cmp_rank_limit", "Include languages that were top N in either year:",
                    min = 5, max = 30, value = 15, step = 1),
        sliderInput("cmp_text_size", "Plot text size:",
                    min = 0.5, max = 2, value = 1, step = 0.1)
      ),
      h4(textOutput("cmp_title")),
      plotOutput("cmp_plot", height = "650px"),
      htmlOutput("cmp_table")
    )
  ),

  nav_panel(
    "Multi-period trends",
    layout_sidebar(
      sidebar = sidebar(
        selectizeInput(
          "series_breaks", "Period boundary years (pick 2 or more, in order):",
          choices = all_years,
          selected = c(all_years[1],
                       all_years[round(length(all_years) / 2)],
                       all_years[length(all_years)]),
          multiple = TRUE
        ),
        sliderInput("series_rank_limit", "Include languages that were top N in either boundary year:",
                    min = 5, max = 30, value = 15, step = 1),
        sliderInput("series_label_size", "Label size:", min = 2, max = 8, value = 4, step = 0.5)
      ),
      plotOutput("series_plot", height = "800px")
    )
  )
)

# --- Server ----------------------------------------------------------------
server <- function(input, output, session) {

  # --- Tab 1: data table ---
  tulos_highlighted <- reactive({
    tulos %>%
      mutate(highlight = ifelse(Rank <= input$table_rank_cutoff, TRUE, FALSE))
  })

  output$main_table <- renderDT({
    datatable(
      tulos_highlighted(),
      rownames = FALSE,
      caption = "Languages in Finland",
      filter = list(position = "top"),
      extensions = "Buttons",
      options = list(
        order = list(list(0, "asc"), list(3, "desc")),
        dom = "Blfrtip",
        autoWidth = TRUE,
        columnDefs = list(
          list(targets = c(4, 5, 6), visible = FALSE),
          list(targets = 0, width = "80px"),
          list(targets = 1, width = "150px"),
          list(targets = 2, width = "80px"),
          list(targets = 3, width = "150px")
        ),
        pageLength = 15,
        searchHighlight = TRUE,
        buttons = c("copy", "excel", "pdf", "print"),
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#000', 'color': '#FE221B'});",
          "$(this.api().table().container()).css({'font-size': '16px'});",
          "}"
        )
      )
    ) %>%
      formatStyle(
        "highlight",
        target = "row",
        color = styleEqual(c(0, 1), c("black", "yellow")),
        backgroundColor = styleEqual(c(0, 1), c("white", "black"))
      )
  })

  # --- Tab 2: two-year comparison ---
  cmp_result <- reactive({
    req(input$cmp_start_year, input$cmp_end_year)
    validate(need(input$cmp_start_year != input$cmp_end_year, "Choose two different years."))
    analyze_language_trends(
      tulos,
      start_year = as.numeric(input$cmp_start_year),
      end_year = as.numeric(input$cmp_end_year),
      rank_limit = input$cmp_rank_limit,
      text_size = input$cmp_text_size
    )
  })

  output$cmp_title <- renderText({
    paste("Comparing", input$cmp_start_year, "vs", input$cmp_end_year,
          "— top", input$cmp_rank_limit, "in either year")
  })

  output$cmp_plot <- renderPlot({
    cmp_result()$plot
  })

  output$cmp_table <- renderText({
    as.character(cmp_result()$table)
  })

  # --- Tab 3: multi-period trends ---
  output$series_plot <- renderPlot({
    validate(need(length(input$series_breaks) >= 2, "Pick at least two boundary years."))
    breaks <- sort(as.numeric(input$series_breaks))
    analyze_series(
      tulos,
      breaks = breaks,
      rank = input$series_rank_limit,
      label_size = input$series_label_size
    )
  })
}

shinyApp(ui, server)
