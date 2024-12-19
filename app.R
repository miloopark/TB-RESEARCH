library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(DT)
library(bslib)
library(tidyr)

# Load data
hbc_data <- read.csv("hbc_df2_profilevars.csv")

# Define guideline categories (adjust if needed based on your dataset)
hbc_data <- hbc_data %>%
  mutate(guidelines_category = case_when(
    grepl("prison-specific", guidelines_recode, ignore.case = TRUE) ~ "Prison-specific",
    grepl("mention as high-risk", guidelines_recode, ignore.case = TRUE) ~ "High-risk mention only",
    TRUE ~ "No prison-specific mention"
  ))

# Identify barrier columns if needed for global analysis
barrier_cols <- grep("^barriers_guidelines_", names(hbc_data), value = TRUE)

# A fallback plotly function to show a simple message when no data in global analysis
fallback_plot <- function(message) {
  plot_ly(type = "scatter", mode = "text", x = 0.5, y = 0.5,
          text = message, textposition = "middle center", showlegend = FALSE) %>%
    layout(
      xaxis = list(showline = FALSE, zeroline = FALSE, showticklabels = FALSE, range = c(0,1)),
      yaxis = list(showline = FALSE, zeroline = FALSE, showticklabels = FALSE, range = c(0,1))
    )
}

# Custom theme
app_theme <- bs_theme(
  version = 5,
  bootswatch = "lux",
  primary = "#3f51b5",
  secondary = "#f50057",
  base_font = font_google("Roboto"),
  heading_font = font_google("Roboto Slab")
)

custom_css <- "
  .container-fluid {
    padding-top: 10px !important;
    padding-left: 20px; 
    padding-right: 20px; 
    padding-bottom: 10px !important;
  }

  .card, .well {
    margin-bottom: 20px !important;
  }

  .intro-box {
    background-color: #e9ecef;
    border-radius: 5px;
    padding: 15px;
    margin-bottom: 20px;
  }

  table.dataTable th, table.dataTable td {
    padding: 10px;
  }

  .navbar-nav > li > a, .navbar-brand {
    margin-top: 5px !important;
    margin-bottom: 5px !important;
  }
"

ui <- navbarPage(
  title = "Global TB Policy Explorer",
  theme = app_theme,
  id = "navbar",
  
  header = tags$head(tags$style(HTML(custom_css))),

  # Country Analysis Tab
  tabPanel("Country Analysis",
    fluidPage(
      div(
        class = "intro-box",
        h3("Explore Country-Specific TB Data"),
        p("Use the dropdown menu below to select a country. The dashboard will update automatically, showing 
          regional TB incidence comparisons, textual details on barriers to TB guidelines implementation, 
          temporal trends in TB incidence within prison populations, and a raw data table for in-depth analysis.")
      ),

      fluidRow(
        column(4,
          wellPanel(
            selectInput("country", "Select a Country:",
                        choices = unique(hbc_data$country),
                        selected = unique(hbc_data$country)[1])
          )
        ),
        column(8)
      ),

      # Charts Row
      fluidRow(
        column(
          6,
          card(
            card_header("TB Incidence by WHO Region"),
            card_body(plotlyOutput("bar_chart", height = "300px"))
          )
        ),
        column(
          6,
          card(
            card_header("Barriers to TB Guidelines Implementation"),
            card_body(uiOutput("barrier_details", style = "min-height:300px;"))
          )
        )
      ),

      fluidRow(
        column(
          12,
          card(
            card_header("Time Trends in TB Incidence in Prisons"),
            card_body(
              plotlyOutput("line_chart", height = "300px"),
              p("This visualization shows how prison-related TB incidence rates have changed over the years in the selected country.")
            )
          )
        )
      ),

      fluidRow(
        column(
          12,
          card(
            card_header("Raw Data Table"),
            card_body(
              DTOutput("data_table"),
              p(style = 'color: #888; font-size:0.9em;', "Source: hbc_df2_profilevars.csv")
            )
          )
        )
      )
    )
  ),
  
  # Global Analysis Tab
  tabPanel("Global Analysis",
    fluidPage(
      div(
        class = "intro-box",
        h3("Global TB Policy and Barrier Insights"),
        p("This section provides a global overview of TB policies and barriers in prisons across multiple countries. 
           It helps identify common patterns, widespread challenges, and areas for improvement.")
      ),
      
      # Global Guidelines Bar Chart
      fluidRow(
        column(
          12,
          card(
            card_header("Global Comparison: Prison-Specific TB Guidelines"),
            card_body(
              plotlyOutput("global_guidelines_chart", height = "300px"),
              p("Shows how many countries have prison-specific TB guidelines, only mention prisoners as high-risk, or no specific mention.")
            )
          )
        )
      ),

      # Global Barriers Frequency
      fluidRow(
        column(
          12,
          card(
            card_header("Frequency of Barriers to TB Guideline Implementation"),
            card_body(
              plotlyOutput("global_barriers_chart", height = "300px"),
              p("Counts how many countries report experiencing each type of barrier.")
            )
          )
        )
      ),

      # Barrier Heatmap
      fluidRow(
        column(
          12,
          card(
            card_header("Barrier Presence Across Countries"),
            card_body(
              plotlyOutput("barrier_heatmap", height = "400px"),
              p("Matrix showing which countries experience which barriers.")
            )
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # Reactive filtered data for the selected country
  filtered_data <- reactive({
    hbc_data %>%
      filter(country == input$country)
  })

  # Per-country Charts
  output$bar_chart <- renderPlotly({
    req(input$country)
    data <- filtered_data()
    if (nrow(data) == 0) return(fallback_plot("No data available for this country."))

    gg <- ggplot(data, aes(x = who_region, y = c_newinc_100k, fill = who_region)) +
      geom_bar(stat = "identity") +
      labs(title = paste("TB Incidence Rate (per 100k) in", input$country),
           x = "WHO Region", y = "Incidence Rate per 100k") +
      theme_minimal() +
      theme(legend.position = "none", plot.title = element_text(size = 14, face = "bold"))

    ggplotly(gg) %>% layout(margin = list(l = 50, r = 50, b = 50, t = 50))
  })

  # Barriers Details (Text Only)
  output$barrier_details <- renderUI({
    req(input$country)
    data <- filtered_data()
    details_col <- "barriers_guidelines_details"

    if (details_col %in% names(data)) {
      details_text <- unique(na.omit(data[[details_col]]))
      if (length(details_text) > 0) {
        return(
          div(
            style = "padding:10px; background-color:#f8f9fa; border-radius:5px;",
            h4("Barriers to TB Guidelines"),
            lapply(details_text, function(txt) p(txt))
          )
        )
      }
    }

    # If no details available
    div(
      style = "padding:10px;",
      "No textual details on barriers to TB guidelines available for this country."
    )
  })

  output$line_chart <- renderPlotly({
    req(input$country)
    data <- filtered_data()

    if (!"prison_pop_year" %in% names(data) || all(is.na(data$prison_pop_year)) ||
        !"TB_inc_estimate_prisons100k" %in% names(data)) {
      return(fallback_plot("No prison incidence trend data available."))
    }

    data_line <- data %>%
      filter(!is.na(prison_pop_year) & !is.na(TB_inc_estimate_prisons100k))

    if (nrow(data_line) == 0) return(fallback_plot("No prison incidence trend data found."))

    gg <- ggplot(data_line, aes(x = prison_pop_year, y = TB_inc_estimate_prisons100k)) +
      geom_line(color = "#3f51b5", size = 1) +
      geom_point(color = "#3f51b5") +
      labs(title = paste("TB Incidence in Prisons Over Time in", input$country),
           x = "Year", y = "Incidence per 100k") +
      theme_minimal() +
      theme(plot.title = element_text(size = 14, face = "bold"))

    ggplotly(gg) %>% layout(margin = list(l = 50, r = 50, b = 50, t = 50))
  })

  output$data_table <- renderDT({
    req(input$country)
    datatable(filtered_data(), options = list(pageLength = 10, autoWidth = TRUE), style = "bootstrap4")
  })

  # Global Analysis Charts
  
  # Guidelines Comparison
  output$global_guidelines_chart <- renderPlotly({
    guidelines_counts <- hbc_data %>%
      group_by(guidelines_category) %>%
      summarise(n = n_distinct(country))

    if (nrow(guidelines_counts) == 0) {
      return(fallback_plot("No data available for guidelines comparison."))
    }

    gg <- ggplot(guidelines_counts, aes(x = guidelines_category, y = n, fill = guidelines_category)) +
      geom_bar(stat = "identity") +
      labs(title = "Number of Countries by Prison-Specific TB Guideline Approach",
           x = "Guideline Category", y = "Number of Countries") +
      theme_minimal() +
      theme(legend.position = "none", plot.title = element_text(face = "bold", size = 14))

    ggplotly(gg) %>% layout(margin = list(l = 50, r = 50, b = 50, t = 50))
  })

  # Global Barriers Frequency
  output$global_barriers_chart <- renderPlotly({
    if (length(barrier_cols) == 0) {
      return(fallback_plot("No barrier columns found."))
    }

    # Check for numeric barriers (if needed)
    numeric_barrier_cols_global <- barrier_cols[sapply(hbc_data[barrier_cols], is.numeric)]
    if (length(numeric_barrier_cols_global) == 0) {
      return(fallback_plot("No numeric barrier data available globally."))
    }

    barrier_country_counts <- hbc_data %>%
      group_by(country) %>%
      summarise(across(all_of(numeric_barrier_cols_global), ~sum(.x, na.rm = TRUE))) %>%
      summarise(across(all_of(numeric_barrier_cols_global), ~sum(.x > 0))) %>%
      pivot_longer(cols = everything(), names_to = "barrier", values_to = "count") %>%
      arrange(desc(count))

    if (nrow(barrier_country_counts) == 0) {
      return(fallback_plot("No barrier data available globally."))
    }

    gg <- ggplot(barrier_country_counts, aes(x = reorder(barrier, count), y = count, fill = barrier)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(title = "How Many Countries Experience Each Barrier?",
           x = "Barrier", y = "Number of Countries") +
      theme_minimal() +
      theme(legend.position = "none", plot.title = element_text(face = "bold", size = 14))

    ggplotly(gg) %>% layout(margin = list(l = 100, r = 50, b = 50, t = 50))
  })

  # Barrier Heatmap
  output$barrier_heatmap <- renderPlotly({
    if (length(barrier_cols) == 0) {
      return(fallback_plot("No barrier columns found."))
    }

    numeric_barrier_cols_global <- barrier_cols[sapply(hbc_data[barrier_cols], is.numeric)]
    if (length(numeric_barrier_cols_global) == 0) {
      return(fallback_plot("No numeric barrier data available for heatmap."))
    }

    barrier_data <- hbc_data %>%
      group_by(country) %>%
      summarise(across(all_of(numeric_barrier_cols_global), ~ sum(.x, na.rm = TRUE))) %>%
      pivot_longer(cols = all_of(numeric_barrier_cols_global), names_to = "barrier", values_to = "count") %>%
      mutate(presence = ifelse(count > 0, 1, 0))

    if (nrow(barrier_data) == 0) {
      return(fallback_plot("No barrier data to display in heatmap."))
    }

    gg <- ggplot(barrier_data, aes(x = barrier, y = country, fill = factor(presence))) +
      geom_tile(color = "white") +
      scale_fill_manual(values = c("0" = "#cccccc", "1" = "#d7301f"), labels = c("No", "Yes")) +
      labs(title = "Barrier Presence Across Countries",
           x = "Barrier", y = "Country", fill = "Barrier Present?") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(face = "bold", size = 14))

    ggplotly(gg, tooltip = c("x", "y", "fill")) %>% layout(margin = list(l = 100, b = 100, r = 50, t = 50))
  })
}

shinyApp(ui, server)