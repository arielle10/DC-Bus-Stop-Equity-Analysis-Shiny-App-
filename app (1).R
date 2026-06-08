library(shiny)
library(ggplot2)
library(shinythemes)
library(tidyverse)
library(rsconnect)
library(broom)
library(leaflet)
library(sf)
library(ggrepel)
library(scales)

metrodata <- read.csv('all_bus_stops.csv')
ward_var <- "ward"
wards_data_simplified <- read.csv('wards_data_simplified.csv')
income_model <- lm(I(perc_removed*100) ~ I(median_income / 10000), data = wards_data_simplified)
public_transpo_model <- lm(perc_removed ~ I(public_commuters / 10000), data = wards_data_simplified)


coef_df1 <- broom::tidy(income_model) %>%
  mutate(
    estimate = round(estimate, 2),
    std.error = round(std.error, 2),
    statistic = round(statistic, 2),
    p.value = pvalue(p.value, accuracy = 0.01),
    Term = case_when(
      term == "(Intercept)" ~ "Intercept",
      term == "I(median_income/10000)" ~ "Median Household Income ÷ $10k",
      TRUE ~ term
    )
  ) %>%
  rename(
    Coefficient = estimate,
    `Standard Error` = std.error,
    `t Value` = statistic,
    `P-value` = p.value
  ) %>%
  select(Term, Coefficient, `Standard Error`, `t Value`, `P-value`)

coef_df2 <- broom::tidy(public_transpo_model) %>%
  mutate(
    estimate = round(estimate, 2),
    std.error = round(std.error, 2),
    statistic = round(statistic, 2),
    p.value = pvalue(p.value, accuracy = 0.01),
    Term = case_when(
      term == "(Intercept)" ~ "Intercept",
      term == "I(public_commuters/10000)" ~ "# Public Commuters ÷ 10k",
      TRUE ~ term
    )
  ) %>%
  rename(
    Coefficient = estimate,
    `Standard Error` = std.error,
    `t Value` = statistic,
    `P-value` = p.value
  ) %>%
  select(Term, Coefficient, `Standard Error`, `t Value`, `P-value`)

wards <- st_read("Wards_from_2022.geojson", quiet = TRUE)
if (is.na(st_crs(wards)) || st_crs(wards)$epsg != 4326) {
  wards <- st_transform(wards, 4326)
}
label_field <- intersect(c("WARD","LABEL","WARD_ID","NAME"), names(wards))[1]
if (length(label_field) == 0) label_field <- names(wards)[1]
ward_pts <- st_point_on_surface(wards) |>
  dplyr::mutate(.label = as.character(.data[[label_field]]))

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "ward_choice",
        label = paste("Select", ward_var),
        choices = c("All", sort(unique(metrodata[[ward_var]]), decreasing = FALSE))
      )
    ),
    mainPanel(
      tabsetPanel(
        type = "pills",
        tabPanel("Overview",  
                 h3("Overview: Better Bus Network"),
                 p("This app will help you explore the impacts of the Better Bus Network’s stop removals across Washington DC."),
                 h4("Background on the Better Bus Network"),
                 tags$ul(
                   tags$li("It was implemented to improve Metrobus service by attempting to make it more frequent, reliable, and easier to use."),
                   tags$li("About 527 stops were removed as part of their mission."),
                   tags$li("The redesign also aimed to address existing inequities in service based on data
                           showing that lower income areas reported walking twice as far and waiting 3–5 minutes longer compared to other customers.")
                 ),
                 h4("How to use the app"),
                 tags$ol(
                   tags$li("Use the dropdown on the left to pick a Ward (1–8) within DC, or view all (map only)."),
                   tags$li("Go to the Map tab to see active vs. removed bus stops (blue = active, red = removed) across all of DC."),
                   tags$li("Open the Ward tab to see a breakdown of statistics for the selected ward, including some demographic factors."),
                   tags$li("Check the Analysis tab for simple models and graphs relating removals to average ward income and public transit use.")
                 )
        ),
        tabPanel("Map", leafletOutput("map", height = 550)),
        tabPanel("Ward", 
                 conditionalPanel(
                   condition = "input.ward_choice == 'All'",
                   h4("Please select a specific ward to view pie chart and statistics.")
                 ),
                 conditionalPanel(
                   condition = "input.ward_choice != 'All'",
                   plotOutput("ward_pie"),
                   tableOutput("ward_table"))),
        tabPanel("Analysis", 
                 h3(HTML("<strong>Analysis:</strong> Income vs. % of Stops Lost by Ward")),
                 p(em("Select a ward from the drop-down menu on the left to see where it falls on the charts below.")),
                 p("The grey trend line below shows a clear pattern - lower income wards generally lost a higher percentage of bus stop than higher income wards:"),
                 plotOutput("income_trend"),
                 plotOutput("removed_by_ward"),
                 h3(HTML("<strong>Regression:</strong> Median Income vs. % of Stops Lost by Ward")),
                 p("While the results of the regression analysis failed to demonstrate statistical significance (p = .39), they provide the directional insight that, for each additional $10k in median income, a ward was likely to lose .37 percentage points fewer bus stops. If we accept this model, we would expect the richest ward (3) to lose almost double the percentage of stops compared to the poorest ward (8)."),
                 tableOutput("regression_table1"),
                 h3(HTML("<strong>Regression:</strong> # of Public Commuters vs. % of Stops Lost by Ward")),
                 p("The results of a regression comparing volume of commuters who use public transportation to percentage of stops lost were less stark. This model also did not reach statistical significance (p = .57) and a 10k increase in public transport commuters was only associated with a .04 percentage point rise in lost bus stops. Given the variance between the highest- and lowest- volume wards by public commuters is just over 5,000, this result shows that there is little association between the number of commuters and the number of stops lost."),
                 tableOutput("regression_table2"))
      )
    )   
  )      
)

server <- function(input, output) {
  output$map <- renderLeaflet({
    stops <- if (input$ward_choice == "All") metrodata else dplyr::filter(metrodata, .data[[ward_var]] == input$ward_choice)
    kept    <- dplyr::filter(stops,   stop_status == "kept")
    removed <- dplyr::filter(stops,   stop_status == "removed")
    leaflet(options = leafletOptions(minZoom = 9)) |>
      addTiles() |>
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolylines(data = wards, color = "darkgrey", weight = 2, opacity = 0.9) |>
      addLabelOnlyMarkers(
        data = ward_pts,
        lng = ~st_coordinates(geometry)[,1],
        lat = ~st_coordinates(geometry)[,2],
        label = ~.label,
        labelOptions = labelOptions(noHide = TRUE, direction = "center", textOnly = TRUE, offset = c(0,0))
      ) |>
      addCircleMarkers(
        data = kept, lng = ~stop_x, lat = ~stop_y,
        radius = 1, opacity = 0.9, fillOpacity = 0.7, color = "steelblue",
        label = ~paste0("Kept: ", stop_location)
      ) |>
      addCircleMarkers(
        data = removed, lng = ~stop_x, lat = ~stop_y,
        radius = 1.2, opacity = 1, fillOpacity = 0.9, color = "tomato",
        label = ~paste0("Removed: ", stop_location)
      ) |>
      addLegend("bottomleft", colors = c("steelblue","tomato"), labels = c("Kept","Removed"), title = "Stops")
  })
  output$ward_pie <- renderPlot({
    metrodata %>%
      filter(ward == !!input$ward_choice) %>%
      mutate(stop_status = fct_relabel(stop_status, ~ tools::toTitleCase(.))) %>%
      ggplot(aes(x = "", y = stop_status, fill = stop_status)) +
      geom_bar(stat = "identity", color = NA, width = 1.02) +  # use geom_bar
      coord_polar("y", start = 0) +
      scale_fill_manual(
        name = "Stop Status",
        values = c(
          "Kept" = "steelblue",
          "Removed" = "tomato"
        )
      ) +
      theme_void()
  })
  output$ward_table <- renderTable({
    req(input$ward_choice != "All")
    wards_data_simplified %>%
      mutate(
        "Median Household Income" = comma(median_income),
        "# Public Transport Commuters" = comma(public_commuters),
        "# Stops Kept" = comma(kept),
        "# Stops Removed" = comma(removed),
        "% Removed" = perc_removed * 100
      ) %>%
      arrange(ward) %>%
      filter(ward == !!input$ward_choice) %>%
      select("Ward" = ward,
             "Median Household Income", 
             "# Public Transport Commuters",
             "# Stops Kept",
             "# Stops Removed",
             "% Removed")
  }, align = "c")
  output$income_trend <- renderPlot({
    highlight_data <- if (input$ward_choice == "All") NULL else wards_data_simplified %>% filter(ward == input$ward_choice)
    wards_data_simplified %>%
      ggplot(aes(x = median_income, y = perc_removed)) +
      geom_point() +
      geom_point(
        data = highlight_data,
        color = "tomato",
        size = 4,
        shape = 21,
        fill = "steelblue",
        stroke = 1.5
      ) +
      geom_smooth(method=lm, se=FALSE, color = "grey") +
      geom_label_repel(
        data = highlight_data,
        aes(label = paste("Ward", ward)),
        size = 6,
        arrow = arrow(length = unit(0.015, "npc")),
        box.padding = 0.5
      ) +
      theme_bw() +
      labs(
        x = "Median Household Income",
        y = "% of Stops Removed"
      ) +
      scale_x_continuous(labels = dollar_format(prefix = "$")) +
      scale_y_continuous(labels = percent_format(scale = 100)) +
      theme(axis.title.x = element_text(size = 16),
            axis.title.y = element_text(size = 16))
  })
  output$removed_by_ward <- renderPlot({
    highlight_data <- if (input$ward_choice == "All") NULL else wards_data_simplified %>% filter(ward == input$ward_choice)
    wards_data_simplified %>%
      ggplot(aes(x = ward, y = perc_removed)) +
      geom_bar(stat = "identity") +
      geom_bar(stat = "identity",
        data = highlight_data,
        size = 1.5,
        color = "tomato",
        fill = "steelblue"
      ) +
      theme_bw() +
      labs(
        x = "Ward",
        y = "% of Stop Removed"
      ) +
      scale_y_continuous(labels = percent_format(scale = 100)) +
      theme(axis.title.x = element_text(size = 16),
            axis.title.y = element_text(size = 16))
  })
  output$regression_table1 <- renderTable({coef_df1})
  output$regression_table2 <- renderTable({coef_df2})
}

shinyApp(ui = ui, server = server)
