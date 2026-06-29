#' Launch an interactive map of US survey data
#'
#' Takes a data frame with a longitude and latitude columns
#' and launches an interactive Shiny dashboard. Points
#' are drawn over United States state boundaries on two tabs: a dynamic Leaflet
#' map and a static ggplot2/Plotly map. The user can colour points by any of the
#' supplied qualitative variables, size them by any of the supplied quantitative
#' variables, shrink all markers with a slider, or show locations only.
#'
#' @details
#'
#' - Boundaries are the contiguous US states from the **spData** package.
#' - Rows with NA values for longitude and latitude are automatically removed,
#'   with a warning message.
#' - Points with NA values for a quantitative variable are dropped from the
#'   size display.
#' - Points with NA values for a qualitative variable are shown in grey.
#' - Colours are generated automatically for any number of categories.
#'
#' @param data A data frame containing the survey points.
#' @param latitude Name of the latitude column, as a string (e.g. `"Latitude"`).
#' @param longitude Name of the longitude column, as a string (e.g. `"Longitude"`).
#' @param qualitative Character vector of categorical column names to offer for
#'   colouring (e.g. `c("State", "Soil_Type")`).
#' @param quantitative Character vector of numeric column names to offer for
#'   sizing the points (e.g. `c("Disease_Incidence", "Mean_gall_count")`).
#' @param title Dashboard title shown in the header. Default `"Spatial Mapping"`.
#' @param radius_range Numeric vector of length 3 giving the marker-radius
#'   slider as `c(minimum, maximum, default)`, in pixels. Default `c(2, 25, 20)`.
#'
#' @return A Shiny application object. When called at an interactive R prompt it
#'   launches the app in your browser.
#' @export
#'
#' @examples
#' if (interactive()) {
#'   df <- read.csv(
#'     system.file("extdata", "us_survey.csv", package = "americas")
#'   )
#'   americas(
#'     data         = df,
#'     latitude     = "Latitude",
#'     longitude    = "Longitude",
#'     qualitative  = c("State", "Rice_Variety", "Soil_Type", "Stage_of_Maturity"),
#'     quantitative = c("Disease_Incidence", "Mean_gall_count")
#'   )
#' }
americas <- function(data,
                     latitude,
                     longitude,
                     qualitative,
                     quantitative,
                     title = "Spatial Mapping",
                     radius_range = c(2, 25, 20)) {

  ## ---- Validate inputs ----
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }
  check_columns(data, c(latitude, longitude), "Coordinate")
  check_columns(data, qualitative, "Qualitative")
  check_columns(data, quantitative, "Quantitative")
  if (length(radius_range) != 3) {
    stop("`radius_range` must be length 3: c(minimum, maximum, default).",
         call. = FALSE)
  }

  radius_floor   <- radius_range[1]
  radius_ceiling <- radius_range[2]
  radius_default <- radius_range[3]

  ## ---- Prepare spatial data ----
  # Drop rows with missing coordinates (st_as_sf cannot place them) and
  # a message displays how many were rows were removed.
  n_before  <- nrow(data)
  data      <- data[!is.na(data[[latitude]]) & !is.na(data[[longitude]]), ,
                    drop = FALSE]
  n_dropped <- n_before - nrow(data)
  if (n_dropped > 0) {
    warning(sprintf("Removed %d row(s) with missing coordinates.", n_dropped),
            call. = FALSE)
  }

  # points -> sf wants co-ordinates in (x, y) = (longitude, latitude) order
  survey_sf <- sf::st_as_sf(as.data.frame(data),
                            coords = c(longitude, latitude),
                            crs = 4326)

  # US state boundaries in WGS 84 so they line up with the points.
  us_states <- sf::st_transform(spData::us_states, 4326)

  ## ---- User interface ----
  ui <- shinydashboard::dashboardPage(
    skin = "green",
    shinydashboard::dashboardHeader(title = title),
    shinydashboard::dashboardSidebar(
      shinydashboard::sidebarMenu(
        id = "sidebarid",
        shinydashboard::menuItem("Dynamic Maps", tabName = "dynamic_graph",
                                 icon = shiny::icon("globe", lib = "glyphicon")),
        shinydashboard::menuItem("Static Maps", tabName = "static_graph",
                                 icon = shiny::icon("map-marker", lib = "glyphicon")),
        shinydashboard::menuItem("About", tabName = "About",
                                 icon = shiny::icon("search", lib = "glyphicon"))
      )
    ),
    shinydashboard::dashboardBody(
      shiny::tags$head(shiny::tags$style(shiny::HTML('
        .main-header .logo {
          font-family: "Calibri", Times, "Calibri", serif;
          font-weight: bold;
          font-size: 28px;
        }
      '))),
      shinydashboard::tabItems(

        # ---- Dynamic (leaflet) tab ----
        shinydashboard::tabItem(
          tabName = "dynamic_graph",
          shiny::fluidRow(
            shinydashboard::box(
              width = 3, height = 675,
              shiny::helpText(shiny::HTML("<strong>Colour points by a category</strong>")),
              shiny::selectInput("qual_var", NULL, choices = qualitative),
              shiny::helpText(shiny::HTML("<strong>Size points by a measure</strong>")),
              shiny::selectInput("quant_var", NULL, choices = quantitative),
              shiny::sliderInput("dyn_radius", "Reduce display radius",
                                 min = radius_floor, max = radius_ceiling,
                                 value = radius_default, step = 1),
              shiny::checkboxInput("loc_only", "Show locations only", value = FALSE)
            ),
            shinydashboard::box(leaflet::leafletOutput("dyn_map", height = 600),
                                width = 9, height = 675)
          )
        ),

        # ---- Static (plotly) tab ----
        shinydashboard::tabItem(
          tabName = "static_graph",
          shiny::fluidRow(
            shinydashboard::box(
              width = 3, height = 675,
              shiny::helpText(shiny::HTML("<strong>Colour points by a category</strong>")),
              shiny::selectInput("qual_var_s", NULL, choices = qualitative),
              shiny::helpText(shiny::HTML("<strong>Size points by a measure</strong>")),
              shiny::selectInput("quant_var_s", NULL, choices = quantitative),
              shiny::sliderInput("stat_radius", "Reduce display radius",
                                 min = radius_floor, max = radius_ceiling,
                                 value = radius_default, step = 1),
              shiny::checkboxInput("loc_only_s", "Show locations only", value = FALSE)
            ),
            shinydashboard::box(plotly::plotlyOutput("static_map", height = 600),
                                width = 9, height = 675)
          )
        ),

        # ---- About tab ----
        shinydashboard::tabItem(
          tabName = "About",
          shiny::h4(shiny::strong("Created by")),
          shiny::p("- Ishara Wijayaratne"),
          shiny::h4(shiny::strong("Dynamic Maps")),
          shiny::p("Built with the Leaflet package. US state boundaries come from the ",
                   shiny::tags$a(href = "https://cran.r-project.org/package=spData", "spData"),
                   " package."),
          shiny::h4(shiny::strong("Static Maps")),
          shiny::p("Built with ggplot2 and Plotly."),
          shiny::h4(shiny::strong("Note")),
          shiny::p("Colours adapt to any number of categories, so the app works
                    for any US dataset with longitude and latitude columns.")
        )
      )
    )
  )

  ## ---- Server ----
  server <- function(input, output, session) {

    # Dynamic map base layer
    output$dyn_map <- leaflet::renderLeaflet({
      leaflet::leaflet() |>
        leaflet::addTiles() |>
        leaflet::addPolygons(data = us_states, fillOpacity = 0.15,
                             smoothFactor = 0.2, weight = 1, color = "#555")
    })

    shiny::observe({
      proxy <- leaflet::leafletProxy("dyn_map") |>
        leaflet::clearMarkers() |>
        leaflet::clearControls()

      # Locations-only mode: uniform markers that still respond to the slider.
      if (isTRUE(input$loc_only)) {
        loc_r <- max(1, input$dyn_radius * 0.35)
        proxy |>
          leaflet::addCircleMarkers(data = survey_sf, color = "blue",
                                    radius = loc_r, stroke = FALSE,
                                    fillOpacity = 0.7)
        return(invisible())
      }

      qvar <- input$qual_var
      svar <- input$quant_var
      shiny::req(qvar, svar)

      vals   <- as.character(survey_sf[[qvar]])
      palv   <- make_palette(vals)
      pal    <- leaflet::colorFactor(palette = unname(palv), levels = names(palv))
      radius <- scaled_radius(survey_sf[[svar]], max_radius = input$dyn_radius)
      popups <- paste0("<b>", qvar, ":</b> ", vals,
                       "<br><b>", svar, ":</b> ", round(survey_sf[[svar]], 3))

      proxy |>
        leaflet::addCircleMarkers(data = survey_sf,
                                  color = pal(vals), radius = radius,
                                  stroke = FALSE, fillOpacity = 0.7,
                                  popup = popups) |>
        leaflet::addLegend("topright", pal = pal, values = vals,
                           title = qvar, opacity = 1)
    })

    # Static map
    output$static_map <- plotly::renderPlotly({
      max_size <- input$stat_radius * 0.5
      min_size <- max_size * 0.2

      base <- ggplot2::ggplot() +
        ggplot2::geom_sf(data = us_states, fill = "grey80",
                         color = "white", linewidth = 0.2)

      # Locations-only mode: single colour, uniform size (still slider-driven).
      if (isTRUE(input$loc_only_s)) {
        p <- base +
          ggplot2::geom_sf(data = survey_sf, color = "red",
                           size = max(0.5, min_size), alpha = 0.8) +
          ggplot2::theme_minimal()
        return(plotly::ggplotly(p))
      }

      qvar <- input$qual_var_s
      svar <- input$quant_var_s
      shiny::req(qvar, svar)

      palv <- make_palette(survey_sf[[qvar]])

      p <- base +
        ggplot2::geom_sf(
          data = survey_sf,
          ggplot2::aes(color = .data[[qvar]], size = .data[[svar]]),
          alpha = 0.75
        ) +
        ggplot2::scale_color_manual(values = palv, name = qvar) +
        ggplot2::scale_size_continuous(range = c(min_size, max_size), name = svar) +
        ggplot2::theme_minimal()

      plotly::ggplotly(p)
    })
  }

  shiny::shinyApp(ui, server)
}
