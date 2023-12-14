library(shiny)
library(leaflet)
library(dplyr)
library(sf)
library(classInt)
library(igraph)
library(sfdep)
library(conflicted)
library(terra)
library(monochromeR)
library(deckgl)


filter <- dplyr::filter
select <- dplyr::select

mono_5_green <- rev(generate_palette("#1cb979", modification = "go_lighter", n_colours = 5, view_palette = FALSE)) 
filtered_zoning <- st_read("app_data.geojson", quiet = TRUE)


# Define UI--------------------------------------------
ui <- fluidPage(
  titlePanel("SmartZoning® UX Prototype"),
  
  sidebarLayout(
    position = "right",  # Move sidebar to the right
    mainPanel(deckglOutput("map"), height = "1000px"),
    sidebarPanel(
      sliderInput("rf_val_preds_input", "Filter by predicted development:",
                  min = 0,
                  max = as.integer(max(filtered_zoning$rf_val_preds, na.rm = TRUE) + 1),
                  value = c(min(filtered_zoning$rf_val_preds, na.rm = TRUE),
                            max(filtered_zoning$rf_val_preds, na.rm = TRUE))),
      checkboxGroupInput("code_input", "Filter by zoning code:",
                  choices = unique(filtered_zoning$CODE),
                  selected = unique(filtered_zoning$CODE))
    )
  )
)

# Define server logic
server <- function(input, output) {
  filteredData <- reactive({
    filtered_zoning %>%
      filter(rf_val_preds > input$rf_val_preds_input, CODE %in% input$code_input) %>%
      st_transform(crs = "+proj=longlat +datum=WGS84")
  })
  
  output$map <- renderDeckgl({
    data <- filteredData()
    
    # Calculate Fisher breaks for rf_val_preds
    breaks <- classIntervals(data$rf_val_preds, n = length(mono_5_green), style = "fisher")$brks
    
    # Create a color palette
    pal <- colorBin(mono_5_green, data$rf_val_preds, bins = breaks, na.color = "transparent")
    
    # Prepare data for deckgl's PolygonLayer
    props <- list(
      getPolygon = ~geometry,
      getFillColor = ~scales::col_bin(pal, rf_val_preds),
      getLineColor = "[0, 0, 0, 0]",  # RGBA color, the last 0 is for alpha (transparency)
      getLineWidth = 0,
      getElevation = 30,
      pickable = TRUE,
      tooltip = use_tooltip(
        html = "{{rf_val_preds}} <br> CODE: {{CODE}}",
        style = "background: steelBlue; border-radius: 5px;"
      )
    )
    
    deckgl(latitude = 39.9526, longitude = -75.1652, zoom = 11, pitch = 25) %>%
      add_basemap() %>%
      add_polygon_layer(data = data, properties = props) %>%
      add_legend_pal(pal, title = "Predicted Development", pos = "bottom-left")
  })
}

# Run the application
shinyApp(ui = ui, server = server)

