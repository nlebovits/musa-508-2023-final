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


filter <- dplyr::filter
select <- dplyr::select

mono_5_green <- rev(generate_palette("#1cb979", modification = "go_lighter", n_colours = 5, view_palette = FALSE)) 

filtered_zoning <- st_read("app_data.geojson", quiet = TRUE)


# Define UI--------------------------------------------
ui <- fluidPage(
  titlePanel("SmartZoning® UI Prototype"),
  
  sidebarLayout(
    position = "right",  # Move sidebar to the right
    mainPanel(leafletOutput("map")),
    sidebarPanel(
      sliderInput("rf_val_preds_input", "Filter by rf_val_preds:",
                  min = min(filtered_zoning$rf_val_preds, na.rm = TRUE),
                  max = max(filtered_zoning$rf_val_preds, na.rm = TRUE),
                  value = c(min(filtered_zoning$rf_val_preds, na.rm = TRUE),
                            max(filtered_zoning$rf_val_preds, na.rm = TRUE))),
      selectInput("code_input", "Filter by CODE:",
                  choices = unique(filtered_zoning$CODE),
                  selected = unique(filtered_zoning$CODE), multiple = TRUE)
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
  
  output$map <- renderLeaflet({
    data <- filteredData()
    
    # Calculate Fisher breaks for rf_val_preds
    breaks <- classIntervals(data$rf_val_preds, n = length(mono_5_green), style = "fisher")$brks
    
    # Create color palette function
    pal <- colorBin(mono_5_green, data$rf_val_preds, bins = breaks, na.color = "transparent")
    
    leaflet(data) %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
      setView(lng = -75.1652, lat = 39.9526, zoom = 11) %>%
      addPolygons(fillColor = ~pal(rf_val_preds),
                  color = NA,  # No border
                  fillOpacity = 0.7,
                  popup = ~paste("rf_val_preds:", rf_val_preds, "<br>CODE:", CODE)) %>%
      addLegend("bottomright", pal = pal, values = ~rf_val_preds,
                title = "rf_val_preds",
                labFormat = labelFormat(),
                opacity = 0.7)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
