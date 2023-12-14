library(shiny)
library(dplyr)
library(sf)
library(classInt)
library(igraph)
library(sfdep)
library(conflicted)
library(terra)
library(monochromeR)
library(leaflet)
library(deckgl)


filter <- dplyr::filter
select <- dplyr::select

mono_5_green <- rev(generate_palette("#1cb979", modification = "go_lighter", n_colours = 5, view_palette = FALSE)) 
zoning_palette <- c(
  "I1" = "#7D26CD", # purple
  "I3" = "#8A2BE2", # blue violet
  "ICMX" = "#DC143C", # crimson red (more visible than pure red)
  "IP" = "#9932CC", # dark orchid
  "IRMX" = "#800080", # purple (darker shade for contrast)
  "RSA1" = "#FFD700", # gold
  "RSA2" = "#DAA520", # golden rod (darker than pure yellow)
  "RSA3" = "#FFB14E", # peach-orange (darker than peach-yellow)
  "RSA4" = "#FFC72C", # saffron (brighter and more visible than lemon chiffon)
  "RSA5" = "#FFD300", # cyber yellow (stands out better than light goldenrod)
  "RSA6" = "#FFDB58", # mustard (visible against white, still yellowish)
  "RSD1" = "#BDB76B", # dark khaki
  "RSD2" = "#E9967A", # dark salmon (more visible than peach puff)
  "RSD3" = "#DEB887"  # burlywood (darker than pale goldenrod)
)


filtered_zoning <- st_read("app_data.geojson", quiet = TRUE)


# Define UI--------------------------------------------
ui <- fluidPage(
  titlePanel("SmartZoning® UX Prototype"),
  
  # Map row with larger height
  fluidRow(
    leafletOutput("map", height = "60vh")  # Use viewport height (vh) for responsive full height
  ),
  
  # Filters row with smaller height
  fluidRow(
    column(6,  # Half width for the slider
           sliderInput("rf_val_preds_input", "Filter by predicted development:",
                       min = 0,
                       max = as.integer(max(filtered_zoning$rf_val_preds, na.rm = TRUE) + 1),
                       value = c(min(filtered_zoning$rf_val_preds, na.rm = TRUE),
                                 max(filtered_zoning$rf_val_preds, na.rm = TRUE))
           )
    ),
    column(6,  # Half width for the slider
           sliderInput("contig_area_input", "Filter by area for assemblage:",
                       min = 0,
                       max = as.integer(max(filtered_zoning$sum_contig_area, na.rm = TRUE) + 1),
                       value = c(min(filtered_zoning$sum_contig_area, na.rm = TRUE),
                                 max(filtered_zoning$sum_contig_area, na.rm = TRUE))
           )
    ),
    column(6,  # Half width for the checkbox group
           checkboxGroupInput("code_input", "Filter by zoning code:",
                              choices = unique(filtered_zoning$code),
                              selected = unique(filtered_zoning$code),
                              inline = TRUE)
    ),
    column(6,  # Half width for the checkbox group
           checkboxGroupInput("district_input", "Filter by Council District:",
                              choices = unique(filtered_zoning$district),
                              selected = unique(filtered_zoning$district),
                              inline = TRUE)
    )
  ),
  # Ensure that UI elements do not expand beyond their container
  tags$style(type = "text/css", "#map { max-width: 100%; height: 100%; }",
             ".leaflet-control-attribution { margin-bottom: 20px; }")
)



# Define server logic
server <- function(input, output) {
  filteredData <- reactive({
    filtered_zoning %>%
      filter(code %in% input$code_input,
             district %in% input$district_input,
             sum_contig_area >= input$contig_area_input[1],
             sum_contig_area <= input$contig_area_input[2],
             rf_val_preds >= input$rf_val_preds_input[1],
             rf_val_preds <= input$rf_val_preds_input[2]) %>%
      st_transform(crs = "+proj=longlat +datum=WGS84")
  })
  
  output$map <- renderLeaflet({
    data <- filteredData()
    
    # Create a color palette function based on code
    pal <- colorFactor(zoning_palette, domain = data$code)
    
    leaflet(data) %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
      setView(lng = -75.1652, lat = 39.9526, zoom = 13) %>%
      addPolygons(
        fillColor = ~pal(code),
        color = NA,  # No border
        fillOpacity = 0.7,
        popup = ~paste(
          "Zoning:", code,
          "<br>District:", district,
          "<br>Predicted Development:", rf_val_preds,
          "<br>Contiguous Area:", sum_contig_area
        )
      ) %>%
      addLegend("bottomright", pal = pal, values = ~code,
                title = "Zoning code",
                labFormat = labelFormat(),
                opacity = 0.7)
  })
}

# Run the application
shinyApp(ui = ui, server = server)

