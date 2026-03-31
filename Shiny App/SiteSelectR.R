# siteselectr Shiny App
remotes::install_github("joe-elias/siteselectr")

library(siteselectr)
library(shiny)
library(bslib)
library(sf)
library(terra)
library(tidyverse)
library(leaflet)
library(leaflet.extras)

source("01_functions.R")

sf_use_s2(FALSE)

# ---------------- UI ---------------------

ui <- fluidPage(
  titlePanel("Welcome to siteselectr, upload your starting coordinates, desired sampling radius, and raster layers to get started"),
  sidebarLayout(
    sidebarPanel(
      numericInput("y", "Longitude", value = -75.9180),
      numericInput("x", "Latitude", value = 42.0987),
      numericInput("extent", "How Far Out? (Meters)", value = 50000),
      numericInput("num", "How Many Sites?", value = 12),
      actionButton("run", "Enhance"),
      fileInput("upload", "Upload Geospatial Data",
                multiple = TRUE, accept = c(".tif", ".nc")),
      tags$hr(),
      helpText("Max upload size: 200MB"),
      tags$style(type="text/css", "#upload_progress { display:none; }")
    ),
    mainPanel(
      leafletOutput("map", height = 500),
      plotOutput("final"),
      plotOutput("sites")
    )
  )
)

# ---------------- SERVER ---------------------

server <- function(input, output, session) {
  
  observeEvent(input$run, {
    
    # 1. sampling extent
    sampling_extent <- siteselectr::sample_ext(
      x = input$x,
      y = input$y,
      radius = input$extent,
      crs = 4326
    )
    
    # 2. load rasters
    rasters <- lapply(input$upload$datapath, function(path){
      r <- terra::rast(path)
      terra::crs(r) <- "EPSG:4326"
      r_proj <- terra::project(r, "EPSG:4326")
      crop(r_proj, vect(sampling_extent))
    })
    
    # 3. resample
    r_resampled <- siteselectr::rast_match(
      rast_list = rasters,
      reference_layer = rasters[[1]],
      extent = sampling_extent
    )
    
    # 4. stack
    r_stack <- siteselectr::rast_stack(r_resampled, sampling_extent)
    
    # 5. site selection
    sites <- siteselectr::site_select(
      long = input$y,
      lat = input$x,
      num = input$num,
      stack = r_stack
    )
    
    site_xy <- terra::xyFromCell(r_stack[[1]], sites)
    site_df <- data.frame(lng = site_xy[,1], lat = site_xy[,2])
    
    # 6. map
    output$map <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        setView(lng = input$y, lat = input$x, zoom = 8) %>%
        addCircleMarkers(
          lng = site_df$lng,
          lat = site_df$lat,
          radius = 6,
          fillColor = "blue",
          fillOpacity = 0.9,
          stroke = TRUE,
          color = "black"
        ) %>%
        addPolygons(
          data = sampling_extent,
          color = "#005A43",
          fillOpacity = 0.3
        )
    })
  })
}

shinyApp(ui = ui, server = server)