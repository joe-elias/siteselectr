# first part of the monitoring network design app 
# visualize a set sampling extent around a single location

library(shiny)
library(bslib)
library(sf)
library(raster)
library(tidyverse)
library(leaflet)
library(vegan)
library(leaflet.extras)

source("01_functions.R")

sf_use_s2(F) # makes sf more compatible with other packages

ui<-fluidPage(
  titlePanel('Where Should I Sample?'), 
  sidebarLayout(
    sidebarPanel(
      numericInput('y', 'Longitude', value=# first part of the monitoring network design app 
                     # visualize a set sampling extent around a single location
                     
                     library(shiny)
                   library(bslib)
                   library(sf)
                   library(raster)
                   library(tidyverse)
                   library(leaflet)
                   library(vegan)
                   library(leaflet.extras)
                   
                   source("01_functions.R")
                   
                   sf_use_s2(F) # makes sf more compatible with other packages
                   
                   ui<-fluidPage(
                     titlePanel('Where Should I Sample?'), 
                     sidebarLayout(
                       sidebarPanel(
                         numericInput('y', 'Longitude', value=-75.9180), 
                         numericInput('x', 'Latitude', value=42.0987), 
                         numericInput('extent', 'How Far Out? (Meters)', value=50000), 
                         actionButton('run', 'Enhance'), 
                         fileInput("upload", "Upload Geospatial Data", 
                                   multiple=T, accept=c(".tif", ".nc")),
                         options(shiny.maxRequestSize = 200 * 1024^2)
                       ), 
                       mainPanel(
                         leafletOutput('map'), # view map with extent
                         plotOutput('final'), # this is the generated extent
                         plotOutput('sites')
                       )
                     )
                   )
                   
                   server<-function(input, output, session) {
                     
                     observeEvent(input$upload, {
                       # set the sample extent 
                       start_pt<-st_as_sf(
                         data.frame(lat=input$x, long=input$y), 
                         coords=c('long', 'lat'), crs=4326) # in the future would be cool for the ui to choose the crs 
                       
                       # transform from projected merc (to generate buffer) to WGS 84 before buffering
                       sample_extent<-start_pt %>% 
                         st_transform(3857) %>% 
                         st_buffer(input$extent) %>% 
                         st_transform(4326)
                       
                       rasters<-lapply(input$upload$datapath, function(path){
                         r<-terra::rast(path)
                         crs(r)<-crs("EPSG:4326")
                         r_proj <- terra::project(r, "EPSG:4326")
                         rasters <- crop(r_proj, sample_extent)
                         
                       })
                       
                       
                       # Resample all rasters to match reference
                       r_resampled <- lapply(rasters, function(x) {
                         smooth_rast(x, sample_extent, rasters[[1]])  # Use x8 as reference raster
                       })
                       
                       
                       # Stack and validate raster layers
                       r_stack <- terra::rast(r_resampled) %>% terra::trim() %>% terra::mask(sample_extent)
                       
                       # Calculate Mahalanobis distance
                       dat <- as.matrix(r_stack)
                       dat[is.nan(dat)] <- NA
                       cov_matrix <- cov(dat, use="pairwise.complete.obs") + diag(0.01, ncol(dat))  # Regularized covariance
                       center_mean <- colMeans(dat, na.rm = TRUE)
                       
                       # Compute Mahalanobis distance
                       distance_matrix <- mahalanobis(dat, center_mean, cov_matrix) 
                       distance_matrix <- as.numeric(distance_matrix)
                       
                       ref_layer<-r_stack[[1]] %>% mask(sample_extent)
                       values(ref_layer) <- NA
                       
                       # Create an empty raster to store distances
                       mahal_raster <- ref_layer 
                       values(mahal_raster) <- distance_matrix  
                       
                       xy1<-st_coordinates(start_pt)
                       cell1<-terra::cellFromXY(ref_layer, xy1)
                       cell.dat<-distance_matrix
                       max.diff<-which.max(cell.dat)
                       
                       # Iteratively select new sites ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ----
                       # run through site_select function for sites 3-8 
                       n_iterations <- 15
                       cells <- c(cell1, max.diff)  # set baseline cells (xy1, xy2)
                       
                       for (i in 1:n_iterations) {  # loops through (x) iterations of field site selection
                         cells <- site_select(cells, dat, ref_layer)
                       }
                       
                       # Get coordinates for valid cells
                       xy_valid <- terra::xyFromCell(ref_layer, cells)
                       
                       output$map<-renderLeaflet({
                         leaflet() %>% 
                           addTiles() %>% 
                           setView(lng=input$y, lat=input$x, zoom=8) %>% 
                           addMarkers(data=xy_valid) %>% 
                           addPolygons(data=sample_extent, color='blue', fillOpacity = 0.4)
                       })
                       
                     })
                   }
                   
                   shinyApp(ui=ui, server=server)
                   
      ), 
      numericInput('x', 'Latitude', value=42.0987), 
      numericInput('extent', 'How Far Out? (Meters)', value=50000), 
      actionButton('run', 'Enhance'), 
      fileInput("upload", "Upload Geospatial Data", 
                multiple=T, accept=c(".tif", ".nc")),
      options(shiny.maxRequestSize = 200 * 1024^2)
    ), 
    mainPanel(
      leafletOutput('map'), # view map with extent
      plotOutput('final'), # this is the generated extent
      plotOutput('sites')
    )
  )
)

server<-function(input, output, session) {
  
  observeEvent(input$upload, {
    # set the sample extent 
    start_pt<-st_as_sf(
      data.frame(lat=input$x, long=input$y), 
      coords=c('long', 'lat'), crs=4326) # in the future would be cool for the ui to choose the crs 
    
    # transform from projected merc (to generate buffer) to WGS 84 before buffering
    sample_extent<-start_pt %>% 
      st_transform(3857) %>% 
      st_buffer(input$extent) %>% 
      st_transform(4326)
    
    rasters<-lapply(input$upload$datapath, function(path){
      r<-terra::rast(path)
      crs(r)<-crs("EPSG:4326")
      r_proj <- terra::project(r, "EPSG:4326")
      rasters <- crop(r_proj, sample_extent)
      
    })
    
    
    # Resample all rasters to match reference
    r_resampled <- lapply(rasters, function(x) {
      smooth_rast(x, sample_extent, rasters[[1]])  # Use x8 as reference raster
    })
    
    
    # Stack and validate raster layers
    r_stack <- terra::rast(r_resampled) %>% terra::trim() %>% terra::mask(sample_extent)
    
    # Calculate Mahalanobis distance
    dat <- as.matrix(r_stack)
    dat[is.nan(dat)] <- NA
    cov_matrix <- cov(dat, use="pairwise.complete.obs") + diag(0.01, ncol(dat))  # Regularized covariance
    center_mean <- colMeans(dat, na.rm = TRUE)
    
    # Compute Mahalanobis distance
    distance_matrix <- mahalanobis(dat, center_mean, cov_matrix) 
    distance_matrix <- as.numeric(distance_matrix)
    
    ref_layer<-r_stack[[1]] %>% mask(sample_extent)
    values(ref_layer) <- NA
    
    # Create an empty raster to store distances
    mahal_raster <- ref_layer 
    values(mahal_raster) <- distance_matrix  
    
    xy1<-st_coordinates(start_pt)
    cell1<-terra::cellFromXY(ref_layer, xy1)
    cell.dat<-distance_matrix
    max.diff<-which.max(cell.dat)
    
    # Iteratively select new sites ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ----
    # run through site_select function for sites 3-8 
    n_iterations <- 15
    cells <- c(cell1, max.diff)  # set baseline cells (xy1, xy2)
    
    for (i in 1:n_iterations) {  # loops through (x) iterations of field site selection
      cells <- site_select(cells, dat, ref_layer)
    }
    
    # Get coordinates for valid cells
    xy_valid <- terra::xyFromCell(ref_layer, cells)
    
    output$map<-renderLeaflet({
      leaflet() %>% 
        addTiles() %>% 
        setView(lng=input$y, lat=input$x, zoom=8) %>% 
        addMarkers(data=xy_valid) %>% 
        addPolygons(data=sample_extent, color='#005A43', fillOpacity = 0.4)
    })
    
  })
}

shinyApp(ui=ui, server=server)
