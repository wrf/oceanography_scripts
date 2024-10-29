# plot of sea level rise, for doomsday models
# special thanks to Dr. Thomas Neubauer
# v1 created by WRF 2024-10-29

library(raster) # maybe install rgdal
library(ggplot2)
library(shiny)
library(leaflet)
library(dplyr)


world_alts <- raster("~/git/oceanography_scripts/sealevel_rise/2_5m_esri/alt/alt")
#system.time (world_alts <- raster("~/git/oceanography_scripts/sealevel_rise/2_5m_esri/alt/alt"))
# x range is -180 to 180, y range is -60 to 90
#dim(world_alts)
# [1] 3600 8640    1
# plot(world_alts)
# 
# world_alts.sub <- crop(world_alts, c( 0, 30, 45, 60 ) ) 
# plot(world_alts.sub)

# downgrade resolution (or take low-resolution map; otherwise plotting takes ages...)
world_alts.agg <- aggregate(world_alts, fact = 3)
#system.time( world_alts.agg <- aggregate(world_alts.sub, fact = 3) )
#dim(world_alts.agg)
# [1] 1200 2880    1

#res(world_alts)
# [1] 0.04166667 0.04166667
#res(world_alts.agg)
# [1] 0.125 0.125

# preparing raster object to plot with geom_tile in ggplot2
#world_alts_points <- rasterToPoints( world_alts.agg )
#head(world_alts_points)
#world_alts_df <- data.frame(world_alts_points)
# > range(world_alts_df$x)
# [1] -179.9375  179.9375
# > range(world_alts_df$y)
# [1] -59.43750  83.56251
#world_alts_df$cuts <- cut(world_alts_df$alt, breaks = c(-Inf, 0, 25, 50, 70, Inf))  # define breaks
#head(world_alts_df)
# dim(table(world_alts_df$x))
# #[1] 2880
# dim(table(world_alts_df$y))
# #[1] 1125

# ggplot(data = world_alts_df) +
#   geom_tile(aes(x = x, y = y, fill = cuts)) +
#   scale_fill_manual("Flooded areas", values = c("gray80",
#                                                 "deepskyblue4",
#                                                 "deepskyblue3",
#                                                 "deepskyblue",
#                                                 "gray25")) +
#   coord_equal() +
#   theme_bw() +
#   theme(panel.grid.major = element_blank(),
#         axis.text.y = element_blank(),
#         axis.text.x = element_blank())



################################################################################
################################################################################
################################################################################

ui <- fluidPage(
  
  # App title ----
  titlePanel("Sea Level Riser v1.0", windowTitle = "Sea Level Riser v1.0"),
    fluidRow(
      column(2,
             radioButtons("tileset", h3("Map Tile Set"),
                          choices = list("OpenStreetMap (default)" = providers$OpenStreetMap.Mapnik ,
                                         "Open Topo Map" = providers$OpenTopoMap ,
                                         "TonerLite" = providers$Stadia.StamenTonerLite , 
                                         "NatGeo World Map" = providers$Esri.NatGeoWorldMap )
             ),
             sliderInput(inputId = "altitude",
                         label = h3("Sea level Rise (meters relative to modern)"),
                         min = 0,
                         max = 100,
                         value = 10,
                         sep="" )
      ),
      column(10,
             leafletOutput(outputId = "worldMap", height=600),
             verbatimTextOutput("showingWhat")
      )
    ) # end fluidRow
) # end fluidPage

#
server <- function(input, output) {
  #output$showingWhat <- renderPrint({ str(leaflet( options=leafletOptions( minZoom=2, worldCopyJump=TRUE ) )) })
  #output$showingWhat <- renderPrint({ input$worldMap_bounds })
  #output$showingWhat <- renderPrint({ paste( input$worldMap_bounds$north , input$worldMap_bounds$south ) })
  output$showingWhat <- renderPrint({ input$worldMap_zoom })
  
  output$worldMap <- renderLeaflet({
    leaflet( options=leafletOptions( minZoom=3, worldCopyJump=TRUE ) ) %>% 
      setView(lng=10, lat=55, zoom=8) %>% 
      addProviderTiles( input$tileset %||% providers$Stadia.StamenTonerLite )
  })
  
  observe({
    req(input$worldMap_bounds)
    # define color function by altitude
    # blue is below selected altitude, full transparent for all other values
    is_flooded = function(x){ifelse(input$altitude > x,"deepskyblue","#00000000")}
    leafletProxy("worldMap", data=NULL) %>%
      clearImages() %>%
      addRasterImage( get_world_subset() , colors = is_flooded, opacity=0.8)
  }) # end observe
  
  get_world_subset = reactive({
    # meaning zoomed out, so use aggregate layer
    if ( input$worldMap_zoom < 7) {
      world_alts.sub = crop(world_alts.agg, c( input$worldMap_bounds$west, input$worldMap_bounds$east,
                                               input$worldMap_bounds$south, input$worldMap_bounds$north ) )
    } else {
      # meaning zoomed in, so use full size raster
      world_alts.sub = crop(world_alts, c( input$worldMap_bounds$west, input$worldMap_bounds$east,
                                           input$worldMap_bounds$south, input$worldMap_bounds$north ) )
    }
    return(world_alts.sub)
  })

}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
