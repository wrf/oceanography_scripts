# iNaturalist/app.R
# make interactive map of iNaturalist samples in Leaflet
# exported using the iNaturalist csv data download
# last updated 2023-06-22

library(shiny)
library(leaflet)
library(dplyr)

# current host of this file at:
inputfilename = "~/git/oceanography_scripts/data/observations-337199.csv"

print(paste("# Reading", inputfilename))
sample_data = read.csv(inputfilename)

names(sample_data)
#  [1] "id"                               "observed_on_string"              
#  [3] "observed_on"                      "time_observed_at"                
#  [5] "time_zone"                        "user_id"                         
#  [7] "user_login"                       "user_name"                       
#  [9] "created_at"                       "updated_at"                      
# [11] "quality_grade"                    "license"                         
# [13] "url"                              "image_url"                       
# [15] "sound_url"                        "tag_list"                        
# [17] "description"                      "num_identification_agreements"   
# [19] "num_identification_disagreements" "captive_cultivated"              
# [21] "oauth_application_id"             "place_guess"                     
# [23] "latitude"                         "longitude"                       
# [25] "positional_accuracy"              "private_place_guess"             
# [27] "private_latitude"                 "private_longitude"               
# [29] "public_positional_accuracy"       "geoprivacy"                      
# [31] "taxon_geoprivacy"                 "coordinates_obscured"            
# [33] "positioning_method"               "positioning_device"              
# [35] "species_guess"                    "scientific_name"                 
# [37] "common_name"                      "iconic_taxon_name"               
# [39] "taxon_id"  

print( "# Building point popup labels" ) #
# could use "url" column instead
sample_link_string = paste0("<b><i><a href='https://www.inaturalist.org/observations/", 
                            sample_data[["id"]] ,"'>", 
                            sample_data[["scientific_name"]] , "</a></i></b>")
# make pop up label of Species, place name, and date observed
sample_labels <- paste(sep = "<br/>",
                    sample_link_string,
                    sample_data[["place_guess"]],
                    sample_data[["observed_on_string"]],
                    sample_data[["latitude"]],
                    sample_data[["longitude"]]
)

# add labels as columns
sample_data = cbind(sample_data, sample_labels)

print( "# Starting user interface" )
# begin actual shiny code
ui <- fluidPage(
  
  titlePanel("iNaturalist Sample Plotter"),
  
  # Sidebar layout with input and output definitions ----
  verticalLayout(
    fluidRow(
      column(4,
             # https://github.com/leaflet-extras/leaflet-providers
             # https://openmaptiles.org/docs/website/leaflet/
             radioButtons("tileset", h3("Map Tile Set"),
                          choices = list("Esri.WorldImagery (satellite)" = "esrisatellite",
                                         "Esri.WorldTopoMap (topography)" = "esritopo",
                                         "Esri.OceanBasemap" = "esriocean",
                                         "TonerLite (gray)" = "tonerlite")
             ),
             h4("Overlay options for satellite view"),
             checkboxInput("lineoverlay", "Border overlay", value = FALSE),
             checkboxInput("nameoverlay", "Placename overlay", value = FALSE)
      ),
      
      column(4, h3("Highlight options"),
             textInput("userSpecies", label = "Highlight genus or species", 
                       value = "")
      ),
      column(4, h3("Filter options"),
             sliderInput(inputId = "year",
                         label = "Year",
                         min = 2010,
                         max = 2025,
                         value = c(2010,2025),
                         sep=""
             ),
             checkboxInput("includeCaptive", "Include captive/cultivated?", value = FALSE)
      )
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(width=12,
              h3("Each point is a sample, click to display stats, Shift+click+drag to zoom"),
              leafletOutput(outputId = "worldMap", height=700),
              verbatimTextOutput(outputId = "debug"), 
              h3("Species in data set:"),
              textOutput(outputId = "totalsamples"), 
              verbatimTextOutput(outputId = "speciesInfo"), 
              # verbatimTextOutput(outputId = "mapBounds"), 
              h3("Samples in current view, zoom to change or filter"),
              dataTableOutput("sampleInfo")
              #  
    ) # end mainPanel
  ) # end verticalLayout
) # end fluidPage

#
server <- function(input, output) {
  #output$debug <- renderText({ unlist(input$worldMap_bounds) })
  
  output$totalsamples <- renderText({ paste("Total samples:", nrow(sample_data) ) })
  output$speciesInfo <- renderPrint({ table(sample_data$scientific_name) })
  
  get_tileset_choice = reactive({
    if (input$tileset == "tonerlite") {
      providers$Stamen.TonerLite }
    else if (input$tileset == "esriocean") {
      providers$Esri.OceanBasemap }
    else if (input$tileset == "esrisatellite") {
      providers$Esri.WorldImagery }
    else { providers$Esri.WorldTopoMap }
  })
  
  # observer for categories of points
  observe({
    selected_samples = arrange(sample_data, observed_on )
    leafletProxy("worldMap", data=selected_samples) %>%
      clearMarkers() %>%
      addCircleMarkers(lng=selected_samples$longitude , lat=selected_samples$latitude ,
                       fillColor= colorFactor(c("#777777","#26aade","#0577ff"), 
                                              domain = c("casual","needs_id","research") )(selected_samples$quality_grade),
                       fillOpacity=0.5 ,
                       color = ifelse(selected_samples$scientific_name==input$userSpecies, "#ff0577", "#000000") , 
                       opacity = 0.8,
                       popup = selected_samples$sample_labels )
  }) # end observe

  # draw map
  output$worldMap <- renderLeaflet({
    leaflet( options=leafletOptions( minZoom=2, worldCopyJump=TRUE, preferCanvas=TRUE ) ) %>% 
      setView(lng=0, lat=35, zoom=4) %>% 
      addProviderTiles( get_tileset_choice() ) %>%
      addScaleBar("bottomright") 
  })

  # observer for border and road overlay
  observe({
    if ( input$lineoverlay==TRUE ) {
      leafletProxy("worldMap", data=NULL) %>%
        removeTiles(layerId="overlaylines") %>%
        addProviderTiles(providers$Stamen.TonerLines, layerId="overlaylines",
                         options = providerTileOptions(opacity = 0.9) )
    } else {
      leafletProxy("worldMap", data=NULL) %>%
        removeTiles(layerId="overlaylines")
    }
  })
  # observer for place label overlay
  observe({
    if ( input$nameoverlay==TRUE ) {
      leafletProxy("worldMap", data=NULL) %>%
        removeTiles(layerId="overlaylabs") %>%
        addProviderTiles(providers$Stamen.TonerLabels, layerId="overlaylabs",
                         options = providerTileOptions(opacity = 0.5) )
    } else {
      leafletProxy("worldMap", data=NULL) %>%
        removeTiles(layerId="overlaylabs")
    }
  })
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
