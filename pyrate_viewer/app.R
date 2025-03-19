# pyrate_viewer/app.R
# last updated 2024-07-29
#
# plots results from PyRate: 
# https://github.com/dsilvestro/PyRate
#

library(shiny)
library(ggplot2)
library(dplyr)
library(scales)

# read taxa for table display
taxon_data_file = "~/git/oceanography_scripts/pyrate_viewer/data/Lithistida_pbdb_data.txt"
taxon_data = read.table(taxon_data_file, header=TRUE)

# read lineage diversity file from PyRate
ltt_data_file = "~/git/oceanography_scripts/pyrate_viewer/data/Lithistida_pbdb_data_1-6_Grj_se_est_ltt.txt"
ltt_data = read.table(file = ltt_data_file, header=TRUE)

# set max time to Cambrian
max_time = 550

# values for geological periods
period_starts =  pmin( c( 635, 541, 485, 443, 419, 358, 298, 252, 201, 145, 66, 23), max_time)
period_ends = c( 541, 485, 443, 419, 358, 298, 252, 201, 145, 66, 23, 0)
period_y1 = rep( (-0.05 * max(ltt_data$diversity)),12)
period_y2 = rep(0.0,12)
#                Ediacaran  Cambrian   Ordovician Silurian   
period_color = c("#d6ab24", "#7fa056", "#28a46e", "#b3e1b6", 
#                Devonian   Carbonifer Permian    Triassic
                 "#cb8c37", "#4fa092", "#d9491c", "#be58d0",
#                Jurassic   Cretaceous Paleogene  Neogene
                 "#34b2c9", "#7fc64e", "#fd9a52", "#e6e635")
geo_periods = data.frame(period_starts, period_ends, period_y1, period_y2, period_color)


# begin app
ui <- fluidPage(
    titlePanel(h1("Diversity through Time v1.0", style="font-weight:bold;"), 
            windowTitle="Diversity through Time v1.0"),
    fluidRow(
            # this draws the upper plot, which can be brushed to show a zoomed in version below
            # also shows the geological periods
            plotOutput(outputId = "mainTimeline",
                      width="95%",
                      height="200px",
                      brush = brushOpts(id = "mainTime_brush", direction="x")
           )
    ), # end fluidRow
    fluidRow(
            # zoomed in version of above plot
            # can be brushed to show taxa around brushed area
            plotOutput(outputId = "zoomTimeline",
                      height="400px",
                      width="95%",
                      brush = brushOpts(id = "zoomTime_brush", direction="x")
                      #hover = hoverOpts(id="zoomHover", delay=500)
           ),
           # table of species from zoomed plot
           verbatimTextOutput("whatsThis")
    ) # end fluidRow
) # end fluidPage

# Define server logic ----
server <- function(input, output) {

    # table of taxa if any are selected
    output$whatsThis <- renderPrint({
        if (!is.null( input$zoomTime_brush ) ) {
        filter(taxon_data, max_age >= input$zoomTime_brush$xmin , min_age <= input$zoomTime_brush$xmax)
        } else {
        print("No taxa selected ; Select area on lower plot to display taxa")
        }
        #str(input$zoomTime_brush)
        #str(input$zoomHover)
    })
    
    # this shows the upper timeline, and is static
    # geological periods are drawn as rectangles below
    output$mainTimeline <- renderPlot({
        ggplot(data=ltt_data, aes(x=time, y=diversity)) +
            theme_bw() +
            labs(subtitle = "Select a range to zoom the lower plot.") + 
            scale_y_continuous(expand = c(0.0, 0.5)) +
            scale_x_reverse(expand = c(0,0), limits=c(max_time,0) ) +
            geom_ribbon(aes(ymin=m_div, ymax=M_div), fill="#44cd88") +
            geom_line(size=1, colour="#08672b") +
            annotate( geom="rect", xmin=period_starts, xmax=period_ends, ymin=period_y1, ymax=period_y2, fill=period_color)
    })
    
    # this draws the lower timeline, which is the zoom of the upper timeline
    # clicking and dragging filters taxa to be displayed in the table
    output$zoomTimeline <- renderPlot({
        # draw roughly the same plot as above
        zgg = ggplot(ltt_data, aes(x=time, y=diversity)) +
            theme_bw() +
            labs(subtitle = "Select a range to display taxa.") + 
            geom_ribbon(aes(ymin=m_div, ymax=M_div), fill="#44cd88") +
            geom_line(size=1, colour="#08672b") +
            annotate( geom="rect", xmin=period_starts, xmax=period_ends, ymin=period_y1, ymax=period_y2, fill=period_color)
        
        # add zoom limits if top plot is brushed, otherwise set to default view
        if (!is.null(input$mainTime_brush) ) {
            zgg = zgg + scale_x_reverse(expand = c(0,0), limits=c(input$mainTime_brush$xmax, input$mainTime_brush$xmin), oob=squish )
        } else {
            zgg = zgg + scale_x_reverse(expand = c(0,0), limits=c(max_time,0) )
        }
        zgg
    })
    
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)

