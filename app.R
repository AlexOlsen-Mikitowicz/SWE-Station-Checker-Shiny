rm(list=ls())

library(shiny)
library(ggplot2)
library(tidyr)
# library(kableExtra)
library(DT)
library(dygraphs)
library(RNRCS)
library(xts)
library(sf)

library(leaflet)

library(tmap) 
load("SnotelAnalysis.RData") 

# 1 - Basic reaction - select a country and the plot updates

# 2 - location of data transformations
# why doesn't summary work? 
# location of data manipulation matters
# planning an app to minimize data transformations within script is key - more work being done in script (as apposed to before hand) will slow down your app!

# 3 - global variables
# global.R 

# 4- reactive vs observe
# reactive creates an object you define (an update)
# an updateable object 
# observe - immediately evaluated, not a variable/object that is saved/updated 
# immediate effect 

# R shiny overview: https://shiny.rstudio.com/articles/reactivity-overview.html

# basic layout #2 
ui <- fluidPage(
  
  titlePanel("SWE Explorer"),
  
  sidebarLayout(
    
    # SNOTEL site
    # choose asos within 30km
    # stream gauges within snotel watershed
    
    sidebarPanel(
      tags$p('Select which SNOTEL sites to look at'),
      leafletOutput("snotelmap", width = "100%", height = 400),
      verbatimTextOutput("snotel_Clicked"),
      
      # tags$p('Select which ASOS sites to use with selected SNOTEL'),
      # leafletOutput("asosmap", width = "100%", height = 400),
      # verbatimTextOutput("asos_Clicked"),
      # tags$br(), 
      # 
      # tags$p('These are the asos sites within 30km of your snotel site. 
      #     Select which asos site to examine: '),
      # tags$p('would print your subsetted asos here'),
      # DTOutput("asos_reacted"),
      # 
      # tags$br(), 
      # tags$p('These are the available stream gauges within your selected snotel watershed. 
      #     Select which watershed to examine: '),
      # tags$p('would print your subsetted wtrshed here'),
      # DTOutput("wtrshed_reacted")
      
    ), # end siebarPanel 
    
    mainPanel(
      tabsetPanel(
        tabPanel("Plots",
                 fluidRow(column(4,
                                 dateInput('date_start', label = "select start date")),
                          column(4,
                                 dateInput('date_end', label = "select end date")),
                          column(4,
                                 textOutput("site")),
                          column(4,
                                 actionButton("dl_snotel", "Download SNOTEL Data for Date Window Selected"))),
                 
                 dygraphOutput("swe", height = '200px'),
                 dygraphOutput("depth", height = "200px"),
                 dygraphOutput("density", height = "200px"),
                 dygraphOutput("snotel_temp", height = "200px")),
        
        tabPanel("Analysis", plotOutput("plot")),
        tabPanel("Table", DTOutput("table")) # tableOutput("table")
      ) # end tabsetPanel
    ) # end mainPanel
  ) # end sidebarLayout
) # end UI



server <- function(input, output, session) {
  
  
  output$snotelmap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("Esri.WorldImagery") %>%
      addCircles(data = snotel_spatial,
                 color = "red",
                 popup = snotel_spatial$site_name,
                 layerId = snotel_spatial$site_no)
    
    
    
  }) #end snotel map
  
  # observeEvent(input$snotelmap_shape_click, {
  # print(input$snotelmap_shape_click$id)
  # one_huc <- subset(snotel_in_hucs, snotel_in_hucs$HUC8 == input$snotelmap_shape_click$id)
  # asos_in_huc <- asos_in_snotel[which(st_contains(one_huc, asos_in_snotel, sparse = FALSE)),]
  # leafletProxy("snotelmap", session) %>%
  # addCircles(data = asos_in_huc,
  #            color = "green",
  #            popup = asos_in_snotel$name,
  #            layerId = asos_in_snotel$id)
  # })
  
  # observeEvent(input$snotelmap_shape_click, {
  #   
  #   print(input$snotelmap_shape_click$id)
  #   
  #   ## another redirect if on whether snotel or asos clicked
  #   
  #   
  #   # find the huc clicked on
  #   one_huc <- subset(snotel_in_hucs, snotel_in_hucs$HUC8 == input$snotelmap_shape_click$id) # 11080002
  #   # find the asos within said huc
  #   asos_in_huc <- asos_in_snotel[which(st_contains(one_huc, asos_in_snotel, sparse = FALSE)),]
  #   
  #   if(nrow(asos_in_huc)<1){
  #     print('no asos in HUC')
  #   } else {
  #     
  #     print(asos_in_huc)
  #     #print('adding asos points')
  #     
  #     leafletProxy("snotelmap", session) %>%
  #       addCircles(data = asos_in_huc,
  #                  color = "green",
  #                  popup = asos_in_huc$name,
  #                  layerId = asos_in_huc$id, group='asos' ) %>%
  #       
  #       addLayersControl(
  #         # baseGroups = c("OSM (default)", "Toner", "Toner Lite"),
  #         overlayGroups = c("asos", "snotel"),
  #         options = layersControlOptions(collapsed = FALSE)
  #       )
  #     # options = pathOptions(pane = "asos_layer") )#  %>%
  #     # showGroup("asos_layer" )
  #     # bringToFront = TRUE
  #   }
  #   
  # })
  
  # output$asosmap <- renderLeaflet({
  #   leaflet() %>%
  #     addProviderTiles("Esri.WorldImagery") %>%
  #     addCircles(data = asos_in_snotel,
  #                color = "green",
  #                popup = asos_in_snotel$name,
  #                layerId = asos_in_snotel$id)
  
  #}) #end asos map  
  
  
  snotel_id <- reactive({
    validate(
      need(
        input$snotelmap_shape_click != "",
        "Please select a SNOTEL site from the map"
      )
    )
    (input$snotelmap_shape_click)
    
    
  }) # end snotel_id reactive
  
  
  snotel_site <- reactive({
    snotel_spatial %>%
      dplyr::filter(site_no == snotel_id()$id)
    
  }) #end snotel_site reactive
  
  
  output$site <- renderText({
    print(snotel_site())
    paste(snotel_site()$site_name)
  })
  
  
  sno_dat <- eventReactive(input$dl_snotel, {
    print
    
    grabNRCS.data(network = "SNTL", 
                  site = snotel_id()$id, 
                  timescale = "daily", 
                  DayBgn = paste(input$date_start), 
                  DayEnd = paste(input$date_end)) %>%
      dplyr::rename(avg_temp_f = Air.Temperature.Average..degF.,
                    max_temp_f = Air.Temperature.Maximum..degF.,
                    min_temp_f = Air.Temperature.Minimum..degF.,
                    obs_temp_f = Air.Temperature.Observed..degF..Start.of.Day.Values,
                    precip_accum = Precipitation.Accumulation..in..Start.of.Day.Values,
                    # avg_relh = Relative.Humidity.Average..pct.,
                    # max_relh = Relative.Humidity.Maximum..pct.,
                    # min_relh = Relative.Humidity.Minimum..pct.,
                    snow_depth_in = Snow.Depth..in..Start.of.Day.Values,
                    swe_in = Snow.Water.Equivalent..in..Start.of.Day.Values) %>%
      dplyr::mutate(date = lubridate::ymd(Date),
                    snow_density = ifelse(snow_depth_in >=2 & swe_in >=1, (swe_in/snow_depth_in)*1000, NA))
    
    
  })
  
  asos_dat <- eventReactive(input$dl_asos, {
    
    riem_measures(station = asos_id()$id, 
                  date_start = paste(as.numeric(input$wy)-1, "10", "01", sep = "-" ), 
                  date_end = paste(input$wy, "09", "30", sep = "-"))
  })
  
  output$swe <-  renderDygraph({
    
    df = sno_dat()
    sno_ts = xts(df$swe_in,
                 order.by=df$date)
    dygraph(sno_ts,  main = "Snow Water Equivalent",group = 'graphs') %>%
      dyAxis("y", label = "SWE (in)")
  })
  
  output$depth <-  renderDygraph({
    df = sno_dat()
    sno_ts = xts(df$snow_depth_in,
                 order.by=df$date)
    dygraph(sno_ts,  main = "Snow Depth",group = 'graphs') %>%
      dySeries(color = "green") %>%
      dyAxis("y", label = "Snow Depth (in)")
  })
  
  output$density <-  renderDygraph({
    df = sno_dat()
    sno_ts = xts(df$snow_density,
                 order.by=df$date)
    dygraph(sno_ts,  main = "Snow Density",group = 'graphs') %>%
      dySeries(color = "purple") %>%
      dyAxis("y", label = "Snow Density (kg/m^3)")
  })
  
  
  output$snotel_temp <-  renderDygraph({
    df = sno_dat()
    sno_ts = xts(df$avg_temp_f,
                 order.by=df$date)
    dygraph(sno_ts,  main = "Avg Daily Temperature",group='graphs') %>%
      dySeries(color = "red") %>%
      dyAxis("y", label = "Temp (F)")
  })
  
} # end server 


shinyApp(ui = ui, server = server)