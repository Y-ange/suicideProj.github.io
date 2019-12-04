library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)

suicidedata <- maindata[order(maindata$suicides_100k_pop),]

function(input, output, session) {
  ## Interactive Map ###########################################
  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
       addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
     setView(lng = 50, lat =  20 , zoom = 2)
})
  
  suicideInBounds <- reactive({
    if (is.null(input$map_bounds))
      return(suicidedata[FALSE,])
    
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    
    subset(suicidedata,
           latitude >= latRng[1] & latitude <= latRng[2] &
           longitude >= lngRng[1] & longitude <= lngRng[2])
  })
  
  
  
  # Precalculate the breaks we'll need for the two histograms
  
  suiciderateBreaks <- hist(plot = FALSE, suicidedata$suicides_100k_pop, breaks = 20)$breaks
  output$hist <- renderPlot({
    
    # If nothing is in view, don't plot
    if (nrow(suicideInBounds()) == 0)
      return(NULL)
    
    hist(suicideInBounds()$suicides_100k_pop,
         breaks = suiciderateBreaks,
         main = "Suicide rate analysis (visible)",
         xlab = "Suicide rate",
         xlim = range(maindata$suicides_100k_pop),
         col = '#00DD00',
         border = 'white')
  })
  
  
  
  output$scatter <- renderPlot({
    # If nothing is in view, don't plot
    
    if (nrow(suicideInBounds()) == 0)
        return(NULL)
 
    print(xyplot(suicides_100k_pop ~ gdp_per_capita, data = suicideInBounds(), xlim = range(maindata$gdp_per_capita), ylim = range(maindata$suicides_100k_pop)))
    
  })
  
  
  
  # This observer is responsible for maintaining the circles and legend,
  
  # according to the variables the user has chosen to map to color and size.
  
  observe({
    
    colorBy <- input$metric
    
    sizeBy <- input$metric
    
    yearBy = input$year1
    
    
    suicidedata2 = suicidedata %>% filter(year == yearBy)
    
    colorData <- suicidedata2[[colorBy]]
      
      pal <- colorBin("viridis", colorData, 7, pretty = FALSE)

    
    
    
   if (sizeBy == "suicides_100k_pop") {
      
     radius <- suicidedata2[[sizeBy]] * 15000
      
    } else {
      
     radius <- suicidedata2[[sizeBy]] * 30
      
    }
      
    
    
    
    leafletProxy("map", data = suicidedata2) %>%
      
      clearShapes() %>%
      
      addCircles(~longitude, ~latitude, radius=radius, layerId=~id,
                 
                 stroke=FALSE, fillOpacity=0.4, fillColor=pal(colorData)) %>%
      
      addLegend("bottomleft", pal=pal, values=colorData, title=colorBy,
                
                layerId="colorLegend")
    })    
  
  
  
  # Show a popup at the given location
  
  showSuicideratePopup <- function(id, lat, lng) {
    
    selectedSuiciderate <- maindata[maindata$id == id,]
    
    content <- as.character(tagList(
      
      tags$h4("Suicide rate: ", round(selectedSuiciderate$suicides_100k_pop, 3)),
      
      tags$strong(HTML(sprintf("%s, %s",
                               
                               selectedSuiciderate$country, selectedSuiciderate$year
                               
      ))), tags$br(),
      
      sprintf("GDP for Year: %s", selectedSuiciderate$gdp_for_year), tags$br(),
      
      sprintf("GDP per Capita: %s", selectedSuiciderate$gdp_per_capita), tags$br(),
      
      sprintf("Population: %s", selectedSuiciderate$population)
      
    ))
    
    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = id)
    
  }
  
  
  
  # When map is clicked, show a popup with country info
  
  observe({
    
    leafletProxy("map") %>% clearPopups()
    
    event <- input$map_shape_click
    
    if (is.null(event))
      
      return()
    
    
    
    isolate({
      
      showSuicideratePopup(event$id, event$lat, event$lng)
      
    })
    
  })
  
  
  
  
  
  ## Data Explorer ###########################################
  
  
  
  observe({
    
    year <- if (is.null(input$country)) character(0) else {
      
      filter(cleantable, Country %in% input$country) %>%
        
        `$`('Year') %>%
        
        unique() %>%
        
        sort()
      
    }
    
    stillSelected <- isolate(input$year[input$year %in% year])
    
    updateSelectizeInput(session, "year", choices = year,
                         
                         selected = stillSelected, server = TRUE)
    
  })

  
  observe({
    
    if (is.null(input$goto))
      
      return()
    
    isolate({
      
      map <- leafletProxy("map")
      
      map %>% clearPopups() %>% clearControls()
      
      dist <- 1
      
      id <- input$goto$id
      
      lat <- input$goto$lat
      
      lng <- input$goto$lng
      
      map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
    })
    
  })
  
  
  
  output$suicidetable <- DT::renderDataTable({
    
    df <- cleantable %>%
      
      filter(
        
        SuicidesRate >= input$minRate,
        
        SuicidesRate <= input$maxRate,
        
        is.null(input$country) | Country %in% input$country,
        
        is.null(input$year) | Year %in% input$year
        
      ) %>%
      
      mutate(Action = paste('<a class="go-map" href="" data-lat="', Lat, '" data-long="', Long, '" data-id="', id, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
    
    action <- DT::dataTableAjax(session, df, outputId = "suicidetable")
    
    
    
    DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
    
  }) 
  
}
  
  
