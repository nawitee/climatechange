library(shiny)
library(leaflet)
library(RColorBrewer)
library(xts)
library(rgdal)
library(shinyWidgets)
library(shinythemes)
load("data/map.RData")
temp_countryF_3 <- readRDS("data/temperature.rds")


#helper function for choropleth animation
setShapeStyle <- function( map, data = getMapData(map), layerId,
                           stroke = NULL, color = NULL,
                           weight = NULL, opacity = NULL,
                           fill = NULL, fillColor = NULL,
                           fillOpacity = NULL, dashArray = NULL,
                           smoothFactor = NULL, noClip = NULL, label = NULL,
                           options = NULL){
  
  options <- c(list(layerId = layerId),
               options,
               filterNULL(list(stroke = stroke, color = color,
                               weight = weight, opacity = opacity,
                               fill = fill, fillColor = fillColor,
                               fillOpacity = fillOpacity, dashArray = dashArray,
                               smoothFactor = smoothFactor, noClip = noClip, label = label
               )))
  
  options <- evalFormula(options, data = data)
  options <- do.call(data.frame, c(options, list(stringsAsFactors=FALSE)))
  
  layerId <- options[[1]]
  style <- options[-1]
  if("label" %in% colnames(style)){
    labelData = style[,"label", FALSE]
    style = style[,-which(colnames(style)=="label"), FALSE]
    leaflet::invokeMethod(map, data, "setLabel", "shape", layerId, label)
  }
  leaflet::invokeMethod(map, data, "setStyle", "shape", layerId, style);
}

#helper function in JS for choropleth animation
leafletjs <-  tags$head(
  tags$script(HTML('
  
window.LeafletWidget.methods.setStyle = function(category, layerId, style){
  var map = this;
  if (!layerId){
    return;
  } else if (!(typeof(layerId) === "object" && layerId.length)){
    layerId = [layerId];
  }
  style = HTMLWidgets.dataframeToD3(style);
  layerId.forEach(function(d,i){
    var layer = map.layerManager.getLayer(category, d);
    if (layer){
      layer.setStyle(style[i]);
    }
  });
};
window.LeafletWidget.methods.setLabel = function(category, layerId, label){
  var map = this;
  if (!layerId){
    return;
  } else if (!(typeof(layerId) === "object" && layerId.length)){
    layerId = [layerId];
  }
  layerId.forEach(function(d,i){
    var layer = map.layerManager.getLayer(category, d);
    if (layer){
      layer.unbindTooltip();
      layer.bindTooltip(label[i])
    }
  });
};
'
  ))
)


selectedTime<- temp_countryF_3 %>% filter(Year==1990)
world_spdf$anomalyTemp<- selectedTime$AnomalyTemp[match(world_spdf$ISO2,selectedTime$Countrycode)]

#label text
world_spdf@data$LabelText <- paste0(
  "<b>Country:</b> ", world_spdf@data$NAME,"<br>", 
  "<b>Anomaly Temperature:</b> ", format(world_spdf@data$anomalyTemp, nsmall=0, big.mark=","))
#define legend
#define colorpalette for chart legend
paletteBins <- c(-3,-1,0,2,4)
colorPalette <- colorBin(palette = "YlOrBr", domain = temp_countryF_3$AnomalyTemp, na.color = "transparent", bins = paletteBins)


#shiny UI
ui <- fluidPage(theme = shinytheme("cyborg"),
  leafletjs,
  titlePanel("World Anomaly Temperature From 1900 Until Now"),
  
  sidebarPanel(width = 2,
               
               uiOutput("yearui")
               
  ),
  
  mainPanel(width = 10,
            
            leafletOutput("map", width = "90%", height = "500px")
            
  )
)


#shiny server
server <- function(input, output, session) {
  
  #create slider input depending on data frequency
  observe({
    
    allyear <- unique(temp_countryF_3$Year)
    
  
    
    output$yearui <- renderUI({
      sliderInput("y", "Year",
                  min = min(allyear),
                  max = max(allyear),
                  value = min(allyear),
                  step = 5,
                  animate = animationOptions(interval = 500, loop = FALSE)
      )
    })
  })
  
  #filter data depending on selected date
  filteredData <- reactive({
    req(input$y)
    temp_countryF_3[temp_countryF_3$Year== input$y, ]
  })
  
  #create the base leaflet map
  output$map <- renderLeaflet({
    
    leaflet(world_spdf) %>% 
      addTiles()  %>% 
      setView(lat = 0, lng = 0, zoom = 2) %>%
      
      addPolygons( 
        layerId = ~ISO2,
        fillColor = "lightgray", 
        stroke = TRUE, 
        fillOpacity = 1, 
        color = "white", 
        weight = 1
      ) %>%
      
      #need to specify the leaflet::addLegend function here to avoid ambiguity with the xts::addLegend function
      leaflet::addLegend(pal = colorPalette, values = temp_countryF_3$AnomalyTemp, opacity = 0.9, title = "Anomaly Temperature", position = "bottomleft")
    
  })
  
  
    observe({
    
    world_spdf$anomalyTemp <- filteredData()$AnomalyTemp[match(world_spdf$ISO2, filteredData()$Countrycode)]
    
    world_spdf@data$LabelText <- paste0(
      "<b>Country:</b> ", world_spdf@data$NAME,"<br>", 
      "<b>Anomaly Temp :</b> ", format(world_spdf@data$anomalyTemp, nsmall=0, big.mark=","))
    
   leafletProxy("map", data = world_spdf) %>%
        clearMarkers() %>%
        setShapeStyle(layerId = ~ISO2, fillColor = ~colorPalette(anomalyTemp), label = world_spdf$LabelText)
             
      
  })
}
shinyApp(ui=ui,server=server)