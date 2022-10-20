### App to create map of air quality sites with NJ fueling stations ###
### Load necessary libraries to make app run ###
### Author: Kevin Zolea ###
library(shiny)
library(leaflet)
library(leaflet.extras)
library(readr)
library(sf)
library(shinycssloaders)
library(readxl)
library(rvest)
library(stringr)
library(dplyr)
library(tidyr)
library(shinyMobile)
library(shinyjs)
library(leaflegend)
###############################################################################
### Read in site information dataset for monitoring sites in NJ ###
sites_nj<-read_csv("Ambient_Air_Quality_Monitors_of_New_Jersey.csv",col_names = T)%>%
  dplyr::select(-X,-Y,-OBJECTID,-AIRS_CODE)
###############################################################################
### Read in fueling stations info ###
#fueling_nj2<-read_xlsx("fueling_stations_info.xlsx",col_names = T)%>%
# dplyr::mutate(LON = as.numeric(LON))%>%
 # dplyr::mutate(LAT = as.numeric(LAT))

### Web scrape NJ fueling station website to access each stations data using rvest
url<-"https://www.nj.gov/treasury/administration/statewide-support/motor-fuel-locations.shtml"
css_selector<-"#budget"

fueling_nj<-url%>%
  read_html()%>%
  html_element(css = css_selector)%>%
  html_table()
# Remove Latiutde and longitude character strings in Location column
fueling_nj$Location<-str_remove(fueling_nj$Location,"Latitude:")
fueling_nj$Location<-str_remove(fueling_nj$Location,"Longitude:")
#Separate Location column into LON and LAT columns so we can plot locations on leaflet
fueling_nj<-fueling_nj%>%
  mutate(LON = as.numeric(str_extract(string = Location,
                           pattern = "-.*")),
         LAT = as.numeric(str_extract(string = Location,
                           pattern = "39.*|40.*|41.*")))
### Read in shapefile of NJ counties ###
counties_nj<-st_read(dsn = getwd(),layer = "NJ_counties")
### Change projection to work with leaflet map ###
counties_nj<-st_transform(counties_nj, crs="+init=epsg:4326")
###############################################################################
# Function for  allowing geolocalisation in leaflet map
jsCode <- '
shinyjs.geoloc = function() {
navigator.geolocation.getCurrentPosition(onSuccess, onError);
function onError (err) {
Shiny.onInputChange("geolocation", false);
}
function onSuccess (position) {
setTimeout(function () {
var coords = position.coords;
console.log(coords.latitude + ", " + coords.longitude);
Shiny.onInputChange("geolocation", true);
Shiny.onInputChange("lat", coords.latitude);
Shiny.onInputChange("long", coords.longitude);
}, 5)
}
};
'
### Define UI for application ###
ui <- f7Page(f7SingleLayout(
   
   ### Application title ###
  navbar=f7Navbar(
  title = h1(id="big-heading", "NJ State Fueling Stations/Air Quality Stations"),
  tags$style(HTML("#big-heading{color: #074dc3;text-align: center;background-color: #D3D3D3;}"))),
  
  toolbar = f7Toolbar(
    position = "bottom",
    f7Link(label = "Fueling Station Website",
      href = "https://www.nj.gov/treasury/administration/statewide-support/motor-fuel-locations.shtml")),
  
  # Tell shiny we will use some Javascript
  useShinyjs(),
  extendShinyjs(text = jsCode, functions = c("geoloc")),
  # One button and one map
  #br(),
  #actionButton("geoloc", "Localize me", class="btn btn-primary", onClick="shinyjs.geoloc()"),
  
   ### Leaflet output ###
   leafletOutput("fuelmap", height = "95vh")%>%
     withSpinner(type = 5, color = "blue")

))
###############################################################################
### Define server logic ###
server <- function(input, output) {
  # Create custom icon for leaflet map
  # Icons for air monitoring stations
  air_Icon <- makeIcon(
    iconUrl = "https://aqicn.org/images/logo/regular.png",
    iconWidth = 64, iconHeight = 64,
    iconAnchorX = 22, iconAnchorY = 94,
    shadowUrl = "http://leafletjs.com/examples/custom-icons/leaf-shadow.png",
    shadowWidth = 50, shadowHeight = 64,
    shadowAnchorX = 4, shadowAnchorY = 62
  )
  
  # Icons for gas stations
  gas_Icon <- makeIcon(
    iconUrl = "https://png.pngtree.com/png-vector/20190319/ourlarge/pngtree-vector-gas-icon-png-image_845941.jpg",
    iconWidth = 64, iconHeight = 64,
    iconAnchorX = 22, iconAnchorY = 94,
    shadowUrl = "http://leafletjs.com/examples/custom-icons/leaf-shadow.png",
    shadowWidth = 50, shadowHeight = 64,
    shadowAnchorX = 4, shadowAnchorY = 62
  )
  
### Create map of ambient air monitoring stations in NJ ###
  output$fuelmap<-renderLeaflet({
    leaflet(data = sites_nj, options = leafletOptions(minZoom = 7))%>%
      addTiles()%>%
      addResetMapButton()%>%
      #addControlGPS(options = gpsOptions(position = "topleft", activate = TRUE, 
      #                                   autoCenter = TRUE, maxZoom = 60, 
       #                                  setView = TRUE))%>%
      addTiles(group = "OSM (default)") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite")%>%
      addPolygons(data = counties_nj,popup= paste("County:",counties_nj$COUNTY,sep=""),color = "black",
                  weight = 1,
                  highlightOptions = highlightOptions(color = "blue",
                                                      weight = 2,bringToFront = TRUE))%>%
      setView(lng = -74.4 ,lat =40, zoom = 8)%>%
      addControl(actionButton("geoloc", "Find My Location", class="btn btn-primary", 
                               onClick="shinyjs.geoloc()"))%>%
      addMarkers(~LON,~LAT, popup = ~paste("<h4> Site Name:</h4>",SITE_NAME,
                                           "<h4> Address:</h4>",ADDRESS,sep = ""),
                 label = ~SITE_NAME,
                 icon = air_Icon,
                 clusterOptions = markerClusterOptions())%>%
      addMarkers(data = fueling_nj,~LON,~LAT,
                        popup = ~paste("<h6> County:</h4>",County,
                                       "<h6> Hours:</h4>",Hours,
                                       "<h6> Phone Number:</h4>",`Phone Number`,
                                       "<h6> Fuel Types:</h4>",`Fuel Types`,
                                       "<h6> Address:</h4>",`Fueling Station Address`,sep = ""),
                 icon = gas_Icon,
                 clusterOptions = markerClusterOptions())%>%
      addLegendImage(images = c("https://png.pngtree.com/png-vector/20190319/ourlarge/pngtree-vector-gas-icon-png-image_845941.jpg",
                                "https://aqicn.org/images/logo/regular.png"),
                     labels = c("State Fueling Stations",
                                "Air Monitoring Stations"),
                     height = 30, width = 30,
                     position = "topright")%>%
      addLayersControl(
        baseGroups = c( "OSM (default)","Satellite"),
        options = layersControlOptions(collapsed = FALSE))
  })
  
  # Find geolocalisation coordinates when user clicks
  observeEvent(input$geoloc, {
    js$geoloc()
  })
  # zoom on the corresponding area
  observe({
    if(!is.null(input$lat)){
      map <- leafletProxy("fuelmap")
      dist <- 0.2
      lat <- input$lat
      lng <- input$long
      map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
    }
  })

  
  
}
###############################################################################
### Run the application ###
shinyApp(ui = ui, server = server)

