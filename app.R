### App to create map of air quality sites with NJ fueling stations ###
# Load necessary libraries to make app run #
library(shiny)
library(leaflet)
library(leaflet.extras)
library(readr)
library(sf)
library(shinycssloaders)
library(readxl)
###############################################################################
### Read in site information dataset for monitoring sites in NJ ###
sites_nj<-read_csv("Ambient_Air_Quality_Monitors_of_New_Jersey.csv",col_names = T)%>%
  dplyr::select(-X,-Y,-OBJECTID,-AIRS_CODE)
###############################################################################
### Read in fueling stations info ###
fueling_nj<-read_xlsx("fueling_stations_info.xlsx",col_names = T)%>%
  dplyr::mutate(LON = as.numeric(LON))%>%
  dplyr::mutate(LAT = as.numeric(LAT))
### Read in shapefile of NJ counties ###
counties_nj<-st_read(dsn = getwd(),layer = "NJ_counties")
### Change projection to work with leaflet map ###
counties_nj<-st_transform(counties_nj, crs="+init=epsg:4326")
###############################################################################
### Define UI for application ###
ui <- fluidPage(
   
   ### Application title ###
  h1(id="big-heading", "NJ State Fueling Stations/Air Quality Stations"),
  tags$style(HTML("#big-heading{color: #074dc3;text-align: center;background-color: #D3D3D3;}")),
  
  
   leafletOutput("fuelmap", height = "95vh")%>%
     withSpinner(type = 5, color = "blue")

)
###############################################################################
### Define server logic ###
server <- function(input, output) {
  
  icons <- awesomeIcons(
    icon = 'gas-pump',
    library = 'fa'
    
  )
### Create map of ambient air monitoring stations in NJ ###
  output$fuelmap<-renderLeaflet({
    leaflet(data = sites_nj, options = leafletOptions(minZoom = 7))%>%
      addTiles()%>%
      addResetMapButton()%>%
      addControlGPS(options = gpsOptions(position = "topleft", activate = TRUE, 
                                         autoCenter = TRUE, maxZoom = 60, 
                                         setView = TRUE))%>%
      addTiles(group = "OSM (default)") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite")%>%
      addPolygons(data = counties_nj,popup= paste("County:",counties_nj$COUNTY,sep=""),color = "black",
                  weight = 1,
                  highlightOptions = highlightOptions(color = "blue",
                                                      weight = 2,bringToFront = TRUE))%>%
      setView(lng = -74.4 ,lat =40, zoom = 8)%>%
      addMarkers(~LON,~LAT, popup = ~paste("<h4> Site Name:</h4>",SITE_NAME,sep = ""),
                 label = ~SITE_NAME)%>%
      addAwesomeMarkers(data = fueling_nj,~LON,~LAT,icon = icons,
                        popup = ~paste("<h6> County:</h4>",County,
                                       "<h6> Gas Station Name:</h4>",Name,
                                       "<h6> Hours:</h4>",Hours,
                                       "<h6> Service:</h4>",Service,
                                       "<h6> Address:</h4>",address,sep = ""))%>%
      addLayersControl(
        baseGroups = c( "OSM (default)","Satellite"),
        options = layersControlOptions(collapsed = FALSE))
  })
  
  
}
###############################################################################
### Run the application ###
shinyApp(ui = ui, server = server)

