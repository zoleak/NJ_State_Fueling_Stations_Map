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
  
   ### Leaflet output ###
   leafletOutput("fuelmap", height = "95vh")%>%
     withSpinner(type = 5, color = "blue")

))
###############################################################################
### Define server logic ###
server <- function(input, output) {
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
      addMarkers(~LON,~LAT, popup = ~paste("<h4> Site Name:</h4>",SITE_NAME,
                                           "<h4> Address:</h4>",ADDRESS,sep = ""),
                 label = ~SITE_NAME)%>%
      addMarkers(data = fueling_nj,~LON,~LAT,
                        popup = ~paste("<h6> County:</h4>",County,
                                       "<h6> Hours:</h4>",Hours,
                                       "<h6> Phone Number:</h4>",`Phone Number`,
                                       "<h6> Fuel Types:</h4>",`Fuel Types`,
                                       "<h6> Address:</h4>",`Fueling Station Address`,sep = ""))%>%
      addLayersControl(
        baseGroups = c( "OSM (default)","Satellite"),
        options = layersControlOptions(collapsed = FALSE))
  })
  
  
}
###############################################################################
### Run the application ###
shinyApp(ui = ui, server = server)

