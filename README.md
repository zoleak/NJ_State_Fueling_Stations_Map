# NJ State Fueling Station Map
<img width="1416" alt="Screen Shot 2022-11-20 at 9 23 00 AM" src="https://user-images.githubusercontent.com/36116239/202907571-06b717bc-3e00-4683-9bb6-afd18d865b9f.png">

## Overview
At my current job with the New Jersey Department of Environmental Protection, I maintain numerous sites throughout the state. 
These sites  have scientific instruments that collect environmental data. The sites are scattered throughout the entire state, which requires the 
field team to drive occasionally. The state of NJ has numerous fueling stations for this exact purpose. Only state-issued vehicles are allowed to use 
these fueling stations. This application was developed to assist field workers on my team to locate fueling stations near their air quality sites. 
They can click the locate me button to have the app geolocate their location.
## Data 
The data used for the fueling station locations was obtained from the following
[website](https://www.nj.gov/treasury/administration/statewide-support/motor-fuel-locations.shtml). The ```rvest``` package was used to webscrape the table
with all the location data for the fueling stations.

## How to use?
This shiny application can be used at the following [website](https://kzolea695.shinyapps.io/NJ_State_Fueling_Stations_Map/)

## Known bugs
The locate me button does not currently work with safari. It works best with google chrome or internet explorer.
