library(broom)
library(dplyr)
library(ggmap)
library(leaflet)
library(leaflet.extras)
library(maptools)
library(RCurl)
library(rvest)
library(sp)
library(tidyverse)
library(tigris)
library(XML)
library(wordcloud2)

ggmap::register_google(key = YOUR_KEY)
neighbourhoods <- read.csv("neighbourhood_codes.csv")

neighbourhood_options <- neighbourhoods$city
neighbourhood_options
items_offset<-c(seq(0,300,20))

#-----------------------------------------------Motor Vehicle Accidents----------------------------------------------
library(jsonlite)
url3 <- "https://data.cityofnewyork.us/resource/h9gi-nx95.json?$limit=50000"
data3 <- fromJSON(url3)
motor_vehicle_accident_data <- as.data.frame(data3)


#Categorizing into fatal, injuries, or neither. 
motor_vehicle_accident_data$severity<-ifelse(motor_vehicle_accident_data$number_of_persons_killed>0,"Fatal",
                                             ifelse(motor_vehicle_accident_data$number_of_persons_injured>0,"Injuries","Neither"))

#Categorizing involvement of the accident-how many ppl involved
motor_vehicle_accident_data$involvement<-as.numeric(motor_vehicle_accident_data$number_of_persons_killed)+
  as.numeric(motor_vehicle_accident_data$number_of_persons_injured)

#trying to calculate if it is multi vehicle or single vehicle. Multi tells us car, single - bang pedestrians.
accident_filtered <- select(motor_vehicle_accident_data, 
                            vehicle_type_code1,vehicle_type_code2,vehicle_type_code_3,vehicle_type_code_4,vehicle_type_code_5)

accident_filtered$NAcount<-rowSums(is.na(accident_filtered))
accident_filtered$vehicles_involved<- 5-accident_filtered$NAcount

motor_vehicle_accident_data<-cbind(motor_vehicle_accident_data,accident_filtered$vehicles_involved)
motor_vehicle_accident_data$vehicles_involved<-ifelse(motor_vehicle_accident_data$`accident_filtered$vehicles_involved`>1,"Multi Vehicle","Single Vehicle")

motor_vehicle_accident_data$day <- weekdays(as.Date(motor_vehicle_accident_data$crash_date))
motor_vehicle_accident_data$longitude[motor_vehicle_accident_data$longitude=="-201.2370600"] <- "-73.95788200"
motor_vehicle_accident_data$longitude[motor_vehicle_accident_data$longitude=="-201.3599900"] <- "-73.83193300"

accident_filtered<-select(motor_vehicle_accident_data,crash_date,day,crash_time,longitude,latitude,severity,
                          involvement,vehicles_involved)

library(httr)
library(rgdal)
r <- GET('http://data.beta.nyc//dataset/0ff93d2d-90ba-457c-9f7e-39e47bf2ac5f/resource/35dd04fb-81b3-479b-a074-a27a37888ce7/download/d085e2f8d0b54d4590b1e7d1f35594c1pediacitiesnycneighborhoods.geojson')
nyc_neighborhoods <- readOGR(content(r,'text'), 'OGRGeoJSON', verbose = F)
accident_filtered <- data.frame(na.omit(accident_filtered))
accident_filtered$latitude<-as.numeric(accident_filtered$latitude)
accident_filtered$longitude<-as.numeric(accident_filtered$longitude)
accident_filtered <- accident_filtered[accident_filtered$latitude != 0 & accident_filtered$longitude != 0,]

points<-select(accident_filtered,latitude,longitude)
points <- data.frame(na.omit(points))

points_spdf <- points
coordinates(points_spdf) <- ~longitude + latitude
proj4string(points_spdf) <- proj4string(nyc_neighborhoods)
matches <- over(points_spdf, nyc_neighborhoods)
points <- cbind(points, matches)


new_table_accidents<-cbind(accident_filtered,points)

names(new_table_accidents)[9] <- "lat"                                          
names(new_table_accidents)[10] <- "lng" 
#leaflet - if injury, neither, fatal

new_table_accidents <- data.frame(na.omit(new_table_accidents))

#Input parameter for app-pick a neighbourhood. 
neighbourhood_options_accidents<-sort(unique(new_table_accidents$neighborhood))
new_table_accidents$severity<-as.factor(new_table_accidents$severity)

new_table_accidents$crash_time_reformatting <- as.POSIXct(new_table_accidents$crash_time,format="%H:%M")
new_table_accidents$crash_time_reformatting<-format(new_table_accidents$crash_time_reformatting, "%H:%M")
new_table_accidents$crash_time_reformatting<-gsub(":", "",new_table_accidents$crash_time_reformatting)

breaks = c(seq(0,2400,100))

time_intervals<-cut(as.numeric(new_table_accidents$crash_time_reformatting), breaks = breaks,include.lowest = TRUE,dig.lab = 5,right=FALSE)
time_intervals
#gsub(":", "",new_table_accidents$crash_time_problem_reformatting)


time_intervals<-gsub(",.*", "",as.character(time_intervals))
time_intervals<-gsub("\\[|\\]", "",as.character(time_intervals))


time_intervals[time_intervals == 0]<- "00:00"
time_intervals[time_intervals == 100]<- "01:00"
time_intervals[time_intervals == 200]<- "02:00"
time_intervals[time_intervals == 300]<- "03:00"
time_intervals[time_intervals == 400]<- "04:00"
time_intervals[time_intervals == 500]<- "05:00"
time_intervals[time_intervals == 600]<- "06:00"
time_intervals[time_intervals == 700]<- "07:00"
time_intervals[time_intervals == 800]<- "08:00"
time_intervals[time_intervals == 900]<- "09:00"
time_intervals[time_intervals == 1000]<- "10:00"
time_intervals[time_intervals == 1100]<- "11:00"
time_intervals[time_intervals == 1200]<- "12:00"
time_intervals[time_intervals == 1300]<- "13:00"
time_intervals[time_intervals == 1400]<- "14:00"
time_intervals[time_intervals == 1500]<- "15:00"
time_intervals[time_intervals == 1600]<- "16:00"
time_intervals[time_intervals == 1700]<- "17:00"
time_intervals[time_intervals == 1800]<- "18:00"
time_intervals[time_intervals == 1900]<- "19:00"
time_intervals[time_intervals == 2000]<- "20:00"
time_intervals[time_intervals == 2100]<- "21:00"
time_intervals[time_intervals == 2200]<- "22:00"
time_intervals[time_intervals == 2300]<- "23:00"

new_table_time<-cbind(new_table_accidents,time_intervals)

#------------------------------------------------Wifi data----------------------------------------------
library(jsonlite)
url2 <- "https://data.cityofnewyork.us/resource/yjub-udmw.json?$limit=50000"
data2 <- fromJSON(url2)
hotspot_data <- as.data.frame(data2)

hotspot_data_filtered<-hotspot_data %>% 
  select(type, name, provider, location, latitude, longitude, remarks, location_t,ssid)

hotspot_data_filtered$latitude<-as.numeric(hotspot_data_filtered$latitude)
hotspot_data_filtered$longitude<-as.numeric(hotspot_data_filtered$longitude)
hotspot_data_filtered <- hotspot_data_filtered[hotspot_data_filtered$latitude != 0 & hotspot_data_filtered$longitude != 0,]

points<-select(hotspot_data_filtered,latitude,longitude)
points <- data.frame(na.omit(points))

points_spdf <- points
coordinates(points_spdf) <- ~longitude + latitude
proj4string(points_spdf) <- proj4string(nyc_neighborhoods)
matches <- over(points_spdf, nyc_neighborhoods)
points <- cbind(points, matches)

new_table_hotspot<-cbind(hotspot_data_filtered,points)

neighbourhood_options_wifi<-sort(unique(new_table_hotspot$neighborhood))

#----------------------------------------------Subway data--------------------------------------------------------
subway_stations<-read.csv("DOITT_SUBWAY_STATION_01.csv")

subway_stations$the_geom<-gsub("POINT ", "",subway_stations$the_geom)
subway_stations$the_geom<-gsub("[()]", "",subway_stations$the_geom)

subway_stations_updated<-subway_stations %>%separate(the_geom, c("longitude", "latitude"), " ")

subway_stations_updated$latitude<-as.numeric(subway_stations_updated$latitude)
subway_stations_updated$longitude<-as.numeric(subway_stations_updated$longitude)
subway_stations_updated <- subway_stations_updated[subway_stations_updated$latitude != 0 & subway_stations_updated$longitude != 0,]

points<-select(subway_stations_updated,latitude,longitude)
points <- data.frame(na.omit(points))

points_spdf <- points
coordinates(points_spdf) <- ~longitude + latitude
proj4string(points_spdf) <- proj4string(nyc_neighborhoods)
matches <- over(points_spdf, nyc_neighborhoods)
points <- cbind(points, matches)

subway_stations_updated<-cbind(subway_stations_updated,points)

neighbourhood_options_subway<-sort(unique(subway_stations_updated$neighborhood))

#--------------------------------------------------Crime rates-----------------------------------------
curlSetOpt(timeout = 2000)

library(jsonlite)
url <- "https://data.cityofnewyork.us/resource/5uac-w243.json?$limit=50000"
data <- fromJSON(url)
crime_data <- as.data.frame(data)
head(crime_data, n=2)
#can calculate crime per 1000 residents

crime_data_filtered<-crime_data%>%select(longitude,latitude,addr_pct_cd,law_cat_cd,ofns_desc,ky_cd,cmplnt_fr_dt,cmplnt_fr_tm)

names(crime_data_filtered)[3] <- "Precinct"                                          
names(crime_data_filtered)[4] <- "Offence_level" 
names(crime_data_filtered)[7] <- "Date" 
names(crime_data_filtered)[8] <- "Time" 


crime_data_filtered$latitude<-as.numeric(crime_data_filtered$latitude)
crime_data_filtered$longitude<-as.numeric(crime_data_filtered$longitude)
crime_data_filtered <- crime_data_filtered[crime_data_filtered$latitude != 0 & crime_data_filtered$longitude != 0,]

points<-select(crime_data_filtered,latitude,longitude)
points <- data.frame(na.omit(points))

points_spdf <- points
coordinates(points_spdf) <- ~longitude + latitude
proj4string(points_spdf) <- proj4string(nyc_neighborhoods)
matches <- over(points_spdf, nyc_neighborhoods)
points <- cbind(points, matches)

crime_data_filtered<-cbind(crime_data_filtered,points)

names(crime_data_filtered)[2] <- "lat"                                          
names(crime_data_filtered)[1] <- "lng" 

neighbourhood_options_crime<-sort(unique(crime_data_filtered$neighborhood))

#---------------------------------------------------------------------------------------------------------

crime_data_filtered$day<-weekdays(as.Date(crime_data_filtered$Date))
crime_data_filtered$time_reformatting <- as.POSIXct(crime_data_filtered$Time,format="%H:%M")
crime_data_filtered$time_reformatting<-format(crime_data_filtered$time_reformatting, "%H:%M")
crime_data_filtered$time_reformatting<-gsub(":", "",crime_data_filtered$time_reformatting)

breaks = c(seq(0,2400,100))

time_intervals<-cut(as.numeric(crime_data_filtered$time_reformatting), breaks = breaks,include.lowest = TRUE,dig.lab = 5,right=FALSE)

time_intervals<-gsub(",.*", "",as.character(time_intervals))
time_intervals<-gsub("\\[|\\]", "",as.character(time_intervals))


time_intervals[time_intervals == 0]<- "00:00"
time_intervals[time_intervals == 100]<- "01:00"
time_intervals[time_intervals == 200]<- "02:00"
time_intervals[time_intervals == 300]<- "03:00"
time_intervals[time_intervals == 400]<- "04:00"
time_intervals[time_intervals == 500]<- "05:00"
time_intervals[time_intervals == 600]<- "06:00"
time_intervals[time_intervals == 700]<- "07:00"
time_intervals[time_intervals == 800]<- "08:00"
time_intervals[time_intervals == 900]<- "09:00"
time_intervals[time_intervals == 1000]<- "10:00"
time_intervals[time_intervals == 1100]<- "11:00"
time_intervals[time_intervals == 1200]<- "12:00"
time_intervals[time_intervals == 1300]<- "13:00"
time_intervals[time_intervals == 1400]<- "14:00"
time_intervals[time_intervals == 1500]<- "15:00"
time_intervals[time_intervals == 1600]<- "16:00"
time_intervals[time_intervals == 1700]<- "17:00"
time_intervals[time_intervals == 1800]<- "18:00"
time_intervals[time_intervals == 1900]<- "19:00"
time_intervals[time_intervals == 2000]<- "20:00"
time_intervals[time_intervals == 2100]<- "21:00"
time_intervals[time_intervals == 2200]<- "22:00"
time_intervals[time_intervals == 2300]<- "23:00"

new_table<-cbind(crime_data_filtered,time_intervals)

#---------------------------------------------------------------------------------------------------------

population_data<-read.csv("pediacities_nyc_neighborhood_populations.csv")

points_by_neighborhood <- points %>%
  group_by(neighborhood) %>%
  summarize(num_points=n())

#Change to character for left join so that no data is lost.Factor gives a warning.
points_by_neighborhood$neighborhood<-as.character(points_by_neighborhood$neighborhood)
population_data$neighborhood<-as.character(population_data$neighborhood)


points_by_neighborhood<-left_join(points_by_neighborhood,population_data, by = "neighborhood")
#Some kind of outlier
points_by_neighborhood$population[points_by_neighborhood$population == 105]<-NA

points_by_neighborhood$crime_rate_per_thousand <-(points_by_neighborhood$num_points/points_by_neighborhood$population)*1000
points_by_neighborhood$crime_rate_per_thousand<-format(round(points_by_neighborhood$crime_rate_per_thousand, 2), nsmall = 2)
points_by_neighborhood$crime_rate_per_thousand<-as.numeric(points_by_neighborhood$crime_rate_per_thousand)

map_data <- geo_join(nyc_neighborhoods, points_by_neighborhood, "neighborhood", "neighborhood")

pal <- colorNumeric(palette = "Reds",
                    domain = range(map_data@data$crime_rate_per_thousand, na.rm=T))

#---------------------------------------------------Ui----------------------------------------------------
library(shiny)
library(shinycssloaders)
library(shinythemes)
library(shinyWidgets)
options(spinner.color="#2c3e50", spinner.color.background="#ffffff", spinner.size=0.7)
ui <- navbarPage("New York", theme = shinytheme("flatly"),
                 tabPanel("Airbnb",
                          sidebarPanel(
                            tags$div(h6("Do remember to narrow your search before searching!")),
                            selectInput("neighbourhoods_Airbnb", label="Neighbourhoods", choices = neighbourhood_options_accidents),
                            sliderInput("price_range", label="Price range", min=10, max=1000, value=c(35,199),pre = "$", sep = ","),
                            dateRangeInput(inputId = "date", "Date:", start=Sys.Date()),
                            numericInput("num_of_adults", label="Number of adults", min=0,value=0),
                            numericInput("num_of_children", label="Number of children", min=0, value=0),
                            numericInput("num_of_infants", label="Number of infants", min=0,value=0),
                            actionButton("go_Airbnb", "Go")),
                          
                          mainPanel(withSpinner(leafletOutput(outputId="mapAirbnb", height=530),type=6))
                 ),
                 tabPanel("Attractions",
                          sidebarPanel(
                            selectInput("neighbourhoods_Attractions", label="Neighbourhoods", choices = neighbourhood_options_accidents),
                            actionButton("go_Attractions", "Go")),
                          
                          
                          mainPanel(withSpinner(leafletOutput(outputId="mapAttractions", height=530),type=6))
                 ),
                 tabPanel("Restaurants",
                          
                          sidebarPanel(
                            selectInput("neighbourhoods", label="Neighbourhoods", choices = neighbourhood_options),
                            selectInput("meals", label="Meal type", choices = c("Breakfast", "Brunch", "Lunch", "Dinner")),
                            radioButtons("prices", label="Price", choices=c("Cheap Eats", "Mid-range", "Fine Dining")),
                            selectInput("diets", label="Dietary preference", choices = c("No preference", "Vegetarian Friendly", "Vegan Options", "Halal", "Kosher", "Gluten Free Options")),
                            actionButton("go_Restaurants", "Go")
                          ), 
                          mainPanel(withSpinner(leafletOutput(outputId="mapRestaurant", height=530),type=6))
                 ),
                 
                 tabPanel("Motor Vehicle Accidents", sidebarLayout(
                   sidebarPanel(
                     selectInput("neighbourhoods_accidents", label="Neighbourhoods", choices=neighbourhood_options_accidents)
                   ),
                   mainPanel(
                     tabsetPanel(
                       tabPanel("Map", withSpinner(leafletOutput(outputId = "accidentsMap", height=480),type=6)),
                       tabPanel("Number of Accidents by Severity", 
                                withSpinner(plotOutput("days_accident"),type=6),
                                withSpinner(plotOutput("vehicles_days"),type=6)),
                       tabPanel("Number of Accidents by Vehicle",
                                withSpinner(plotOutput("time_accident"),type=6),
                                withSpinner(plotOutput("vehicles_time"),type=5))
                     ))
                 )),
                 tabPanel("Wifi & Subway", sidebarPanel(
                   tags$div(h4(p(strong("Wifi")))), tags$div(h6("Free: Green, Limited Free: Orange, Partner Site: Red")), 
                   selectInput("neighbourhoods_wifi", label="Neighbourhoods", choices=neighbourhood_options_wifi),
                   tags$div(h4(p(strong("Subway")))),
                   selectInput("neighbourhoods_subway", label="Neighbourhoods", choices=neighbourhood_options_subway)),
                   mainPanel(tabsetPanel(tabPanel("Map",leafletOutput(outputId = "wifiSubwayMap", height=480))))
                 ),
                 tabPanel("Crime Rates", sidebarLayout(
                   sidebarPanel(
                     selectInput("neighbourhoods_crime", label="Neighbourhoods", choices=neighbourhood_options_crime)
                   ),
                   mainPanel(
                     tabsetPanel(
                       tabPanel("Map",withSpinner(leafletOutput(outputId = "marker", height=480),type=6)),
                       tabPanel("Overview",withSpinner( leafletOutput(outputId="polygon", height=480),type=6)), 
                       tabPanel("Number of Crimes by Days of Week ",
                                withSpinner(plotOutput("crimePlot1"),type=6)),
                       tabPanel("Number of Crimes by Time of Day",
                                withSpinner(plotOutput("crimePlot2"),type=6)),
                       tabPanel("Top 10 Crimes",
                                withSpinner(tableOutput("crimeTable"),type=6))
                     )))),
                 tabPanel("Twitter Analysis", sidebarLayout(
                   sidebarPanel(
                     textInput("searchTerm", "Find out what's trending!"),
                     textInput("filterTerm", "Keywords to exclude"),
                     sliderInput("maxTweets","Number of recent tweets to use for analysis:",min=5,max=1000,value=500), 
                     actionButton("analyse", "Analyse")
                   ),
                   mainPanel(
                     tags$div(h4(p(strong("Word Cloud")))),
                     withSpinner(wordcloud2Output(outputId = "word", height = 530),type=6))
                 )),setBackgroundColor("#F5F5F5")
)

server <- function(input, output) {
  dataReactive <- reactiveValues(clickedMarker=NULL)
  
  output$mapRestaurant <- renderLeaflet({
    data <- GetRestaurantsByPriceByDishByDiet()
    leaflet() %>% addTiles() %>% 
      addAwesomeMarkers(data=data, lng=~Long, lat=~Lat,
                 popup=~paste("<h4 align = center style=color:BLUE;>",Name,"</h4>",
                              "<img src = ",Image_Url, "width = 110 height = 110 align=left  hspace=10/>",
                              "<b>Stars: </b>",Stars,"<br>",
                              "<b>Cuisine: </b>",Cuisine,"<br>",
                              "<b>Opening hours: </b>",Opening_Hours,"<br>",
                              "<a href=",Restaurant_Url,">Read restaurant review</a>",
                              "<br><br><br><br>",sep=" "),
                 popupOptions = popupOptions(minWidth=300, closeOnClick =  TRUE),
                 icon = awesomeIcons(icon="cutlery", library="fa", markerColor = "blue")
                                     ,clusterOptions = markerClusterOptions(maxClusterRadius = 20))
  })
  
  output$mapAirbnb <- renderLeaflet({
    df_total <- GetAirbnb()
    leaflet() %>% addTiles() %>% 
      addAwesomeMarkers(data = df_total, lng = ~lng, lat = ~lat, popup = ~paste( 
        "<h4 align = center style=color:MEDIUMORCHID;>",name,"</h4>",
        "<img src = ",picture_url, "width = 110 height = 110 align=left  hspace=10/>",
        "<b>Type:</b>", space_type_and_superhost,"<br>",
        "<b>Rating (Review):</b>", ratings_review,"<br>",
        
        "<b>Amenities:</b>", preview_amenities, "<br>",
        "<b>Guests:</b>", person_capacity,"<br>",
        "<b>Features:</b>", feature,"<br>",
        "<b>Total Price:</b>", Total_Price,"<br>",
        "<a href=",url," align = center >Find out more!</a>",sep=" "),
        popupOptions =popupOptions(minWidth = 389, closeOnClick = TRUE),
        icon = awesomeIcons(markerColor = "black"),
        clusterOptions = markerClusterOptions(maxClusterRadius = 20))
  })
  
  output$mapAttractions <- renderLeaflet({
    data_POI <- GetAttraction()
    leaflet() %>%  addTiles() %>%
      addAwesomeMarkers(data = data_POI, lng = ~Longitude, lat = ~Latitude, popup = ~paste( 
        "<h4 align=middle style=color:MEDIUMORCHID;>",PointOfInterest,"</h4>",
        "<img src = ",image_url, "width = 110 height = 110 align=left   hspace=10/>","<br>",
        "<b>Ratings:</b>", Rating,"<br>",
        "<b>Reviews:</b>", Numberofratings,"<br>",
        "<b>Open/Closed:</b>", opening_hour,"<br>",
        "<a href=",link," > Google it!</a>",sep=" ","<br>","<br>","<br>"),
        popupOptions =popupOptions(minWidth = 280,maxHeight = 500, closeOnClick = TRUE),
        icon = awesomeIcons(icon="camera", library="fa",markerColor = "blue"),
        clusterOptions = markerClusterOptions(maxClusterRadius = 20))
    
  })
  
  output$accidentsMap <- renderLeaflet({
    #Colour palatte based on Fatal - red, Injuries - Blue, Neither - orange.
    new_table_accidents<-data.frame(na.omit(new_table_accidents))
    pal <- colorFactor(c("red","navy","orange"), domain =  new_table_accidents[new_table_accidents$neighborhood==input$neighbourhoods_accidents,]$severity)
    groups = as.character(unique(new_table_accidents[new_table_accidents$neighborhood==input$neighbourhoods_accidents,]$severity))
    
    map = leaflet(new_table_accidents[new_table_accidents$neighborhood==input$neighbourhoods_accidents,]) %>% addTiles()
    df<-new_table_accidents[new_table_accidents$neighborhood==input$neighbourhoods_accidents,]
    
    for(g in groups){
      d = df[df$severity == g, ]
      
      map = map %>% addCircleMarkers(data = d, lng = ~as.numeric(longitude), lat = ~as.numeric(latitude), 
                                     fillColor = ~pal(severity),weight = 2, color = "black", stroke = TRUE, fillOpacity = 0.8,
                                     group = g) 
    }
    map %>% addLayersControl(overlayGroups = groups) %>%
      addLegend("bottomright", pal=pal, values=~severity, title="Severity")
  })
  
  output$days_accident <- renderPlot({
    ggplot(filter(new_table_time,new_table_time$neighborhood==input$neighbourhoods_accidents)) + geom_bar(aes(x=day, fill=severity),position=position_dodge(),colour="black") + ylab("Number of Accidents") + theme(axis.text.x = element_text(angle = 0))
  })
  
  output$vehicles_days <- renderPlot({
    ggplot(filter(new_table_time,new_table_time$neighborhood==input$neighbourhoods_accidents)) + geom_bar(aes(x=time_intervals, fill=severity),position=position_dodge()) + ylab("Number of Accidents") + theme(axis.text.x = element_text(angle = 0))
  })
  
  output$time_accident <- renderPlot({
    ggplot(filter(new_table_time,new_table_time$neighborhood==input$neighbourhoods_accidents)) + geom_bar(aes(x=day, fill=vehicles_involved),position=position_dodge(),colour="black")+scale_fill_manual(values = c("Orange","Green"))+ ylab("Number of Accidents") + theme(axis.text.x = element_text(angle = 0))
  })
  
  output$vehicles_time <- renderPlot({
    ggplot(filter(new_table_time,new_table_time$neighborhood==input$neighbourhoods_accidents)) + geom_bar(aes(x=time_intervals, fill=vehicles_involved),position=position_dodge())+scale_fill_manual(values = c("Orange","Green"))+ ylab("Number of Accidents") + theme(axis.text.x = element_text(angle = 0))
  })
  
  output$wifiSubwayMap <- renderLeaflet({
    #need to download the 3 icons. Green icon = free, Orange = limited free, Red = need to use their website.
    wifiIcons <- iconList(
      "Free" = makeIcon("wifi-16 (green).png", iconWidth=30, iconHeight=30),
      "Limited Free" = makeIcon("wifi-16 (yellow).png", iconWidth=30, iconHeight=30),
      "Partner Site" = makeIcon("wifi-16 (red).png", iconWidth=30, iconHeight=30)
    )
    
    groups = as.character(unique(new_table_hotspot[new_table_hotspot$neighborhood==input$neighbourhoods_wifi,]$type))
    groups <- groups[!is.na(groups)]
    map = leaflet(new_table_hotspot[new_table_hotspot$neighborhood==input$neighbourhoods_wifi,]) %>% addTiles()
    df<-new_table_hotspot[new_table_hotspot$neighborhood==input$neighbourhoods_wifi,]

    for (g in groups) {
      d = df[df$type == g, ]
      map <- map %>% addMarkers(data = d,
                   lng = ~as.numeric(longitude), lat = ~as.numeric(latitude), group = g, 
                   icon = ~wifiIcons[type], 
                   popup = ~paste( 
                     "<h4 align = center style=color:blue;>",name,"</h4>",
                     "<b>Wifi name:</b>", ssid,"<br>",
                     "<b>Provider:</b>", provider,"<br>",
                     "<b>Type:</b>", location_t,"<br>",
                     "<b>Location:</b>", location, "<br>",
                     "<b>Remarks:</b>", remarks)) %>%
        addMarkers(data = subway_stations_updated[subway_stations_updated$neighborhood==input$neighbourhoods_subway,], 
                   lng = ~as.numeric(longitude), lat = ~as.numeric(latitude),
                   popup = ~paste( 
                     "<h4 align = center style=color:blue;>",NAME,"</h4>",
                     "<b>line:</b>", LINE,"<br>")
                   ,icon = makeIcon("Metro.png", iconWidth=20, iconHeight=20)) 
    }
    map %>% addLayersControl(overlayGroups = groups) 
  })
  
  output$marker <- renderLeaflet({
    crime_data_filtered<-data.frame(na.omit(crime_data_filtered))
    pal <- colorFactor(c("red","yellow","orange"), domain =  crime_data_filtered[crime_data_filtered$neighborhood==input$neighbourhoods_crime,]$Offence_level)
    groups = as.character(unique(crime_data_filtered[crime_data_filtered$neighborhood==input$neighbourhoods_crime,]$Offence_level))
    map = leaflet(crime_data_filtered[crime_data_filtered$neighborhood==input$neighbourhoods_crime,]) %>% addTiles()
    df<-crime_data_filtered[crime_data_filtered$neighborhood==input$neighbourhoods_crime,]
    for(g in groups){
      d = df[df$Offence_level == g, ]
      
      map = map %>% addCircleMarkers(data = d, lng = ~as.numeric(longitude), lat = ~as.numeric(latitude), 
                                     fillColor = ~pal(Offence_level),weight = 2, color = "black", stroke = TRUE, fillOpacity = 0.8,
                                     popup = ~paste(ofns_desc),
                                     group = g)
      
    }
    map %>% addLayersControl(overlayGroups = groups) %>% 
      addLegend("bottomright", pal=pal, values=~Offence_level, title="Offence Level")
    
   
  })
  
  output$polygon <- renderLeaflet({
    leaflet(map_data) %>%
      addTiles() %>% 
      addPolygons(weight=1.5,fillOpacity = 0.8,
                  fillColor = ~pal(crime_rate_per_thousand), popup = ~paste(
                    "<h4 align = center style=color:orange;>",neighborhood,"</h4>",
                    "<b>crime rate per 1000:</b>",crime_rate_per_thousand,"<br>",
                    "<b>total count:</b>",num_points)) %>%
      
      addProviderTiles("CartoDB.Positron") %>%
      setView(-73.98, 40.75, zoom = 11)  
  })
  
  output$crimePlot1 <- renderPlot({
    ggplot(filter(new_table,new_table$neighborhood==input$neighbourhoods_crime)) + geom_bar(aes(x=day, fill=Offence_level),position=position_dodge(),colour="black") + ylab("Number of Crimes") + theme(axis.text.x = element_text(angle = 0))
  })
  
  output$crimePlot2 <- renderPlot({
    #ggplot(filter(new_table_time,new_table_time$neighborhood==input$neighbourhoods_accidents)) + geom_bar(aes(x=time_intervals, fill=vehicles_involved),position=position_dodge())+scale_fill_manual(values = c("Orange","Green"))+ ylab("Number of Accidents") + theme(axis.text.x = element_text(angle = 0))
    
    ggplot(filter(new_table,new_table$neighborhood==input$neighbourhoods_crime)) + geom_bar(aes(x=time_intervals, fill=Offence_level),position=position_dodge()) + ylab("Number of Crimes") + theme(axis.text.x = element_text(angle = 0))
  })
  
  output$crimeTable <- renderTable({
    top_10_crimes<-as.data.frame(crime_data_filtered%>%filter(neighborhood==input$neighbourhoods_crime)%>% group_by(ofns_desc)%>%count()%>%arrange(desc(n))%>%head(10))
    names(top_10_crimes)[1]<-"Type of Crime"
    names(top_10_crimes)[2]<-"Count"
    top_10_crimes$Percentage_of_total <- top_10_crimes$Count/ nrow(new_table[new_table$neighborhood==input$neighbourhoods_crime,]) * 100
    top_10_crimes$Percentage_of_total<-format(round(top_10_crimes$Percentage_of_total, 2), nsmall = 2)
    top_10_crimes$Percentage_of_total <- as.character(top_10_crimes$Percentage_of_total)
    top_10_crimes$Percentage_of_total <- paste0(top_10_crimes$Percentage_of_total," %")
    top_10_crimes
  }, align = 'r')
  
  output$word <- renderWordcloud2({ 
    text_word<-wordclouds()
    pal2 <- brewer.pal(8, "Dark2")
    wordcloud2(text_word, color='random-dark', size= 0.7 , minSize = 3, shape = "pentagon"
               )
    
  }) 
  
  wordclouds<-eventReactive(input$analyse,{
    library(tm)
    library(wordcloud)
    library(rtweet)
    library("httpuv")
    library("tidytext")
    library("curl")
    library("wordcloud2")
    

    search.text <- input$searchTerm
    
    filter.text <- input$filterTerm
    filtered.text <- unlist(strsplit(filter.text," "))
    create_token(
      app = "R_Project",
      consumer_key = "BOn8hVzQ2SZquLbrm11d3pvpr",
      consumer_secret = "dfrIeAUxzcfjhduqhaxUGhAFrp4EKsqokep0GvWMxtVCm1UFC0",
      access_token="385953975-Z5D1rxpBNf7KqWSTiS0l01kSNVX5F79FMk7RStxG",
      access_secret="A8mRRCctIIhFr6GWAOZvGoiwse9lLatmLCi7uxqYUc18n")
    
    tweets<- search_tweets(q=search.text, n=input$maxTweets, lang="en", include_rts=FALSE)
    tweets<- tweets %>% unnest_tokens(word,text)
    
    data(stop_words)
    
    tweets<- tweets %>% anti_join(stop_words)
    tweets<- tweets %>% count(word, sort=TRUE)
    filtered.text <- c(filtered.text,search.text)
    tweets<- tweets %>% filter(!word %in% c(filtered.text))
    tweets<- tweets %>% filter(!word %in% c("singapore","register","join","event","twitter","https","t.co","amp"))
    
    return (tweets)
  })

  GetRestaurantsByPriceByDishByDiet <-eventReactive(input$go_Restaurants,{
    meals <- input$meals
    diets <- input$diets
    prices <- input$prices
    neighbourhood <- input$neighbourhoods
    meals <- ifelse(meals=="Breakfast", "zfp10597", ifelse(meals=="Brunch", "zfp10606", ifelse(
      meals=="Lunch", "zfp30", "zfp58")))
    diets <- ifelse(diets=="No preference", "", ifelse(diets=="Vegetarian Friendly", "-zfz10665", ifelse(
      diets=="Vegan Options", "-zfz10697", ifelse(diets=="Halal", "-zfz10768", ifelse(diets=="Kosher", "-zfz10751", "-zfz100992")))))
    prices <- ifelse(prices=="Cheap Eats", "p1", ifelse(prices=="Mid-range", "p6", "p8"))
    
    n.neighbourhood <- as.character(neighbourhoods[neighbourhoods$city==neighbourhood,"code"])
    n.borough <- as.character(neighbourhoods[neighbourhoods$city==neighbourhood,"Borough_code"])
    n.name <- as.character(neighbourhoods[neighbourhoods$city==neighbourhood,"name"])
    
    data <-data.frame()
    data.new <- data.frame()
    n <- 0
    while(T) {
      data.new <- CrawlOnePage(data.new, meals, diets, prices, n.neighbourhood, n.borough, n.name, n)
      if (length(data.new)<37) {
        data <- rbind(data, data.new)
        break
      }
      else {
        data <- rbind(data, data.new)
        n <- n + 30
      }
    }
    return (data)
  })
  
  CrawlOnePage <- function(d, meals, diets, prices, n.neighbourhood, n.borough, n.name, n) {
    url <- paste0("https://www.tripadvisor.com.sg/Restaurants-",n.borough,"-oa",n,"-",prices,"-",n.neighbourhood,"-",meals,diets,"-",n.name,"_New_York.html")
    page <- read_html(url)
    restaurant.urls <- html_nodes(page, "._15_ydu6b") %>% html_attr('href')
    
    data <- data.frame()
    for (restaurant.url in restaurant.urls) {
      restaurant.url <- paste0("https://www.tripadvisor.com.sg",restaurant.url)
      data.new <- CrawlOneRestaurant(restaurant.url)
      data <- rbind(data, data.new)
    }
    return (data)
  }
  
  CrawlOneRestaurant <- function(restaurant.url) {
    dietary <- c("Vegetarian Friendly", "Vegan Options", "Halal", "Kosher", "Gluten Free Options")
    page <- read_html(restaurant.url)
    name <- html_node(page, ".h1") %>% html_text()
    image_url <- html_node(page, ".basicImg") %>% html_attr("data-lazyurl")
    stars <- html_node(page, ".restaurants-detail-overview-cards-RatingsOverviewCard__overallRating--nohTl") %>% html_text()
    
    address <- html_node(page, ".address .level_4") %>% html_text()
    geocode <- geocode(address, output="latlona", source="google")
    long <- as.numeric(geocode[1])
    lat <- as.numeric(geocode[2])
    
    opening_hours<-html_node(page,".public-location-hours-LocationHours__bold--2oLr-+ span") %>% html_text()
    if (is.na(opening_hours) == TRUE) {
      opening_hours = "-"
    } else if (opening_hours == "See all hours") {
      opening_hours = "Closed"
    } 
    
    tags <- html_node(page, ".tagsContainer") %>% html_text()
    tagList <- as.list(strsplit(tags, ",")[[1]])
    tagList <- tagList[-1]
    cuisine <- vector()
    i = 1
    for (diet in tagList) {
      exact = FALSE
      for (d in dietary) {
        if (diet == d) {
          exact = TRUE
        }
      }
      if (exact == FALSE) {
        cuisine[i] <- diet
        i = i + 1
      } 
    }
    cuisine <- paste(cuisine, collapse=",")
    data <- data.frame(name=name, stars=stars, restaurant.url=restaurant.url, image.url=image_url, address=address, long=long, lat=lat, cuisine=c(cuisine), opening.hours=opening_hours)
    colnames(data) <- c("Name", "Stars", "Restaurant_Url", "Image_Url", "Address", "Long", "Lat", "Cuisine", "Opening_Hours")
    return (data)
  }
  
  GetAirbnb <- eventReactive(input$go_Airbnb,{
    shiny::validate(
      need(as.character(input$date[2]) > as.character(input$date[1]), "End date should not be the same or before start date!")
    )
    neighbourhood <- input$neighbourhoods_Airbnb
    min_price <- input$price_range[1]
    max_price <- input$price_range[2]
    start_date <- as.character(input$date[1])
    end_date <- as.character(input$date[2])
    num_of_adults <- input$num_of_adults
    num_of_children <- input$num_of_children
    num_of_infants <- input$num_of_infants
    
    neighbourhoods_to_function<-gsub(" ", "%20",neighbourhood)
    neighbourhoods_to_function<-gsub("/", "%20",neighbourhoods_to_function)
    neighbourhoods_to_function<-gsub("[()]", "",neighbourhoods_to_function)
    
    query<-c()
    url<-c()
    df_total<-data.frame()
    
    for (i in 1:length(items_offset)){ 
      urltest<-paste("https://www.airbnb.com.sg/api/v2/explore_tabs?_
      format=for_explore_search_web&adults=",num_of_adults,"&auto_ib=true&checkin=",start_date,"&
      checkout=",end_date,"&children=",num_of_children,"&client_session_id=49f80fc4-0ff0-4c19-9ee5-
      8b42f4390345&currency=USD&current_tab_id=home_tab&experiences_per_grid=20&
      federated_search_session_id=cf433aec-f2b4-45ba-9fcd-baa87538a072&fetch_filters
      =true&guidebooks_per_grid=20&has_zero_guest_treatment=true&hide_dates_and_guests_filters=false
      &infants=",num_of_infants,"&is_guided_search=true&is_new_cards_experiment=true&is_standard_search=true&
      items_offset=",items_offset[i],"&items_per_grid=20&key=d306zoyjsyarp7ifhu67rjxn52tv0t20&locale=en-SG&
      metadata_only=false&place_id=ChIJQSrBBv1bwokRbNfFHCnyeYI&price_max=",max_price,"&price_min=",min_price,"
      &query=",neighbourhoods_to_function,"%2C%20NY&query_understanding_enabled=true
      &refinement_paths%5B%5D=%2Fhomes&s_tag=WdUKaxPM&satori_version=1.2.3&screen_height=416&
      screen_size=large&screen_width=1821&search_type=pagination&section_offset=6&selected_tab_id=home_tab&
      show_groupings=true&supports_for_you_v3=true&timezone_offset=480&version=1.7.3",sep="")
      query[i] <- strwrap(urltest, width=10000, simplify=TRUE)
      query[i]<-gsub(" ", "", query[i], fixed = TRUE)
        
      library(jsonlite)
        
      data1 <- fromJSON(query[i])
        
      airbnblisting<-as.data.frame(data1$explore_tabs$sections[[1]]$listings[[1]]$listing)
        
      #problem<-"Error in .f(.x[[i]], ...) : object 'lat' not found"
      if (!exists('lat', where=airbnblisting)) break
        
        
      simplifiedlistingtable<-select(airbnblisting, name,lat,lng,star_rating,reviews_count,preview_amenities,bathrooms,bedrooms,beds,person_capacity,picture_url,is_superhost,space_type)
      airbnblistingpricetotal<-as.data.frame(data1$explore_tabs$sections[[1]]$listings[[1]]$pricing_quote$price$total$amount_formatted)
      airbnblistingpricepernight<-as.data.frame(data1$explore_tabs$sections[[1]]$listings[[1]]$pricing_quote$price_string)
        
      data1$explore_tabs$sections[[1]]$listings[[1]]$pricing_quote$price$total
        
      Room_ID = c(data1$explore_tabs$sections[[1]]$listings[[1]]$listing$id)
        
      for (i in 1:length(Room_ID)){
        url <- paste("https://www.airbnb.com.sg/rooms/",Room_ID,"
        ?adults=",num_of_adults,"&children=",num_of_children,"&infants=",num_of_infants,"
        &check_in=",start_date,"&check_out=",end_date,"&
        source_impression_id=p3_1584890175_Q%2FEree8qG%2B0NnkK7",sep = "")
        url <- strwrap(url, width=10000, simplify=TRUE)
        url <-gsub(" ", "", url, fixed = TRUE)
        url_dataframe<-as.data.frame(url)
      }
        
      finalairbnbprice<-cbind(airbnblistingpricepernight,airbnblistingpricetotal)
      totaltable<-cbind(simplifiedlistingtable,finalairbnbprice)
      totaltablewithurl<-cbind(totaltable,url_dataframe)
      df_total<-rbind(df_total,totaltablewithurl)
    }  
      
    names(df_total)[14] <- "Price_per_night"                                          
    names(df_total)[15] <- "Total_Price" 
      
    dim(df_total)
      
    df_total$superhost_display<-ifelse(df_total$is_superhost == "TRUE","SUPERHOST","")
    df_total$bathrooms<-ifelse(df_total$bathrooms > 1,paste0(df_total$bathrooms," bathrooms"),paste0(df_total$bathrooms," bathroom")) 
    df_total$beds<-ifelse(df_total$beds > 1,paste0(df_total$beds," beds"),paste0(df_total$beds," bed")) 
    df_total$bedrooms<-ifelse(df_total$bedrooms > 1,paste0(df_total$bedrooms," bedrooms"),paste0(df_total$bedrooms," bedroom")) 
      
    df_total$feature <- paste0(df_total$bathrooms, ",", df_total$beds,",",df_total$bedrooms)
      
      
    df_total$ratings_review <- paste(" (", df_total$reviews_count, ")")
    df_total$ratings_review <-gsub("[[:space:]]", "", df_total$ratings_review)
    df_total$ratings_review <- paste(df_total$star_rating,df_total$ratings_review)
      
    df_total$superhost_display <-ifelse(df_total$superhost_display == "","", paste(" [", df_total$superhost_display, "]"))
    df_total$superhost_display <-gsub("[[:space:]]", "", df_total$superhost_display)
    df_total$space_type_and_superhost <- paste(df_total$space_type,df_total$superhost_display)
    return (df_total) 
  })
  
  GetAttraction <- eventReactive(input$go_Attractions,{
    neighbourhood <- input$neighbourhoods_Attractions
    neighbourhoods_for_POI<-gsub(" / ", " ",neighbourhood)
    neighbourhoods_for_POI<-gsub("/", " ",neighbourhoods_for_POI)
    neighbourhoods_for_POI<-gsub("-", " ",neighbourhoods_for_POI)
    neighbourhoods_for_POI<-gsub("\\s*\\([^\\)]+\\)","",neighbourhoods_for_POI)
    neighbourhoods_for_POI<-gsub(" ","+",neighbourhoods_for_POI)
    neighbourhoods_for_POI<-paste0(neighbourhoods_for_POI,"+new+york")
    
    url <- paste0("https://maps.googleapis.com/maps/api/place/textsearch/json?query=things+to+do+in+",neighbourhoods_for_POI,"&key=AIzaSyCsw9nE3U7XtG__0TENdwOk11UkRz5llLU")
    foo <- fromJSON(url)
    results <- foo$results
    data_POI = data.frame()
    data_POI <- data.frame(results$name, results$formatted_address, results$geometry$location$lat, results$geometry$location$lng, results$rating, results$user_ratings_total)
    
    
    names(data_POI) <- c("PointOfInterest", "Address", "Latitude", "Longitude", "Rating", "Numberofratings")
    
    data_POI$opening_hour<-foo$results$opening_hours
    data_POI$opening_hour<-ifelse(data_POI$opening_hour == "TRUE","Open",ifelse(data_POI$opening_hour == "FALSE","Closed",""))
    
    picture_reference<-c()
    for (i in 1:nrow(results)) {
      picture_reference <- c(picture_reference, foo$results$photos[[i]]$photo_reference)
    }
    
    data_POI$picture_reference<-picture_reference
    data_POI$image_url<-paste0("https://maps.googleapis.com/maps/api/place/photo?maxwidth=400&photoreference=",data_POI$picture_reference,"&key=AIzaSyCsw9nE3U7XtG__0TENdwOk11UkRz5llLU")
    
    
    data_POI$link <-gsub(" &","", data_POI$PointOfInterest)
    data_POI$link <-gsub(" ", "+", data_POI$link)
    data_POI$link<-paste0(data_POI$link,"+new+york")
    data_POI$link<-paste0("https://google.com/search?q=",data_POI$link)
    data_POI$Numberofratings<-formatC(data_POI$Numberofratings, big.mark=',')
    return(data_POI)
  })
}

shinyApp(ui = ui, server = server)

