library(leaflet)
library(shiny)
library(readr)
library(dplyr)
library(shinydashboard)
library(rgeos)
library(rgdal)
library(ggmap)
library(shinyjs)
library(maptools)
library(raster)
library(tidyr)
library(stringr)
library(leaflet.extras)
library(erer)
library(RColorBrewer)
library(ggplot2)
library(plyr)
library(sp)
library(readr)
library(shinyBS)
library(ggthemes)
library(DT)
library(data.table)
###########################################################################################################################################
#CODE TO CLEAN DATA FOR INTERACTIVE MAP

data <- read_csv("test data.csv")
data$Date<- as.Date(data$Date, format="%m/%d/%Y")


###########################################################################################################################################

#CODE FOR POLYGON MAP
shapeData <- readOGR(dsn = ".", layer = "Area1")


##############################################################################################################################################
# LINE GRAPH DATA
Median.all <- read_csv("district.data.csv")
##########################################################################################################################################
#CODE FOR DISTRICT TABLE

district.code <- read_csv("District_Table.csv")

################################################################################################################################

# UI INTERFACE


header <- dashboardHeader(
  title=strong("RentXplorer", style="font-family: 'Tahoma'; font-size: 30px;", img(src="pin.png",height=40)), 
  titleWidth = 330)

sidebar <- dashboardSidebar(
  sidebarMenu(id="tabs",menuItem(strong("Overview"), tabName = "Overview", icon= icon("info-circle")),
              menuItem(strong("Interactive Map"), 
                                 tabName="Map", icon = icon("map-marker")), 
              menuItem(strong("District Trends"), tabName="Prediction", icon = icon("laptop")) 
  )
)

body<- dashboardBody(
  tabItems( tabItem("Overview", tags$style( type = "text/css", "label {font-size:16 px;}"),
                    fluidPage(tags$head(tags$style(HTML("
                        @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
                        
                        h1 {
                          font-family: 'Lobster', cursive;
                          font-weight: 500;
                          line-height: 4;
                          color:#72B7E3;
                        }"))),
                    headerPanel("Welcome to RentXplorer!"),
                    fluidRow(p("Always feeling lost when thinking of  what price you should price your rental house at?
                                  Fear not as this application allows you to look at historical rent data as well as the district median
                                  which allows you to discover the optimal price!", style="font-size:16pt"),
                               column(width = 8,box(width=NULL, leafletOutput("map4", height = 500), status = "success", solidHeader = TRUE,title = "Availability")),
                                column(width = 4, box(width = NULL , dataTableOutput("District_table", height = 500), status ="primary",solidHeader = TRUE,title="First Find Your District!")))
                    )),
    
    
    
              tabItem("Map", 
                    tags$style(type = "text/css", 
                               "label { font-size: 16px; }", 
                               ".selectize-input { font-size: 16px;} ",   
                               ".selectize-dropdown { font-size: 16px; }"
                               ),
                    
                    
                    fluidRow(
                      
                       
                      column(width = 8, 
                             HTML("<div style='height: 600px;'>"),
                             p("Please wait to click until map loads.",
                               p("Click on the map and markers for additional statistics",style="font-size:10pt"),
                               style="color:grey; font-size:12pt"),  
                            tabsetPanel(
                              tabPanel("Map",
                                                
                                box(width = NULL,
                                   status="success", 
                                   solidHeader = TRUE, 
                                   title="Map of Singapore", 
                                   leafletOutput("map2", height=730))),
                              tabPanel("Price Graph",
                                
                                box(width = NULL,
                                    status="success", 
                                    solidHeader = TRUE, 
                                    title="Mean Property prices based on criteria", 
                                    plotOutput("pricegraph")))),
                            
                                                                    
                             HTML("</div>")), #add some space at the bottom of the map
                   
                    
                    
                      #add padding so that the title of this box lines up with the map
                      column(width=4, style=' padding : 0px;',
                             h3(strong("Map a Specific Location")),
                             
                             
                             box(width=NULL,
                                 status = "primary", 
                                 
                                 # inputs address
                                 textInput("address",
                                           h5(strong("Street address"))), 
                                  #input action button for map
                                  box(width=6, 
                                     status="info",
                                     actionButton("geocode", 
                                                  h6(strong("Map location")))),
                                 
                                 #input action button to clear map
                                 box (width=6, 
                                      status="danger", 
                                      actionButton("clear", 
                                                   h6(strong("Clear location")))),
                                 #inputs radius around property through slider
                                 sliderInput("radius",
                                             h5(strong("Select a buffer distance (km):")), 
                                             1, 10, 5),
                                 
                                 # inputs number of rooms
                                 textInput("rms",
                                           h5(strong("No.Rooms")),
                                           1),
                                 p(em("Put N/A if not Applicable", style="color:red; font-size:12pt")),
                                 p(em("For Terrace Houses please indicate N/A", style="color:red; font-size:12pt")),
                                 # selects type of property
                                 selectInput("type",
                                             h5(strong("Type")),
                                             c("Executive Condominium","Terrace House", "Private Condominium"),
                                             selected = " "),
                                 
                                 #inputs range of area 
                                 sliderInput("area", 
                                             h5(strong("Floor Area (Sqm)")),
                                             min = min(data$mean_FloorArea), 
                                             max = max(data$mean_FloorArea), 
                                             value = range(data$mean_FloorArea)),
                                 
                                 #Input Districts
                                 selectInput("district",
                                             h5(strong("Select District")),
                                             c(1:28)),
                                #input initial search button
                                 box(width = 6,
                                     status = "primary",
                                     actionButton("search", h6(strong("Search"))))
                                 
                                 )
                                 )
                             )),
                   
                    
  
  tabItem("Prediction",
          fluidRow(
            
            #set height of the box the map is in so that it lines up with the right-hand boxes
            column(width = 8, 
                   HTML("<div style='height: 600px;'>"),
                   box(width = NULL, 
                       status="success", 
                       solidHeader = TRUE, 
                       title="Map of Singapore", #create a green box that has the map
                       leafletOutput("map3", height=600)), #show map in this box
                   HTML("</div>")), #add some space at the bottom of the map 
            
            #Creating column for selecting district input to plot
            column(width=4,
                   style='padding:0px;',
                   h3(strong("Explore District PSF(price per square feet")),
                   p("Click on the map for districts informations",style="font-size:12pt"),
                   h3(strong("Show Specific District Trends")),
                   box(width = NULL,
                       status = "primary",
                       selectInput("Plot",
                                   h4(strong("Plot District")),
                                   c(1:28)))),
           
             #Creating Column to output plot
            column(width=4,
                   style='padding:0px',
                   box(width = NULL, 
                       status = 'primary',
                       plotOutput("lineplot")))
          )
  )
  )
)

#put entire dashboard together
ui<- dashboardPage(header, sidebar,  body) 



#########################################################################################################################

server<-function(input,output,session){
  
  #Creating a Base Map
  output$map2<- renderLeaflet({leaflet(data) %>% addTiles(urlTemplate = "//{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",options = tileOptions(minZoom=9, maxZoom=16))%>%
      setView(103.851959,1.290270, zoom=11)})
  
  #Reactive markers in base map according to inputs
  observeEvent(input$search, {
    observeEvent(c(input$rms,input$type,input$area,input$district),{
    #set default so the map resets in between choice
    map2<-leafletProxy("map2") %>% clearGroup("A")
   
    
    #Adding Information of property to marker
    
      data <- data[data$No.ofBedroom == input$rms & data$Type == input$type & data$mean_FloorArea >= input$area[1] & data$mean_FloorArea<=input$area[2] & data$PostalDistrict == input$district,]
      if (nrow(data)>0){
      leafletProxy("map2") %>%addMarkers(data=data, 
                                       popup = ~paste0("<b>","Name: ","</b>",as.character(data$BuildingName),"<br/>","<b>","Type: ","</b>",as.character(data$Type),"<br/>","<b>",
                                                       "Number of Rooms: ","</b>",data$No.ofBedroom,"<br/>","<b>", "Land Area (Sqm): ","</b>",as.character(data$mean_FloorArea),"<b>", "<br/>", 
                                                                                                                                                           "Mean Monthly Rent :","</b>", paste0("$",data$Mean.Rental)), group="A") %>% 
      fitBounds(min(data$lon),min(data$lat),
                max(data$lon),min(data$lat))
    }
    else {
    
    showNotification("No data for this criteria! Change Filters!")
    output$map2<- renderLeaflet({leaflet(data) %>% addTiles(urlTemplate = "//{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",options = tileOptions(minZoom=9, maxZoom=16))%>%
          setView(103.851959,1.290270, zoom=11)})
      
    }
    }) 
  })
  
  
  #Geocoding Input addresses and plotting marker
  observeEvent(input$geocode,{
    address <- input$address
    add <- paste(address,"Singapore")
    v<- geocode(add, output="latlon")
    
    #Creating Icon for Input Address's Marker
    leaficon<-icons (
      iconUrl="http://icons.iconarchive.com/icons/hopstarter/sleek-xp-basic/256/Home-icon.png",
      iconWidth = 44, iconHeight = 44)
    
    #add marker for geocoded address with radius in KM
    if (!is.na(v$lon) & !is.na(v$lat)) 
    {
      
      leafletProxy('map2') %>% setView(lng=v$lon, lat=v$lat, zoom=12) %>% addMarkers(data=v,icon = leaficon,layerId="B") %>% 
        addCircles(lng=v$lon,lat=v$lat, fillOpacity = 0.3, color = "red", radius = (input$radius)*1000, layerId="A") 
    }
    
    #if there is no address, just show the default map and remove previous geocoded markers and circle
     else if (is.na(v$lon) & is.na(v$lat))
    {
      
      leafletProxy('map2') %>% setView(103.851959,1.290270, zoom=10) %>% removeShape("A") %>% removeMarker("B")
      updateTextInput(session, "address", value = " ")    
    }
    
  })
  
  #Creating price graph
  observeEvent(c(input$rms,input$type,input$district),{
    data2 <- data[data$No.ofBedroom == input$rms & data$Type == input$type & data$PostalDistrict == input$district & data$mean_FloorArea >= input$area[1] & data$mean_FloorArea<=input$area[2],]
    data2 <- data2 %>% dplyr::arrange(Date) %>% dplyr::group_by(Date) %>% dplyr::summarise(Mean.Rental = round(mean(Mean.Rental),digit=2))
    output$pricegraph <- renderPlot({ggplot(data2, aes(Date,Mean.Rental, group= 1, label= data2$Mean.Rental)) + geom_line(color="#000099", size = 1 )+geom_point(color="#CC0000", size = 3)+
        theme_economist()+ scale_color_economist()+ theme(axis.text.x = element_text(angle=90,vjust = 0.5)) + 
        scale_x_date(breaks= data2$Date)+ geom_label(aes(label=data2$Mean.Rental, fill="red"),fontface="bold",colour="white",hjust=1,vjust=-1, show.legend = FALSE)}) 
  })
  
  
  #Creating Polygon Map to show district median
  observe({
    pal <- colorNumeric( palette = "Oranges", domain =shapeData$Avrg_Md)
    popup <- paste0("<b>","District: ","</b>",shapeData$PLN_AREA_N,"<br/>",
                    "<b>", "Avg.Median Price (psf) : ", "</b>", round(shapeData$Avrg_Md, digits = 2))
    
    output$map3 <- renderLeaflet({leaflet("map3", data = shapeData) %>% addTiles() %>% addPolygons(data=shapeData, weight = 2, stroke = TRUE, color="White", smoothFactor = 0.1, fillOpacity = 0.5, fillColor = ~pal(shapeData$Avrg_Md),
                                                                                                    highlight= highlightOptions(color = "white", weight = 1,bringToFront = FALSE), popup = popup, dashArray = "")%>%
        addLegend("bottomright", pal = pal, values = ~Avrg_Md,labFormat = labelFormat(prefix = "$"))})
  })
  
  #Creating Median Across Quaters per District
  observeEvent(input$Plot,{
    data1 <- Median.all[Median.all$Postal.District == input$Plot,]
    data1$Median <- round(data1$Median, digits = 2)
    data1$Quarter<- factor(data1$Quarter, ordered = TRUE, levels = c("Q4.2018","Q1.2019","Q2.2019","Q3.2019"))
    output$lineplot<- renderPlot({ggplot(data1,aes(Quarter,Median, group=Postal.District, label= data1$Median))+geom_line(colour="#000099", size=1)+
        geom_point(color="#CC0000", size = 3) +
        ggtitle("Quarterly District Median (Psf)") + theme_economist()+ scale_color_economist()+ geom_label(aes(label=data1$Median, fill="red"),fontface="bold",colour="white",hjust=1,vjust=-1, show.legend = FALSE) })
    
  })
  #Creating a District table for users to identify district codes
  output$District_table<- renderDataTable({
    datatable(data.frame(district.code),escape = F)})
  #Creating clear button
  observeEvent (input$clear, {
    leafletProxy('map2') %>% removeShape("A") %>%setView(103.851959,1.290270, zoom=10) %>% removeMarker("B")
    updateTextInput(session, "address", value = " ")   
  })
  
  #creating map to show overall availability
  pal2<- colorFactor("Oranges", domain = data$Type)
  output$map4<- renderLeaflet({leaflet(data) %>% addProviderTiles(providers$CartoDB.Positron)%>%
      setView(103.851959,1.290270, zoom=11) %>% addCircleMarkers(data = data, color = ~pal2(data$Type), opacity = 0.4, radius = 0.4) %>%
      addLegend("bottomright", pal=pal2, values = ~Type)})
  
  
}
  
shinyApp(ui,server)