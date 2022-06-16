#install packages if needed- remove the # symbol on each line to uncomment the following commands 
#install.packages("leaflet")
#install.packages("readr")
#install.packages("shiny")
#install.packages("shinydashboard")
#install.packages("ggplot2")
#install.packages("plotly")
#install.packages("hrbrthemes")

#load libraries
library(readr)
library(leaflet)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(hrbrthemes)

#set your working directory, uncomment the following line and change the filepath appropriately
#setwd("/Users/jennifershelton/Desktop/LittleRock")

#import data
stations = read.csv("Police_Facilities.csv", header = TRUE, stringsAsFactors = TRUE)
vcrime = read.csv("vcrime2017_2020.csv", header = TRUE, stringsAsFactors = TRUE)
publicwifi = read.csv("PublicWifi.csv", header = TRUE, stringsAsFactors = TRUE)
cals = read.csv("CALS.csv", header = TRUE, stringsAsFactors = TRUE)
ptc = read.csv("pivot.csv", header = TRUE, stringsAsFactors = TRUE)
historicsites = read.csv("Historic.csv", header = TRUE, stringsAsFactors = TRUE)
licenses = read.csv("Business_Licenses.csv", header = TRUE, stringsAsFactors = TRUE)
homevalues =  read.csv("LRhomevalues.csv", header = TRUE, stringsAsFactors = FALSE)
schools= read.csv("LRSchools.csv", header = TRUE, stringsAsFactors = FALSE)
wages = read.csv("LR_Occ.csv", header = TRUE, stringsAsFactors = TRUE)

#set icons
policeicon = awesomeIcons(icon = 'flag', iconColor = 'black', library = 'glyphicon', markerColor = "lightblue")
wifiicon = awesomeIcons(icon = 'signal', iconColor = 'black', library = 'glyphicon', markerColor = "white")
CALSicon = awesomeIcons(icon = 'book', iconColor = 'black', library = 'glyphicon', markerColor = "white")
histicon = awesomeIcons(icon = 'home', iconColor = 'black', library = 'glyphicon', markerColor = "lightgray")
schoolicon= awesomeIcons(icon= 'apple', iconColor = 'black', library = 'glyphicon', markerColor = "beige")

#format home value data for chart
homevalues$Date= as.Date(homevalues$Date)
homevalues$Value = as.integer(homevalues$Value)

# define shiny dashboard header 
header <- dashboardHeader(title = 'Little Rock Data')

# define shiny sidebar
sidebar <- dashboardSidebar(
  sidebarMenu(
    id = 'menu_tabs'
    , menuItem('Map', tabName = 'Map')
    , menuItem('Charts', tabName = 'Charts')
  )
)

# define dashboard Body 
body <- dashboardBody(
  tabItems(
    tabItem(
      tabName = 'Charts', 
      #row 1
      fluidRow(
        box(title = "Little Rock Median Home Value since 1996", width=12, height = 500,
            plotlyOutput("p1"))
      ),
      #row 2
      fluidRow(
        box(title = "Changes in Number of Violent Offenses Over Time, 2017 - 2020", width=12, height = 500,
            plotlyOutput("p4"))
      ),
      #row 3
      fluidRow(
      box(title = "Current Business Licenses by Category", width = 12, height = 500, 
          plotlyOutput("p3"))
      ),
      #row 4
      fluidRow(
        box(title = "Comparison of Little Rock Wages to National Average", width = 12, height = 600, 
            plotlyOutput("p5"))
      )
    ),
    tabItem(
      tabName = 'Map'
      , leafletOutput("mymap", height = 900, width = 900)
      , verbatimTextOutput('summary')
    )
  )
)

# Define Shiny UI
ui = dashboardPage(
  title = 'Little Rock Data Dashboard',
  header,
  sidebar,
  body
)

server = function(input, output, session) {
  observe({
    req(input$mydata)
    updateTabItems(session, 'menu_tabs', 'menu2')
    
  })

  output$p1 = renderPlotly({
    p1= ggplot(homevalues, aes(x=Date, y=Value)) +
      geom_line(color="#69b3a2", size=1) +
      labs(y="Median Home Value in USD")
  })
  
  output$p3 = renderPlotly({
    p3 = ggplot(licenses, aes(x=Category)) +
      geom_bar() +
      coord_flip() +
      theme(legend.position="none")  
    })
  
  output$p4 = renderPlotly({
    p4 = ggplot(ptc, aes(x=Year, y=Total, color = Offense)) +
      geom_line(size=1) +
      geom_point() +
      scale_fill_brewer() +
      theme_ipsum() 
  })
  
  output$p5 = renderPlotly({
   p5= ggplot(wages, aes(x=Category, y=MeanHourlyWage, fill=Group))+
      geom_bar(position="dodge", stat="identity")+
      theme(axis.text.x = element_text(size = 10, angle = 45)) +
      theme(legend.position="none")  
  })
  
  
  #build map
  output$mymap = renderLeaflet({leaflet() %>%
      #add base layer of map    
      addTiles() %>%
      
      #add police station data markers    
      addAwesomeMarkers(data = stations, group = "Police Stations", 
                        popup = paste("Facility Name:", stations$FACILITY,"<br>",
                                      "Address:", stations$ADDRESS), 
                        icon = policeicon, layerId = 1) %>%
      
      #add public wifi data markers    
      addAwesomeMarkers(data = publicwifi, group = "Public Wifi", 
                        popup = paste("Facility Name:", publicwifi$Facility,"<br>",
                                      "Address:", publicwifi$Address), 
                        icon = wifiicon, layerId = 2) %>%
      
      #add library data markers    
      addAwesomeMarkers(data = cals, group = "Central Arkansas Library System", 
                        popup = paste("Facility Name:", cals$Branch,"<br>",
                                      "Address:", cals$Address), 
                        icon = CALSicon, layerId = 3) %>%
      
      #add historic site data markers    
      addAwesomeMarkers(data = historicsites, group = "Historic Sites", 
                        popup = paste("Historic Property Name:", historicsites$Property, "<br>",
                                      "Address:", historicsites$Address,"<br>",
                                      "Year Built:", historicsites$Construction, "<br>"), 
                        icon = histicon, layerId = 4) %>%
      
      #add school data markers    
      addAwesomeMarkers(data = schools, group = "K-12 Schools", 
                        popup = paste("School Name", schools$Location.Description, "<br>",
                                      "Address:", schools$Address,"<br>"),
                        icon = schoolicon, layerId = 5) %>%
      
      #add dispatch calls since 2019 in clusters   
      addMarkers(data = vcrime, group = "Crimes", 
                 popup= paste("Offense:", vcrime$Offense, "<br>", "Location:", vcrime$Address, "<br>", "Date:", vcrime$Date), 
                 clusterOptions = markerClusterOptions(), clusterId = "Crimes")%>%
      
      #hide the layers at startup for a blank map
      hideGroup("Crimes") %>%
      hideGroup("Police Stations") %>%
      hideGroup("Public Wifi") %>%
      hideGroup("Central Arkansas Library System") %>%
      hideGroup("Historic Sites") %>%
      hideGroup("K-12 Schools") %>%
    
      #add layer control on legend   
      addLayersControl(
        overlayGroups = c("Crimes", "Police Stations", "Public Wifi", "Central Arkansas Library System", "Historic Sites", "K-12 Schools"),
        options = layersControlOptions(collapsed = FALSE))
  })

}

shinyApp(ui, server)
