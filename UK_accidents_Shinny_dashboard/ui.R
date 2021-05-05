library(shiny)
library(highcharter)
library(ggplot2)
library(shiny)
library(dplyr)
library(lubridate)
library(leaflet)
library(RColorBrewer)
library(KernSmooth)
library(ggplot2)
library(gganimate)
library(ggplot2)
library(highcharter) 
library(shinydashboard)
# Define UI for application that draws a histogram

Accidents <- c(
    "2005" =1,
    "2006" =2,
    "2007" =3,
    "2008" =4,
    "2009" =5,
    "2010"=6,
    "2011" =7,
    "2012" =8,
    "2013"=9,
    "2014"=10
)

Acc_data <- c(
    "2005" =1,
    "2006" =2,
    "2007" =3,
    "2008" =4,
    "2009" =5,
    "2010"=6,
    "2011" =7,
    "2012" =8,
    "2013"=9,
    "2014"=10
)
Factors_affecting <- c(
    "Speed limit" = 1,
    "Road Surface Conditions" =2,
    "Road Type" =3
)

vehicles <- c(
    "AIIHGVs" = 1,
    "ALLMotorVehicles" =2,
    "ALL" = 3
)
Road <- c(
    "TM" =1,
    "PU" =2,
    "TR" =3,
    "PR" = 4,
    "TU" = 5,
    "PM" =6
)
Road_1 <- c(
    "TM" =1,
    "PU" =2,
    "TR" =3,
    "PR" = 4,
    "TU" = 5,
    "PM" =6
)
shinyUI(
    dashboardPage(
        skin = "yellow",
        dashboardHeader(
            title = 'UK ACCIDENTS', titleWidth = 450),
        dashboardSidebar(
            width = 250,
            # Custom CSS to hide the default logout panel
            
            # The dynamically-generated user panel
            sidebarMenu(
                menuItem("Accidents", tabName = "Accidents", icon = icon("car")),
                menuItem("Factors", tabName = "Factors" , icon = icon("car")),
                menuItem("MAP", tabName = "MAP" , icon = icon("car")),
                menuItem("vehicles", tabName = "vehicles" , icon = icon("car"))
            )
        ),
        dashboardBody(
            tabItems(
                tabItem(
                    tabName = "Accidents",
                    box(selectInput("Accidents", "Choose any Year:", Accidents, selected = "None"),highchartOutput("barchart")),
                    box(highchartOutput("barchart_1"))
                    

                    
                ),
                tabItem(
                    tabName = "Factors",
                    box(selectInput("Acc_data", "Choose any Year:", Acc_data, selected = "None"),highchartOutput("barchart_3")),
                    box(highchartOutput("barchart_4")),
                    box(highchartOutput("barchart_5"))
                ),
                tabItem(
                    tags$style(type = "text/css", "html, body {width:100%;height:80%}"),
                    leafletOutput("map"),
                    tabName = "MAP",
                    box(selectInput("sel_severity",
                                                label = "Choose Accident Severity",
                                                choices = c("All"=0, "Fatal"=1, "Serious"=2, "Slight"=3),
                                                selected = "0"),
                        selectInput("sel_road_type",
                                    label = "Choose a Road Type",
                                    choices = c("All"=0, "Roundabout"=1, "One Way Street"=2, "Dual Carriageway"=3,
                                                "Single Carriageway"=6, "Slip Road"=7),
                                    selected = "0"),
                        dateRangeInput('sel_date_range',
                                       label = 'Date range input: yyyy-mm-dd',
                                       start = "2005-01-01", end = "2014-12-31"
                        ),
                        selectInput("sel_heat_map",
                                    label = "Show Density Map",
                                    choices = c("No"=0, "Yes"=1),
                                    selected = "0"),
                        p("Please Note: Turn off the density map to view accident details."),
                        
                        tableOutput("results")
                    )
                

            ),
            tabItem(
                tabName = "vehicles",
                box(selectInput("vehicles", "Choose Vehicle Type:", vehicles, selected = "None"),highchartOutput("linechart_1")),
                box(selectInput("Road", "Choose Road type for hgv :", Road, selected = "None"),highchartOutput("linechart_2")),
                box(selectInput("Road_1", "Choose Road type for AMV :", Road_1, selected = "None"),highchartOutput("linechart_3"))
                
            )
           
            
        )
    )
))


