library(shiny)
library(dplyr)
library(lubridate)
library(leaflet)
library(RColorBrewer)
library(KernSmooth)
library(ggplot2)
library(gganimate)
library(idbr)
library(plotly)
library(highcharter) 


options(highcharter.theme = hc_theme_smpl(tooltip = list(valueDecimals = 2)))

data=read.csv("accidents_data.csv")
accidents <- read.csv("accidents_data.csv", header=TRUE, na.strings = c("","NA","-1"))
uk_data = read.csv('UK_Accident_data.csv',header = TRUE, na.strings = c("","NA","-1"))

# Create Presets
severity_enum <- c("Fatal", "Serious", "Slight")
light_cond_enum <- c("Daylight","","","Darkness - Lights Lit", "Darkness - Lights Unlit", "Darkness - No Lighting",
                     "Darkness - Lighting Unknown")
weather_enum <- c("Fine no High Winds", "Raining no High Winds", "Snowing no High Winds", "Fine + High Winds",
                  "Raining + High Winds", "Snowing + High Winds", "Fog or Mist", "Other", "Unknown")
road_surface_enum <- c("Dry", "Wet or Damp", "Snow", "Frost or Ice", "Flood over 3cm", "Oil or Diesel", "Mud")


# Convert the date
accidents$Date <- dmy_hm(accidents$Date)

# Add Day Name to Dataset
accidents$Day <- factor(wday(accidents$Day_of_Week, label = TRUE, abbr = FALSE))

# Create Severity Column
accidents$Severity <- severity_enum[accidents$Accident_Severity]

shinyServer(function(input, output) {
    output$barchart <- renderHighchart({
        if (input$Accidents == 1) { 
            ploty_data_1 <- accidents %>% filter(year==2005) %>% group_by(year,month) %>% count(year)
            ploty_data_1$mon [ploty_data_1$month == 1] <- 'Jan'
            ploty_data_1$mon [ploty_data_1$month == 2] <- 'Feb'
            ploty_data_1$mon [ploty_data_1$month == 3] <- 'March'
            ploty_data_1$mon [ploty_data_1$month == 4] <- 'April'
            ploty_data_1$mon [ploty_data_1$month == 5] <- 'May'
            ploty_data_1$mon [ploty_data_1$month == 6] <- 'June'
            ploty_data_1$mon [ploty_data_1$month == 7] <- 'July'
            ploty_data_1$mon [ploty_data_1$month == 8] <- 'Aug'
            ploty_data_1$mon [ploty_data_1$month == 9] <- 'Sep'
            ploty_data_1$mon [ploty_data_1$month == 10] <- 'Oct'
            ploty_data_1$mon [ploty_data_1$month == 11] <- 'Nov'
            ploty_data_1$mon [ploty_data_1$month == 12] <- 'Dec'
        }
        else if(input$Accidents == 2){
            ploty_data_1 <- accidents %>% filter(year==2006) %>% group_by(year,month) %>% count(year)
            ploty_data_1$mon [ploty_data_1$month == 1] <- 'Jan'
            ploty_data_1$mon [ploty_data_1$month == 2] <- 'Feb'
            ploty_data_1$mon [ploty_data_1$month == 3] <- 'March'
            ploty_data_1$mon [ploty_data_1$month == 4] <- 'April'
            ploty_data_1$mon [ploty_data_1$month == 5] <- 'May'
            ploty_data_1$mon [ploty_data_1$month == 6] <- 'June'
            ploty_data_1$mon [ploty_data_1$month == 7] <- 'July'
            ploty_data_1$mon [ploty_data_1$month == 8] <- 'Aug'
            ploty_data_1$mon [ploty_data_1$month == 9] <- 'Sep'
            ploty_data_1$mon [ploty_data_1$month == 10] <- 'Oct'
            ploty_data_1$mon [ploty_data_1$month == 11] <- 'Nov'
            ploty_data_1$mon [ploty_data_1$month == 12] <- 'Dec'
        }
        else if(input$Accidents == 3){
            ploty_data_1 <- accidents %>% filter(year==2007) %>% group_by(year,month) %>% count(year)
            ploty_data_1$mon [ploty_data_1$month == 1] <- 'Jan'
            ploty_data_1$mon [ploty_data_1$month == 2] <- 'Feb'
            ploty_data_1$mon [ploty_data_1$month == 3] <- 'March'
            ploty_data_1$mon [ploty_data_1$month == 4] <- 'April'
            ploty_data_1$mon [ploty_data_1$month == 5] <- 'May'
            ploty_data_1$mon [ploty_data_1$month == 6] <- 'June'
            ploty_data_1$mon [ploty_data_1$month == 7] <- 'July'
            ploty_data_1$mon [ploty_data_1$month == 8] <- 'Aug'
            ploty_data_1$mon [ploty_data_1$month == 9] <- 'Sep'
            ploty_data_1$mon [ploty_data_1$month == 10] <- 'Oct'
            ploty_data_1$mon [ploty_data_1$month == 11] <- 'Nov'
            ploty_data_1$mon [ploty_data_1$month == 12] <- 'Dec'
        }
        else if(input$Accidents == 4){
            ploty_data_1 <- accidents %>% filter(year==2008) %>% group_by(year,month) %>% count(year)
            ploty_data_1$mon [ploty_data_1$month == 1] <- 'Jan'
            ploty_data_1$mon [ploty_data_1$month == 2] <- 'Feb'
            ploty_data_1$mon [ploty_data_1$month == 3] <- 'March'
            ploty_data_1$mon [ploty_data_1$month == 4] <- 'April'
            ploty_data_1$mon [ploty_data_1$month == 5] <- 'May'
            ploty_data_1$mon [ploty_data_1$month == 6] <- 'June'
            ploty_data_1$mon [ploty_data_1$month == 7] <- 'July'
            ploty_data_1$mon [ploty_data_1$month == 8] <- 'Aug'
            ploty_data_1$mon [ploty_data_1$month == 9] <- 'Sep'
            ploty_data_1$mon [ploty_data_1$month == 10] <- 'Oct'
            ploty_data_1$mon [ploty_data_1$month == 11] <- 'Nov'
            ploty_data_1$mon [ploty_data_1$month == 12] <- 'Dec'
        }
        else if(input$Accidents == 5){
            ploty_data_1 <- accidents %>% filter(year==2009) %>% group_by(year,month) %>% count(year)
            ploty_data_1$mon [ploty_data_1$month == 1] <- 'Jan'
            ploty_data_1$mon [ploty_data_1$month == 2] <- 'Feb'
            ploty_data_1$mon [ploty_data_1$month == 3] <- 'March'
            ploty_data_1$mon [ploty_data_1$month == 4] <- 'April'
            ploty_data_1$mon [ploty_data_1$month == 5] <- 'May'
            ploty_data_1$mon [ploty_data_1$month == 6] <- 'June'
            ploty_data_1$mon [ploty_data_1$month == 7] <- 'July'
            ploty_data_1$mon [ploty_data_1$month == 8] <- 'Aug'
            ploty_data_1$mon [ploty_data_1$month == 9] <- 'Sep'
            ploty_data_1$mon [ploty_data_1$month == 10] <- 'Oct'
            ploty_data_1$mon [ploty_data_1$month == 11] <- 'Nov'
            ploty_data_1$mon [ploty_data_1$month == 12] <- 'Dec'
        }
        else if(input$Accidents == 6){
            ploty_data_1 <- accidents %>% filter(year==2010) %>% group_by(year,month) %>% count(year)
            ploty_data_1$mon [ploty_data_1$month == 1] <- 'Jan'
            ploty_data_1$mon [ploty_data_1$month == 2] <- 'Feb'
            ploty_data_1$mon [ploty_data_1$month == 3] <- 'March'
            ploty_data_1$mon [ploty_data_1$month == 4] <- 'April'
            ploty_data_1$mon [ploty_data_1$month == 5] <- 'May'
            ploty_data_1$mon [ploty_data_1$month == 6] <- 'June'
            ploty_data_1$mon [ploty_data_1$month == 7] <- 'July'
            ploty_data_1$mon [ploty_data_1$month == 8] <- 'Aug'
            ploty_data_1$mon [ploty_data_1$month == 9] <- 'Sep'
            ploty_data_1$mon [ploty_data_1$month == 10] <- 'Oct'
            ploty_data_1$mon [ploty_data_1$month == 11] <- 'Nov'
            ploty_data_1$mon [ploty_data_1$month == 12] <- 'Dec'
        }
        else if(input$Accidents == 7){
            ploty_data_1 <- accidents %>% filter(year==2011) %>% group_by(year,month) %>% count(year)
            ploty_data_1$mon [ploty_data_1$month == 1] <- 'Jan'
            ploty_data_1$mon [ploty_data_1$month == 2] <- 'Feb'
            ploty_data_1$mon [ploty_data_1$month == 3] <- 'March'
            ploty_data_1$mon [ploty_data_1$month == 4] <- 'April'
            ploty_data_1$mon [ploty_data_1$month == 5] <- 'May'
            ploty_data_1$mon [ploty_data_1$month == 6] <- 'June'
            ploty_data_1$mon [ploty_data_1$month == 7] <- 'July'
            ploty_data_1$mon [ploty_data_1$month == 8] <- 'Aug'
            ploty_data_1$mon [ploty_data_1$month == 9] <- 'Sep'
            ploty_data_1$mon [ploty_data_1$month == 10] <- 'Oct'
            ploty_data_1$mon [ploty_data_1$month == 11] <- 'Nov'
            ploty_data_1$mon [ploty_data_1$month == 12] <- 'Dec'
        }
        else if(input$Accidents == 8){
            ploty_data_1 <- accidents %>% filter(year==2012) %>% group_by(year,month) %>% count(year)
            ploty_data_1$mon [ploty_data_1$month == 1] <- 'Jan'
            ploty_data_1$mon [ploty_data_1$month == 2] <- 'Feb'
            ploty_data_1$mon [ploty_data_1$month == 3] <- 'March'
            ploty_data_1$mon [ploty_data_1$month == 4] <- 'April'
            ploty_data_1$mon [ploty_data_1$month == 5] <- 'May'
            ploty_data_1$mon [ploty_data_1$month == 6] <- 'June'
            ploty_data_1$mon [ploty_data_1$month == 7] <- 'July'
            ploty_data_1$mon [ploty_data_1$month == 8] <- 'Aug'
            ploty_data_1$mon [ploty_data_1$month == 9] <- 'Sep'
            ploty_data_1$mon [ploty_data_1$month == 10] <- 'Oct'
            ploty_data_1$mon [ploty_data_1$month == 11] <- 'Nov'
            ploty_data_1$mon [ploty_data_1$month == 12] <- 'Dec'
        }
        else if(input$Accidents == 9){
            ploty_data_1 <- accidents %>% filter(year==2013) %>% group_by(year,month) %>% count(year)
            ploty_data_1$mon [ploty_data_1$month == 1] <- 'Jan'
            ploty_data_1$mon [ploty_data_1$month == 2] <- 'Feb'
            ploty_data_1$mon [ploty_data_1$month == 3] <- 'March'
            ploty_data_1$mon [ploty_data_1$month == 4] <- 'April'
            ploty_data_1$mon [ploty_data_1$month == 5] <- 'May'
            ploty_data_1$mon [ploty_data_1$month == 6] <- 'June'
            ploty_data_1$mon [ploty_data_1$month == 7] <- 'July'
            ploty_data_1$mon [ploty_data_1$month == 8] <- 'Aug'
            ploty_data_1$mon [ploty_data_1$month == 9] <- 'Sep'
            ploty_data_1$mon [ploty_data_1$month == 10] <- 'Oct'
            ploty_data_1$mon [ploty_data_1$month == 11] <- 'Nov'
            ploty_data_1$mon [ploty_data_1$month == 12] <- 'Dec'
        }
        else if(input$Accidents == 10){
            ploty_data_1 <- accidents %>% filter(year==2014) %>% group_by(year,month) %>% count(year)
            ploty_data_1$mon [ploty_data_1$month == 1] <- 'Jan'
            ploty_data_1$mon [ploty_data_1$month == 2] <- 'Feb'
            ploty_data_1$mon [ploty_data_1$month == 3] <- 'March'
            ploty_data_1$mon [ploty_data_1$month == 4] <- 'April'
            ploty_data_1$mon [ploty_data_1$month == 5] <- 'May'
            ploty_data_1$mon [ploty_data_1$month == 6] <- 'June'
            ploty_data_1$mon [ploty_data_1$month == 7] <- 'July'
            ploty_data_1$mon [ploty_data_1$month == 8] <- 'Aug'
            ploty_data_1$mon [ploty_data_1$month == 9] <- 'Sep'
            ploty_data_1$mon [ploty_data_1$month == 10] <- 'Oct'
            ploty_data_1$mon [ploty_data_1$month == 11] <- 'Nov'
            ploty_data_1$mon [ploty_data_1$month == 12] <- 'Dec'
        }
        
        hc<-ploty_data_1 %>%
            hchart('column', hcaes(x = mon, y = n),name='Accidents') %>% 
            hc_xAxis(title = list(text = "month"),
                     opposite = FALSE) %>% 
            hc_yAxis(title = list(text = "Accidents"),
                     opposite = FALSE,
                     minorTickInterval = "auto",
                     minorGridLineDashStyle = "LongDashDotDot",
                     showFirstLabel = FALSE,
                     showLastLabel = FALSE)%>% hc_title(text = "Number of Accidents over months")%>%hc_add_theme(hc_theme_538())
        hc
        
    })
    output$barchart_1 <- renderHighchart({
        if (input$Accidents == 1) { 
            ploty_data_2 <- accidents %>% filter(year==2005) %>% group_by(year,Day_of_Week) %>% count(year)
            ploty_data_2$week [ploty_data_2$Day_of_Week == 1] <- 'Monday'
            ploty_data_2$week [ploty_data_2$Day_of_Week == 2] <- 'Tuesday'
            ploty_data_2$week [ploty_data_2$Day_of_Week == 3] <- 'Wednesday'
            ploty_data_2$week [ploty_data_2$Day_of_Week == 4] <- 'Thurday'
            ploty_data_2$week [ploty_data_2$Day_of_Week == 5] <- 'Friday'
            ploty_data_2$week [ploty_data_2$Day_of_Week == 6] <- 'Saturday'
            ploty_data_2$week [ploty_data_2$Day_of_Week == 7] <- 'Sunday'
        }
        else if(input$Accidents == 2){
            ploty_data_2 <- accidents %>% filter(year==2006) %>% group_by(year,Day_of_Week) %>% count(year)
            ploty_data_2$week [ploty_data_2$Day_of_Week == 1] <- 'Monday'
            ploty_data_2$week [ploty_data_2$Day_of_Week == 2] <- 'Tuesday'
            ploty_data_2$week [ploty_data_2$Day_of_Week == 3] <- 'Wednesday'
            ploty_data_2$week [ploty_data_2$Day_of_Week == 4] <- 'Thurday'
            ploty_data_2$week [ploty_data_2$Day_of_Week == 5] <- 'Friday'
            ploty_data_2$week [ploty_data_2$Day_of_Week == 6] <- 'Saturday'
            ploty_data_2$week [ploty_data_2$Day_of_Week == 7] <- 'Sunday'
        }
        else if(input$Accidents == 3){
            ploty_data_2 <- accidents %>% filter(year==2007) %>% group_by(year,Day_of_Week) %>% count(year)
            ploty_data_2$week [ploty_data_2$Day_of_Week == 1] <- 'Monday'
            ploty_data_2$week [ploty_data_2$Day_of_Week == 2] <- 'Tuesday'
            ploty_data_2$week [ploty_data_2$Day_of_Week == 3] <- 'Wednesday'
            ploty_data_2$week [ploty_data_2$Day_of_Week == 4] <- 'Thurday'
            ploty_data_2$week [ploty_data_2$Day_of_Week == 5] <- 'Friday'
            ploty_data_2$week [ploty_data_2$Day_of_Week == 6] <- 'Saturday'
            ploty_data_2$week [ploty_data_2$Day_of_Week == 7] <- 'Sunday'
        }
        else if(input$Accidents == 5){
            ploty_data_2 <- accidents %>% filter(year==2009) %>% group_by(year,Day_of_Week) %>% count(year)
            ploty_data_2$week [ploty_data_2$Day_of_Week == 1] <- 'Monday'
            ploty_data_2$week [ploty_data_2$Day_of_Week == 2] <- 'Tuesday'
            ploty_data_2$week [ploty_data_2$Day_of_Week == 3] <- 'Wednesday'
            ploty_data_2$week [ploty_data_2$Day_of_Week == 4] <- 'Thurday'
            ploty_data_2$week [ploty_data_2$Day_of_Week == 5] <- 'Friday'
            ploty_data_2$week [ploty_data_2$Day_of_Week == 6] <- 'Saturday'
            ploty_data_2$week [ploty_data_2$Day_of_Week == 7] <- 'Sunday'
        }
        else if(input$Accidents == 6){
            ploty_data_2 <- accidents %>% filter(year==2010) %>% group_by(year,Day_of_Week) %>% count(year)
            ploty_data_2$week [ploty_data_2$Day_of_Week == 1] <- 'Monday'
            ploty_data_2$week [ploty_data_2$Day_of_Week == 2] <- 'Tuesday'
            ploty_data_2$week [ploty_data_2$Day_of_Week == 3] <- 'Wednesday'
            ploty_data_2$week [ploty_data_2$Day_of_Week == 4] <- 'Thurday'
            ploty_data_2$week [ploty_data_2$Day_of_Week == 5] <- 'Friday'
            ploty_data_2$week [ploty_data_2$Day_of_Week == 6] <- 'Saturday'
            ploty_data_2$week [ploty_data_2$Day_of_Week == 7] <- 'Sunday'
        }
        else if(input$Accidents == 7){
            ploty_data_2 <- accidents %>% filter(year==2011) %>% group_by(year,Day_of_Week) %>% count(year)
            ploty_data_2$week [ploty_data_2$Day_of_Week == 1] <- 'Monday'
            ploty_data_2$week [ploty_data_2$Day_of_Week == 2] <- 'Tuesday'
            ploty_data_2$week [ploty_data_2$Day_of_Week == 3] <- 'Wednesday'
            ploty_data_2$week [ploty_data_2$Day_of_Week == 4] <- 'Thurday'
            ploty_data_2$week [ploty_data_2$Day_of_Week == 5] <- 'Friday'
            ploty_data_2$week [ploty_data_2$Day_of_Week == 6] <- 'Saturday'
            ploty_data_2$week [ploty_data_2$Day_of_Week == 7] <- 'Sunday'
        }
        else if(input$Accidents == 8){
            ploty_data_2 <- accidents %>% filter(year==2012) %>% group_by(year,Day_of_Week) %>% count(year)
            ploty_data_2$week [ploty_data_2$Day_of_Week == 1] <- 'Monday'
            ploty_data_2$week [ploty_data_2$Day_of_Week == 2] <- 'Tuesday'
            ploty_data_2$week [ploty_data_2$Day_of_Week == 3] <- 'Wednesday'
            ploty_data_2$week [ploty_data_2$Day_of_Week == 4] <- 'Thurday'
            ploty_data_2$week [ploty_data_2$Day_of_Week == 5] <- 'Friday'
            ploty_data_2$week [ploty_data_2$Day_of_Week == 6] <- 'Saturday'
            ploty_data_2$week [ploty_data_2$Day_of_Week == 7] <- 'Sunday'
        }
        else if(input$Accidents == 9){
            ploty_data_2 <- data %>% filter(year==2013) %>% group_by(year,Day_of_Week) %>% count(year)
            ploty_data_2$week [ploty_data_2$Day_of_Week == 1] <- 'Monday'
            ploty_data_2$week [ploty_data_2$Day_of_Week == 2] <- 'Tuesday'
            ploty_data_2$week [ploty_data_2$Day_of_Week == 3] <- 'Wednesday'
            ploty_data_2$week [ploty_data_2$Day_of_Week == 4] <- 'Thurday'
            ploty_data_2$week [ploty_data_2$Day_of_Week == 5] <- 'Friday'
            ploty_data_2$week [ploty_data_2$Day_of_Week == 6] <- 'Saturday'
            ploty_data_2$week [ploty_data_2$Day_of_Week == 7] <- 'Sunday'
        }
        else if(input$Accidents == 10){
            ploty_data_2 <- accidents %>% filter(year==2014) %>% group_by(year,Day_of_Week) %>% count(year)
            ploty_data_2$week [ploty_data_2$Day_of_Week == 1] <- 'Monday'
            ploty_data_2$week [ploty_data_2$Day_of_Week == 2] <- 'Tuesday'
            ploty_data_2$week [ploty_data_2$Day_of_Week == 3] <- 'Wednesday'
            ploty_data_2$week [ploty_data_2$Day_of_Week == 4] <- 'Thurday'
            ploty_data_2$week [ploty_data_2$Day_of_Week == 5] <- 'Friday'
            ploty_data_2$week [ploty_data_2$Day_of_Week == 6] <- 'Saturday'
            ploty_data_2$week [ploty_data_2$Day_of_Week == 7] <- 'Sunday'
        }
    
        
        hc_1<-ploty_data_2 %>%
            hchart('column', hcaes(x = week, y = n),name='Accidents') %>% 
            hc_xAxis(title = list(text = "Week"),
                     opposite = FALSE) %>% 
            hc_yAxis(title = list(text = "Accidents"),
                     opposite = FALSE,
                     minorTickInterval = "auto",
                     minorGridLineDashStyle = "LongDashDotDot",
                     showFirstLabel = FALSE,
                     showLastLabel = FALSE)%>% hc_title(text = "Number of Accidents over days")%>%hc_add_theme(hc_theme_538())
        hc_1
        
      

    })
    output$barchart_3 <-  renderHighchart({
        if (input$Acc_data == 1) { 
            ploty_data_3 <- accidents %>% filter(year==2005) %>% group_by(year,Speed_limit) %>% count(year)
            ploty_data_3
            
        }
        else if (input$Acc_data == 2) { 
            ploty_data_3 <- accidents %>% filter(year==2006) %>% group_by(year,Speed_limit) %>% count(year)
            ploty_data_3
        }
       else  if (input$Acc_data == 3) { 
            ploty_data_3 <- accidents %>% filter(year==2007) %>% group_by(year,Speed_limit) %>% count(year)
        }
        else if (input$Acc_data == 4) { 
            ploty_data_3 <- accidents %>% filter(year==2008) %>% group_by(year,Speed_limit) %>% count(year)
        }
        else if (input$Acc_data == 5) { 
            ploty_data_3 <- accidents %>% filter(year==2009) %>% group_by(year,Speed_limit) %>% count(year)
        }
        else if (input$Acc_data == 6) { 
            ploty_data_3 <- accidents %>% filter(year==2010) %>% group_by(year,Speed_limit) %>% count(year)
        }
        else if (input$Acc_data == 7) { 
            ploty_data_3 <- accidents %>% filter(year==2011) %>% group_by(year,Speed_limit) %>% count(year)
        }
        else if (input$Acc_data == 8) { 
            ploty_data_3 <- accidents %>% filter(year==2012) %>% group_by(year,Speed_limit) %>% count(year)
        }
        else if (input$Acc_data == 9) { 
            ploty_data_3 <- accidents %>% filter(year==2013) %>% group_by(year,Speed_limit) %>% count(year)
        }
        else if (input$Acc_data == 10) { 
            ploty_data_3 <- accidents %>% filter(year==2014) %>% group_by(year,Speed_limit) %>% count(year)
        }
        
        hc_speed<-ploty_data_3 %>%
            hchart('line', hcaes(x = Speed_limit, y = n),name='Accidents') %>% 
            hc_xAxis(title = list(text = "Speed_limit"),
                     opposite = FALSE) %>% 
            hc_yAxis(title = list(text = "Accidents"),
                     opposite = FALSE,
                     minorTickInterval = "auto",
                     minorGridLineDashStyle = "LongDashDotDot",
                     showFirstLabel = FALSE,
                     showLastLabel = FALSE)%>% hc_title(text = "Number of Accidents based on speed_limit")%>%hc_add_theme(hc_theme_538())
        hc_speed
        
    })
    output$barchart_4<-  renderHighchart({
        if (input$Acc_data == 1) { 
            ploty_data_4 <- accidents %>% filter(year==2005) %>% group_by(year,Road_Surface_Conditions) %>% count(year)
            ploty_data_4$surface [ploty_data_4$Road_Surface_Conditions == 1] <- 'Wet/Damp'
            ploty_data_4$surface [ploty_data_4$Road_Surface_Conditions == 2] <- 'Dry'
            ploty_data_4$surface [ploty_data_4$Road_Surface_Conditions == 3] <- 'Frost/Ice'
            ploty_data_4$surface [ploty_data_4$Road_Surface_Conditions == 4] <- 'Snow'
            ploty_data_4$surface [ploty_data_4$Road_Surface_Conditions == 5] <- 'Flood (Over 3cm of water)'
            ploty_data_4$surface [ploty_data_4$Road_Surface_Conditions == 6] <- 'Unknown'
        }
        else if (input$Acc_data == 2) { 
            ploty_data_4 <- accidents %>% filter(year==2006) %>% group_by(year,Road_Surface_Conditions) %>% count(year)
            ploty_data_4$surface [ploty_data_4$Road_Surface_Conditions == 1] <- 'Wet/Damp'
            ploty_data_4$surface [ploty_data_4$Road_Surface_Conditions == 2] <- 'Dry'
            ploty_data_4$surface [ploty_data_4$Road_Surface_Conditions == 3] <- 'Frost/Ice'
            ploty_data_4$surface [ploty_data_4$Road_Surface_Conditions == 4] <- 'Snow'
            ploty_data_4$surface [ploty_data_4$Road_Surface_Conditions == 5] <- 'Flood (Over 3cm of water)'
            ploty_data_4$surface [ploty_data_4$Road_Surface_Conditions == 6] <- 'Unknown'
        }
        else if (input$Acc_data == 3) { 
            ploty_data_4 <- accidents %>% filter(year==2007) %>% group_by(year,Road_Surface_Conditions) %>% count(year)
            ploty_data_4$surface [ploty_data_4$Road_Surface_Conditions == 1] <- 'Wet/Damp'
            ploty_data_4$surface [ploty_data_4$Road_Surface_Conditions == 2] <- 'Dry'
            ploty_data_4$surface [ploty_data_4$Road_Surface_Conditions == 3] <- 'Frost/Ice'
            ploty_data_4$surface [ploty_data_4$Road_Surface_Conditions == 4] <- 'Snow'
            ploty_data_4$surface [ploty_data_4$Road_Surface_Conditions == 5] <- 'Flood (Over 3cm of water)'
            ploty_data_4$surface [ploty_data_4$Road_Surface_Conditions == 6] <- 'Unknown'
        }
        else if (input$Acc_data == 4) { 
            ploty_data_4 <- accidents %>% filter(year==2008) %>% group_by(year,Road_Surface_Conditions) %>% count(year)
            ploty_data_4$surface [ploty_data_4$Road_Surface_Conditions == 1] <- 'Wet/Damp'
            ploty_data_4$surface [ploty_data_4$Road_Surface_Conditions == 2] <- 'Dry'
            ploty_data_4$surface [ploty_data_4$Road_Surface_Conditions == 3] <- 'Frost/Ice'
            ploty_data_4$surface [ploty_data_4$Road_Surface_Conditions == 4] <- 'Snow'
            ploty_data_4$surface [ploty_data_4$Road_Surface_Conditions == 5] <- 'Flood (Over 3cm of water)'
            ploty_data_4$surface [ploty_data_4$Road_Surface_Conditions == 6] <- 'Unknown'
        }
        else if (input$Acc_data == 5) { 
            ploty_data_4 <- accidents %>% filter(year==2009) %>% group_by(year,Road_Surface_Conditions) %>% count(year)
            ploty_data_4$surface [ploty_data_4$Road_Surface_Conditions == 1] <- 'Wet/Damp'
            ploty_data_4$surface [ploty_data_4$Road_Surface_Conditions == 2] <- 'Dry'
            ploty_data_4$surface [ploty_data_4$Road_Surface_Conditions == 3] <- 'Frost/Ice'
            ploty_data_4$surface [ploty_data_4$Road_Surface_Conditions == 4] <- 'Snow'
            ploty_data_4$surface [ploty_data_4$Road_Surface_Conditions == 5] <- 'Flood (Over 3cm of water)'
            ploty_data_4$surface [ploty_data_4$Road_Surface_Conditions == 6] <- 'Unknown'
        }
        else if (input$Acc_data == 6) { 
            ploty_data_4 <- accidents %>% filter(year==2010) %>% group_by(year,Road_Surface_Conditions) %>% count(year)
            ploty_data_4$surface [ploty_data_4$Road_Surface_Conditions == 1] <- 'Wet/Damp'
            ploty_data_4$surface [ploty_data_4$Road_Surface_Conditions == 2] <- 'Dry'
            ploty_data_4$surface [ploty_data_4$Road_Surface_Conditions == 3] <- 'Frost/Ice'
            ploty_data_4$surface [ploty_data_4$Road_Surface_Conditions == 4] <- 'Snow'
            ploty_data_4$surface [ploty_data_4$Road_Surface_Conditions == 5] <- 'Flood (Over 3cm of water)'
            ploty_data_4$surface [ploty_data_4$Road_Surface_Conditions == 6] <- 'Unknown'
        }
        else if (input$Acc_data == 7) { 
            ploty_data_4 <- accidents %>% filter(year==2011) %>% group_by(year,Road_Surface_Conditions) %>% count(year)
            ploty_data_4$surface [ploty_data_4$Road_Surface_Conditions == 1] <- 'Wet/Damp'
            ploty_data_4$surface [ploty_data_4$Road_Surface_Conditions == 2] <- 'Dry'
            ploty_data_4$surface [ploty_data_4$Road_Surface_Conditions == 3] <- 'Frost/Ice'
            ploty_data_4$surface [ploty_data_4$Road_Surface_Conditions == 4] <- 'Snow'
            ploty_data_4$surface [ploty_data_4$Road_Surface_Conditions == 5] <- 'Flood (Over 3cm of water)'
            ploty_data_4$surface [ploty_data_4$Road_Surface_Conditions == 6] <- 'Unknown'
        }
       else  if (input$Acc_data == 8) { 
            ploty_data_4 <- accidents %>% filter(year==2012) %>% group_by(year,Road_Surface_Conditions) %>% count(year)
            ploty_data_4$surface [ploty_data_4$Road_Surface_Conditions == 1] <- 'Wet/Damp'
            ploty_data_4$surface [ploty_data_4$Road_Surface_Conditions == 2] <- 'Dry'
            ploty_data_4$surface [ploty_data_4$Road_Surface_Conditions == 3] <- 'Frost/Ice'
            ploty_data_4$surface [ploty_data_4$Road_Surface_Conditions == 4] <- 'Snow'
            ploty_data_4$surface [ploty_data_4$Road_Surface_Conditions == 5] <- 'Flood (Over 3cm of water)'
            ploty_data_4$surface [ploty_data_4$Road_Surface_Conditions == 6] <- 'Unknown'
        }
       else if (input$Acc_data == 9) { 
            ploty_data_4 <- accidents %>% filter(year==2013) %>% group_by(year,Road_Surface_Conditions) %>% count(year)
            ploty_data_4$surface [ploty_data_4$Road_Surface_Conditions == 1] <- 'Wet/Damp'
            ploty_data_4$surface [ploty_data_4$Road_Surface_Conditions == 2] <- 'Dry'
            ploty_data_4$surface [ploty_data_4$Road_Surface_Conditions == 3] <- 'Frost/Ice'
            ploty_data_4$surface [ploty_data_4$Road_Surface_Conditions == 4] <- 'Snow'
            ploty_data_4$surface [ploty_data_4$Road_Surface_Conditions == 5] <- 'Flood (Over 3cm of water)'
            ploty_data_4$surface [ploty_data_4$Road_Surface_Conditions == 6] <- 'Unknown'
        }
       else  if (input$Acc_data == 10) { 
            ploty_data_4 <- accidents %>% filter(year==2014) %>% group_by(year,Road_Surface_Conditions) %>% count(year)
            ploty_data_4$surface [ploty_data_4$Road_Surface_Conditions == 1] <- 'Wet/Damp'
            ploty_data_4$surface [ploty_data_4$Road_Surface_Conditions == 2] <- 'Dry'
            ploty_data_4$surface [ploty_data_4$Road_Surface_Conditions == 3] <- 'Frost/Ice'
            ploty_data_4$surface [ploty_data_4$Road_Surface_Conditions == 4] <- 'Snow'
            ploty_data_4$surface [ploty_data_4$Road_Surface_Conditions == 5] <- 'Flood (Over 3cm of water)'
            ploty_data_4$surface [ploty_data_4$Road_Surface_Conditions == 6] <- 'Unknown'
            
        }
        hc_Road_surface<-ploty_data_4 %>%
            hchart('bar', hcaes(x = surface, y = n),name='Accidents') %>% 
            hc_xAxis(title = list(text = "Road_Surface_condition"),
                     opposite = FALSE) %>% 
            hc_yAxis(title = list(text = "Accidents"),
                     opposite = FALSE,
                     minorTickInterval = "auto",
                     minorGridLineDashStyle = "LongDashDotDot",
                     showFirstLabel = FALSE,
                     showLastLabel = FALSE)%>% hc_title(text = "Number of Accidents based on Road_surface_conditions")%>%hc_add_theme(hc_theme_538())
        hc_Road_surface
        
    })
    output$barchart_5 <-  renderHighchart({
        if (input$Acc_data == 1) { 
            ploty_data_5 <- accidents %>% filter(year==2005) %>% group_by(year,Road_Type) %>% count(year)
           
        }
        else if (input$Acc_data == 2) { 
            ploty_data_5 <- accidents %>% filter(year==2006) %>% group_by(year,Road_Type) %>% count(year)
        
        }
        else if (input$Acc_data == 3) { 
            ploty_data_5 <- accidents %>% filter(year==2007) %>% group_by(year,Road_Type) %>% count(year)
          
        }
        else if (input$Acc_data == 4) { 
            ploty_data_5 <- accidents %>% filter(year==2008) %>% group_by(year,Road_Type) %>% count(year)
            
        }
        else if (input$Acc_data == 5) { 
            ploty_data_5 <- accidents %>% filter(year==2009) %>% group_by(year,Road_Type) %>% count(year)
        
        }
        else if (input$Acc_data == 6) { 
            ploty_data_5 <- accidents %>% filter(year==2010) %>% group_by(year,Road_Type) %>% count(year)
        
        }
        else if (input$Acc_data == 7) { 
            ploty_data_5 <- accidents %>% filter(year==2011) %>% group_by(year,Road_Type) %>% count(year)
            
        }
        else if (input$Acc_data == 8) { 
            ploty_data_5 <- accidents %>% filter(year==2012) %>% group_by(year,Road_Type) %>% count(year)
        }
    
        else if (input$Acc_data == 9) { 
            ploty_data_5 <- accidents %>% filter(year==2013) %>% group_by(year,Road_Type) %>% count(year)

        }
       else  if (input$Acc_data == 10) { 
            ploty_data_5 <- accidents %>% filter(year==2014) %>% group_by(year,Road_Type) %>% count(year)
        }
        hc_Road_type<-ploty_data_5 %>%
            hchart('column', hcaes(x = Road_Type, y = n),name='Accidents') %>% 
            hc_xAxis(title = list(text = "Road_Type"),
                     opposite = FALSE) %>% 
            hc_yAxis(title = list(text = "Accidents"),
                     opposite = FALSE,
                     minorTickInterval = "auto",
                     minorGridLineDashStyle = "LongDashDotDot",
                     showFirstLabel = FALSE,
                     showLastLabel = FALSE)%>% hc_title(text = "Number of Accidents based on Road type")%>%hc_add_theme(hc_theme_538())
        hc_Road_type
        
    })
    pal <- colorFactor(c("red","blue","green4"), domain=c("1","2","3"))
    
    accident_popup <- paste0("<h4>Accident Details</h4><strong>Date: </strong>", accidents$Date,
                             "<br><strong>Severity: </strong>", severity_enum[accidents$Accident_Severity],
                             "<br><strong>Casualties: </strong>", accidents$Number_of_Casualties,
                             "<br><strong>Vehicles: </strong>", accidents$Number_of_Vehicles,
                             "<br><strong>Light Conditions: </strong>", light_cond_enum[accidents$Light_Conditions],
                             "<br><strong>Weather Conditions: </strong>", weather_enum[accidents$Weather_Conditions],
                             "<br><strong>Road Surface: </strong>", road_surface_enum[accidents$Road_Surface_Conditions])
    
    road_type <- reactive({
        temp <- accidents[accidents$Date >= as.character(input$sel_date_range[1]) &
                              accidents$Date <= as.character(input$sel_date_range[2]), ]
        
        if (input$sel_road_type > 0) {
            temp <- temp[temp$Road_Type == input$sel_road_type,]
        }
        
        if(input$sel_severity > 0) {
            temp <- temp[temp$Accident_Severity == input$sel_severity,]
        }
        road_type <- temp
    })
    
    output$map <- renderLeaflet({
        leaflet(accidents) %>%
            addTiles() %>%
            fitBounds(~min(Longitude), ~min(Latitude), ~max(Longitude), ~max(Latitude))
    })
    #fitBounds(~min(Longitude), ~min(Latitude), ~max(Longitude), ~max(Latitude))
    observe({
        X <- cbind(road_type()$Longitude, road_type()$Latitude)
        
        if (input$sel_road_type == 2) {
            kde2d <- bkde2D(X, bandwidth=c(0.00225,0.00225))
        }
        else {
            kde2d <- bkde2D(X, bandwidth=c(0.00500,0.00500))
        }
        x=kde2d$x1
        y=kde2d$x2
        z=kde2d$fhat
        
        CL <- contourLines(x, y, z)
        
        leafletProxy("map", data=road_type()) %>%
            clearShapes() %>%
            addCircles(popup=accident_popup,
                       radius=~((4 - Accident_Severity) * 30),
                       stroke = FALSE, fillColor = ~pal(Accident_Severity),
                       fillOpacity = 0.7)
        
        if (input$sel_heat_map == 1) {
            leafletProxy("map", data=road_type()) %>%
                addTiles() %>%
                addPolygons(CL[[1]]$x, CL[[1]]$y,fillColor = "red", stroke = FALSE) %>%
                addPolygons(CL[[2]]$x, CL[[2]]$y,fillColor = "red", stroke = FALSE) %>%
                addPolygons(CL[[3]]$x, CL[[3]]$y,fillColor = "red", stroke = FALSE) %>%
                addPolygons(CL[[4]]$x, CL[[4]]$y,fillColor = "red", stroke = FALSE) %>%
                addPolygons(CL[[5]]$x, CL[[5]]$y,fillColor = "red", stroke = FALSE) %>%
                addPolygons(CL[[6]]$x, CL[[6]]$y,fillColor = "red", stroke = FALSE) %>%
                addPolygons(CL[[7]]$x, CL[[7]]$y,fillColor = "red", stroke = FALSE) %>%
                addPolygons(CL[[8]]$x, CL[[8]]$y,fillColor = "red", stroke = FALSE) %>%
                addPolygons(CL[[9]]$x, CL[[9]]$y,fillColor = "red", stroke = FALSE) %>%
                addPolygons(CL[[10]]$x, CL[[10]]$y,fillColor = "red", stroke = FALSE)
        }
    })
    
    observe({
        proxy <- leafletProxy("map", data = accidents)
        
        proxy %>% clearControls()
        
        val <- proxy %>%
            addLegend(position = "bottomleft",
                      title = "Accident Severity",
                      pal = pal,
                      values = ~factor(accidents$Accident_Severity, labels=c("Fatal","Serious","Slight"))
            )
        
        output$results <- renderTable({
            accident_count <- road_type() %>%
                group_by(Severity) %>%
                summarise(Total = length(Accident_Severity))
            
            accident_count
        })
    })
    
    output$plot_hist  <- renderPlot({
        map <- get_map(location = "UK",
                       zoom = 14,
                       maptype = "roadmap")
        
        p <- ggmap(map)
        overlay <- stat_density2d(data=road_type(),
                                  aes(x = Longitude, y = Latitude, fill= ..level.., alpha=..level..),
                                  size=5, bins=8, geom="polygon")
        p <- p + overlay
        print(p)
    })
    output$linechart_1 <- renderHighchart({
        if (input$vehicles == 4) {
            data_cc <- uk_data %>% group_by(AADFYear) %>% summarise('hgv' = mean(AllHGVs))
            data_cc
            hc_veh_33<-data_cc %>%
                hchart('line', hcaes(x = AADFYear, y =hgv ),name='hgv')%>%hc_add_theme(hc_theme_538())
            hc_veh_33
        }
        else if (input$vehicles == 1) {
            data_cc <- uk_data %>% group_by(AADFYear) %>% summarise('hgv' = mean(AllHGVs))
            data_cc
            hc_veh_1<-data_cc %>%
                hchart('line', hcaes(x = AADFYear, y =hgv ),name='hgv')%>%hc_add_theme(hc_theme_538())
            hc_veh_1
        }
        else if (input$vehicles ==2) {
            data_ccv<- uk_data %>% group_by(AADFYear) %>% summarise('AMV' = mean(AllMotorVehicles))
            data_ccv
            hc_veh_2<-data_ccv %>%
                hchart('line', hcaes(x = AADFYear, y =AMV ),name='AMV')%>%hc_add_theme(hc_theme_538())
            hc_veh_2
        }
        else if (input$vehicles == 3) {
            data_cc <- uk_data %>% group_by(AADFYear) %>% summarise('hgv' = mean(AllHGVs))
            data_cc
            data_ccv<- uk_data %>% group_by(AADFYear) %>% summarise('AMV' = mean(AllMotorVehicles))
            data_ccv
            highchart() %>% 
                hc_yAxis_multiples(
                    list(title = list(text = 'hgv')),
                    list(title = list(text = 'AMV'))
                ) %>% 
                hc_add_series(data = data_cc, hcaes(x = AADFYear, y = hgv), color = 'steelblue' ,type = 'line', name = 'hgv') %>% 
                hc_add_series(data = data_ccv, hcaes(x = AADFYear, y = AMV ),color = 'red',type = "line", name = 'AMV',yAxis = 1)%>%
                hc_title(text = "Average of vehicles Travelling")%>%
                hc_add_theme(hc_theme_flat())
        }
        
        
    })
    output$linechart_2 <- renderHighchart({
        if (input$Road == 7){
            df_TM = uk_data %>%filter(RoadCategory == 'TM') %>% group_by(AADFYear,RoadCategory) %>% summarise('hgv' = mean(AllHGVs))
            df_TM
            hc_veh_3<-df_TM %>%
                hchart('line', hcaes(x = AADFYear, y =hgv ),name='TM')%>%hc_add_theme(hc_theme_538())
            hc_veh_3
            
        }
        else if (input$Road == 1){
            df_TM = uk_data %>%filter(RoadCategory == 'TM') %>% group_by(AADFYear,RoadCategory) %>% summarise('hgv' = mean(AllHGVs))
            df_TM
            hc_veh_3<-df_TM %>%
                hchart('line', hcaes(x = AADFYear, y =hgv ),name='TM')%>%hc_add_theme(hc_theme_538())
            hc_veh_3

        }
        else if (input$Road == 2){
            df_PU = uk_data %>%filter(RoadCategory == 'PU') %>% group_by(AADFYear,RoadCategory) %>% summarise('hgv' = mean(AllHGVs))
            df_PU
            hc_veh_4<-df_PU %>%
                hchart('line', hcaes(x = AADFYear, y =hgv ),name='TM')%>%hc_add_theme(hc_theme_538())
            hc_veh_4
        }
        else if (input$Road == 3){
            df_TR = uk_data %>%filter(RoadCategory == 'TR') %>% group_by(AADFYear,RoadCategory) %>% summarise('hgv' = mean(AllHGVs))
            df_TR
            hc_veh_5<-df_TR %>%
                hchart('line', hcaes(x = AADFYear, y =hgv ),name='TR')%>%hc_add_theme(hc_theme_538())
            hc_veh_5
        }
        else if (input$Road == 4){
            df_PR = uk_data %>%filter(RoadCategory == 'PR') %>% group_by(AADFYear,RoadCategory) %>% summarise('hgv' = mean(AllHGVs))
            df_PR
            hc_veh_6<-df_PR %>%
                hchart('line', hcaes(x = AADFYear, y =hgv ),name='PR')%>%hc_add_theme(hc_theme_538())
            hc_veh_6
        }
        else if (input$Road == 5){
            df_TU = uk_data %>%filter(RoadCategory == 'TU') %>% group_by(AADFYear,RoadCategory) %>% summarise('hgv' = mean(AllHGVs))
            df_TU
            hc_veh_7<-df_TU %>%
                hchart('line', hcaes(x = AADFYear, y =hgv ),name='TU')%>%hc_add_theme(hc_theme_538())
            hc_veh_7
        }
        else if (input$Road == 6){
            df_PM = uk_data %>%filter(RoadCategory == 'PM') %>% group_by(AADFYear,RoadCategory) %>% summarise('hgv' = mean(AllHGVs))
            df_PM
            hc_veh_8<-df_PM %>%
                hchart('line', hcaes(x = AADFYear, y =hgv ),name='PM')%>%hc_add_theme(hc_theme_538())
            hc_veh_8
        }
        
    })
    output$linechart_3 <- renderHighchart({
        if (input$Road_1 == 7){
            df_TM = uk_data %>%filter(RoadCategory == 'TM') %>% group_by(AADFYear,RoadCategory) %>% summarise('AMV' = mean(AllMotorVehicles))
            df_TM
            hc_veh_14<-df_TM %>%
                hchart('line', hcaes(x = AADFYear, y =AMV ),name='TM')%>%hc_add_theme(hc_theme_538())
            hc_veh_14
            
        }
        else if (input$Road_1 == 1){
            df_TM = uk_data %>%filter(RoadCategory == 'TM') %>% group_by(AADFYear,RoadCategory) %>% summarise('AMV' = mean(AllMotorVehicles))
            df_TM
            hc_veh_14<-df_TM %>%
                hchart('line', hcaes(x = AADFYear, y =AMV ),name='TM')%>%hc_add_theme(hc_theme_538())
            hc_veh_14
            
        }
        else if (input$Road_1 == 2){
            df_PU = uk_data %>%filter(RoadCategory == 'PU') %>% group_by(AADFYear,RoadCategory) %>% summarise('AMV' = mean(AllMotorVehicles))
            df_PU
            hc_veh_9<-df_PU %>%
                hchart('line', hcaes(x = AADFYear, y =AMV ),name='TM')%>%hc_add_theme(hc_theme_538())
            hc_veh_9
        }
        else if (input$Road_1 == 3){
            df_TR = uk_data %>%filter(RoadCategory == 'TR') %>% group_by(AADFYear,RoadCategory) %>% summarise('AMV' = mean(AllMotorVehicles))
            df_TR
            hc_veh_10<-df_TR %>%
                hchart('line', hcaes(x = AADFYear, y =AMV ),name='TR')%>%hc_add_theme(hc_theme_538())
            hc_veh_10
        }
        else if (input$Road_1 == 4){
            df_PR = uk_data %>%filter(RoadCategory == 'PR') %>% group_by(AADFYear,RoadCategory) %>% summarise('AMV' = mean(AllMotorVehicles))
            df_PR
            hc_veh_11<-df_PR %>%
                hchart('line', hcaes(x = AADFYear, y =AMV ),name='PR')%>%hc_add_theme(hc_theme_538())
            hc_veh_11
        }
        else if (input$Road_1 == 5){
            df_TU = uk_data %>%filter(RoadCategory == 'TU') %>% group_by(AADFYear,RoadCategory) %>% summarise('AMV' = mean(AllMotorVehicles))
            df_TU
            hc_veh_12<-df_TU %>%
                hchart('line', hcaes(x = AADFYear, y =AMV ),name='TU')%>%hc_add_theme(hc_theme_538())
            hc_veh_12
        }
        else if (input$Road_1 == 6){
            df_PM = uk_data %>%filter(RoadCategory == 'PM') %>% group_by(AADFYear,RoadCategory) %>% summarise('AMV' = mean(AllMotorVehicles))
            df_PM
            hc_veh_13<-df_PM %>%
                hchart('coloredline', hcaes(x = AADFYear, y =AMV),name='PM')%>%hc_add_theme(hc_theme_538())
            hc_veh_13
        }
        
            
        })
    
    
            
    
})   

