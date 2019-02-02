# Shiny Web Application for Displaying Open Medical Examiner Data for Chicago on a Map
# with the Ability to Change The Primary Cause and Have the Map Update and Only Show Those Cases
# Developing Data Products: Week 4
# By: Jonathan Gross

# Imports Open Medical Examiner Data for Chicago/Cook County
# Source: https://datacatalog.cookcountyil.gov/Public-Safety/Medical-Examiner-Case-Archive/cjeq-bs86

#mapdata$count<-1
#mapagg<-aggregate(count ~Primary.Cause,data=mapdata,FUN = "sum")

# Loads libraries. use install.packages() first.
library(shiny)
library(leaflet)
library(magrittr)

# For Data Tables.
library(DT)

# Define User Interface
ui <- fluidPage(
   
   # Application title
   titlePanel("Chicago: Open Medical Examiner Case Data, 2018 (First Half)"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
          selectInput("select", label = h3("Select Primary Cause"), 
                      choices = list("MULTIPLE GUNSHOT WOUNDS" = "MULTIPLE GUNSHOT WOUNDS", "ORGANIC CARDIOVASCULAR DISEASE" = "ORGANIC CARDIOVASCULAR DISEASE", "CHRONIC ETHANOLISM" = "CHRONIC ETHANOLISM",
                                    "HYPERTENSIVE CARDIOVASCULAR DISEASE"="HYPERTENSIVE CARDIOVASCULAR DISEASE","DIABETIC KETOACIDOSIS"="DIABETIC KETOACIDOSIS",
                                    "COMPLICATIONS OF FALL"="COMPLICATIONS OF FALL"), selected = "MULTIPLE GUNSHOT WOUNDS"),
          h4("Welcome to an app that helps you examine open data from the Chicago/Cook County Medical Examiner. To use this app, select a cause of death from the drop-down menu. The map and table will update the results based on what you selected. Also, click on map markers to bring up a pop-up with information."),
          h5("Created by Jonathan Gross on February 2, 2019 for Data Driven Products - Week #4")
           ),
      
      # Show map
      mainPanel(
        leafletOutput("map",height="400"),
        h3("Data Source:",a("Cook County Open Data",href="https://datacatalog.cookcountyil.gov/Public-Safety/Medical-Examiner-Case-Archive/cjeq-bs86"),
           div(dataTableOutput("table"),style = "font-size:45%")
      ))
    
))

# Server
server <- function(input, output) {
   output$map <- renderLeaflet({
     mapdata<-read.csv("C:/Users/Jon/Desktop/MyApp/Medical_Examiner_Case_Archive_2018_Half.csv",header=TRUE,sep=",")
     mapdata<-mapdata[!is.na(mapdata$latitude) | !is.na(mapdata$longitude),]
     mapdata<-mapdata[mapdata$Primary.Cause==input$select,]
     #mapreact<-reactive({mapdata})
     map_label<-paste0("Date:",mapdata$Date.of.Death,"<br>","Age:",mapdata$Age,"<br>","Gender:",mapdata$Gender,"<br>","Race:",mapdata$Race,"<br>","Manner of Death:",mapdata$Manner.of.Death,"<br>","Primary Cause:",mapdata$Primary.Cause)
     #}
     
     leaflet(data=mapdata[mapdata$Primary.Cause==input$select,])%>%addTiles()%>%setView(lat=41.8,lng=-87.75,zoom=10)%>%
       addMarkers(lat=~latitude,lng=~longitude,popup=map_label)%>%
       addMiniMap()
   })
   
   output$table<-renderDataTable({
     mapdata<-read.csv("C:/Users/Jon/Desktop/MyApp/Medical_Examiner_Case_Archive_2018_Half.csv",header=TRUE,sep=",")
     keep_vars<-c("Case.Number","Date.of.Death","Primary.Cause","Manner.of.Death","Age","Gender","Race","Latino","latitude","longitude")
     mapdata<-mapdata[keep_vars]
     mapdata<-mapdata[!is.na(mapdata$latitude) | !is.na(mapdata$longitude),]
     mapdata<-mapdata[mapdata$Primary.Cause==input$select,]
     mapdata[mapdata$Primary.Cause==input$select,]
     })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

# End of Program