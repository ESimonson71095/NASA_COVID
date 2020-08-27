##################################
# Biodiversity in National Parks #
# by Alessio Benedetti           #
# server.R file                  #
##################################

library(shiny)
library(tidyverse)
library(lubridate)
library(leaflet.extras)
library(rvest)

#####################
# SUPPORT FUNCTIONS #
#####################

# function to retrieve a park image from the park wiki page
park_image <- function (park_Name){
  
  #bug1_fix#
  park_WikiUrl <- gsub(" ","_",paste0("https://en.wikipedia.org/wiki/",park_Name))
  #bug1_fix#
  park_Img <- read_html(park_WikiUrl)
  park_Img <- park_Img %>% html_nodes("img")
  
  list_park_Img <- (grepl("This is a featured article", park_Img) | grepl("Question_book-new.svg.png", park_Img) | grepl("Listen to this article", park_Img) | grepl("This is a good article", park_Img))
  park_Img <- park_Img[min(which(list_park_Img == FALSE))]
  
  park_Img <- gsub("\"","'",park_Img)
  park_Img <- gsub("//upload.wikimedia.org","https://upload.wikimedia.org",park_Img)
  park_Img <- sub("<img","<img style = 'max-width:100%; max-height:200px; margin: 10px 0px 0px 0px; border-radius: 5%; border: 1px solid black;'",park_Img)
  
  return(park_Img)
  
}
  
# function that build the park card html pop up
park_card <- function (park_Name, park_Code, park_State, park_Acres, park_Latitude, park_Longitude) {
  
  card_content <- paste0("<style>div.leaflet-popup-content {width:auto !important;}</style>",
                    "<link rel='stylesheet' href='https://use.fontawesome.com/releases/v5.7.1/css/all.css' integrity='sha384-fnmOCqbTlWIlj8LyTjo7mOUStjsKC4pOpQbqyi7RrhN7udi9RwhKkMHpvLbHG9Sr' crossorigin='anonymous'>",
                    "<table style='width:100%;'>",
                    "<tr>",
                    "<th><b><h2 style='text-align: left;'>",park_Name,"</h2></b></th>",
                    "<th><img style = 'border:1px solid black;' src='https://www.crwflags.com/art/states/",park_State,".gif' alt='flag' title='Flag of ",state.name[match(park_State,state.abb)]," ' width=80></th>",
                    "</tr>",
                    "</table>",
                    "<div class='flip-card'>",
                      "<div class='flip-card-inner'>",
                        "<div class='flip-card-front'>",
                          "<table style='width:100%;'>",
                            "<tr>",
                              "<td colspan='2'>",park_image(park_Name),"</td>",
                            "</tr>",
                            "<tr>",
                              "<td style='padding: 5px;'><h4><b>Code: </b>",park_Code,"</h4></td>",
                              "<td style='padding: 5px;'><h4><b>Acres: </b>",format(park_Acres, big.mark = ' '),"</h4></td>",
                            "</tr>",
                            "<tr>",
                              "<td style='padding: 5px;'><h4><b>Latitude: </b>",park_Latitude,"</h4></td>",
                              "<td style='padding: 5px;'><h4><b>Longitude: </b>",park_Longitude,"</h4></td>",
                            "</tr>",
                          "</table>",
                        "</div>",
                        "<div class='flip-card-back'>",
                          "<h3>Media links</h3> ",
                          "<hr>",
                          "<table style='width:80%;'>",
                            "<tr>",
                              "<td style='text-align: left; padding-left: 25px;'><h4>Official page:</h4></td>",
                              "<td><a style='color:white;' href='https://www.nps.gov/",park_Code,"/index.htm' target='_blank'><i class='fas fa-globe fa-2x'></i></a></td>",
                            "</tr>",
                            "<tr>",
                              "<td style='text-align: left; padding-left: 25px;'><h4>Wikipedia page:<h4></td>",
                              "<td><a style='color:white' href='https://en.wikipedia.org/wiki/",park_Name,"' target='_blank'><i class='fab fa-wikipedia-w fa-2x'></i></td></p>",
                            "</tr>",        
                            "<tr>",
                              "<td style='text-align: left; padding-left: 25px;'><h4>Pictures:<h4></td>",
                              "<td><a style='color:white' href='https://www.google.com/search?tbm=isch&q=",park_Name,"&tbs=isz:m' target='_blank'><i class='fas fa-images fa-2x'></i></a></td>",
                            "</tr>",
                            "<tr>",
                              "<td style='text-align: left; padding-left: 25px;'><h4>Youtube videos:<h4></td>",
                              "<td><a style='color:white' href='https://www.youtube.com/results?search_query=",park_Name,"' target='_blank'><i class='fab fa-youtube fa-2x'></i></td>",
                            "</tr>",
                          "</table>",
                        "</div>",
                      "</div>",
                    "</div>"
  )
  
  return(card_content)
  
}

##################
# DATA WRANGLING #
##################

# preprocessed parks file:
#   3 records were multi states parks, only was was attributed
#     DEVA,Death Valley National Park,CA/NV,4740912,36.24,-116.82  --> CA
#     GRSM,Great Smoky Mountains National Park,TN/NC,521490,35.68,-83.53 --> TN
#     YELL,Yellowstone National Park,WY/MT/ID,2219791,44.6,-110.5 --> WY
#   added (U.S.) suffix to Glacier National Park record for wiki disambigaution

parks <- read.csv("www/parks.csv")
species <- read.csv("www/species.csv")

##############################################################################################################################################
States_BA <- read.csv("C:/Users/esimonson/Desktop/Fire-COVID19-ES/data/MCD64A1/MCD64A1_PixelCount_TimeSeries_US_States.csv")
Fedland_BA <- read.csv("C:/Users/esimonson/Desktop/Fire-COVID19-ES/data/MCD64A1/MCD64A1_PixelCount_TimeSeries_US_Fedlands.csv")

States_BA$imageId <- as.Date(States_BA$imageId,, tryFormats = ("%Y_%m_%d"))
MOD <- States_BA[2:4]
MOD1 <- MOD[order(MOD$NAME),]

Fedland_BA$imageId <- as.Date(Fedland_BA$imageId,, tryFormats = ("%Y_%m_%d"))
MOD_2 <- Fedland_BA[2:4]
MOD2 <- MOD_2[order(MOD_2$NAME),]

same_year <- function(x) {
  year(x) <- 2000
  x
}

#Set up array to organize state BA data
modis.states <- state.name
modis.ba <- array(0,c(2,length(modis.states),12,length(2000:2020)))
modis.years <- 2000:2020
modis.months <- 1:12
 
#####################################################################################################################################################

################
# SERVER LOGIC #
################

shinyServer(function(input, output) {
   
 # ggplot2 charts
 output$MCD64A1_ByState_Plot <- renderUI({
   selectInput("SelectedState","Select a state:", MOD2$NAME)
 })
 output$MCD64A1_ByState_Plot2 <- renderUI({
   selectInput("SelectedState","Select a state:", MOD2$NAME)
 })
 
 AllLandGgplot1 <- reactive(MOD1[MOD1$NAME==input$SelectedState,])
 FedLandGgplot2 <- reactive(MOD2[MOD2$NAME==input$SelectedState,])
 
 output$ggplotAllLand <- renderPlot({
   
   g1 <- ggplot(data = AllLandGgplot1()) + aes(x= same_year(imageId), y=count, color = as.factor(year(imageId))) + geom_line() + labs(title=paste("MCD64A1 Pixel Counts by Month for All Land in ", input$SelectedState), x = paste0("Time Series for ", input$SelectedState), y = "BA Pixel Count") + scale_x_date(date_breaks = "1 month", date_labels="%b")
   mtext(expression(paste("MCD64A1 Burned Area (km"^"2",")")),2,cex=1.4,line=2.1)
   mtext("Months Since Jan.",1,cex=1.4,line=2.1)
   #g1 <- ggplot(data = speciesGgplot1()) + stat_count(mapping = aes(x=fct_rev(Park.Name)), fill="green3") + labs(title="MCD64A1 Pixel Counts by Month for All Land", x ="BA Pixel Count", y = paste0("Time Series for ", input$selectedCategoryChart)) + coord_flip() + theme_classic() + geom_text(stat='count', aes(fct_rev(Park.Name), label=..count..), hjust=2, size=4)
   print(g1)
   
 })
 
 output$ggplotFedLand <- renderPlot({
   
   g2 <- ggplot(data = FedLandGgplot2()) + aes(x= same_year(imageId), y=count, color = as.factor(year(imageId))) + geom_line() + labs(title=paste("MCD64A1 Pixel Counts by Month for Federal Land in ", input$SelectedState), x = paste0("Time Series for Federal Lands in  ", input$SelectedState), y = "BA Pixel Count in Federal Land") + scale_x_date(date_breaks = "1 month", date_labels="%b")
   #g2 <- ggplot(data = FedLandGgplot2()) + stat_count(mapping = aes(x=fct_rev(Park.Name)), fill="green3") + labs(title="MCD64A1 Pixel Counts by Month for Federal Land", x ="BA Pixel Count", y = paste0("Time Series for ", input$selectedCategoryChart)) + coord_flip() + theme_classic() + geom_text(stat='count', aes(fct_rev(Park.Name), label=..count..), hjust=2, size=4)
   print(g2)
   
 })
 
 output$dto <- renderTable(AllLandGgplot1(), include.rownames=FALSE)
 output$download <- downloadHandler(
   filename = function(){paste("MCD64A1_TimeSeries_",input$SelectedState,".csv")}, 
   content = function(fname){
     write.csv(output$dto, fname)
 })
 
 output$dto2 <- renderTable(FedLandGgplot2(), include.rownames=FALSE)
 output$download <- downloadHandler(
   filename = function(){paste("MCD64A1_TimeSeries_FedLand_",input$SelectedState,".csv")}, 
   content = function(fname){
     write.csv(output$dto2, fname)
   })
 
})