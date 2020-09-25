##################################
# Fire Trendr Shiny App          #
# Source: Alessio Benedetti      #
# server.R file                  #
##################################

library(shiny)
library(tidyverse)
library(lubridate)
library(rgdal)
library(sp)
library(raster)

#####################################################################################################################################################

#########################
# BENS'S DATA WRANGLING #
#########################

# MCD64A1
# THIS SECTION CREATES A 4D ARRAY [Landowner, State, Month, Year] FROM THE MCD64A1 DATA TO BE USED FOR PLOT CREATION

#Specify last month in archive for 2020 (6=June)
modis.endmonth <- 6

#Set up array to organize state BA data
modis.states <- state.name
modis.ba <- array(0,c(2,length(modis.states),12,length(2000:2020)))
modis.years <- 2000:2020
modis.months <- 1:12
LandType <- 1:2

for(landowner in 1:2){
  print(landowner)
  #Read in MODIS
  if(landowner == 1){
    ba.modis <- read.csv("F:/ESimonson_Workspace/NASA_Fire_Project/Fire-COVID19-ES/data/MCD64A1/MCD64A1_PixelCount_TimeSeries_US_Fedlands.csv")
    owner <- "FED"
  }
  if(landowner == 2){
    ba.modis <- read.csv("F:/ESimonson_Workspace/NASA_Fire_Project/Fire-COVID19-ES/data/MCD64A1/MCD64A1_PixelCount_TimeSeries_US_States.csv")
    owner <- "ALL"
  }
  
  #Populate the state BA array
  for(state in modis.states){ 
  #
  state.id <- which(ba.modis$NAME==state)
  
  #
  for(id in state.id){
    state.date <- ba.modis$imageId[id]
    state.month <- as.numeric(substr(state.date,6,7))
    state.year <- as.numeric(substr(state.date,1,4))
    
    #
    state.fires <- ba.modis$count[id]
    
    #
    modis.ba[landowner,which(state==modis.states),which(state.month==modis.months),which(state.year==modis.years)] <- state.fires
   }
  }
}

#Remove Jul-Dec 2020
modis.ba[,,(modis.endmonth+1):12,21] <- NA

#Rescale from 500 m to km2
modis.ba <- modis.ba * ((500*500)/(1000*1000))

#####################################################################################################################################################

# DATA WRANGLING FOR VIIRS ACTIVE FIRE
fdirIn <- "C:/Users/esimonson/Desktop/Fire-COVID19-ES/data/ActiveFire/"
fdirOut <- "F:/ESimonson_Workspace/NASA_Fire_Project/Fire-COVID19-ES/data/ActiveFire/VIIRS/"

#List states
#states <- c("Florida","Georgia") #"Mississippi","North_Carolina","South_Carolina","Alabama","California","Arizona","New_Mexico","Texas","Louisiana","Tennessee","Nevada","Colorado")

# THIS SECTION IMPORTS THE SUOMI WEEKLY TIME SERIES DATA AND REFORMATS IT FOR PLOTTING
# THIS FORMAT COULD BE EXPORTED AND SAVED AS NEW DATASETS TO AVOID THIS PROCESSING IN THE APP SCRIPT
# Import VIIRS time series data for each state
#Florida_AF <- read.csv(paste(fdirOut,"SUOMI_AF_timeseries_allstates_Florida_allland.csv", sep=""))
#Georgia_AF <- read.csv(paste(fdirOut,"SUOMI_AF_timeseries_allstates_Georgia_allland.csv", sep=""))
# Convert to matrix
#Florida_AF <- as.matrix(Florida_AF)
#Georgia_AF <- as.matrix(Georgia_AF)
# Remove column names
#colnames(Florida_AF) <- c()
#colnames(Georgia_AF) <- c()
# Reformat strings into columns
#Florida_AF <- str_split_fixed((Florida_AF), " ", 10)
#Georgia_AF <- str_split_fixed((Georgia_AF), " ", 10)
# Remove unwanted columns
#Florida_AF <- Florida_AF[,-1]
#Georgia_AF <- Georgia_AF[,-1]
# Make matrix integer class
#class(Florida_AF) <- "integer"
#class(Georgia_AF) <- "integer"

#states <- c(Florida_AF, Georgia_AF)

# MODIS ACTIVE FIRE
# THIS SECTION CREATES A 4D ARRAY [Landowner, State, Month, Year] FROM THE MODIS AF DATA TO BE USED FOR PLOT CREATION

# Import the MODIS AF Weekly Dataset
MODIS_AF_Weekly_States <- read.csv("F:/ESimonson_Workspace/NASA_Fire_Project/Fire-COVID19-ES/data/ActiveFire/MODIS/MODIS_AF_Weekly_TimeSeries_US_States_LONG.csv", sep="")
MODIS_AF_Weekly_Feds <- read.csv("F:/ESimonson_Workspace/NASA_Fire_Project/Fire-COVID19-ES/data/ActiveFire/MODIS/MODIS_AF_Weekly_TimeSeries_US_Fedlands_LONG.csv", sep="")

#Set up array to organize state BA data
modis.af <- array(0,c(2,length(modis.states),52,length(2001:2020)))
modisAF.years <- 2001:2020
modisAF.weeks <- 1:52
modisAF.endweek <- 37
#LandTypeAF <- 1:2

for(landowner in 1:2){
   print(landowner)
   #Read in MODIS
   if(landowner == 1){
      #ba.modis <- read.csv("F:/ESimonson_Workspace/NASA_Fire_Project/Fire-COVID19-ES/data/ActiveFire/MODIS/Weekly_ByState/MODIS_AF_Weekly_TimeSeries_US_Fedlands.csv")
      af.modis <- MODIS_AF_Weekly_Feds
      owner <- "FED"
   }
   if(landowner == 2){
      #ba.modis <- read.csv("F:/ESimonson_Workspace/NASA_Fire_Project/Fire-COVID19-ES/data/ActiveFire/MODIS/Weekly_ByState/MODIS_AF_Weekly_TimeSeries_US_States.csv")
      af.modis <- MODIS_AF_Weekly_States
      owner <- "ALL"
   }
   
   #Populate the state BA array
   for(state in modis.states){ 
      #
      state.id <- which(af.modis$NAME==state)
      
      #
      for(id in state.id){
         #state.date <- ba.modis$imageId[id]
         state.week <- af.modis$Week[id]
         state.year <- af.modis$Year[id]
         state.fires <- af.modis$sumweek[id]
         
         #
         modis.af[landowner,which(state==modis.states),which(state.week==modisAF.weeks),which(state.year==modisAF.years)] <- state.fires
      }
   }
}

#Remove Sept/Oct-Dec 2020
modis.af[,,(modisAF.endweek+1):52,20] <- NA

#####################################################################################################################################################

################
# SERVER LOGIC #
################

shinyServer(function(input, output) {
 # MCD64A1 SECTION
 # CREATE FUNCTION FOR USER TO SELECT A STATE FROM DROP-DOWN LIST
 output$Land_Type <- renderUI({
   selectInput("SelectedLand","Federal (1) or Statewide (2):", LandType)
 })
 output$MCD64A1_ByState_Plot <- renderUI({
   selectInput("SelectedState","Select a state:", modis.states)
 })
 
 # CREATE A REACTIVE VARIABLE THAT RESPONDS TO THE USER SELECTION
 ss <- reactive(which(input$SelectedState==modis.states))
 LT <- reactive(which(input$SelectedLand==LandType))
 
 # FOR FILE NAMING
 LT_Name <- reactive(if(LT() == 1){
   OWNER = "FederalLand"
 } else {
   OWNER = "Statewide"
 })
 
 # CREATE A REACTIVE PLOT THAT RESPONDS TO THE USER SELECTION
 # IMBED THE REACTIVE VARIABLEs 'SS()' AND 'LT()' INTO THE PLOT CODE
 
 plotInput <- function(){
   #par(mfrow=c(1,1),mar=c(4,4,4,4),cex=1.3)
   plot(0,0,xlim=c(1,12),ylim=c(1,max(modis.ba[LT(),ss(),,],na.rm=T)),col="white",xlab="", ylab="",main=modis.states[ss()])
   mtext(expression(paste("MCD64A1 Burned Area (km"^"2",")")),2,cex=1.4,line=2.1)
   mtext("Months Since Jan.",1,cex=1.4,line=2.1)
   for(i in 1:length(2000:2020)){
     lines(1:12, modis.ba[LT(),ss(),,i],col=rev(heat.colors(21))[i])
   }
   lines(1:12,rowMeans(modis.ba[LT(),ss(),,],na.rm=T),lwd=3,col=1)
   lines(1:12,modis.ba[LT(),ss(),,20],lwd=3,col=rev(heat.colors(20))[20],lty=2)
   lines(1:12,modis.ba[LT(),ss(),,21],lwd=3,col=3)
   legend(1,max(modis.ba[LT(),ss(),,],na.rm=T),c(2000:2019),col=rev(heat.colors(20)),lty=c(rep(1,19),2),ncol=3,lwd=2,bty="n",cex=0.7)
   legend(1,max(modis.ba[LT(),ss(),,],na.rm=T)*.7,c(2020),col=3,lwd=2,bty="n",cex=0.7)
   legend(1,max(modis.ba[LT(),ss(),,],na.rm=T)*.65,"mean",col=1,lwd=2,bty="n",cex=0.7)
 }
 
 plotInput2 <- function(){
   #par(mfrow=c(1,1),mar=c(4,4,4,4),cex=1.3)
   ymax <- max(c(colSums(modis.ba[LT(),ss(),,],na.rm=T),colSums(modis.ba[LT(),ss(),,],na.rm=T)))
   plot(0,0,xlim=c(1,12),ylim=c(0,ymax),col="white",xlab="", ylab="",main=modis.states[ss()])
   mtext(expression(paste("MCD64A1 Burned Area (km"^"2",")")),2,cex=1.4,line=2.1)
   mtext("Months Since Jan.",1,cex=1.4,line=2.1)
   for(i in 1:length(2000:2020)){
     lines(1:12, cumsum(modis.ba[LT(),ss(),,i]),col=rev(heat.colors(21))[i])
   }
   lines(1:12,cumsum(rowMeans(modis.ba[LT(),ss(),,],na.rm=T)),lwd=3,col=1)
   lines(1:12,cumsum(modis.ba[LT(),ss(),,20]),lwd=3,col=rev(heat.colors(20))[20],lty=2)
   lines(1:12,cumsum(modis.ba[LT(),ss(),,21]),lwd=3,col=3)
   legend(1,ymax,c(2000:2019),col=rev(heat.colors(20)),lty=c(rep(1,19),2),ncol=3,lwd=2,bty="n",cex=0.7)
   legend(1,ymax*.7,c(2020),col=3,lwd=2,bty="n",cex=0.7)
   legend(1,ymax*.65,"mean",col=1,lwd=2,bty="n",cex=0.7)
 }

 # RENDER REACTIVE PLOTS ON THE OPEN TAB 
 output$ggplotAllLand <- renderPlot({print(plotInput())})
 
 # REPEAT THE PROCESS FOR CUMULATIVE PLOTS
 output$ggplotCumSum <- renderPlot({print(plotInput2())})
 
# THE SECTION OF CODE BELOW GIVES THE OPTION TO DOWNLOAD THE PLOT AS A PNG
 output$download_ggplotAllLand <- downloadHandler(
    filename = function() {paste("MCD64A1_TimeSeriesPlot_",input$SelectedState,"_",LT_Name(),".png")},
    content = function(file){
      png(file)
      plotInput()
      dev.off()
  })
 
 output$download_ggplotCumSum <- downloadHandler(
   filename = function() {paste("MCD64A1_Cumulative_TimeSeriesPlot_",input$SelectedState,"_",LT_Name(),".png")},
   content = function(file){
     png(file)
     plotInput2()
     dev.off()
   })
 
 # THIS SECTION OF CODE BELOW CAN BE USED TO SAVE TABLES AS A CSV
#output$dto <- renderTable(modis.ba[LT(),ss(),,], include.rownames=FALSE)
output$download_dto <- downloadHandler(
  filename = function(){paste("MCD64A1_TimeSeries_",input$SelectedState,"_",LT_Name(),".csv")},
  content = function(file)
    write.csv(modis.ba[LT(),ss(),,], file)
)
 
# output$dto2 <- renderTable(modis.ba[1,ss(),1,1], include.rownames=FALSE)
# output$download <- downloadHandler(
#   filename = function(){paste("MCD64A1_TimeSeries_FedLand_",input$SelectedState,".csv")}, 
#   content = function(fname){
#     write.csv(output$dto2, fname)
#   })
 
 
#####################################################################################################################################################
 
# ACTIVE FIRE SECTION 
# CREATE FUNCTION FOR USER TO SELECT A STATE FROM DROP-DOWN LIST
 output$ActiveFire_ByState_Plot <- renderUI({
   selectInput("SelectedState2","Select a state:", modis.states)
 })
 output$ActiveFire_Land_Type <- renderUI({
   selectInput("SelectedLand2","Federal (1) or Statewide (2):", LandType)
 })

# CREATE A REACTIVE VARIABLE THAT RESPONDS TO THE USER SELECTION
 
 #suomi.af <- reactive(which(input$SelectedState2==states))
 #suomi.af <- reactive(which(input$SelectedState2==paste(states,"_AF", sep = "")))
 #suomi.af() <- MODIS_AF_Weekly[MODIS_AF_Weekly$NAME==State.Names,]

 ss2 <- reactive(which(input$SelectedState2==modis.states))
 LT2 <- reactive(which(input$SelectedLand2==LandType))
 
 # FOR FILE NAMING
 LT2_Name <- reactive(if(LT2() == 1){
    OWNER = "FederalLand"
 } else {
    OWNER = "Statewide"
 })
 
# Plot time series of AF
 plotInputAF <- function(){
    plot(0,0,xlim=c(0,53),ylim=c(1,max(modis.af[LT2(),ss2(),,],na.rm=T)),col="white",xlab="Weeks Since Jan 1", ylab="MODIS Weekly Fire Counts (#)",main=paste(modis.states[ss2()]))
    for(i in 1:length(2001:2019)){
       lines(1:52, modis.af[LT2(),ss2(),,i],col=rev(heat.colors(19))[i])
    }
    lines(1:52,rowMeans(modis.af[LT2(), ss2(),,],na.rm=T),lwd=3,col=1)
    lines(1:52,modis.af[LT2(),ss2(),,20],lwd=3,col=3)
    legend(0,max(modis.af[LT2(), ss2(),,],na.rm=T),c(2012:2019),col=rev(heat.colors(8)),ncol=3,lwd=2,bty="n",cex=0.8)
    legend(0,max(modis.af[LT2(), ss2(),,],na.rm=T)*0.87,c(2020),col=3,lwd=2,bty="n",cex=0.8)
    legend(0,max(modis.af[LT2(), ss2(),,],na.rm=T)*0.83,"mean",col=1,lwd=2,bty="n",cex=0.8)
 
 }
 
# Plot cumulative AF
 plotInputAF2 <- function(){
 modis.af[is.na(modis.af)] <- 0
 plot(0,0,xlim=c(0,53),ylim=c(1,max(colSums(modis.af[LT2(), ss2(),,],na.rm=T))),col="white",xlab="Weeks Since Jan 1", ylab="MODIS (TERRA/AQUA) Weekly Fire Counts (#)",main=paste(modis.states[ss2()]))
 for(i in 1:length(2001:2019)){
    lines(1:52, cumsum(modis.af[LT2(), ss2(),,i]),col=rev(heat.colors(20))[i])
 }
 lines(1:52, cumsum(rowMeans(modis.af[LT2(),ss2(),,],na.rm=T)),lwd=3,col=1)
 lines(1:37, cumsum(modis.af[LT2(),ss2(),1:37,20]),lwd=3,col=3)
 legend(25,max(colSums(modis.af[LT2(), ss2(),,],na.rm=T))*0.5,c(2001:2019),col=rev(heat.colors(21)),ncol=3,lwd=2,bty="n",cex=0.8)
 legend(25,max(colSums(modis.af[LT2(), ss2(),,],na.rm=T))*0.15,c(2020),col=3,lwd=2,bty="n",cex=0.8)
 legend(25,max(colSums(modis.af[LT2(), ss2(),,],na.rm=T))*0.1,"mean",col=1,lwd=2,bty="n",cex=0.8)
 }
 
 # RENDER REACTIVE PLOTS
 output$ggplotAllLandAF <- renderPlot({print(plotInputAF())})
 output$ggplotCumSumAF <- renderPlot({print(plotInputAF2())})
 
 # THE SECTION OF CODE BELOW GIVES THE OPTION TO DOWNLOAD THE PLOT AS A PNG
 output$download_ggplotAllLandAF <- downloadHandler(
    filename = function() {paste("MOD_ActiveFire_TimeSeriesPlot_",input$SelectedState2,"_",LT2_Name(),".png")},
    content = function(file){
       png(file)
       plotInputAF()
       dev.off()
    })
 
 output$download_ggplotCumSumAF <- downloadHandler(
    filename = function() {paste("MOD_ActiveFire_Cumulative_TimeSeriesPlot_",input$SelectedState2,"_",LT2_Name(),".png")},
    content = function(file){
       png(file)
       plotInputAF2()
       dev.off()
    })
})