##################################
# Fire Trendr Shiny App          #
# Source: Eli Simonson           #
# server.R file                  #
##################################

library(shiny)
library(tidyverse)
library(lubridate)
library(rgdal)
library(sp)
library(raster)

#####################################################################################################################################################

# MCD64A1
# THIS SECTION CREATES A 4D ARRAY [Landowner, State, Month, Year] FROM THE MCD64A1 DATA TO BE USED FOR PLOT CREATION

#Specify last month in archive for 2020 (6=June)
modis.endmonth <- 6

#Set up array to organize state BA data
modis.states <- state.name
modis.ba <- array(0,c(2,length(modis.states),12,length(2000:2020)))
modis.years <- 2000:2020
modis.months <- 1:12
#LandType <- c("Federal", "Statewide")
LandType <- c("Statewide", "Federal")

for(landowner in 1:2){
  print(landowner)
  #Read in MCD64A1
  if(landowner == 2){
    ba.modis <- read.csv("www/MCD64A1_PixelCount_TimeSeries_US_Fedlands.csv")
    owner <- "FED"
  }
  if(landowner == 1){
    ba.modis <- read.csv("www/MCD64A1_PixelCount_TimeSeries_US_States.csv")
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

# Create dimnames for the table so that columns and rows are named when CSV is exported
dimnames(modis.ba) <- list(c("Statewide", "Federal"),c(),1:12,2000:2020)

#####################################################################################################################################################

# DATA WRANGLING FOR MODIS ACTIVE FIRE
fdir <- "www/"

# MODIS ACTIVE FIRE
# THIS SECTION CREATES A 4D ARRAY [Landowner, State, Month, Year] FROM THE MODIS AF DATA TO BE USED FOR PLOT CREATION

StateNames <- c("Alabama","Alaska","Arizona","Arkansas","California","Colorado","Connecticut","Delaware","Florida","Georgia","Hawaii","Idaho","Illinois","Indiana","Iowa","Kansas",
                "Kentucky","Louisiana","Maine","Maryland","Massachusetts","Michigan","Minnesota","Mississippi","Missouri","Montana","Nebraska","Nevada","New_Hampshire","New_Jersey",
                "New_Mexico","New_York","North_Carolina","North_Dakota","Ohio","Oklahoma","Oregon","Pennsylvania","Rhode_Island","South_Carolina","South_Dakota","Tennessee","Texas",
                "Utah","Vermont","Virginia","Washington","West_Virginia","Wisconsin","Wyoming")

#Set up array to organize state AF data
modis.af <- array(0,c(2,length(StateNames),52,length(2001:2020)))
modisAF.years <- 2001:2020
modisAF.weeks <- 1:52

for(landowner in 1:2){
   print(landowner)
   #Read in MODIS
   if(landowner == 2){
      af.modis <- read.csv(paste0(fdir,"MODIS_AF_Weekly_TimeSeries_US_Fedlands_R.csv"), sep = "")
      owner <- "FED"
   }
   if(landowner == 1){
      af.modis <- read.csv(paste0(fdir,"MODIS_AF_Weekly_TimeSeries_US_States_R.csv"), sep = "")
      owner <- "ALL"
   }
   
   #Populate the state BA array
   for(state in StateNames){ 
      #
      state.id <- which(af.modis$NAME==state)
      
      #
      for(id in state.id){
         state.week <- af.modis$Week[id]
         state.year <- af.modis$Year[id]
         state.fires <- af.modis$SumWeek[id]
         
         #
         modis.af[landowner,which(state==StateNames),which(state.week==modisAF.weeks),which(state.year==modisAF.years)] <- state.fires
      }
   }
}

# Remove Last Weeks of 2020 w/o data
modisAF.endweek <- length(which(!is.na(modis.af[1,5,,20])))

# Create dimnames for the table so that columns and rows are named when CSV is exported
dimnames(modis.af) <- list(c("Statewide", "Federal"),c(),1:52,2001:2020)

# Clean Up
modis.af[is.na(modis.af)] <- 0
modis.af[,,(modisAF.endweek+1):52,20] <- NA

#####################################################################################################################################################

# DATA WRANGLING FOR VIIRS ACTIVE FIRE

# CREATE 4D ARRAY PRIOR TO PLOTTING
viirs.af <- array(0,c(2,length(StateNames),52,length(2012:2020)))
viirsAF.years <- 2012:2020
viirsAF.weeks <- 1:52


for(landowner in 1:2){
  print(landowner)
  #Read in VIIRS
  if(landowner == 2){
    af.viirs <- read.csv(paste0(fdir,"VIIRS_AF_Weekly_TimeSeries_US_Fedlands_R.csv"), sep = "")
    owner <- "FED"
  }
  if(landowner == 1){
    af.viirs <- read.csv(paste0(fdir,"VIIRS_AF_Weekly_TimeSeries_US_States_R.csv"), sep = "")
    owner <- "ALL"
  }
  
  #Populate the state BA array
  for(state in StateNames){ 
    #
    state.id <- which(af.viirs$NAME==state)
    
    #
    for(id in state.id){
      state.week <- af.viirs$Week[id]
      state.year <- af.viirs$Year[id]
      state.fires <- af.viirs$SumWeek[id]
      
      #
      viirs.af[landowner,which(state==StateNames),which(state.week==viirsAF.weeks),which(state.year==viirsAF.years)] <- state.fires
    }
  }
}

# Remove Last Weeks of 2020 w/o data
viirsAF.endweek <- length(which(!is.na(viirs.af[1,5,,9]))) # [AllLand, California, ,2020]

# Create dimnames for the table so that columns and rows are named when CSV is exported
dimnames(viirs.af) <- list(c("Statewide", "Federal"),c(),1:52,2012:2020)

# Clean Up
viirs.af[is.na(viirs.af)] <- 0
viirs.af[,,(viirsAF.endweek+1):52,9] <- NA

#####################################################################################################################################################

################
# SERVER LOGIC #
################

shinyServer(function(input, output) {
 # MCD64A1 SECTION
 # CREATE FUNCTION FOR USER TO SELECT A STATE FROM DROP-DOWN LIST
 output$MCD64A1_Land_Type <- renderUI({
   selectInput("MCD64A1_SelectedLand","Statewide or Federal Land Only:", LandType)
 })
 output$MCD64A1_Selected_State <- renderUI({
   selectInput("MCD64A1_SelectedState","Select a state:", modis.states)
 })
 
 # CREATE A REACTIVE VARIABLE THAT RESPONDS TO THE USER SELECTION
 ss <- reactive(which(input$MCD64A1_SelectedState==modis.states))
 LT <- reactive(which(input$MCD64A1_SelectedLand==LandType))
 
 # FOR FILE NAMING
 LT_Name <- reactive(if(LT() == 1){
   OWNER = "Statewide"
 } else {
   OWNER = "FederalLand"
 })
 
 # CREATE A REACTIVE PLOT THAT RESPONDS TO THE USER SELECTION
 # IMBED THE REACTIVE VARIABLEs 'SS()' AND 'LT()' INTO THE PLOT CODE
 
 MCD64A1_TimeSeries_Plot <- function(){
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
 
 MCD64A1_Cumulative_TimeSeries_Plot  <- function(){
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
 output$MCD64A1_TimeSeries <- renderPlot({print(MCD64A1_TimeSeries_Plot())})
 
 # REPEAT THE PROCESS FOR CUMULATIVE PLOTS
 output$MCD64A1_Cumulative_TimeSeries <- renderPlot({print(MCD64A1_Cumulative_TimeSeries_Plot())})
 
# THE SECTION OF CODE BELOW GIVES THE OPTION TO DOWNLOAD THE PLOT AS A PNG
 output$download_MCD64A1_TimeSeries_Plot <- downloadHandler(
    filename = function() {paste("MCD64A1_TimeSeriesPlot_",input$MCD64A1_SelectedState,"_",LT_Name(),".png")},
    content = function(file){
      png(file)
      MCD64A1_TimeSeries_Plot()
      dev.off()
  })
 
 output$download_MCD64A1_Cumulative_TimeSeries_Plot <- downloadHandler(
   filename = function() {paste("MCD64A1_Cumulative_TimeSeriesPlot_",input$MCD64A1_SelectedState,"_",LT_Name(),".png")},
   content = function(file){
     png(file)
     MCD64A1_Cumulative_TimeSeries_Plot()
     dev.off()
   })

# THIS SECTION OF CODE BELOW CAN BE USED TO SAVE TABLES AS A CSV
output$MCD64A1_dto <- downloadHandler(
  filename = function(){paste("MCD64A1_TimeSeries_",input$MCD64A1_SelectedState,"_",LT_Name(),".csv")},
  content = function(file)
    write.csv(modis.ba[LT(),ss(),,], file)
)

#####################################################################################################################################################
 
# MODIS ACTIVE FIRE SECTION 
# CREATE FUNCTION FOR USER TO SELECT A STATE FROM DROP-DOWN LIST
 output$ActiveFire_ByState_Plot <- renderUI({
   selectInput("SelectedState2","Select a state:", StateNames)
 })
 output$ActiveFire_Land_Type <- renderUI({
   selectInput("SelectedLand2","Statewide or Federal Land Only:", LandType)
 })

# CREATE REACTIVE VARIABLES THAT RESPONDS TO THE USER SELECTION
 ss2 <- reactive(which(input$SelectedState2==StateNames))
 LT2 <- reactive(which(input$SelectedLand2==LandType))
 
 # FOR FILE NAMING
 LT2_Name <- reactive(if(LT2() == 1){
    OWNER = "Statewide"
 } else {
    OWNER = "FederalLand"
 })
 
# Plot time series of AF
 plotInputAF <- function(){
    modis.af[is.na(modis.af)] <- 0
    modis.af[LT2(),ss2(),(modisAF.endweek+1):52,20] <- NA
    plot(0,0,xlim=c(0,53),ylim=c(1,max(modis.af[LT2(),ss2(),,],na.rm=T)),col="white",xlab="Weeks Since Jan 1", ylab="MODIS (TERRA/AQUA) Weekly Fire Counts (#)",main=paste(StateNames[ss2()]))
    for(i in 1:length(2001:2020)){
       lines(1:52, modis.af[LT2(),ss2(),,i],col=rev(heat.colors(20))[i])
    }
    lines(1:52,rowMeans(modis.af[LT2(), ss2(),,],na.rm=T),lwd=3,col=1)
    lines(1:52,modis.af[LT2(),ss2(),,19],lwd=3,col=rev(heat.colors(20))[20],lty=2)
    lines(1:52,modis.af[LT2(),ss2(),,20],lwd=3,col=3)
    legend(1,max(modis.af[LT2(), ss2(),,],na.rm=T),c(2001:2019),col=rev(heat.colors(19)),lty=c(rep(1,18),2),ncol=3,lwd=2,bty="n",cex=0.7)
    legend(1,max(modis.af[LT2(), ss2(),,],na.rm=T)*.7,c(2020),col=3,lwd=2,bty="n",cex=0.7)
    legend(1,max(modis.af[LT2(), ss2(),,],na.rm=T)*.65,"mean",col=1,lwd=2,bty="n",cex=0.7)
 
 }
 
# Plot cumulative AF
 plotInputAF2 <- function(){
    modis.af[is.na(modis.af)] <- 0
    plot(0,0,xlim=c(0,53),ylim=c(1,max(colSums(modis.af[LT2(), ss2(),,],na.rm=T))),col="white",xlab="Weeks Since Jan 1", ylab="MODIS (TERRA/AQUA) Weekly Fire Counts (#)",main=paste(StateNames[ss2()]))
    for(i in 1:length(2001:2019)){
        lines(1:52, cumsum(modis.af[LT2(), ss2(),,i]),col=rev(heat.colors(20))[i])
    }
    lines(1:52, cumsum(rowMeans(modis.af[LT2(),ss2(),,],na.rm=T)),lwd=3,col=1)
    lines(1:52, cumsum(modis.af[LT2(),ss2(),,19]),lwd=3,col=rev(heat.colors(20))[20],lty=2)
    lines(1:modisAF.endweek, cumsum(modis.af[LT2(),ss2(),1:modisAF.endweek,20]),lwd=3,col=3)
    legend(1,max(colSums(modis.af[LT2(), ss2(),,],na.rm=T)),c(2001:2019),col=rev(heat.colors(19)),lty=c(rep(1,18),2),ncol=3,lwd=2,bty="n",cex=0.7)
    legend(1,max(colSums(modis.af[LT2(), ss2(),,],na.rm=T))*0.7,c(2020),col=3,lwd=2,bty="n",cex=0.7)
    legend(1,max(colSums(modis.af[LT2(), ss2(),,],na.rm=T))*0.65,"mean",col=1,lwd=2,bty="n",cex=0.7)
 }
 
 # RENDER REACTIVE PLOTS
 output$ggplotAllLandAF <- renderPlot({print(plotInputAF())})
 output$ggplotCumSumAF <- renderPlot({print(plotInputAF2())})
 
 # THE SECTION OF CODE BELOW GIVES THE OPTION TO DOWNLOAD THE PLOT AS A PNG
 output$download_ggplotAllLandAF <- downloadHandler(
    filename = function() {paste("MODIS_ActiveFire_TimeSeriesPlot_",input$SelectedState2,"_",LT2_Name(),".png")},
    content = function(file){
       png(file)
       plotInputAF()
       dev.off()
    })
 
 output$download_ggplotCumSumAF <- downloadHandler(
    filename = function() {paste("MODIS_ActiveFire_Cumulative_TimeSeriesPlot_",input$SelectedState2,"_",LT2_Name(),".png")},
    content = function(file){
       png(file)
       plotInputAF2()
       dev.off()
    })

 # THIS SECTION OF CODE BELOW CAN BE USED TO SAVE TABLES AS A CSV
 output$MODIS_AF_dto <- downloadHandler(
   filename = function(){paste("MODIS_ActiveFire_TimeSeries_",input$SelectedState2,"_",LT2_Name(),".csv")},
   content = function(file)
     write.csv(modis.af[LT2(),ss2(),,], file)
 )

#####################################################################################################################################################

# VIIRS ACTIVE FIRE SECTION 
# CREATE FUNCTION FOR USER TO SELECT A STATE FROM DROP-DOWN LIST
output$ViirsActiveFire_ByState_Plot <- renderUI({
  selectInput("SelectedState3","Select a state:", StateNames)
})
output$ViirsActiveFire_Land_Type <- renderUI({
  selectInput("SelectedLand3","Statewide or Federal Land Only:", LandType)
})

# CREATE A REACTIVE VARIABLE THAT RESPONDS TO THE USER SELECTION

ss3 <- reactive(which(input$SelectedState3==StateNames))
LT3 <- reactive(which(input$SelectedLand3==LandType))

# FOR FILE NAMING
LT3_Name <- reactive(if(LT3() == 1){
  OWNER = "Statewide"
} else {
  OWNER = "FederalLand"
})

# Plot time series of AF
plotInputViirsAF <- function(){
  viirs.af[is.na(viirs.af)] <- 0
  viirs.af[LT3(),ss3(),(viirsAF.endweek+1):52,9] <- NA
  plot(0,0,xlim=c(0,53),ylim=c(1,max(viirs.af[LT3(),ss3(),,],na.rm=T)),col="white",xlab="Weeks Since Jan 1", ylab="VIIRS (SUOMI-NPP) Weekly Fire Counts (#)",main=paste(StateNames[ss3()]))
  for(i in 1:length(2012:2019)){
    lines(1:52, viirs.af[LT3(),ss3(),,i],col=rev(heat.colors(8))[i])
  }
  lines(1:52,rowMeans(viirs.af[LT3(), ss3(),,],na.rm=T),lwd=3,col=1)
  lines(1:52,viirs.af[LT3(),ss3(),,8],lwd=3,col=rev(heat.colors(20))[20],lty=2)
  lines(1:52,viirs.af[LT3(),ss3(),,9],lwd=3,col=3)
  legend(1,max(viirs.af[LT3(), ss3(),,],na.rm=T),c(2012:2019),col=rev(heat.colors(8)),lty=c(rep(1,7),2),ncol=3,lwd=2,bty="n",cex=0.8)
  legend(1,max(viirs.af[LT3(), ss3(),,],na.rm=T)*0.87,c(2020),col=3,lwd=2,bty="n",cex=0.8)
  legend(1,max(viirs.af[LT3(), ss3(),,],na.rm=T)*0.83,"mean",col=1,lwd=2,bty="n",cex=0.8)
  
}

# Plot cumulative AF
plotInputViirsAF2 <- function(){
  viirs.af[is.na(viirs.af)] <- 0	#Set NA values to zero for cumsum to work
  plot(0,0,xlim=c(0,53),ylim=c(1,max(colSums(viirs.af[LT3(),ss3(),,],na.rm=T))),col="white",xlab="Weeks Since Jan 1", ylab="VIIRS (SUOMI-NPP) Weekly Fire Counts (#)",main=paste(StateNames[ss3()]))
  for(i in 1:length(2012:2019)){
    lines(1:52, cumsum(viirs.af[LT3(),ss3(),,i]),col=rev(heat.colors(8))[i])
  }
  lines(1:52, cumsum(rowMeans(viirs.af[LT3(),ss3(),,1:8],na.rm=T)),lwd=3,col=1)
  lines(1:52, cumsum(viirs.af[LT3(),ss3(),,8]),lwd=3,col=rev(heat.colors(20))[20],lty=2)
  lines(1:viirsAF.endweek, cumsum(viirs.af[LT3(),ss3(),1:viirsAF.endweek,9]),lwd=3,col=3)
  legend(1,max(colSums(viirs.af[LT3(),ss3(),,],na.rm=T)),c(2012:2019),col=rev(heat.colors(8)),lty=c(rep(1,7),2),ncol=3,lwd=2,bty="n",cex=0.8)
  legend(1,max(colSums(viirs.af[LT3(),ss3(),,],na.rm=T))*.87,c(2020),col=3,lwd=2,bty="n",cex=0.8)
  legend(1,max(colSums(viirs.af[LT3(),ss3(),,],na.rm=T))*.83,"mean",col=1,lwd=2,bty="n",cex=0.8)
}

# RENDER REACTIVE PLOTS
output$ggplotAllLandViirsAF <- renderPlot({print(plotInputViirsAF())})
output$ggplotCumSumViirsAF <- renderPlot({print(plotInputViirsAF2())})

# THE SECTION OF CODE BELOW GIVES THE OPTION TO DOWNLOAD THE PLOT AS A PNG
output$download_ggplotAllLandViirsAF <- downloadHandler(
  filename = function() {paste("VIIRS_ActiveFire_TimeSeriesPlot_",input$SelectedState3,"_",LT3_Name(),".png")},
  content = function(file){
    png(file)
    plotInputViirsAF()
    dev.off()
  })

output$download_ggplotCumSumViirsAF <- downloadHandler(
  filename = function() {paste("VIIRS_ActiveFire_Cumulative_TimeSeriesPlot_",input$SelectedState3,"_",LT3_Name(),".png")},
  content = function(file){
    png(file)
    plotInputViirsAF2()
    dev.off()
  })

# THIS SECTION OF CODE BELOW CAN BE USED TO SAVE TABLES AS A CSV
output$VIIRS_AF_dto <- downloadHandler(
  filename = function(){paste("VIIRS_ActiveFire_TimeSeries_",input$SelectedState3,"_",LT3_Name(),".csv")},
  content = function(file)
    write.csv(viirs.af[LT3(),ss3(),,], file)
)

})
