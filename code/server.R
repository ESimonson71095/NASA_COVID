##################################
# Fire Trendr Shiny App          #
# Source: Alessio Benedetti      #
# server.R file                  #
##################################

library(shiny)
library(tidyverse)
library(lubridate)

#####################################################################################################################################################

#########################
# BENS'S DATA WRANGLING #
#########################

#Specify last month in archive for 2020 (6=June)
modis.endmonth <- 6

#Set up in/out directories
fdirIn <- "C:/Users/esimonson/Desktop/Fire-COVID19/data/MCD64A1/"
fdirOut <- "C:/Users/esimonson/Desktop/Fire-COVID19/figures/MCD64A1/"

#Set up array to organize state BA data
modis.states <- state.name
modis.ba <- array(0,c(2,length(modis.states),12,length(2000:2020)))
modis.years <- 2000:2020
modis.months <- 1:12

for(landowner in 1:2){
  print(landowner)
  #Read in MODIS
  if(landowner == 1){
    ba.modis <- read.table(paste(fdirIn,"MCD64A1_PixelCount_TimeSeries_US_FedLands.csv", sep=""), sep=",",header=T)
    owner <- "FED"
  }
  if(landowner == 2){
    ba.modis <- read.table(paste(fdirIn,"MCD64A1_PixelCount_TimeSeries_US_States.csv", sep=""), sep=",",header=T)
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

#Make plots, keep y-axis same for Fed/All lands
for(landowner in 1:2){
  if(landowner ==1){
    owner <- "FED"
  }
  if(landowner ==2){
    owner <- "ALL"
  }
  #Plot time series
  for(ss in 1:length(modis.states)){
    pdf(paste(fdirOut,"MCD64A1_timeseries_",modis.states[ss],"_",owner,".pdf",sep=""),width=9)
    par(mfrow=c(1,1),mar=c(4,4,4,4),cex=1.3)
    plot(0,0,xlim=c(1,12),ylim=c(1,max(modis.ba[,ss,,],na.rm=T)),col="white",xlab="", ylab="",main=modis.states[ss])
    mtext(expression(paste("MCD64A1 Burned Area (km"^"2",")")),2,cex=1.4,line=2.1)
    mtext("Months Since Jan.",1,cex=1.4,line=2.1)
    for(i in 1:length(2000:2020)){
      lines(1:12, modis.ba[landowner,ss,,i],col=rev(heat.colors(21))[i])
    }
    lines(1:12,rowMeans(modis.ba[landowner,ss,,],na.rm=T),lwd=3,col=1)
    lines(1:12,modis.ba[landowner,ss,,20],lwd=3,col=rev(heat.colors(20))[20],lty=2)
    lines(1:12,modis.ba[landowner,ss,,21],lwd=3,col=3)
    abline(v=3,col=1)
    legend(1,max(modis.ba[,ss,,],na.rm=T),c(2000:2019),col=rev(heat.colors(20)),lty=c(rep(1,19),2),ncol=3,lwd=2,bty="n",cex=0.7)
    legend(1,max(modis.ba[,ss,,],na.rm=T)*.7,c(2020),col=3,lwd=2,bty="n",cex=0.7)
    legend(1,max(modis.ba[,ss,,],na.rm=T)*.65,"mean",col=1,lwd=2,bty="n",cex=0.7)
    box(lwd=2)
    dev.off()
  }
  
  #Plot cumulative BA
  for(ss in 1:length(modis.states)){
    pdf(paste(fdirOut,"MCD64A1_cumsum_",modis.states[ss],"_",owner,".pdf",sep=""),width=9)
    par(mfrow=c(1,1),mar=c(4,4,4,4),cex=1.3)
    ymax <- max(c(colSums(modis.ba[1,ss,,],na.rm=T),colSums(modis.ba[2,ss,,],na.rm=T)))
    plot(0,0,xlim=c(1,12),ylim=c(1,ymax),col="white",xlab="", ylab="",main=modis.states[ss])
    mtext(expression(paste("MCD64A1 Burned Area (km"^"2",")")),2,cex=1.4,line=2.1)
    mtext("Months Since Jan.",1,cex=1.4,line=2.1)
    for(i in 1:length(2000:2020)){
      lines(1:12, cumsum(modis.ba[landowner,ss,,i]),col=rev(heat.colors(21))[i])
    }
    lines(1:12,cumsum(rowMeans(modis.ba[landowner,ss,,],na.rm=T)),lwd=3,col=1)
    lines(1:12,cumsum(modis.ba[landowner,ss,,20]),lwd=3,col=rev(heat.colors(20))[20],lty=2)
    lines(1:12,cumsum(modis.ba[landowner,ss,,21]),lwd=3,col=3)
    abline(v=3,col=1)
    legend(1,ymax,c(2000:2019),col=rev(heat.colors(20)),lty=c(rep(1,19),2),ncol=3,lwd=2,bty="n",cex=0.7)
    legend(1,ymax*.7,c(2020),col=3,lwd=2,bty="n",cex=0.7)
    legend(1,ymax*.65,"mean",col=1,lwd=2,bty="n",cex=0.7)
    box(lwd=2)
    dev.off()
  }
}

#####################################################################################################################################################

########################
# ELI'S DATA WRANGLING #
########################

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
   
   g1 <- ggplot(data = AllLandGgplot1()) + aes(x= same_year(imageId), y=count, color = as.factor(year(imageId))) + geom_line() + labs(title=paste("MCD64A1 Pixel Counts by Month for All Land in ", input$SelectedState), x = paste0("Time Series for ", input$SelectedState), y = "BA Pixel Count") + scale_x_date(date_breaks = "1 month", date_labels="%b") + scale_color_discrete(name="Year")
   g1+theme(legend.title = element_text(colour="black", size=14, face=""))+scale_color_discrete(name="Year")
   print(g1)
   
 })
 
 output$ggplotFedLand <- renderPlot({
   
   g2 <- ggplot(data = FedLandGgplot2()) + aes(x= same_year(imageId), y=count, color = as.factor(year(imageId))) + geom_line() + labs(title=paste("MCD64A1 Pixel Counts by Month for Federal Land in ", input$SelectedState), x = paste0("Time Series for Federal Lands in  ", input$SelectedState), y = "BA Pixel Count in Federal Land") + scale_x_date(date_breaks = "1 month", date_labels="%b")
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