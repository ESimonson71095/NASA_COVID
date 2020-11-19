##################################
# Fire Trendr Shiny App          #
# Source: Eli Simonson           #
# ui.R file                      #
##################################

library(leaflet)
library(shinydashboard)
library(collapsibleTree)
library(shinycssloaders)
library(DT)
library(tigris)

###########
# LOAD UI #
###########

shinyUI(fluidPage(
  
  # load custom stylesheet
  includeCSS("www/style.css"),
  
  tags$head(tags$link(rel = "icon", type = "image/png", href = "fire.png")),
  # remove shiny "red" warning messages on GUI
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),
  
  # load page layout
  dashboardPage(
    
    skin = "green",
      
    dashboardHeader(title="Fire TrendR", titleWidth = 300),
    
    dashboardSidebar(width = 300,
      sidebarMenu(
        HTML(paste0(
          "<br>",
          "<a href='https://www.talltimbers.org/' target='_blank'><img style = 'display: block; margin-left: auto; margin-right: auto;' src='TT2.png' width = '186'></a>",
          "<br>",
          # "<p style = 'text-align: center;'><small><a href='https://firms.modaps.eosdis.nasa.gov/map/' target='_blank'> Link to FIRMS Fire Map</a></small></p>",
          "<br>"
        )),
        menuItem("Home", tabName = "home", icon = icon("home")),
        menuItem("VIIRS Satellite - Active Fire", tabName = "chartsVIIRS", icon = icon("stats", lib = "glyphicon")),
        menuItem("MODIS Satellite - Active Fire", tabName = "chartsMODIS", icon = icon("stats", lib = "glyphicon")),
        menuItem("MODIS Satellite - Burned Area", tabName = "chartsBA", icon = icon("stats", lib = "glyphicon")),
        menuItem("App Info", tabName = "AppInfo", icon = icon("tasks"))
      )
      
    ), # end dashboardSidebar
    
    dashboardBody(
      tags$head(tags$style(HTML('
              /* logo */
        .skin-green .main-header .logo {
                              background-color: #42683c;
                              }
        /* navbar */
        .skin-green .main-header .navbar {
                              background-color: #42683c;
                              }'
                              ))),
      tabItems(
        
        tabItem(tabName = "home",
          
          # home section
          includeMarkdown("www/home.md")
          
        ),
        
        tabItem(tabName = "chartsVIIRS", includeMarkdown("www/chartsVIIRS.md"),
                
                fluidRow(column(6, uiOutput("ViirsActiveFire_ByState_Plot"))),
                
                fluidRow(column(6, uiOutput("ViirsActiveFire_Land_Type"))),
                
                fluidRow(column(6, downloadButton('VIIRS_AF_dto',"Download time series table as csv"))),
                
                fluidRow(column(6, plotOutput("ggplotAllLandViirsAF")), column(6, plotOutput("ggplotCumSumViirsAF"))),
                
                fluidRow(column(6, downloadButton('download_ggplotAllLandViirsAF', "Download time series chart")), column(6, downloadButton('download_ggplotCumSumViirsAF', "Download the cumulative time series chart"))),     
                
        ),
        
        tabItem(tabName = "chartsMODIS", includeMarkdown("www/chartsMODIS.md"),
          
                fluidRow(column(6, uiOutput("ActiveFire_ByState_Plot"))),
                
                fluidRow(column(6, uiOutput("ActiveFire_Land_Type"))),
          
                fluidRow(column(6, downloadButton('MODIS_AF_dto',"Download time series table as csv"))),
          
                fluidRow(column(6, plotOutput("ggplotAllLandAF")), column(6, plotOutput("ggplotCumSumAF"))),
                
                fluidRow(column(6, downloadButton('download_ggplotAllLandAF', "Download time series chart")), column(6, downloadButton('download_ggplotCumSumAF', "Download the cumulative time series chart"))),     
        ),
        

        tabItem(tabName = "chartsBA", includeMarkdown("www/chartsBA.md"),
                
                fluidRow(column(6, uiOutput("MCD64A1_Selected_State"))),
                
                fluidRow(column(6, uiOutput("MCD64A1_Land_Type"))),
                
                fluidRow(column(6, downloadButton('MCD64A1_dto',"Download time series table as csv"))), 
                
                fluidRow(column(6, plotOutput("MCD64A1_TimeSeries")), column(6, plotOutput("MCD64A1_Cumulative_TimeSeries"))),
                
                fluidRow(column(6,), column(6,)),
                
                fluidRow(column(6, downloadButton('download_MCD64A1_TimeSeries_Plot', "Download time series chart")), column(6, downloadButton('download_MCD64A1_Cumulative_TimeSeries_Plot', "Download the cumulative time series chart"))),
                
        ),
        
        tabItem(tabName = "AppInfo", includeMarkdown("www/releases.md"))
              
      )
    
    ) # end dashboardBody
  
  )# end dashboardPage

))

