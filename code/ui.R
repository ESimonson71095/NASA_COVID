##################################
# Fire Trendr Shiny App          #
# Source: Alessio Benedetti      #
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
  
  # load google analytics script
  #tags$head(includeScript("www/google-analytics-bioNPS.js")),
  tags$head(tags$link(rel = "icon", type = "image/png", href = "fire.png")),
  # remove shiny "red" warning messages on GUI
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),
  
  # load page layout
  dashboardPage(
    
    skin = "green",
      
    dashboardHeader(title="Fire Trendr", titleWidth = 300),
    
    dashboardSidebar(width = 300,
      sidebarMenu(
        HTML(paste0(
          "<br>",
          "<a href='https://firms.modaps.eosdis.nasa.gov/' target='_blank'><img style = 'display: block; margin-left: auto; margin-right: auto;' src='NASA_FIRMS.svg' width = '186'></a>",
          "<br>",
          "<p style = 'text-align: center;'><small><a href='https://firms.modaps.eosdis.nasa.gov/map/' target='_blank'> Link to FIRMS Fire Map</a></small></p>",
          "<br>"
        )),
        menuItem("Home", tabName = "home", icon = icon("home")),
        menuItem("MODIS Active Fire", tabName = "chartsMODIS", icon = icon("stats", lib = "glyphicon")),
        menuItem("VIIRS Active Fire", tabName = "chartsVIIRS", icon = icon("stats", lib = "glyphicon")),
        menuItem("MCD64A1 - Burned Area", tabName = "chartsBA", icon = icon("stats", lib = "glyphicon")),
        menuItem("App Info", tabName = "AppInfo", icon = icon("tasks")),
        HTML(paste0(
          "<br><br><br><br><br><br><br><br><br>",
          "<table style='margin-left:auto; margin-right:auto;'>",
            "<tr>",
              "<td style='padding: 5px;'><a href='https://www.facebook.com/nationalparkservice' target='_blank'><i class='fab fa-facebook-square fa-lg'></i></a></td>",
              "<td style='padding: 5px;'><a href='https://www.youtube.com/nationalparkservice' target='_blank'><i class='fab fa-youtube fa-lg'></i></a></td>",
              "<td style='padding: 5px;'><a href='https://www.twitter.com/natlparkservice' target='_blank'><i class='fab fa-twitter fa-lg'></i></a></td>",
              "<td style='padding: 5px;'><a href='https://www.instagram.com/nationalparkservice' target='_blank'><i class='fab fa-instagram fa-lg'></i></a></td>",
              "<td style='padding: 5px;'><a href='https://www.flickr.com/nationalparkservice' target='_blank'><i class='fab fa-flickr fa-lg'></i></a></td>",
            "</tr>",
          "</table>",
          "<br>"),
        HTML(paste0(
          "<script>",
            "var today = new Date();",
            "var yyyy = today.getFullYear();",
          "</script>",
          "<p style = 'text-align: center;'><small>&copy; - <a href='https://alessiobenedetti.com' target='_blank'>alessiobenedetti.com</a> - <script>document.write(yyyy);</script></small></p>")
        ))
      )
      
    ), # end dashboardSidebar
    
    dashboardBody(
      
      tabItems(
        
        tabItem(tabName = "home",
          
          # home section
          includeMarkdown("www/home.md")
          
        ),
        
        tabItem(tabName = "chartsMODIS",
                
          # ggplot2 species charts section
          includeMarkdown("www/chartsMODIS.md"),
                
          fluidRow(column(6, uiOutput("ActiveFire_Land_Type"))),
          
          fluidRow(column(6, uiOutput("ActiveFire_ByState_Plot"))),
          
          fluidRow(column(6, downloadButton('MODIS_AF_dto',"Download time series table as csv"))),
          
          fluidRow(column(6, plotOutput("ggplotAllLandAF")), column(6, plotOutput("ggplotCumSumAF"))),
                
          fluidRow(column(6, downloadButton('download_ggplotAllLandAF', "Download time series chart")), column(6, downloadButton('download_ggplotCumSumAF', "Download the cumulative time series chart"))),     
        ),
        
        tabItem(tabName = "chartsVIIRS", includeMarkdown("www/chartsVIIRS.md"),
          
          fluidRow(column(6, uiOutput("ViirsActiveFire_Land_Type"))),
                
          fluidRow(column(6, uiOutput("ViirsActiveFire_ByState_Plot"))),
          
          fluidRow(column(6, downloadButton('VIIRS_AF_dto',"Download time series table as csv"))),
                
          fluidRow(column(6, plotOutput("ggplotAllLandViirsAF")), column(6, plotOutput("ggplotCumSumViirsAF"))),
                
          fluidRow(column(6, downloadButton('download_ggplotAllLandViirsAF', "Download time series chart")), column(6, downloadButton('download_ggplotCumSumViirsAF', "Download the cumulative time series chart"))),     
                
        ),
        
        tabItem(tabName = "chartsBA",
                
                # ggplot2 species charts section
                includeMarkdown("www/chartsBA.md"),
                
                fluidRow(column(6, uiOutput("MCD64A1_Land_Type"))),
                
                fluidRow(column(6, uiOutput("MCD64A1_Selected_State"))),
                
                fluidRow(column(6, downloadButton('MCD64A1_dto',"Download time series table as csv"))), 
                
                fluidRow(column(6, plotOutput("MCD64A1_TimeSeries")), column(6, plotOutput("MCD64A1_Cumulative_TimeSeries"))),
                
                fluidRow(column(6,), column(6,)),
                
                fluidRow(column(6, downloadButton('download_MCD64A1_TimeSeries_Plot', "Download time series chart")), column(6, downloadButton('download_MCD64A1_Cumulative_TimeSeries_Plot', "Download the cumulative time series chart"))),
                
                fluidRow(column(6, tableOutput('dto'))),
        ),
        
        tabItem(tabName = "AppInfo", includeMarkdown("www/releases.md"))
              
      )
    
    ) # end dashboardBody
  
  )# end dashboardPage

))

