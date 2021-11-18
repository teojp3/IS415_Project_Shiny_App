library(shiny)
library(leaflet)
library(tidyverse)
library(tools)
library(shinythemes)
library(shinyjs)
library(tmap)
library(readr)
library(sp)
library(sf)
library(rgdal)
library(spNetwork)
library(spatstat)
library(raster)
library(maptools)



# Import Data

## NetSPPA
### Road Network
network <- readOGR(dsn="data/Punggol", 
                   layer="Punggol_St",
                   verbose = FALSE)

network <- spTransform(network,CRS("+init=epsg:3414"))

### Childcare Centres
childcare <- readOGR(dsn="data/Punggol",
                     layer="Punggol_CC",
                     verbose = FALSE)

childcare <-spTransform(childcare, CRS("+init=epsg:3414"))

### Cross K Factors
Bus <- readOGR(dsn="data/BusStop", 
               layer="BusStop_P",
               verbose = FALSE)

Bus <- spTransform(Bus,CRS("+init=epsg:3414"))

MRT <- readOGR(dsn="data/MRT", 
               layer="MRT_P",
               verbose = FALSE)

MRT <- spTransform(MRT,CRS("+init=epsg:3414"))

Schools <- read_csv("data/NetSPPA_aspatial/Schools.csv")

Schools <- st_as_sf(Schools, coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(crs = 3414)

Schools <- as(Schools, "Spatial")

### Remove NA
lixels <- spNetwork::lixelize_lines(network,700,mindist = 350)
samples <- spNetwork::lines_center(lixels)


## SPPA
### SG Outline
sg_sf <- st_read(dsn = "data2/sg", layer="CostalOutline")
sg_sf <- st_set_crs(sg_sf, 3414)

### McDonald
mc <- read_csv("data2/mc_prepared.csv")
mc_sf <- st_as_sf(mc, coords = c("LONGITUDE", "LATITUDE"), crs=4326) %>%
  st_transform(crs = 3414)

### Cross L Factors
kfc <- read_csv("data2/kfc_prepared.csv")
kfc_sf <- st_as_sf(kfc, coords = c("LONGITUDE", "LATITUDE"), crs=4326) %>%
  st_transform(crs = 3414)

mrt <- read_csv("data2/MRT_prepared.csv")
mrt_sf <- st_as_sf(mrt, coords = c("LONGITUDE", "LATITUDE"), crs=4326) %>%
  st_transform(crs = 3414)

gym_sf <- st_read("data2/gym/gyms-sg-geojson.geojson") %>%
  st_transform(crs = 3414)

cc_sf <- st_read("data2/CC/community-clubs-geojson.geojson") %>%
  st_transform(crs = 3414)

### data wrangling
kfc <- as_Spatial(kfc_sf)
mrt <- as_Spatial(mrt_sf)
mc <- as_Spatial(mc_sf)
gym <- as_Spatial(gym_sf)
cc <- as_Spatial(cc_sf)
sg <- as_Spatial(st_zm(sg_sf))
kfc_sp <- as(kfc, "SpatialPoints")
mrt_sp <- as(mrt, "SpatialPoints")
mc_sp <- as(mc, "SpatialPoints")
gym_sp <- as(gym, "SpatialPoints")
cc_sp <- as(cc, "SpatialPoints")
sg_sp <- as(sg, "SpatialPolygons")
kfc_ppp <- as(kfc_sp, "ppp")
mrt_ppp <- as(mrt_sp, "ppp")
mc_ppp <- as(mc_sp, "ppp")
gym_ppp <- as(gym_sp, "ppp")
cc_ppp <- as(cc_sp, "ppp")
kfc_ppp_jit <- rjitter(kfc_ppp, retry=TRUE, nsim=1, drop=TRUE)
mrt_ppp_jit <- rjitter(mrt_ppp, retry=TRUE, nsim=1, drop=TRUE)
mc_ppp_jit <- rjitter(mc_ppp, retry=TRUE, nsim=1, drop=TRUE)
gym_ppp_jit <- rjitter(gym_ppp, retry=TRUE, nsim=1, drop=TRUE)
cc_ppp_jit <- rjitter(cc_ppp, retry=TRUE, nsim=1, drop=TRUE)

sg_owin <- as(sg_sp, "owin")
kfcSG_ppp = kfc_ppp_jit[sg_owin]
mrtSG_ppp = mrt_ppp_jit[sg_owin]
mcSG_ppp = mc_ppp_jit[sg_owin]
gymSG_ppp = gym_ppp_jit[sg_owin]
ccSG_ppp = cc_ppp_jit[sg_owin]
kfcSG_ppp.km <- rescale(kfcSG_ppp, 1000, "km")
mrtSG_ppp.km <- rescale(mrtSG_ppp, 1000, "km")
mcSG_ppp.km <- rescale(mcSG_ppp, 1000, "km")
gymSG_ppp.km <- rescale(gymSG_ppp, 1000, "km")
ccSG_ppp.km <- rescale(ccSG_ppp, 1000, "km")



# UI
ui <- fluidPage(theme = shinytheme("lumen"),
                useShinyjs(),
                # Navigation Bar
                navbarPage(
                  title = div(img(src = 'App_Logo.png', style = "margin-top: 0px; padding-right:6px;padding-bottom: 30px", height = 60)),
                  
                  # Homepage Panel
                  tabPanel("Home Page", fluid = TRUE, icon=icon("info-circle"),
                           sidebarLayout(position = 'right',
                                         sidebarPanel(img(src = 'App_Logo.png', height = "80%", width = "80%", 
                                                          style="display: block; margin-left: auto; margin-right: auto;"),
                                                      # White Space below Logo
                                                      tags$br(),
                                                      tags$br(),
                                                      h4(tags$strong("A Group Project done by:")),
                                                      tags$ul(
                                                        tags$li(tags$a(href = "https://www.linkedin.com/in/jun-peng-teo/", "Teo Jun Peng"), style = "font-size: 18px;"),
                                                        tags$li(tags$a(href = "https://www.linkedin.com/in/yiling-yu/", "Yu Yiling"), style = "font-size: 18px;")
                                                      ),
                                                      tags$br(),
                                                      h4("This project is done for IS415 Geospatial Analytics & Application (SMU-X) 
                                             under the guidance of Professor Kam Tin Seong.", 
                                                         align = "center"),
                                                      img(src = 'SMU_Logo.png', height = "50%", width = "50%", 
                                                          style="display: block; margin-left: auto; margin-right: auto;"),
                                                      width = 3),
                                         
                                         mainPanel(h3(tags$strong("Project Motivation")),
                                                   tags$hr(),
                                                   fluidRow(
                                                     column(5,
                                                            h4("Countless data sources exist in the form of spatial data, with geographic elements such as the shape, size or location of the features. 
                                       Such spatial data could be analysed to generate useful insights or drive insightful decisions such as planning locations of facilities and 
                                       understanding more about Ecology."),
                                                            h4("However, not many people are technically trained to do such spatial analysis. Additionally, the only way for them to improve their breadth 
                                       and depth of knowledge pertaining to this area is limited to online resources. 
                                       Without proper foundation, any analysis done could be highly inaccurate as well."),
                                                            h4("Therefore, our main focus is to develop a web-based geospatial analytical tool dedicated to Point Pattern Analysis, with two methods available for use."),
                                                            h4("Through this geospatial application, we hope to give pointers to and allow users to conduct Point Pattern Analysis for their selected data with ease, regardless of their technical background. 
                                       Hence, the name Spatial Pointers is given for our application.")),
                                                     column(7,align = 'center',
                                                            img(src = 'Fig1.png', height = "50%", width = "50%", 
                                                                style="display: block; margin-left: auto; margin-right: auto;"),
                                                            tags$a(href = "https://gistbok.ucgis.org/bok-topics/point-pattern-analysis", 
                                                                   "Example of using Point Pattern Analysis for analysing existing fire stations in Austin, Texas, USA"
                                                            ))),
                                                   tags$br(),
                                                   
                                                   
                                                   h3(tags$strong("What does Point Pattern Analysis do?")),
                                                   tags$hr(),
                                                   h4("Point Pattern Analysis methods helps provide insights about where things occur, how the distribution of incidents or the arrangement of
                                          data aligns with other features in the landscape, and what the patterns may reveal about potential connections and correlations."),
                                                   tags$br(),
                                                   
                                                   h3(tags$strong("About our application: Spatial Pointers")),
                                                   tags$hr(),
                                                   h4("Our application will assist users with two methods of Point Pattern Analysis:"), 
                                                   tags$ul(
                                                     tags$li("Spatial Point Patterns Analysis (SPPA)", style = "font-size: 18px; font-weight: 500;"),
                                                     tags$li("Network-Constrained Point Patterns Analysis (NetSPPA)", style = "font-size: 18px; font-weight: 500;")),
                                                   h4("For each analysis, our application is able to provide users with kernel density maps
                                          of the input spatial point datasets and conduct various hypothesis tests to derive statistical conclusions 
                                          on the distributions of datasets."),
                                                   width = 9)
                           )),
                  
                  # SPPA Panel
                  tabPanel("SPPA", fluid = TRUE, icon=icon("map"),
                           sidebarLayout(position = 'right',
                                         sidebarPanel(fluid = TRUE, width = 3,
                                                      
                                                      
                                                      # If Kernel Density Estimation tabPanel is clicked, the sidebarpanel below will be shown
                                                      conditionalPanel(
                                                        'input.SPPA_var === "SPPA Kernel Density Estimation"',
                                                        tags$strong("SPPA Kernel Density Estimation Variable Inputs"),
                                                        #helpText("Click the Kernel Density Estimation Tab to see the changes)
                                                        selectInput(inputId = "SPPA_main_var",
                                                                    label = "Choose a variable to compute Kernel Density Estimation:",
                                                                    choices = c("McDonald's" = "mcSG_ppp",
                                                                                "KFC" = "kfcSG_ppp",
                                                                                "MRT Stations" = "mrtSG_ppp",
                                                                                "Gyms" = "gymSG_ppp",
                                                                                "Community Centers" = "ccSG_ppp",
                                                                                "Uploaded Data" = "upload"),
                                                                    selected = "mcSG_ppp"),
                                                        selectInput(inputId = "SPPA_kernel",
                                                                    label = "Choose the Kernel smoothing method to be used:",
                                                                    choices = c("Gaussian" = "gaussian",
                                                                                "Epanechnikov" = "epanechnikov",
                                                                                "Quartic" = "quartic",
                                                                                "Disc" = "disc"),
                                                                    selected = "gaussian"),
                                                        radioButtons(inputId = "SPPA_bandwidth_method",
                                                                     label = "Select the bandwidth method to be used:",
                                                                     choices = c("Auto" = "auto",
                                                                                 "Fixed" = "fixed", 
                                                                                 "Adaptive" = "adaptive"),
                                                                     selected = "auto"),
                                                        conditionalPanel(
                                                          condition = "input.SPPA_bandwidth_method == 'auto'",
                                                          selectInput(inputId = "SPPA_bw_auto_var",
                                                                      label = "Select the automatic bandwidth method to be used:",
                                                                      choices = c("bw.diggle()" = "bw.diggle",
                                                                                  "bw.CvL()" =  "bw.CvL", 
                                                                                  "bw.scott()" = "bw.scott",
                                                                                  "bw.ppl()" = "bw.ppl"),
                                                                      selected = "bw.diggle"),
                                                        ),
                                                        conditionalPanel(
                                                          condition = "input.SPPA_bandwidth_method == 'fixed'",
                                                          sliderInput(inputId = "SPPA_bw_fix_var",
                                                                      label = "Select the fixed bandwidth to be used: (in km)",
                                                                      min = 0,
                                                                      max = 5,
                                                                      step = 0.1,
                                                                      value = 1),
                                                        ),
                                                        actionButton("SPPA_Run_KDE", "Run Analysis")
                                                      ),
                                                      
                                                      # If G-Function tabPanel is clicked, the sidebarpanel below will be shown
                                                      conditionalPanel(
                                                        'input.SPPA_var === "SPPA G-Function"',
                                                        tags$strong("SPPA G-Function Variable Inputs"),
                                                        #helpText("Click the G-Function Tab to see the changes)
                                                        selectInput(inputId = "SPPA_G_Main",
                                                                    label = "Choose a variable to compute G-Function:",
                                                                    choices = c("McDonald's" = "mcSG_ppp",
                                                                                "KFC" = "kfcSG_ppp",
                                                                                "MRT Stations" = "mrtSG_ppp",
                                                                                "Gyms" = "gymSG_ppp",
                                                                                "Community Centers" = "ccSG_ppp",
                                                                                "Uploaded Data" = "upload"),
                                                                    selected = "mcSG_ppp"),
                                                        numericInput(inputId = "SPPA_G_No_Simulations",
                                                                     label = "Number of Simulations: (key in a number between 0 to 1000)",
                                                                     min = 0,
                                                                     max = 1000,
                                                                     step = 1,
                                                                     value = 99),
                                                        actionButton("SPPA_Run_Gfunc", "Run Analysis")
                                                      ),
                                                      
                                                      # If F-Function tabPanel is clicked, the sidebarpanel below will be shown
                                                      conditionalPanel(
                                                        'input.SPPA_var === "SPPA F-Function"',
                                                        tags$strong("SPPA F-Function Variable Inputs"),
                                                        #helpText("Click the F-Function Tab to see the changes)
                                                        selectInput(inputId = "SPPA_F_Main",
                                                                    label = "Choose a variable to compute F-Function:",
                                                                    choices = c("McDonald's" = "mcSG_ppp",
                                                                                "KFC" = "kfcSG_ppp",
                                                                                "MRT Stations" = "mrtSG_ppp",
                                                                                "Gyms" = "gymSG_ppp",
                                                                                "Community Centers" = "ccSG_ppp",
                                                                                "Uploaded Data" = "upload"),
                                                                    selected = "mcSG_ppp"),
                                                        numericInput(inputId = "SPPA_F_No_Simulations",
                                                                     label = "Number of Simulations: (key in a number between 0 to 1000)",
                                                                     min = 0,
                                                                     max = 1000,
                                                                     step = 1,
                                                                     value = 99),
                                                        actionButton("SPPA_Run_Ffunc", "Run Analysis")
                                                      ),
                                                      
                                                      # If Cross L-Function tabPanel is clicked, the sidebarpanel below will be shown
                                                      conditionalPanel(
                                                        'input.SPPA_var === "SPPA Cross L-Function"',
                                                        tags$strong("SPPA Cross L-Function Variable Inputs"),
                                                        #helpText("Click the Cross L-Function Tab to see the changes)
                                                        selectInput(inputId = "SPPA_CrossL_Main",
                                                                    label = "Choose the main variable to compute Cross L-Function:",
                                                                    choices = c("McDonald's" = "mcSG_ppp",
                                                                                "KFC" = "kfcSG_ppp",
                                                                                "MRT Stations" = "mrtSG_ppp",
                                                                                "Gyms" = "gymSG_ppp",
                                                                                "Community Centers" = "ccSG_ppp",
                                                                                "Uploaded Data" = "upload"),
                                                                    selected = "mcSG_ppp"),
                                                        selectInput(inputId = "SPPA_CrossL_Secondary",
                                                                    label = "Choose the secondary variable (cannot be the same as main variable) to compute Cross L-Function:",
                                                                    choices = c("McDonald's" = "mcSG_ppp",
                                                                                "KFC" = "kfcSG_ppp",
                                                                                "MRT Stations" = "mrtSG_ppp",
                                                                                "Gyms" = "gymSG_ppp",
                                                                                "Community Centers" = "ccSG_ppp",
                                                                                "Uploaded Data" = "upload"),
                                                                    selected = "kfcSG_ppp"),
                                                        numericInput(inputId = "SPPA_CrossL_No_Simulations",
                                                                     label = "Number of Simulations: (key in a number between 0 to 1000)",
                                                                     min = 0,
                                                                     max = 1000,
                                                                     step = 1,
                                                                     value = 99),
                                                        actionButton("SPPA_Run_Cross_Lfunc", "Run Analysis")
                                                      )                                                      
                                         ),
                                         mainPanel(width = 9,
                                                   tabsetPanel(
                                                     id = "SPPA_var",
                                                     tabPanel("SPPA Kernel Density Estimation",
                                                              column(12,
                                                                     h6(tags$strong("Note:")),
                                                                     h6(tags$i("Please wait a short while for the default map to load.")),
                                                                     h6(tags$i("Variable: McDonald's, Kernel: Gaussian and Bandwidth method: auto-bw.diggle is used to plot the default map,
                                                                        select alternative choices and click on 'Run Analysis' to update the map.")),
                                                                     tmapOutput("SPPA_KDE_Map"),
                                                                     tabsetPanel(
                                                                       id = "SPPA_KDE_info",
                                                                       tabPanel("About Spatial Kernel Density Estimation",
                                                                                column(12,
                                                                                       h2("What is Spatial Kernel Density Estimation?"),
                                                                                       h5("Kernel Density Estimation (KDE) is one of the mostly used density-based measures to estimate local density. 
                                                                                          It creates a grid which each cell is assigned the density value of the kernel window centered on that cell. 
                                                                                          The density value is estimated by counting the number of object/events in that kernel window."),
                                                                                       h3("How to interpret the output?"),
                                                                                       h5("The v in the legend indicate the number of object/events in kernel window centered in each grid.
                                                                                       Essentially, the darker the color of the area, the higher the intensity of points density in that area."),
                                                                                )))
                                                              )),
                                                     
                                                     tabPanel("SPPA G-Function", 
                                                              column(12,
                                                                     h6(tags$strong("Note:")),
                                                                     h6(tags$i("Please wait a short while for the default graph to load.")),
                                                                     h6(tags$i("Variable: McDonald's and Number of Simulations: 99 is used to plot the default map,
                                                                        select alternative choices and click on 'Run Analysis' to update the map.")),
                                                                     plotOutput("SPPA_G_Function"),
                                                                     tabsetPanel(
                                                                       id = "SPPA_G_info",
                                                                       tabPanel("About G-Function",
                                                                                column(12,
                                                                                       h2("What is G-Function?"),
                                                                                       h5("The G-function calculates the cumulative frequency distribution of the nearest neighbor distance of a point pattern."),
                                                                                       h3("How to interpret the graph?"),
                                                                                       h5("If the observed G is above the envelope, then "),
                                                                                       h5(tags$strong("we can reject null hypothesis and conclude the points resemble clustered distribution.")),
                                                                                       h5("If not, if the observed G is below the envelope, then "),
                                                                                       h5(tags$strong("we can reject null hypothesis and conclude the points resemble dispersed distribution.")),
                                                                                       h5("Else, if the observed G is inside the envelope, it means "),
                                                                                       h5(tags$strong("the null hypothesis of CSR cannot be rejected and we conclude the points resemble random distribution."))
                                                                                ))))),
                                                     
                                                     tabPanel("SPPA F-Function", 
                                                              column(12,
                                                                     h6(tags$strong("Note:")),
                                                                     h6(tags$i("Please wait a short while for the default graph to load.")),
                                                                     h6(tags$i("Variable: McDonald's and Number of Simulations: 99 is used to plot the default map,
                                                                        select alternative choices and click on 'Run Analysis' to update the map.")),
                                                                     plotOutput("SPPA_F_Function"),
                                                                     tabsetPanel(
                                                                       id = "SPPA_F_info",
                                                                       tabPanel("About F-Function",
                                                                                column(12,
                                                                                       h2("What is F-Function?"),
                                                                                       h5("The F-function first generates a few random points in the study area, 
                                                                                          and then it determines the minimum distance from each random point in P to any original points in the study area."),
                                                                                       h3("How to interpret the graph?"),
                                                                                       h5("If the observed F is above the envelope, then "),
                                                                                       h5(tags$strong("we can reject null hypothesis and conclude the points resemble dispersed distribution.")),
                                                                                       h5("If not, if the observed F is below the envelope, then "),
                                                                                       h5(tags$strong("we can reject null hypothesis and conclude the points resemble clustered distribution.")),
                                                                                       h5("Else, if the observed F is inside the envelope, it means "),
                                                                                       h5(tags$strong("the null hypothesis of CSR cannot be rejected and we conclude the points resemble random distribution."))
                                                                                ))))),
                                                     
                                                     tabPanel("SPPA Cross L-Function", 
                                                              column(12,
                                                                     h6(tags$strong("Note:")),
                                                                     h6(tags$i("Please wait a short while for the default graph to load.")),
                                                                     h6(tags$i("Main Variable: McDonald's, Secondary Variable: KFC and Number of Simulations: 99 is used to plot the default map,
                                                                        select alternative choices and click on 'Run Analysis' to update the map.")),
                                                                     plotOutput("SPPA_Cross_L_Function"),
                                                                     tabsetPanel(
                                                                       id = "SPPA_CrossL_info",
                                                                       tabPanel("About Cross L-Function",
                                                                                column(12,
                                                                                       h2("What is Cross L-Function?"),
                                                                                       h5("Simply put, Cross L-function measure the number of type A points up to a given distance from a type B point."),
                                                                                       h3("How to interpret the graph?"),
                                                                                       h5("If the observed L is above the envelope, "),
                                                                                       h5(tags$strong("we can reject null hypothesis and conclude two types of points resemble attraction patterns, suggesting clustering.")),
                                                                                       h5("If not, if the observed L is below the enveloped, "),
                                                                                       h5(tags$strong("we can reject null hypothesis and conclude the two types of points resemble repulsion patterns, suggesting dispersion.")),
                                                                                       h5("Else, if the observed L is inside the envelope, it means "),
                                                                                       h5(tags$strong("the null hypothesis of CSR cannot be rejected and we conclude the two types of points resemble random distribution and are independent of each other."))
                                                                                ))))),
                                                   ))
                           )),
                  
                  # NetSPPA Panel
                  tabPanel("NetSPPA", fluid = TRUE, icon=icon("road"),
                           sidebarLayout(position = 'right',
                                         sidebarPanel(fluid = TRUE, width = 3,
                                                      
                                                      
                                                      # If Kernel Density Estimation tabPanel is clicked, the sidebarpanel below will be shown
                                                      conditionalPanel(
                                                        'input.NetSPPA_var === "NetSPPA Kernel Density Estimation"',
                                                        tags$strong("NetSPPA Kernel Density Estimation Variable Inputs"),
                                                        #helpText("Click the Kernel Density Estimation Tab to see the changes)
                                                        selectInput(inputId = "NetSPPA_main_var",
                                                                    label = "Choose a variable to compute Kernel Density Estimation:",
                                                                    choices = c("Childcare Centres" = "childcare",
                                                                                "Bus Stops" = "Bus",
                                                                                "MRT Stations" = "MRT",
                                                                                "Schools" = "Schools",
                                                                                "Uploaded Data" = "upload"),
                                                                    selected = "childcare"),
                                                        selectInput(inputId = "NetSPPA_kernel",
                                                                    label = "Choose the Kernel to be used:",
                                                                    choices = c("Quartic" = "quartic",
                                                                                "Triangle" = "triangle",
                                                                                "Tricube" = "tricube",
                                                                                "Cosine" = "cosine",
                                                                                "Triweight" = "triweight",
                                                                                "Epanechnikov" = "epanechnikov",
                                                                                "Uniform" = "uniform"),
                                                                    selected = "quartic"),
                                                        selectInput(inputId = "NetSPPA_method",
                                                                    label = "Select the Method to be used:",
                                                                    choices = c("Simple" = "simple",
                                                                                "Discontinuous" = "discontinuous", 
                                                                                "Continuous" = "continuous"),
                                                                    selected = "simple"),
                                                        actionButton("NetSPPA_Run_KDE", "Run Analysis")
                                                      ),
                                                      
                                                      # If K-Function tabPanel is clicked, the sidebarpanel below will be shown
                                                      conditionalPanel(
                                                        'input.NetSPPA_var === "NetSPPA K-Function"',
                                                        tags$strong("NetSPPA K-Function Variable Inputs"),
                                                        #helpText("Click the K-Function Tab to see the changes)
                                                        selectInput(inputId = "NetSPPA_K_Main",
                                                                    label = "Choose a variable to compute K-Function:",
                                                                    choices = c("Childcare Centres" = "childcare",
                                                                                "Bus Stops" = "Bus",
                                                                                "MRT Stations" = "MRT",
                                                                                "Schools" = "Schools",
                                                                                "Uploaded Data" = "upload"),
                                                                    selected = "childcare"),
                                                        numericInput(inputId = "NetSPPA_K_No_Simulations",
                                                                     label = "Number of Simulations: (key in a number between 0 to 1000)",
                                                                     min = 0,
                                                                     max = 1000,
                                                                     step = 1,
                                                                     value = 99),
                                                        actionButton("NetSPPA_Run_Kfunc", "Run Analysis")
                                                      ),
                                                      
                                                      # If Cross K-Function tabPanel is clicked, the sidebarpanel below will be shown
                                                      conditionalPanel(
                                                        'input.NetSPPA_var === "NetSPPA Cross K-Function"',
                                                        tags$strong("NetSPPA Cross K-Function Variable Inputs"),
                                                        #helpText("Click the Cross K-Function Tab to see the changes)
                                                        selectInput(inputId = "NetSPPA_CrossK_Main",
                                                                    label = "Choose the main variable to compute Cross K-Function:",
                                                                    choices = c("Childcare Centres" = "childcare",
                                                                                "Bus Stops" = "Bus",
                                                                                "MRT Stations" = "MRT",
                                                                                "Schools" = "Schools",
                                                                                "Uploaded Data" = "upload"),
                                                                    selected = "childcare"),
                                                        selectInput(inputId = "NetSPPA_CrossK_Secondary",
                                                                    label = "Choose the secondary variable (cannot be the same as main variable) to compute Cross K-Function:",
                                                                    choices = c("Childcare Centres" = "childcare",
                                                                                "Bus Stops" = "Bus",
                                                                                "MRT Stations" = "MRT",
                                                                                "Schools" = "Schools",
                                                                                "Uploaded Data" = "upload"),
                                                                    selected = "Bus"),
                                                        numericInput(inputId = "NetSPPA_CrossK_No_Simulations",
                                                                     label = "Number of Simulations: (key in a number between 0 to 1000)",
                                                                     min = 0,
                                                                     max = 1000,
                                                                     step = 1,
                                                                     value = 99),
                                                        actionButton("NetSPPA_Run_Cross_Kfunc", "Run Analysis")
                                                      )                                                      
                                                      
                                         ),
                                         mainPanel(width = 9,
                                                   tabsetPanel(
                                                     id = "NetSPPA_var",
                                                     tabPanel("NetSPPA Kernel Density Estimation",
                                                              column(12,
                                                                     h6(tags$strong("Note:")),
                                                                     h6(tags$i("Please wait a short while for the default map to load.")),
                                                                     h6(tags$i("Variable: Childcare Centres, Kernel: Quartic and Method: Simple is used to plot the default map,
                                                                        select alternative choices and click on 'Run Analysis' to update the map.")),
                                                                     tmapOutput("NetSPPA_KDE_Map"),
                                                             tabsetPanel(
                                                               id = "NetSPPA_KDE_info",
                                                               tabPanel("About Network-Constrained Kernel Density Estimation",
                                                                        column(12,
                                                                               h2("What is Network-Constrained Kernel Density Estimation?"),
                                                                               h5("A classical Kernel Density Estimate (KDE) estimates the continuous density of a set of events in a
                                                                                  two-dimensional space, which is not suitable for analysing density of events occuring on a network.
                                                                                  Therefore, the modified Network-Constrained Kernel Density Estimation is used to calculate density of events
                                                                                  occuring along the edges of a network."),
                                                                               h3("How to interpret the output?"),
                                                                               h5("Essentially, the darker the color of the road, the higher the relative density of the point features as compared 
                                                                                  to road segments with ligher color (meaning lower density)."),
                                                                        )))
                                                              )),
                                                     
                                                     tabPanel("NetSPPA K-Function", 
                                                              column(12,
                                                                     h6(tags$strong("Note:")),
                                                                     h6(tags$i("Please wait a short while for the default graph to load.")),
                                                                     h6(tags$i("Variable: Childcare Centres and Number of Simulations: 99 is used to plot the default map,
                                                                        select alternative choices and click on 'Run Analysis' to update the map.")),
                                                                     plotOutput("NetSPPA_K_Function"),
                                                               tabsetPanel(
                                                                 id = "NetSPPA_K_info",
                                                                 tabPanel("About K-Function",
                                                                          column(12,
                                                                                 h2("What is Ripley's K-Function?"),
                                                                                 h5("Essentially, Ripley's K-function measures the number of events found up to a 
                                                                                 given distance of any particular event, and the graph helps illustrates the spatial dependence (clustering 
                                                                                    or dispersion) of point features (which in this case, is our chosen variable from the side panel) over a wide range of scales."),
                                                                                 h3("How to interpret the graph?"),
                                                                                 h5("If the observed line (black line) is above the envelop, then it means that the estimated K(h) is:"),
                                                                                 h5(tags$strong("statistically significant and the point features shows a Clustering pattern.")),
                                                                                 h5("If not, if the observed line (black line) is below the envelop, then it means that the estimated K(h) is:"),
                                                                                 h5(tags$strong("statistically significant and the point features shows a Regular/Dispersion pattern.")),
                                                                                 h5("Else, if the observed line (black line) is within the envelop, then it means that the estimated K(h) is:"),
                                                                                 h5(tags$strong("not statistically significant and
                                                                                    the point features shows a Complete Spatial Randomness pattern."))
                                                              ))))),
                                                     
                                                     tabPanel("NetSPPA Cross K-Function", 
                                                              column(12,
                                                                     h6(tags$strong("Note:")),
                                                                     h6(tags$i("Please wait a short while for the default graph to load.")),
                                                                     h6(tags$i("Main Variable: Childcare Centres, Secondary Variable: Bus Stops and Number of Simulations: 99 is used to plot the default map,
                                                                        select alternative choices and click on 'Run Analysis' to update the map.")),
                                                               plotOutput("NetSPPA_Cross_K_Function"),
                                                       tabsetPanel(
                                                         id = "NetSPPA_CrossK_info",
                                                         tabPanel("About Cross K-Function",
                                                                  column(12,
                                                                         h2("What is Ripley's Cross K-Function?"),
                                                                         h5("An extension of Ripley's K-function, the Cross K-function measures the number of main point events (A) around
                                                                         a set of secondary point events (B), and again the graph helps illustrates the spatial dependence (clustering 
                                                                         or dispersion) of the point A features around point B features (which in this case, are our chosen variables from the side panel) over a wide range of scales."),
                                                                         h3("How to interpret the graph?"),
                                                                         h5("If the observed line (black line) is above the envelop, then it means that the estimated Cross K(h) is:"),
                                                                         h5(tags$strong("statistically significant and the point A features shows a Clustering pattern around Point B features.")),
                                                                         h5("If not, if the observed line (black line) is below the envelop, then it means that the estimated Cross K(h) is:"),
                                                                         h5(tags$strong("statistically significant and the point A features shows a Regular/Dispersion pattern around Point B features.")),
                                                                         h5("Else, if the observed line (black line) is within the envelop, then it means that the estimated Cross K(h) is:"),
                                                                         h5(tags$strong("not statistically significant and
                                                                      the point features shows a Complete Spatial Randomness pattern."))
                                                                  )))
                                                              )),
                                                     
                                                     
                                                   ))
                           )),
                  
                  # Data Import Panel
                  tabPanel("Data Import", fluid = TRUE, icon=icon("database"),
                           sidebarLayout(position = 'right',
                                         sidebarPanel(fluid = TRUE, width = 3,
                                                      tags$strong("Shapefile Data Import:"),
                                                      tags$br(),
                                                      tags$i("*Multiple files needed; 1) .shp 2) .shx 3) .dbf 4) .prj * "),
                                                      tags$hr(),
                                                      shapeFileUI("Shapefile_Import")
                                                      
                                        ),
                                        mainPanel(width = 9,
                                                  tabsetPanel(
                                                    id = "Data_Import",
                                                    tabPanel("Shapefile Data Import",
                                                             column(12,
                                                                    tmapOutput("map")
                                                             )))
                                                  )
                                        )
                           )
))



server <- function(input, output, session){
  
  # Data Import Map
  output$map <- renderTmap({
    
    input$NetSPPA_Run_KDE
    
    if (is.null(map)) {
      return(NULL)
    }
    
    map <- shapeFileServer("Shapefile_Import")
    
    tmap_mode('view')

    imported_map <- tm_shape(map) +
                      tm_dots(col = 'red')
  })
  
  
  # NetSPPA KDE Map
  output$NetSPPA_KDE_Map <- renderTmap({
    
    input$NetSPPA_Run_KDE
    
    kde_var <- reactive({
      if (input$NetSPPA_main_var == "childcare"){
        dataset <- childcare
      }
      else if (input$NetSPPA_main_var == "Bus"){
        dataset <- Bus
      }
      else if (input$NetSPPA_main_var == "MRT"){
        dataset <- MRT
      }
      else if (input$NetSPPA_main_var == "Schools"){
        dataset <- Schools
      }
      return(dataset)
    })
    
    # isolate() is used to ensure that the code doesnt run unless the Action Button is clicked
    densities <- isolate(spNetwork::nkde(network, 
                                         events = kde_var(), #input$NetSPPA_main_var
                                         w = rep(1,nrow(kde_var())), #input$NetSPPA_main_var
                                         samples = samples,
                                         kernel_name = input$NetSPPA_kernel,
                                         bw = 300, 
                                         div= "bw", 
                                         method = input$NetSPPA_method, 
                                         digits = 1, 
                                         tol = 1,
                                         grid_shape = c(1,1), 
                                         max_depth = 8,
                                         agg = 5, #we aggregate events within a 5m radius (faster calculation)
                                         sparse = TRUE,
                                         verbose = FALSE))
    
    samples$density <- densities
    lixels$density <- densities
    
    # rescaling to help the mapping
    samples$density <- samples$density*1000
    lixels$density <- lixels$density*1000
    tmap_mode('view')
    
    NetSPPA_KDE <- isolate(tm_shape(lixels)+
      tm_lines(col="density")+
      tm_shape(kde_var())+
      tm_dots() +
      tmap_options(basemaps = c("Esri.WorldGrayCanvas","OpenStreetMap", "Stamen.TonerLite"),
                   basemaps.alpha = c(0.8, 0.8, 0.8)) +
      tm_view(set.zoom.limits = c(14,16)))
    
    
  })
  
  # NetSPPA K_Function
  output$NetSPPA_K_Function <- renderPlot({
    
    input$NetSPPA_Run_Kfunc
    
    k_main <- reactive({
      if (input$NetSPPA_K_Main == "childcare"){
        dataset <- childcare
      }
      else if (input$NetSPPA_K_Main == "Bus"){
        dataset <- Bus
      }
      else if (input$NetSPPA_K_Main == "MRT"){
        dataset <- MRT
      }
      else if (input$NetSPPA_K_Main == "Schools"){
        dataset <- Schools
      }
      return(dataset)
    })
    
    k_func <- isolate(kfunctions(network, 
                                         k_main(),
                                         start = 0, 
                                         end = 1000, 
                                         step = 50, 
                                         width = 50, 
                                         nsim = input$NetSPPA_K_No_Simulations, 
                                         resolution = 50,
                                         verbose = FALSE, 
                                         conf_int = 0.05))
    
    k_func$plotk
    
  })
  
  # NetSPPA Cross_K_Function
  output$NetSPPA_Cross_K_Function <- renderPlot({
    
    input$NetSPPA_Run_Cross_Kfunc
    
    k_main <- reactive({
      if (input$NetSPPA_CrossK_Main == "childcare"){
        dataset <- childcare
      }
      else if (input$NetSPPA_CrossK_Main == "Bus"){
        dataset <- Bus
      }
      else if (input$NetSPPA_CrossK_Main == "MRT"){
        dataset <- MRT
      }
      else if (input$NetSPPA_CrossK_Main == "Schools"){
        dataset <- Schools
      }
      return(dataset)
    })
    
    kcross_sec <- reactive({
      if (input$NetSPPA_CrossK_Secondary == "childcare"){
        dataset <- childcare
      }
      else if (input$NetSPPA_CrossK_Secondary == "Bus"){
        dataset <- Bus
      }
      else if (input$NetSPPA_CrossK_Secondary == "MRT"){
        dataset <- MRT
      }
      else if (input$NetSPPA_CrossK_Secondary == "Schools"){
        dataset <- Schools
      }
      return(dataset)
    })
    
    crossk <- isolate(cross_kfunctions(network, 
                                       k_main(),
                                       kcross_sec(),
                                       start = 0, 
                                       end = 1000, 
                                       step = 50, 
                                       width = 50, 
                                       nsim = input$NetSPPA_CrossK_No_Simulations, 
                                       resolution = 50,
                                       verbose = FALSE, 
                                       conf_int = 0.05))
    
    crossk$plotk
    
  })
  
  

  # SPPA KDE Map
  output$SPPA_KDE_Map <- renderTmap({
    
    input$SPPA_Run_KDE

    kde_var <- reactive({
      if (input$SPPA_main_var == "mcSG_ppp"){
        dataset <- mcSG_ppp.km
      }
      else if (input$SPPA_main_var == "kfcSG_ppp"){
        dataset <- kfcSG_ppp.km
      }
      else if (input$SPPA_main_var == "mrtSG_ppp"){
        dataset <- mrtSG_ppp.km
      }
      else if (input$SPPA_main_var == "gymSG_ppp"){
        dataset <- gymSG_ppp.km
      }
      else if (input$SPPA_main_var == "ccSG_ppp"){
        dataset <- ccSG_ppp.km
      }
      return(dataset)
    })

    kde <- reactive({
      if (input$SPPA_bandwidth_method == 'auto'){
        if (input$SPPA_bw_auto_var == 'bw.diggle'){
          the_bw <- bw.diggle(kde_var())
        }
        else if (input$SPPA_bw_auto_var == 'bw.CvL'){
          the_bw <- bw.CvL(kde_var())
        }
        else if (input$SPPA_bw_auto_var == 'bw.scott'){
          the_bw <- bw.scott(kde_var())
        }
        else if (input$SPPA_bw_auto_var == 'bw.ppl'){
          the_bw <- bw.ppl(kde_var())
        }
        kde <- isolate(density(kde_var(),
                               sigma=as.numeric(the_bw),
                               edge=TRUE,
                               kernel=input$SPPA_kernel))
        
      }
      else if (input$SPPA_bandwidth_method == 'fixed'){
        kde <- isolate(density(kde_var(),
                               sigma=input$SPPA_bw_fix_var,
                               edge=TRUE,
                               kernel=input$SPPA_kernel))
      }
      else if (input$SPPA_bandwidth_method == 'adaptive'){
        kde <- isolate(adaptive.density(kde_var(), 
                                        method="kernel"))
      }
      return (kde)
    })
    
    # isolate() is used to ensure that the code doesnt run unless the Action Button is clicked
    gridded_kde <- isolate(as.SpatialGridDataFrame.im(kde()))
    kde_raster <- isolate(raster(gridded_kde))
    projection(kde_raster) <- CRS("+init=EPSG:3414 +datum=WGS84 +units=km")
    
    tmap_mode("view")

    SPPA_KDE <- isolate(tm_shape(sg_sf) +
                          tm_borders(col = 'black',
                                     lwd = 1,
                                     alpha = 0.5) +
                          tm_shape(kde_raster) + 
                          tm_raster("v", alpha = 0.7) +
                          tm_layout(legend.outside = TRUE, frame = FALSE, title = "KDE") +
                          tmap_options(basemaps = c("Esri.WorldGrayCanvas","OpenStreetMap", "Stamen.TonerLite"),
                                       basemaps.alpha = c(0.8, 0.8, 0.8)) +
                          tm_view(set.zoom.limits = c(11,13))) 
  })
  
  # SPPA G_Function
  output$SPPA_G_Function <- renderPlot({
    
    input$SPPA_Run_Gfunc
    
    g_main <- reactive({
      if (input$SPPA_G_Main == "mcSG_ppp"){
        dataset <- mcSG_ppp.km
      }
      else if (input$SPPA_G_Main == "kfcSG_ppp"){
        dataset <- kfcSG_ppp.km
      }
      else if (input$SPPA_G_Main == "mrtSG_ppp"){
        dataset <- mrtSG_ppp.km
      }
      else if (input$SPPA_G_Main == "gymSG_ppp"){
        dataset <- gymSG_ppp.km
      }
      else if (input$SPPA_G_Main == "ccSG_ppp"){
        dataset <- ccSG_ppp.km
      }
      return(dataset)
    })
    
    #g_func <- isolate(Gest(g_main(), 
    #                       correction = "border"))
    #plot(g_func, xlim=c(0,1))
    
    g_func.csr <- isolate(envelope(g_main(), 
                                   Gest, 
                                   nsim = input$SPPA_G_No_Simulations))
    plot(g_func.csr, xlim=c(0,1))
    
  })
  
  # SPPA F_Function
  output$SPPA_F_Function <- renderPlot({
    
    input$SPPA_Run_Ffunc
    
    f_main <- reactive({
      if (input$SPPA_F_Main == "mcSG_ppp"){
        dataset <- mcSG_ppp.km
      }
      else if (input$SPPA_F_Main == "kfcSG_ppp"){
        dataset <- kfcSG_ppp.km
      }
      else if (input$SPPA_F_Main == "mrtSG_ppp"){
        dataset <- mrtSG_ppp.km
      }
      else if (input$SPPA_F_Main == "gymSG_ppp"){
        dataset <- gymSG_ppp.km
      }
      else if (input$SPPA_F_Main == "ccSG_ppp"){
        dataset <- ccSG_ppp.km
      }
      return(dataset)
    })
    
    #f_func <- isolate(Fest(f_main(), 
    #                       correction = "border"))
    #plot(f_func, xlim=c(0,1))
    
    f_func.csr <- isolate(envelope(f_main(), 
                                   Fest, 
                                   nsim = input$SPPA_F_No_Simulations))
    plot(f_func.csr, xlim=c(0,1))
    
  })
  
  # SPPA Cross_L_Function
  output$SPPA_Cross_L_Function <- renderPlot({
    
    input$SPPA_Run_Cross_Lfunc
    
    lcross_main <- reactive({
      if (input$SPPA_CrossL_Main == "mcSG_ppp"){
        dataset <- mcSG_ppp.km
      }
      else if (input$SPPA_CrossL_Main == "kfcSG_ppp"){
        dataset <- kfcSG_ppp.km
      }
      else if (input$SPPA_CrossL_Main == "mrtSG_ppp"){
        dataset <- mrtSG_ppp.km
      }
      else if (input$SPPA_CrossL_Main == "gymSG_ppp"){
        dataset <- gymSG_ppp.km
      }
      else if (input$SPPA_CrossL_Main == "ccSG_ppp"){
        dataset <- ccSG_ppp.km
      }
      return(dataset)
    })
    
    lcross_sec <- reactive({
      if (input$SPPA_CrossL_Secondary == "mcSG_ppp"){
        dataset <- mcSG_ppp.km
      }
      else if (input$SPPA_CrossL_Secondary == "kfcSG_ppp"){
        dataset <- kfcSG_ppp.km
      }
      else if (input$SPPA_CrossL_Secondary == "mrtSG_ppp"){
        dataset <- mrtSG_ppp.km
      }
      else if (input$SPPA_CrossL_Secondary == "gymSG_ppp"){
        dataset <- gymSG_ppp.km
      }
      else if (input$SPPA_CrossL_Secondary == "ccSG_ppp"){
        dataset <- ccSG_ppp.km
      }
      return(dataset)
    })
    
    main_vs_sec<- superimpose('main_var'=lcross_main(), 'sec_var'=lcross_sec())
    
    #crossl <- isolate(Lcross(main_vs_sec, 
    #                         i="main_var", j="sec_var",
    #                         correction='border'))
    #plot(crossl, . -r ~ r, 
    #     xlab = "distance(km)", 
    #     xlim=c(0, 10))
    
    crossl.csr <- isolate(envelope(main_vs_sec, 
                                   Lcross, 
                                   i="main_var", j="sec_var", 
                                   correction='border', 
                                   nsim=input$SPPA_CrossL_No_Simulations))
    
    plot(crossl.csr, . -r ~ r, xlab="distance(km)", xlim=c(0,10))

  })
  
}

shinyApp(ui=ui, server=server)




























