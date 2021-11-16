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
                                                            h4("Therefore, our main focus is to develop a web-based geospatial analytical tool dedicated to Spatial Point Analysis, with two methods available for use."),
                                                            h4("Through this geospatial application, we hope to give pointers to and allow users to conduct Spatial Point Analysis for their selected data with ease, regardless of their technical background. 
                                       Hence, the name Spatial Pointers is given for our application.")),
                                                     column(7,align = 'center',
                                                            img(src = 'Fig1.png', height = "50%", width = "50%", 
                                                                style="display: block; margin-left: auto; margin-right: auto;"),
                                                            tags$a(href = "https://gistbok.ucgis.org/bok-topics/point-pattern-analysis", 
                                                                   "Example of using Spatial Point Analysis for analysing existing fire stations in Austin, Texas, USA"
                                                            ))),
                                                   tags$br(),
                                                   
                                                   
                                                   h3(tags$strong("What does Spatial Point Analysis do?")),
                                                   tags$hr(),
                                                   h4("Spatial point analysis methods helps provide insights about where things occur, how the distribution of incidents or the arrangement of
                                          data aligns with other features in the landscape, and what the patterns may reveal about potential connections and correlations."),
                                                   tags$br(),
                                                   
                                                   h3(tags$strong("About our application: Spatial Pointers")),
                                                   tags$hr(),
                                                   h4("Our application will assist users with two methods of Spatial Point Analysis:"), 
                                                   tags$ul(
                                                     tags$li("Spatial Point Patterns Analysis (SPPA)", style = "font-size: 18px; font-weight: 500;"),
                                                     tags$li("Network-Constrained Point Patterns Analysis (NetSPPA)", style = "font-size: 18px; font-weight: 500;")),
                                                   h4("For each analysis, our application is able to provide users with kernel density maps
                                          of the input spatial point datasets and conduct various hypothesis tests to derive statistical conclusions 
                                          on the distributions of datasets."),
                                                   width = 9)
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
                                                        sliderInput(inputId = "NetSPPA_K_No_Simulations",
                                                                    label = "Number of Simulations:",
                                                                    min = 5,
                                                                    max = 99,
                                                                    value = 50),
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
                                                        sliderInput(inputId = "NetSPPA_CrossK_No_Simulations",
                                                                    label = "Number of Simulations:",
                                                                    min = 5,
                                                                    max = 99,
                                                                    value = 50),
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
                                                                     h6(tags$i("Variable: Childcare Centres and Number of Simulations: 50 is used to plot the default map,
                                                                        select alternative choices and click on 'Run Analysis' to update the map.")),
                                                                     plotOutput("NetSPPA_K_Function"),
                                                               tabsetPanel(
                                                                 id = "NetSPPA_K_info",
                                                                 tabPanel("About K-Function",
                                                                          column(12,
                                                                                 h2("What is Ripley's K-Function?"),
                                                                                 h5("Essentially, Ripley's K-function K-function measures the number of events found up to a 
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
                                                                     h6(tags$i("Main Variable: Childcare Centres, Secondary Variable: Bus Stops and Number of Simulations: 50 is used to plot the default map,
                                                                        select alternative choices and click on 'Run Analysis' to update the map.")),
                                                               plotOutput("NetSPPA_Cross_K_Function"),
                                                       tabsetPanel(
                                                         id = "NetSPPA_CrossK_info",
                                                         tabPanel("About Cross K-Function",
                                                                  column(12,
                                                                         h2("What is Ripley's Cross K-Function?"),
                                                                         h5("An extension of Ripley's K-function, the Cross K-function measures the number of main events (A) around
                                                                         a set of secondary events (B), and again the graph helps illustrates the spatial dependence (clustering 
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
                                                             ))
                ))))))



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
  
  output$NetSPPA_Cross_K_Function <- renderPlot({
    
    input$NetSPPA_Run_Cross_Kfunc
    
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
    
    kcross_sec <- reactive({
      if (input$NetSPPA_K_Main == "childcare"){
        dataset <- childcare
      }
      if (input$NetSPPA_CrossK_Secondary == "Bus"){
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
  
}

shinyApp(ui=ui, server=server)




























