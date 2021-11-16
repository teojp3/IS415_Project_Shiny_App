# Module UI function
shapeFileUI <- function(id) {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)
  
  tagList(
    fileInput(ns(id),
              label = "Choose shapefile:",
              multiple = TRUE,
              accept = c('.shp','.dbf','.sbn','.sbx','.shx','.prj')),
  )
}

# Module server function
shapeFileServer <- function(id) {
  
  moduleServer(id, function(input, output, session){
    
  imported_map <- observe({
    
    req(input$ns)
    
    if (!is.null(input$ns)){
    # shpdf is a data.frame with the name, size, type and datapath of the uploaded files
    shpdf <- input$ns
    
    # Name of the temporary directory where files are uploaded
    tempdirname <- dirname(shpdf$datapath[1])

    # Rename files
    for (i in 1:nrow(shpdf)) {
      file.rename(
        shpdf$datapath[i],
        paste0(tempdirname, "/", shpdf$name[i])
      )
    }

    # Now we read the shapefile with readOGR() of rgdal package
    # passing the name of the file with .shp extension.

    # We use the function grep() to search the pattern "*.shp$"
    # within each element of the character vector shpdf$name.
    # grep(pattern="*.shp$", shpdf$name)
    # ($ at the end denote files that finish with .shp,
    # not only that contain .shp)
    imported_map <- readOGR(paste(tempdirname,
                                  shpdf$name[grep(pattern = "*.shp$", shpdf$name)],
                                  sep = "/"
                                  ))
    
    ### METHOD 2
    
    # prevWD <- getwd()
    # uploadDirectory <- dirname(shpdf$datapath[1])
    # setwd(uploadDirectory)
    # 
    # for (i in 1:nrow(shpdf)){
    #   file.rename(shpdf$datapath[i], shpdf$name[i])
    # }
    # shpName <- shpDF$name[grep(x=shpdf$name, pattern="*.shp")]
    # 
    # shpPath <- paste(uploadDirectory, shpName, sep="/")
    # 
    # imported_map <- readOGR(shpPath)
    
    imported_map <- spTransform(imported_map,CRS("+init=epsg:3414"))
    
    return(imported_map)
    } else {
    return()
  }
    })

  }
  )
}

