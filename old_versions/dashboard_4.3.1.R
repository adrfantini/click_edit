#Adriano Fantini
#afantini@ictp.it
#click_edit
version="4.3.1"
versiontime="9 June 2016"

#### READ ALL OPTIONS CAREFULLY! ####

{##TODO and CHANGELOG
##CHANGELOG
#4.3.0 -> 4.3.1 (9 june 2016)
#Added geThan, similar to leThan
#lldepth is now updated together with rmap$map, so that on crash one can save that in case of error. A message is added to explain that.
#3d view now shows only the area which is currently zoomed in

#4.2.0 -> 4.3.0 (3 june 2016)
#Color bar now updates if content is updated
#New option to choose the color bar type: "cont", "discr" and "quant", and relative select input
#Changed default color scheme to "Spectral"
#If you are not in the edit tab, pressing keyboard buttons will NOT trigger edits
#Added shortcut (L) for locking selection
#Help function
#Added tooltips
#Added informational warnings.
#Fixed crash in mover() function when moving with no point selected
#Added an input for NA color
#Changed default color for NAs to "black"
#minor edits
#Added this changelog!

##TODO
#Remove the possibility to use NA as input, as A il left
#suggest that changing keybindings could be a bad idea
#Fix TODOs in the code
#Add help documentation to know which keyboard buttons do what
#Possibly add info from:
  #input$map_bounds
  #input$map_zoom
#Function to automatically interpolate all grid points inside the input$map_bounds
#Add map layers options, possibly removing the background choice: https://rstudio.github.io/leaflet/showhide.html
#Projected inputs have NOT been tested.

##willfix
#Add addMeasure() !!! http://environmentalinformatics-marburg.github.io/web-presentations/20150723_mapView.html#mapview-leaflet
#Add an option to add more color palettes.
#Undo button
#In multiple selection, you you reclick on a point, it maybe should be deselected, and tooltips should be on hover only
#3d view only for the zoomed area?
#Show selected rectangle tooltip on mouseover, see https://rstudio.github.io/leaflet/shiny.html (input$map_shape_mouseover)


##willNOTfix (probably)
#Important: add the possibility to NOT use projections, see:
  #https://environmentalinformatics-marburg.github.io/web-presentations/20150723_mapView.html#non-projected-data
  #I'm NOT sure it's actually possible
}

{## OPTIONS (READ ME!)

  #Options with a _DEFAULT in the name can be changed during runtime

  ##COMMONLY CHANGED OPTIONS

  #Input options
  inputPath    = "/home/adriano/projects/click_edit/shiny/data/"#Input file path
  inputFile    = "depth.nc" #Input file name
  inputVarName = "auto" #"auto" works if the file only has 1 variable

  #Output options
  outputPath_DEFAULT = "./"  #Output file path
  outputFile_DEFAULT = "outfile" #Output file name. Program writes in .nc and .DAT. Extension will be ignored
  outvarName_DEFAULT = "z" #Output variable name in the netCDF file
  outvarUnit_DEFAULT = "m"  #Output variable unit in the netCDF file
  outvarLongName_DEFAULT = "Bathymetry" #Long output variable name in the netCDF file
  writeOutputDat_DEFAULT= FALSE  #Write output in .DAT ASCII format?
  writeOutputNc_DEFAULT = TRUE  #Write output in .NC ASCII format?

  #Conversion values. Note: if you change these you may want to change the color scale.
  readMultiply   = 1  #Constant to multiply values for when reading the file. DEFAULT=1
  saveMultiply   = 1  #Constant to multiply values for when saving  the file. Applied AFTER readMultiply. DEFAULT=1

  #How to treat values <=leThan
  leThan      = 0  #Threshold value to use leThanValue. Set to NA to ignore. DEFAULT=0
  leThanValue = NA #Value to set to all cells <= leThan. This is done AFTER multiplication by readMultiply. DEFAULT=NA
  #How to treat values >=geThan
  geThan      = NA #Threshold value to use geThanValue. Set to NA to ignore. DEFAULT=NA
  geThanValue = 0  #Value to set to all cells >= geThan. This is done AFTER multiplication by readMultiply. DEFAULT=0

  #At saving time, should NA be set to something? Default NA for .nc, 0 for .dat
  saveNaAs_Nc    = NA #DEFAULT=NA
  saveNaAs_Dat   = 0  #DEFAULT=0

  ##LESS COMMONLY CHANGED OPTIONS

  ##Graphic options
  lit3d  = TRUE      #If the RGL 3d surface image should or shouldn't be lit. DEFAULT=TRUE
  smooth3d = TRUE      #If the RGL 3d surface image should be color smoothed. DEFAULT=TRUE
  opacity_DEFAULT = 0.8 #Default map plot opacity. DEFAULT=0.8
  showExtent = TRUE #Draw a rectangle to show the data extent. DEFAULT=TRUE
  showExtentColor = "red"
  color_DEFAULT = "Spectral" #Default color. Must be one of rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
  naColor_DEFAULT = "black" #Color for NAs. NA = transparent. DEFAULT="black"
  colorbar_DEFAULT = "discr" #Color bar type. Can be "cont", "discr", "quant". DEFAULT="cont"
  colorbar_bins = 10 #Number of bins for the "discr" color bar. DEFAULT=10
  colorbar_quants = 5 #Number of quantile ranges for the "quant" color bar. Too high may lead to crashes. If this is a vector of probabilities (e.g. c(0,0.3,0.7,1.0)), they will be used instead. DEFAULT=5

  ##Edit options
  interpSize_DEFAULT = 3  #Default interpolation size. DEFAULT=3
  interpItself_DEFAULT = FALSE  #Use also the value of the selected point for interpolation, or only neighbours? Default=FALSE
  percentDelta_DEFAULT  = 0.1      #Percentage of value to be added/subtracted on +/- operations. DEFAULT=0.1

  sbwidth = 300 #Sidebar width. DEFAULT=300

  showPopupOnFirstClick = FALSE #Wether to show the popup on the first or second click.

  debug = FALSE #For debugging or changing shortcuts. DEFAULT=300

  ##Projections

  #Input projection will be used only if the program cannot autodetect the input projection.
  #Beware! Projected (non-lat-lon) inputs have NOT been tested.
  inputProj="+proj=longlat +datum=WGS84 +no_defs"  #DEFAULT="+proj=longlat +datum=WGS84 +no_defs"
  #Leaflet projection below should not need to be changed
  # "+nadgrids=@null +wktext" should be very important
  leafletProj <- "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs"
  #DEFAULT="+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs"
  projMethod <- "ngb" #Projection method. Can be "bilinear" or "ngb", which is nearest neighbour. I strongly recommend nearest neighbour (ngb) as this does not change the values of the points. DEFAULT="ngb"

  #Select available tiles. They will be loaded only if necessary.
  #See https://leaflet-extras.github.io/leaflet-providers/preview/
  tiles <- c("Hydda.Base",
	   "Hydda.Full",
	   "Esri.OceanBasemap",
	   "Esri.WorldImagery",
	   "Esri.WorldTopoMap",
	   "Esri.DeLorme",
	   "Esri.WorldTerrain",
	   "Esri.WorldPhysical",
	   "Esri.NatGeoWorldMap",
	   "Esri.WorldGreyCanvas",
	   "OpenStreetMap.Mapnik",
	   "OpenStreetMap.BlackAndWhite",
	   "OpenTopoMap",
	   "Thunderforest.Outdoors",
	   "MapQuestOpen.Aerial",
	   "Stamen.Toner",
	   "Stamen.Watercolor",
	   "CartoDB.DarkMatter",
	   "NASAGIBS.ModisTerraTrueColorCR",
	   "NASAGIBS.ModisTerraBands367CR",
	   "NASAGIBS.ViirsEarthAtNight2012",
	   "Thunderforest.SpinalMap",
	   "OpenMapSurder.Roads",
	   "No background"
  )

#Here you can change which key does what
keybindings <- function(key) { #Keybindings. You can modify them, add them, whatever!
  key <- as.character(key)
  switch(key, #http://www.cambiaresearch.com/articles/15/javascript-key-codes
          "65" = {return("left")}, #A
          "83" = {return("down")}, #S
          "68" = {return("right")}, #D
          "87" = {return("up")}, #W
          "69" = {return("edit")}, #E
          "73" = {return("interp")}, #I
          "86" = {return("view3d")}, #V
          "76" = {return("lock")}, #L
          "72" = {return("help")}, #H
          "88" = {return("+")}, #X
          "90" = {return("-")}, #Z
                {return(key)}#Else, pass on the key
        )
}
}## END OPTIONS

{##INIT
suppressWarnings(rm(cell))
check_package <- function(pname) {#Check if a package is installed, else install it (and load it)
  if (!(pname %in% rownames(installed.packages()))) {
    message(paste0("Package ", pname, " not installed. Installing it... Ctrl+C will stop the program. You can always install packages manually with install.packages(). See ?install.packages for help"))
    install.packages(pkgs=pname,repos="http://cran.r-project.org", quiet=TRUE)
  }
  library(pname, character.only=TRUE)
}

check_package("shiny")#For the UI
check_package("shinyjs")#For popup messages
check_package("shinyBS")#For tipify
check_package("leaflet")#For the map
check_package("RColorBrewer")#Map colors
check_package("raster")#Reading and writing files
check_package("shinydashboard")#For the dashboard

#Load input file
inputFile <- paste0(inputPath, inputFile)
if (!file.exists(inputFile)) {stop("Input file does NOT exist!")}
if (inputVarName=="auto" | inputVarName=="automatic") {
  lldepth <- raster(inputFile)
} else {
  lldepth <- raster(inputFile, varname=inputVarName)
}
if (!is.na(readMultiply)) if (readMultiply !=1) lldepth <- lldepth*readMultiply

#Set all cells<=give nalue to a given constant
if (!is.na(leThan)) { #If it's NA, skip
  if (is.na(leThanValue)) {
    lldepth[Which(lldepth<=leThan, cells=T)] <- NA
  } else {
    lldepth[Which(lldepth<=leThan, cells=T)] <- leThanValue
  }
}

#Set all cells>=given value to a given constant
if (!is.na(geThan)) { #If it's NA, skip
  if (is.na(geThanValue)) {
    lldepth[Which(lldepth>=geThan, cells=T)] <- NA
  } else {
    lldepth[Which(lldepth>=geThan, cells=T)] <- geThanValue
  }
}

#Some constants we'll need later
ext <- extent(lldepth)
resol <- res(lldepth)
ncell_depth <- ncell(lldepth)
cell<- NULL

#If no projection is detected, use inputProj
if (is.na(projection(lldepth))) {
  projection(lldepth) <- inputProj
}

#Project for Leaflet
myProjectRasterForLeaflet <- function (x, method) {#Function to project
    raster::projectRaster(x, raster::projectExtent(x, crs = sp::CRS(leafletProj)), method=method)
}
depth <- myProjectRasterForLeaflet(lldepth, projMethod)

#Some constants we'll need later
depthmin <- cellStats(depth, min)
depthmax <- cellStats(depth, max)
dncol <- ncol(depth)
vName <- names(depth)[1]

#Some functions we'll need.
selrange <- function(r, min, max) {  #Very fast way of selecting raster range, even faster than clamp.
#http://stackoverflow.com/questions/34064738/fastest-way-to-select-a-valid-range-for-raster-data
  rr <- r[]
  rr[rr < min | rr > max] <- NA
  r[] <- rr
  r
}

convertMenuItem <- function(mi,tabName) {#Necessary to get info on active menuItem
#See: http://stackoverflow.com/questions/37595124/react-to-menuitem-tab-selection
  mi$children[[1]]$attribs['data-toggle']="tab"
  mi$children[[1]]$attribs['data-value'] = tabName
  if(length(mi$attribs$class)>0 && mi$attribs$class=="treeview"){
    mi$attribs$class=NULL
  }
  mi
}

pan3d <- function(button=2) { #Function to pan the view taken directly from the RGL manual
    start <- list()
    begin <- function(x, y) {
        start$userMatrix <<- par3d("userMatrix")
        start$viewport <<- par3d("viewport")
        start$scale <<- par3d("scale")
        start$projection <<- rgl.projection()
        start$pos <<- rgl.window2user( x/start$viewport[3], 1 - y/start$viewport[4], 0.5,
        projection = start$projection)
    }
    update <- function(x, y) {
        xlat <- (rgl.window2user( x/start$viewport[3], 1 - y/start$viewport[4], 0.5,
        projection = start$projection) - start$pos)*start$scale
        mouseMatrix <- translationMatrix(xlat[1], xlat[2], xlat[3])
        par3d(userMatrix = start$userMatrix %*% t(mouseMatrix) )
    }
    rgl.setMouseCallbacks(button, begin, update)
    cat("Callbacks set on button", button, "of rgl device", rgl.cur(), "\n")
}
}##END INIT

{##UI definition
sidebar <- dashboardSidebar( width=sbwidth,
            sidebarMenu(id="sbmenu",
              convertMenuItem(menuItem("Edit map", tabName = "editmap", icon = icon("map-o"),
                tipify(checkboxInput("multiple", "Lock selection of multiple points", value=FALSE, width="99%"), "Tick this box to lock the point selection. This way you can select multiple points simultaneously. Untick the box to cancel the selection.", placement="bottom", trigger="hover"),
                tipify(div(style="height: 80px;", textInput("newvalue", "New value", value=0, width="99%")), "Insert here the new value wanted for the point(s). 'NA', 'n' or 'N' are fill values.", placement="bottom", trigger="hover"),
                actionButton("newvalueB", "Edit value. Set 'N' or 'n' for non-defined", icon=icon("edit"), width="99%"),
                div(style="height: 5px;",tags$hr()),
                div(style="height: 55px;", tipify(numericInput("interpsize", "Interpolation box size:", value=interpSize_DEFAULT, min = 3, step=2, width="99%"), "Interpolation for each selected point is performed in a square box with all values equally weighted. All selected points are interpolated at the same time, without affecting each other. Fill values (NAs) are removed. The box size must be an odd integer number, and the center point can be included by ticking the box below.", placement="bottom", trigger="hover")),
                div(style="height: 50px;",
                checkboxInput("interpItself_DEFAULT", "Include center point", value=interpItself_DEFAULT)),
                actionButton("interpB", "Interpolate", icon=icon("th"), width="99%"),
                div(style="height: 5px;",tags$hr()),
                div(style="height: 90px;", tipify(numericInput("percent", "Percent change:", value=100*percentDelta_DEFAULT, min = 1, step=1, width="99%"), "Selected points can be changed by this given percentage of their current value.", placement="bottom", trigger="hover")),
                actionButton("plusB", "Add", icon=icon("plus-square"), width="49%"),
                actionButton("minusB", "Subtract", icon=icon("minus-square"), width="49%"),
                div(style="height: 5px;",tags$hr()),
                tipify(actionButton("view3dB", "3D view", width="99%"), "Show a 3d view of the map, using the RGL package. Only the currently shown area is plotted.", placement="bottom", trigger="hover")
              ), "editmap"),
              convertMenuItem(menuItem("Map options", tabName = "options", icon = icon("cogs"),
                selectInput("tiles", "Background map type", tiles),
                selectInput("colors", "Map color scheme", rownames(subset(brewer.pal.info, category %in% c("seq", "div"))), selected=color_DEFAULT),
                tipify(textInput("naColor", "Color for fill values (NAs)", value=naColor_DEFAULT), "You can set any valid R color. See colors(). Anything else will show NAs as transparent.", placement="bottom", trigger="hover"),
                tipify(selectInput("colorbar", "Type of color bar", c("Discrete"="discr", "Continuous"="cont", "Quantiles"="quant"), selected=colorbar_DEFAULT), "The three types of color bar can be fine tuned in the script source code.", placement="bottom", trigger="hover"),
                tipify(sliderInput("opacity", "Opacity", 0, 1,
                  value = opacity_DEFAULT, step = 0.01
                ), "Map tansparency. 0 is completely transparent, 1 is completely opaque.", placement="bottom", trigger="hover"),
                tipify(sliderInput("range", "Range", floor(depthmin), ceiling(depthmax),
                  value = c(floor(depthmin), ceiling(depthmax)), step = 1, post=" meters"), "Out of range values will be shown as fill values (NA).", placement="bottom", trigger="hover")
              ), "options"),
              convertMenuItem(menuItem("Save", icon=icon("save"), tabName = "save",
                tipify(textInput("outputF", "Output file", value=paste0(outputPath_DEFAULT, outputFile_DEFAULT)), "Do not include the output extension. It will be added depending on the output type.", placement="bottom", trigger="hover"),
                checkboxInput("saveextnc", "NetCDF output", value = writeOutputNc_DEFAULT),
                checkboxInput("saveextdat", "Plain text output", value = writeOutputDat_DEFAULT),
                conditionalPanel(condition = "input.saveextnc == true",
                  textInput("outputVname", "NetCDF variable name", value=outvarName_DEFAULT),
                  textInput("outputVLname", "NetCDF long variable name", value=outvarLongName_DEFAULT),
                  textInput("outputVunit", "NetCDF variable unit", value=outvarUnit_DEFAULT)
                ),
                conditionalPanel(condition = "input.saveextnc == true || input.saveextdat == true",
                  actionButton("saveB", "Save", icon=icon("save"), width="99%")
                )
              ), "save")
            )
          )

body <- dashboardBody(useShinyjs(),
          box( #http://stackoverflow.com/questions/31278938/how-can-i-make-my-shiny-leafletoutput-have-height-100-while-inside-a-navbarpa
            div(class="outer",width = NULL, solidHeader = TRUE, tags$style(type = "text/css", paste0(".outer {position: fixed; top: 50px; left: ", sbwidth, "px; right: 0; bottom: 0px; overflow: hidden; padding: 0}")),
            leafletOutput("map", width = "100%", height = "100%")
          )),
          #The following allows for keypresses:
          #http://stackoverflow.com/questions/24973549/r-shiny-key-input-binding
          #You can use also other inputs (e.g. mouse)
          tags$script('
            $(document).on("keydown", function (e) {
              Shiny.onInputChange("kpress", [e.which,e.timeStamp]);
            });
          ')
          #Timestamp is necessary: http://stackoverflow.com/questions/35831811/register-repeated-keyboard-presses-in-shiny
        )

ui <- dashboardPage(
  dashboardHeader(title = paste0("click_edit by Adriano Fantini - version ", version, " (", versiontime, ") - afantini@ictp.it"), titleWidth = "100%",disable=FALSE),
  sidebar,
  body
)
}##END UI definition

{##SERVER
server <- function(input, output, session) {

  rmap <- reactiveValues(map=depth)

  output$map <- renderLeaflet({#Set map and extent
    leaflet()  %>%
      fitBounds(ext[1], ext[3], ext[2], ext[4])
  })

  view_3d <- function() { #Show 3d map of the raster
    check_package("rasterVis")
    check_package("rgl")
    if (debug) message("DEBUG ### Called function view_3d()")
    bs <- isolate(input$map_bounds)
    pext <- extent(c(bs$west, bs$east, bs$south, bs$north))
    rasterVis::plot3D(crop(lldepth, pext)*(-1), col=rev(topo.colors(255)), lit=lit3d, smooth=smooth3d, rev=TRUE)
    par3d(windowRect = c(0,0,500,500))
    pan3d()
    message("Plotted 3D surface.")
  }

  interpolator <- function() { #interpolate the value of the point
    if (debug) message("DEBUG ### Called function interpolator()")
    if (isolate(!exists("cell") | is.null(cell))) return()
    intsize <- isolate(input$interpsize)
    if (intsize %% 2 != 1) {warning("Interpolation box size must be odd. Not interpolating."); return()}
    map <- isolate(rmap$map)
    if (!is.null(lockedCells)) cell <- unique(lockedCells)
    if (debug) message("Interpolator function edits cell(s): ", cell, ", with box size: ", intsize, ". Using center point?", isolate(input$interpItself_DEFAULT))
    dirmat <- matrix(1, intsize, intsize) #setup interpolation matrix
    dirmat[(intsize+1)/2, (intsize+1)/2] <- 0
    newval <- NULL
    for (c in cell) {# Compute interpolation for each selected cell
      newval <- c(newval, mean(map[
                          adjacent(map, c, directions=dirmat, include=isolate(input$interpItself_DEFAULT), pairs=FALSE)
                              ], na.rm=TRUE))
    }
    #Check if you should update the color bar and range slider
    if (maxValue(rmap$map) < max(newval, na.rm=TRUE)) updateSliderInput(session, "range", value=c(floor(input$range[1]), ceiling(max(newval, na.rm=TRUE))), max=ceiling(max(newval, na.rm=TRUE)))
    if (minValue(rmap$map) > min(newval, na.rm=TRUE)) updateSliderInput(session, "range", value=c(floor(min(newval, na.rm=TRUE)), ceiling(input$range[2])), min=floor(min(newval, na.rm=TRUE)))
    #Update point(s)
    rmap$map[cell] <- newval
    lldepth[cell] <<- newval
    replot()
    renewPopup()
  }

  saver <- function() {#Function to save the file
    if (debug) message("DEBUG ### Called saver() function")
    savedepth <- depth
    if (!is.na(saveMultiply)) if (saveMultiply != 1) {
      savedepth[] <- rmap$map[] * saveMultiply
      } else {
      savedepth[] <- rmap$map[]
      }
    outF <- isolate(input$outputF)
    #Reproject to the original proj
    if (debug) message("DEBUG ### Reprojecting to the original grid...")
    savedepth <- projectRaster(from=savedepth, to=lldepth, method=projMethod)
    ## TODO : Problem: if remap back and forth, so what you set as values, get remapped to the original grid and DO CHANGE. You may use method="ngb" to fix this a bit, if you please, but it is not perfect. You should make a list of all changes and apply them to the original file, maybe.
    if (isolate(input$saveextdat)) { #Write dat file?
      if (debug) message("DEBUG ### Writing DAT file...")
      nas <- Which(is.na(savedepth), cells=T)
      if (is.na(saveNaAs_Dat) & length(nas)>0) {
        info("Cannot save NAs as NAs to a text file, they will just show as empty spaces.")
        warning("Cannot save NAs as NAs to a text file, they will just show as empty spaces.")
      } else {
        savedepthDAT <- savedepth
        savedepthDAT[nas] <- saveNaAs_Dat
        check_package("gdata")#For write.fwf
        write.fwf(as.data.frame(values(flip(savedepthDAT, "y"))), file=paste0(outF, ".dat"), colnames=FALSE, sep=" ", nsmall=2)
        rm(savedepthDAT)
      }
    }
    if (isolate(input$saveextnc)) { #Write NetCDF file?
      if (debug) message("DEBUG ### Writing NetCDF file...")
      if (!is.na(saveNaAs_Nc)) savedepth[Which(is.na(savedepth), cells=T)] <- saveNaAs_Nc #Set NAs to a constant
      writeRaster(savedepth, filename=paste0(outF, ".nc"), overwrite=TRUE,
        varname=isolate(as.character(input$outputVname)),
        varunit=isolate(as.character(input$outputVunit)),
        longname=isolate(as.character(input$outputVLname))
        )
    }
    info("Saved!")
    message("Saved!")
  }

  mover <- function(direction) {
    if (debug) message("DEBUG ### Called mover() function on cell", cell, ", with input: ", direction)
    if (isolate(!exists("cell") | is.null(cell))) return()
    if (exists("cell")) {
      switch(direction,
            "left"  = assign("cell", cell-1, envir=.GlobalEnv),
            "down"  = assign("cell", cell+dncol, envir=.GlobalEnv),
            "right" = assign("cell", cell+1, envir=.GlobalEnv),
            "up"    = assign("cell", cell-dncol, envir=.GlobalEnv))
      showpos(incell=cell)
    }
  }

  editor <- function(i) {#Function to edit the value of a point
    if (debug) message("DEBUG ### Called function editor()")
    if (isolate(!exists("cell") | is.null(cell))) return()
    percent <- isolate(input$percent)
    if (!is.null(lockedCells)) cell <- unique(lockedCells)
    curval <- isolate(rmap$map[cell])
    if (debug) message("Editor function edits cell(s): ", cell, ", with input: ", as.character(i))
    switch(as.character(i),
            "+" = {newval <- curval*(100+percent)/100},
            "-" = {newval <- curval*(100-percent)/100},
            "n" = {newval <- NA},
            "N" = {newval <- NA},
            "na"= {newval <- NA},
            "NA"= {newval <- NA},
                  {newval <- as.numeric(i)
                    if (is.na(newval)) {
                      mes <- "Invalid new value input. Input should be numeric; use 'NA', 'n' or 'N' for setting fill values. No points were modified."
                      message(mes); info(mes)
                      return()
                    }
                  })
    #Check if you should update the color bar and range slider
    if (maxValue(rmap$map) < max(newval, na.rm=TRUE)) updateSliderInput(session, "range", value=c(floor(input$range[1]), ceiling(max(newval, na.rm=TRUE))), max=ceiling(max(newval, na.rm=TRUE)))
    if (minValue(rmap$map) > min(newval, na.rm=TRUE)) updateSliderInput(session, "range", value=c(floor(min(newval, na.rm=TRUE)), ceiling(input$range[2])), min=floor(min(newval, na.rm=TRUE)))
    #Update point(s)
    rmap$map[cell] <- newval
    lldepth[cell] <<- newval
    replot()
    renewPopup()
  }

  replot <- reactive({#Replot function
    inputopacity <- input$opacity
    proxy <- leafletProxy("map")
    proxy %>% hideGroup("mapImage")
    proxy %>% removeControl("mapLegend")
    filtdata <- filteredData()
    pal <- colorpal()
    proxy %>%
    removeImage(layerId="mapImage") %>%
    showGroup("mapImage")%>%
    addRasterImage(filtdata, opacity=inputopacity, project=FALSE, colors=pal, group="mapImage", layerId="mapImage") %>%
    addLegend(position = "bottomright", layerId = "mapLegend",
    pal = pal, values = values(filtdata), opacity=1,  title = vName)
    if (showExtent) {
      proxy %>% addRectangles(ext[1], ext[3], ext[2], ext[4], fill=FALSE, color=showExtentColor, opacity=1, layerId="extent_rect", dashArray="5, 10")
    }
  })

  filteredData <- reactive({# Reactive expression for the data subsetted to what the user selected
    selrange(rmap$map, input$range[1], input$range[2])
  })

  colorpal <- reactive({#Color palette
  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
    filtdata <- filteredData()
    naColor <- tolower(input$naColor)
    mapColor <- input$colors
    if (! naColor %in% c("transparent", NA, colors())) naColor<-"transparent"
    switch(input$colorbar,
           "cont" =  colorNumeric(mapColor, c(minValue(filtdata), maxValue(filtdata)), na.color = naColor),
           "discr" =      colorBin(mapColor, c(minValue(filtdata), maxValue(filtdata)), na.color = naColor, bins=colorbar_bins),
           "quant"= {
             if (length(colorbar_quants) != 1 ) {
               colorQuantile(mapColor, values(filtdata), na.color = naColor, probs=colorbar_quants)
             } else {
               colorQuantile(mapColor, values(filtdata), na.color = naColor, n=colorbar_quants)
             }
           },
           {info("Invalid colorbar parameter"); stop("Invalid colorbar parameter")}
           )
  })

  showpos <- function(x=NULL, y=NULL, incell=NULL) {#Show position of clicks
    if (debug) message("DEBUG ### Called showpos() function")
    if (!is.null(incell)) { #If the input is a cell number...
      cell <- incell
      #If you are outside the domain...
      if (cell > ncell_depth | cell < 1) {leafletProxy("map") %>% clearPopups(); return()}
    } else { #If the input are lat-lon coords...
      xy <- SpatialPoints(data.frame(x,y))
      proj4string(xy) <- inputProj
      xy <- as.data.frame(spTransform(xy, leafletProj))
      cell <- cellFromXY(depth, c(xy$x, xy$y))
    }
    #Now we have the cell number, go on
    rc <- rowColFromCell(depth, cell)

    if (debug) {message("DEBUG ### Cell: ", cell)
                message("DEBUG ### row/col:", rc[1], "/", rc[2])}

    #Retransform to cell center lat-lon coords
    xy <- SpatialPoints(xyFromCell(depth, cell))
    proj4string(xy) <- leafletProj
    xy <- as.data.frame(spTransform(xy, inputProj))
    x <- xy$x
    y <- xy$y
    assign("cell", cell, envir=.GlobalEnv)
    assign("rc"  , rc  , envir=.GlobalEnv)
    assign("x"   , x   , envir=.GlobalEnv)
    assign("y"   , y   , envir=.GlobalEnv)
    renewPopup()
  }

  renewPopup <- function() {#Function to plot pupup/rectangles
    if (debug) message("DEBUG ### Called renewPopup() function")
    proxy <- leafletProxy("map")
    if (is.na(cell)) {message("Clicked outside the map"); return()}
    if (x < 0) {xstr <- "E"; xc=-1} else {xstr <- "W"; xc=1}
    if (y < 0) {ystr <- "S"; yc=-1} else {ystr <- "N"; yc=1}
    val = isolate(rmap$map[cell])
    content <- paste0("X=",rc[2],
                      "; Y=",rc[1], tags$br(),
                      "Lon=", round(x*xc, 5), xstr,
                      "; Lat=", round(y*yc, 5), ystr)
    if (!is.na(val)) content <- paste0(content, tags$br(), vName,"=", round(val, 1), "m")
    if (isolate(input$multiple)) {
      #If locked in multiple selection then store cell numbers
      lockedCells <- c(lockedCells, cell)
      assign("lockedCells", lockedCells, envir=.GlobalEnv)
    } else {
      #If not locked, clear rectangles
      proxy %>% clearGroup("selectedCell_rect")
    }
    if (showPopupOnFirstClick) {
      proxy %>% clearPopups() %>% addPopups(x, y, popup = content) %>%
      addRectangles(x-resol[1]/2, y-resol[2]/2, x+resol[1]/2, y+resol[2]/2, fillColor="transparent", color="black", opacity=1, group="selectedCell_rect")
    } else {
      proxy %>%
      addRectangles(x-resol[1]/2, y-resol[2]/2, x+resol[1]/2, y+resol[2]/2, fillColor="transparent", color="black", opacity=1, popup=content, group="selectedCell_rect")
    }
  }

  {##OBSERVERS
  observeEvent(input$multiple, {#Observer to reset the locked cells array on every toggle of the lock button
    if (input$multiple) {
      lockedCells <- cell
    } else {
      lockedCells <- NULL
      leafletProxy("map") %>% clearGroup("selectedCell_rect")
    }
    assign("lockedCells", lockedCells, envir=.GlobalEnv)
  })

  observeEvent(input$newvalueB, {#Observer to call editor() on button
    if (exists("cell")) {
      isolate(editor(isolate(input$newvalue)))
      isolate(replot())
    }
  })

  observeEvent(input$interpB, {#Observer to call interpolator() on button
    if (exists("cell")) {
      isolate(interpolator())
      isolate(replot())
    }
  })

  observeEvent(input$plusB, {#Observer to call editor("+") on button
    if (exists("cell")) {
      isolate(editor("+"))
      isolate(replot())
    }
  })

  observeEvent(input$minusB, {#Observer to call editor("-") on button
    if (exists("cell")) {
      isolate(editor("-"))
      isolate(replot())
    }
  })

  observeEvent(input$view3dB, {#Observer to call view_3d() on button
    isolate(view_3d())
  })

  observeEvent(input$saveB, {#Observer to call saver() on button
    isolate(saver())
  })

  observeEvent(input$map_click, {#Observer to show Popups on click
    click <- input$map_click
    if (!is.null(click)) {
      showpos(x=click$lng, y=click$lat)
    }
  })

  observe({ #Observer to modify the raster it on control edits
    if (debug) message("DEBUG ### Called observer to modify the raster it on control edits")
    replot()
  })

  observeEvent(input$tiles, {#Observer to edit tiles
    selectedTiles <- input$tiles
    proxy <- leafletProxy("map")
    #isolate prevents the observer to rerun this code on raster data or palette changes
    #(That is taken care of in other observers)
    #The best way would be not to replot the rasterImage every time tiles change,
    #but for some reason the raster goes below:
    #http://stackoverflow.com/questions/34159578/raster-image-goes-below-base-layer-while-markers-stay-above-xindex-is-ignored
    proxy %>% hideGroup("mapImage")
    proxy %>% clearTiles()
    if (selectedTiles!="No background") {
    proxy %>%
      addProviderTiles(selectedTiles, providerTileOptions(zIndex=-10, continuousWorld=FALSE), group="base")
    }
    proxy %>% showGroup("mapImage")
  })

  observeEvent(input$kpress[1], {#Observer to react to keyboard presses
    kpress <- input$kpress[1]
    if (debug) message("DEBUG ### Called via keyboard (raw): ", kpress)
    if (is.null(kpress)) return()
    #Do NOT edit points if you are not in the edit tab
    if (isolate(input$sbmenu) != "editmap") return()
    kpress <- keybindings(kpress)
    if (debug) message("DEBUG ### Called via keyboard (translated): ", kpress)
    switch(kpress,#Depending on the input, call the right function
            "edit"     = isolate(editor(isolate(input$newvalue))),
            "left"     = mover(kpress),
            "down"     = mover(kpress),
            "right"    = mover(kpress),
            "up"       = mover(kpress),
            "interp"   = isolate(interpolator()),
            "view3d"   = view_3d(),
            "lock"     = updateCheckboxInput(session, "multiple", value = !(isolate(input$multiple))),
            "help"     = isolate(helper()),
            "+"        = isolate(editor("+")),
            "-"        = isolate(editor("-")),
            "na"       = isolate(editor("na"))
          )
  })
  }

  helper <- function() {
    helpmessage <- "
###   HELP   ###
    Keyboard shortcuts only work if you are in the editing menu!
    The default keyboard buttons are:
    W -> Move selection up
    A -> Move selection left
    S -> Move selection down
    D -> Move selection right
    E -> Edit point(s) with value in the 'New value' box
    I -> Interpolate around selected point(s)
    V -> Show 3D view
    L -> Lock/unlock selection of multiple points
    Z -> Decrease selected point(s) by the given percentage
    X -> Increase selected point(s) by the given percentage
    H -> This help
###   HAVE FUN!   ###"
    info(helpmessage)
    message(helpmessage)
  }
}
}##END server

#Run!
message("

Welcome to click_edit! If the program crashes, your progress is always saved in the 'lldepth' variable.
This way, you can save your progress nonetheless. For example, for saving 'lldepth' to NetCDF, just issue:
writeRaster(lldepth, overwrite=TRUE,
             filename='outfilename.nc',
             varname='outvarname',
             varunit='outvarunit',
             longname='outvarlongname'
           )

")
print(shinyApp(ui, server))
