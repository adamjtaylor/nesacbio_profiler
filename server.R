
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

# Load required packages
library(shiny) # for interactivity
library(readr) # for data import
library(dplyr) # for data munging
library(plyr) # for data munging
library(tidyr) # for data munging
library(reshape2) # for data munging
library(ggplot2) # for plotting data
library(pracma) # for the error function
library(shinyapps) # for app deployment
library(fBasics) # for the heavyside function

# Open shiny server
shinyServer(function(input, output) {
        
# reactive to upload file
# returns uploaded file
Upload <- reactive ({                 

    # Input file, initially null
    inFile <- input$file1
  
    # If no file uploaded return null
    if (is.null(inFile))
      return(NULL)
    
    # If filetype is generic "ownfile" format just upload the data as is
    if(input$filetype == "ownfile") {
      upload <- read.table(inFile$datapath, header = TRUE)
    }
    
    # If filetype is an export from Ion-ToF profiles 
    # Then we need to do some data munging to get in a workable format
    else if (input$filetype == "iontof") {
            
      # Import peaks from exported data file from 2nd row      
      peaks <- read.table(inFile$datapath,
                          sep="\t",
                          fill=TRUE,
                          header=FALSE,
                          nrows=1,
                          skip=1)
      
      # Upload data from exported file and rename columns with peaks
      upload <- read.table(inFile$datapath,
                           sep="\t", fill=TRUE,
                           header=FALSE,
                           skip=3) %>%
              rename(peaks)
      
      # Drop column names as no longer needed
      peaks <- NULL 
      
      # Drop the last column which should be full of NAs
      upload[,ncol(upload)] <- NULL 
                        
      # Paste "X" before peake mass so that they work (won't accept numbers)
      colnames(upload) <- paste("X",colnames(upload), sep="")
      
      # Rename the first column as time
      upload <- upload %>% rename(c("XNA" = "time"))

  } # close the else if
                
  # return the uploaded file
  return(upload)

}) # close the reactive
        
# reactive to upload example data
# and for users to select dataset
# returns the selected dataset
pickData <- reactive({
  
  # Upload example datasets
  bl3 <- read.table("./examples/bl3.txt", header=TRUE)
  bl6 <- read.table("./examples/bl6.txt", header=TRUE)
  vamas <- read.table("./examples/vamas.txt", header=TRUE)
  testdelta <- read.table("./examples/testdelta2.txt", header=TRUE, sep=",")
  
  # Switch between datasets as user selects them
  switch(input$dataset,
         "Example 1: 6% PS/PMMA bilayer" = bl6,
         "Example 2: 3% PS/PMMA bilayer" = bl3,
         "Example 3: VAMAS multilayer" = vamas,
         "Test delta layer" = testdelta,
         "User uploaded file" = Upload()
         )

}) # closes the reactive
        
        
# reactive to melt selected data into "long" format
GetData <- reactive({
        
  # Load the selected dataset        
  pickdata <- pickData()
  
  # Melt the dataset into columns of time, peak and intensity
  sampledata <- pickdata %>%
                melt(id.vars = "time",
                     variable.name = "peak",
                     value.name = "intensity")
  
  return(sampledata) # Return the melted dataset

}) # close the reactive
        
# reactive to get the maximum time / length of sputtering
# returns maximum value in time column
MaxTime <- reactive({
  sampledata <- GetData()
  MaxTime <- max(sampledata$time)
  return(MaxTime)
}) # close the reactive
        
# reactive to list peaks avaliable to plot
# returns list of peaks
peakList <- reactive({
  sampledata <- GetData()
  peakList <- sampledata %>% distinct(peak)
  peakList$time <- NULL 
  peakList$intensity <- NULL
  peakList$peak <- as.character(peakList$peak)
  peakList <- peakList$peak
  return(peakList)
}) # close the reactive
        
# reactive to subset data based on selected peak
# returns subset of data for selected peak
subData <- reactive({
  sampledata <- GetData()
  selectedPeak <- input$peak
  peakdata <- if (is.null(input$peak))
                 (subset(sampledata, peak == "X69"))
              else(subset(sampledata, peak == input$peak))
  return(peakdata)
}) # close the reactive
        
# renderPlot to plot points
# returns ggplot of points
output$plotpoints <- renderPlot({
  
  # Load the subset data
  peakdata <- subData()

  # Plot sample data
  plotpoints <- ggplot() + 
                geom_point(data = peakdata,
                           aes(time,intensity), 
                           colour = "blue") +
                geom_line(data = peakdata,
                          aes(time,intensity), 
                          colour = "blue") +
                theme_classic() + 
                xlim(min(input$zoomrange), max(input$zoomrange))
  
  # Scales intensity by log10 if selected by user
  if(input$dispLog == TRUE){
    return(plotpoints+scale_y_log10() )
  } 
  
  # Otherwise returns unscaled plot
  else {
    return(plotpoints)
  }
 
}) # close the renderPlot
  
  
# reactive to subset data to brushed points
# and fit error function across this region
# returns NLS fit
fitData <- reactive({
  
  # load the subset data        
  peakdata <- subData()
  
  # Get the brushed interface and define the start and end points
  brushedinterface <- brushedPoints(peakdata, 
                                    input$plotpoints_brush, 
                                    xvar="time", 
                                    yvar="intensity")
    
  start <- min(brushedinterface$time) #min(input$range)
  end <- max(brushedinterface$time) #max(input$range)
  
  
  # If log10 fitting is sleected, scale the data
  if (input$fitLog == TRUE){
    interface <- brushedinterface %>% 
      select(one_of(c("time", "intensity"))) %>%
      transmute(t = time, intensity=log10(intensity))
  }
  
  # Otherwise don't scale the data
  else {
    interface <- brushedinterface %>%
      select(one_of(c("time", "intensity"))) %>%
      transmute(t = time, intensity=(intensity))   
  }

  # Define the error function
  error.func <- function(t, B, sigma, i, A) {
    0.5 * (B-A) * (1 + erf( (t - i) / (sigma *sqrt(2) ) ) ) + A
  }
   
  # Write the delta layer function - not complete
  delta.func <- function(t, A, B, sigma, i) {
    (A-B) * exp(-0.5*(t - i)^2/sigma^2) + B
  }

# # Write the heavyside function  - NOT IMPLEMENTED          
# heavyside.func <- function(t, A, B, i) {
#    (((sign(t-i) + 1)/2)*(B-A)+A)
# }
          
  
  # Optimize the fit
  
  # If fitting an interface fit with the error function
  if (input$interfacetype == "interface"){
    nls_fit <-nls(intensity ~ error.func(t, B, sigma, i, A),
                  data = interface,
                  start = list(B = input$B, #10, # max intensity
                               A = input$A, #3# baseline intensity
                               sigma = input$sigma, # sigma
                               i = ((end-start)/2)+start # x position
                               ),
                  trace = F)
  }
  # If fitting delta layer use the delta function
  else {
    nls_fit <-nls(intensity ~ delta.func(t, B, sigma, i, A),
                  data = interface,
                  start = list(B = input$B, #10, # max intensity
                               A = input$A, #3# baseline intensity
                               sigma = input$sigma, # sigma
                               i = ((end-start)/2)+start # x position
                               ),
                  trace = F)
  }
          
  return(nls_fit) # Eeturns fit

}) # close the reactive
  
# reactive to prodict fitted data across brushed interface region
# returns data frame of 100 predicted points
predictData <- reactive({

  # Load the subsetr data
  peakdata <- subData()

  # Get the brushed interface and define the start and end points
  brushedinterface <- brushedPoints(peakdata, 
                                    input$plotpoints_brush, 
                                    xvar="time", 
                                    yvar="intensity")
  
  start <- min(brushedinterface$time)
  end <- max(brushedinterface$time)
  
  # Load the optimized fit
  nls_fit <- fitData()

  # predict new data points from fit
  t <- seq(start , end, by=10/(end-start))
  intensity <- as.numeric(predict(nls_fit, list(t = t)))
          
  # Untransform data if fit was on log10 scaled data
  if (input$fitLog == TRUE){
    intensity <- 10^(intensity) }
  else {intensity <- intensity}
          
  # Put predicted data into a data frame
  fit <- cbind(t,intensity) %>% as.data.frame()
  colnames(fit) <- c("t", "intensity")
  
  return(fit)
          
}) # close the reactive
  
# Output plot of fitted data over points
output$plotfit <- renderPlot({ 

  # Call requied data frame
  peakdata  <- subData()
  fit <- predictData()
  
  # Plot the raw data
  plotpoints <- ggplot() +
                geom_point(data = peakdata,
                           aes(time,intensity), 
                           colour = "blue") +
                geom_line(data = peakdata,
                          aes(time,intensity), 
                          colour = "blue") +
                theme_classic() 
          
  # Overlay fit on raw data
  plotfit <- plotpoints + 
             geom_line(data=fit, 
                       aes(t,intensity), 
                       colour="red", 
                       size=2) + 
             theme_classic() + 
             xlim(min(input$zoomrange), 
                  max(input$zoomrange))
          
  # Log10 scale the data if selected by user
  if(input$dispLog == TRUE){
    return(plotfit+scale_y_log10() )
  }
  
  # Otherwise return the plot as is
  else {
    return(plotfit) 
  }

}) # Close the reactive

# Prepare a summary of the fit
output$nls_summary <- renderPrint({
        summary( fitData() )
}) # close the reactive

# Summarize the coefficients and interface widths
output$fitsummary <- renderTable({

  # Load the data and interface start and finish
  peakdata <- subData()
  
  brushedinterface <- brushedPoints(peakdata, 
                                    input$plotpoints_brush, 
                                    xvar="time", 
                                    yvar="intensity")
  nls_fit <- fitData()
  start <- min(brushedinterface$time) #min(input$range)
  end <- max(brushedinterface$time)
  
  # Extract the coefficients and calculate FWHM and FWTM
  coef <- coef(nls_fit) %>% t() %>% as.data.frame() %>%
          mutate(fwhm = 2*sqrt(2*log(2))*sigma,
                 "16%-84%" = 2 * sigma,
                 fwtm = 2*sqrt(2*log(10))*sigma) %>%
          t()
  
  # Arrange data frame and round didgits
  coef <- cbind(Row.Names = rownames(coef), coef)
  rownames(coef) <- NULL
  colnames(coef) <- c("coef", "val")
  coef <- as.data.frame(coef)
  coef$val <- as.numeric(as.character(coef$val))
  coef <- as.data.frame(coef)
  coef <- mutate(coef, val = round(val, digits = 2))
  coef$coef <- c("Underlayer intensity", 
                 "Overlayer intensity", 
                 "Sigma",
                 "Interface position", 
                 "FWHM", 
                 "16%-84%", 
                 "FWTM")
  
  return(coef)
}) # close the reactive
  
 
# Render the peak selector
output$peakList <- renderUI({ 
  selectInput("peak",
              "Select peak",
              peakList()
              )
}) # close the reactive
  
# renders zoom range selector
output$zoomrange <- renderUI({ 
  sliderInput("zoomrange",
              "Zoom range",
              min = 0,
              max = MaxTime(),
              value = c(0,MaxTime())
              )
}) # close the reactive

}) # closes shinyServer