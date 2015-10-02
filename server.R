
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
#library(shinyapps) # for app deployment
library(fBasics) # for the heavyside function

# Open shiny server

shinyServer(function(input, output) {
        
        # reactive to upload file
        # returns uploaded file
        Upload <- reactive ({                 
                # Input file, initially null
                
                inFile <- input$file1
                
                
                if (is.null(inFile))
                        return(NULL)
                
                if(input$filetype == "ownfile") {
                
                upload <- read.table(inFile$datapath, header = TRUE) 
                }
                else if (input$filetype == "iontof") {
                        peaks <- read.table(inFile$datapath, sep="\t", fill=TRUE, header=FALSE, nrows=1, skip=1) # imports masses (second row of df)
                        
                        upload <- read.table(inFile$datapath, 
                                             sep="\t", fill=TRUE, 
                                             header=FALSE, skip=3) %>%
                                rename(peaks)
                        peaks <- NULL # drop colnames
                        upload[,ncol(upload)] <- NULL # drops last column which should be full of NAs
                        
                        colnames(upload) <- paste("X",colnames(upload), sep="")
                        
                        
                        upload <- upload %>% rename(c("XNA" = "time"))

                        
                }
                
               return(upload)
        })
        
        # reactive to upload example data
        # and for users to select dataset
        # returns selected dataset
        pickData <- reactive({
                    
        bl3 <- read.table("./examples/bl3.txt", header=TRUE)
        bl6 <- read.table("./examples/bl6.txt", header=TRUE)
        vamas <- read.table("./examples/vamas.txt", header=TRUE)
        testdelta <- read.table("./examples/testdelta2.txt", header=TRUE, sep=",")
        switch(input$dataset,
               "Example 1: 6% PS/PMMA bilayer" = bl6,
               "Example 2: 3% PS/PMMA bilayer" = bl3,
               "Example 3: VAMAS multilayer" = vamas,
               "Test delta layer" = testdelta,
              "User uploaded file" = Upload()
        )
         })
        
        
        # reactive to melt selected data into "long" format
        GetData <- reactive({
                
                pickdata <- pickData()
              
                
                
                sampledata <- pickdata %>%
               # sampledata <-  read.table("vamas_example.txt", header=TRUE) %>%
                        melt(id.vars = "time",
                             variable.name = "peak",
                             value.name = "intensity")
                return(sampledata)
        })
        
        # reactive to get max time
        # returns max time
        MaxTime <- reactive({
                sampledata <- GetData()
                MaxTime <- max(sampledata$time)
                return(MaxTime)
        })
        
        # reactive to list peaks avaliable to plot
        # returns list of peaks
        peakList <- reactive({
                sampledata <- GetData()
                peakList <- sampledata %>% distinct(peak) # unique(as.character(colnames(peakSelect)))
                peakList$time <- NULL 
                peakList$intensity <- NULL
                peakList$peak <- as.character(peakList$peak)
                peakList <- peakList$peak
                return(peakList)
        })
        
        # reactive to subset data based on selected peak
        # returns subset of data for selected peak
        subData <- reactive({
                sampledata <- GetData()
                selectedPeak <- input$peak
                peakdata <- if (is.null(input$peak)) 
                                (subset(sampledata, peak == "X69"))
                           else(subset(sampledata, peak == input$peak))
                return(peakdata)
        })
        
        # reactive to plot points
        # returns ggplot of points
        output$plotpoints <- renderPlot({


          # 'Charactistic peaks of VAMAS samples include m/z 26, 42, 43.99, 344.2, 563.4, 43, 59.02, 231.2, 277.2, 1175.8'
          
          #635.5
          
          peakdata <- subData()
          
          
          #interface <- cbind(interface$time, log(interface$intensity)) %>% as.data.frame()
          #colnames(interface) <- c("t", "logIntensity")
          
          # Plot sample data
          plotpoints <- ggplot() + 
                  geom_point(data = peakdata,
                             aes(time,intensity), colour = "blue") +
                  geom_line(data = peakdata,
                            aes(time,intensity), colour = "blue") +  
                  #    geom_point(data = subset(peakdata, peak == "X1175.8"),
                  #               aes(time,log(intensity)), colour = "green") +
                  #    geom_line(data = subset(peakdata, peak == "X1175.8"),
                  #              aes(time,log(intensity)), colour = "green") + 
                  theme_classic() + xlim(min(input$zoomrange), max(input$zoomrange))
          if(input$dispLog == TRUE){
                  return(plotpoints+scale_y_log10() )
          } else {
          
          return(plotpoints) }
  })
  
  
  # reactive to subset data to brushed points
  # and fit error function across this region
  # returns NLS fit
  fitData <- reactive({
          
          peakdata <- subData()
          

                  

          brushedinterface <- brushedPoints(peakdata, input$plotpoints_brush, xvar="time", yvar="intensity")
          start <- min(brushedinterface$time) #min(input$range)
          end <- max(brushedinterface$time) #max(input$range)
          
          if (input$fitLog == TRUE){
          interface <- brushedinterface %>% 
                  #subset(peak == "X69") %>%
                  #subset(time<=end & time >= start) %>%
                  select(one_of(c("time", "intensity"))) %>%
                  transmute(t = time, intensity=log10(intensity))
          }
          else {
                  interface <- brushedinterface %>% 
                          #subset(peak == "X69") %>%
                          #subset(time<=end & time >= start) %>%
                          select(one_of(c("time", "intensity"))) %>%
                          transmute(t = time, intensity=(intensity))   
          }

          
          
          # Write the function
          error.func <- function(t, B, sigma, i, A) {
                  0.5 * (B-A) * (1 + erf( (t - i) / (sigma *sqrt(2) ) ) ) + A
                  #erf(x)v
          }
          
          delta.func <- function(t, A, B, sigma, i) {
                  (A-B) * exp(-0.5*(t - i)^2/sigma^2) + B
          }
          
#           heavyside.func <- function(t, A, B, i) {
#                           (((sign(t-i) + 1)/2)*(B-A)+A)
#           }
          
          
          # Generate fitted data
          #t <- seq(start,end, by=0.01)
          #logIntensity <- error.func(seq(start, end, by=0.01), 13, 5, 45, 6)
          
          # Optimize fit
if (input$interfacetype == "interface"){
          nls_fit <-nls(intensity ~ error.func(t, B, sigma, i, A), 
                        data = interface, 
                        start = list(B = input$B, #10, # max intensity 
                                     A = input$A, #3# baseline intensity
                                     sigma = input$sigma, # sigma
                                     #i = 120
                                     i = ((end-start)/2)+start # interface position
                        ), 
                        trace = F)
} else {
        nls_fit <-nls(intensity ~ delta.func(t, B, sigma, i, A), 
                      data = interface, 
                      start = list(B = input$B, #10, # max intensity 
                                   A = input$A, #3# baseline intensity
                                   sigma = input$sigma, # sigma
                                   #i = 120
                                   i = ((end-start)/2)+start # interface position
                      ), 
                      trace = F)
}
          
return(nls_fit)
          # need to add and if else statement depending on if this converged!
  })
  
  # reactive to prodict fitted data across brushed interface region
  # returns data frame of 100 predicted points
  predictData <- reactive({
          peakdata <- subData()
          brushedinterface <- brushedPoints(peakdata, input$plotpoints_brush, xvar="time", yvar="intensity")
          nls_fit <- fitData()
          start <- min(brushedinterface$time) #min(input$range)
          end <- max(brushedinterface$time)
          # predict new data points from fit
          t <- seq(start , end, by=10/(end-start))
          intensity <- as.numeric(predict(nls_fit, list(t = t)))
          
         if (input$fitLog == TRUE){
                  intensity <- 10^(intensity) }
          else {intensity <- intensity}
          
          # Make a data frame
          fit <- cbind(t,intensity) %>% as.data.frame()
          colnames(fit) <- c("t", "intensity")
          return(fit)
          
  })
  
  # output plot of fitted data over points
  output$plotfit <- renderPlot({ 
          peakdata  <- subData()
          fit <- predictData()
          plotpoints <- ggplot() + 
                  geom_point(data = peakdata,
                             aes(time,intensity), colour = "blue") +
                  geom_line(data = peakdata,
                            aes(time,intensity), colour = "blue") +  
                  #    geom_point(data = subset(peakdata, peak == "X1175.8"),
                  #               aes(time,log(intensity)), colour = "green") +
                  #    geom_line(data = subset(peakdata, peak == "X1175.8"),
                  #              aes(time,log(intensity)), colour = "green") + 
                  theme_classic() 
          
          # Overlay fit on 
          plotfit <- plotpoints + geom_line(data=fit, aes(t,intensity), colour="red", size=2) + theme_classic() + xlim(min(input$zoomrange), max(input$zoomrange))#+ scale_x_continuous(breaks=seq(0, 500, by =  25))
          
          if(input$dispLog == TRUE){
                  return(plotfit+scale_y_log10() )
          } else {
                  
                  return(plotfit) }

  })
# summarizes NLS fit
output$nls_summary <- renderPrint({
        summary( fitData() )
})

# summarizes coefficients and interface positions
  output$fitsummary <- renderTable({
          peakdata <- subData()
          brushedinterface <- brushedPoints(peakdata, input$plotpoints_brush, xvar="time", yvar="intensity")
          nls_fit <- fitData()
          start <- min(brushedinterface$time) #min(input$range)
          end <- max(brushedinterface$time)
          coef <- coef(nls_fit) %>% t() %>% as.data.frame() %>%
                  mutate(fwhm = 2*sqrt(2*log(2))*sigma, #2.3548 * z,
                         "16%-84%" = 2 * sigma ,
                         fwtm = 2*sqrt(2*log(10))*sigma) %>%
                  t()
          coef <- cbind(Row.Names = rownames(coef), coef)
          rownames(coef) <- NULL
          colnames(coef) <- c("coef", "val")
          coef <- as.data.frame(coef)
          coef$val <- as.numeric(as.character(coef$val))
          coef <- as.data.frame(coef)
          coef <- mutate(coef, val = round(val, digits = 2))
          coef$coef <- c("Underlayer intensity", "Overlayer intensity", "Sigma", 
                         "Interface position", "FWHM", "16%-84%", "FWTM")
          return(coef)
  })
  
#   output$rangeSelect <- renderUI({
#           #if (is.null(input$file1)) { return() }
#           
#           sliderInput("range", "Search Range",
#                       min = 0, max = 250, #MaxTime(),
#                       value = c(50, 150), step = 0.5)
#           
#   })
  
# renders peak list slector
  output$peakList <- renderUI({ 
          #if (is.null(input$file1)) { return() }
          selectInput("peak",
                      "Select peak",
                      peakList())
          
          
  })
  
# renders zoom range selector
output$zoomrange <- renderUI({ 
        #if (is.null(input$file1)) { return() }
        sliderInput("zoomrange",
                    "Zoom range",
                    min = 0,
                    max = MaxTime(),
                    value = c(0,MaxTime()))
        
        
})

  

}) # closes shinyServer
