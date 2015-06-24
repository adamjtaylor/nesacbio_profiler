
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

# load required packages
library(shiny) # for interactivity
library(dplyr) # for data munging
library(tidyr) # for data munging
library(ggplot2) # for plotting data
library(reshape2) # for melting data
library(scales) # for something I can't remember
library(plyr) # for data munging
library(pracma) # for error function
library(readr) # for data import
library(shinyapps) # for app deployment



# open shiny server

shinyServer(function(input, output) {
        
        # reactive to upload file
        # returns uploaded file
        Upload <- reactive ({                 
                # Input file, initially null
                
                inFile <- input$file1
                
                
                if (is.null(inFile))
                        return(NULL)
                
                upload <- read.table(inFile$datapath, header = TRUE) 
        })
        
        # reactive to upload example data
        # and for users to select dataset
        # returns selected dataset
        pickData <- reactive({
                    
        bl3 <- read.table("./examples/bl3.txt", header=TRUE)
        bl6 <- read.table("./examples/bl6.txt", header=TRUE)
        vamas <- read.table("./examples/vamas.txt", header=TRUE)
        switch(input$dataset,
               "Example 1: 3% PS/PMMA bilayer" = bl3,
               "Example 2: 6% PS/PMMA bilayer" = bl6,
               "Example 3: VAMAS multilayer" = vamas,
              "User uploaded file" = Upload()
        )
         })
        
        
        # reactive to meld selected data
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
        # MAY NOT BE NEEDED ANY MORE
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
                             aes(time,log10(intensity)), colour = "blue") +
                  geom_line(data = peakdata,
                            aes(time,log10(intensity)), colour = "blue") +  
                  #    geom_point(data = subset(peakdata, peak == "X1175.8"),
                  #               aes(time,log(intensity)), colour = "green") +
                  #    geom_line(data = subset(peakdata, peak == "X1175.8"),
                  #              aes(time,log(intensity)), colour = "green") + 
                  theme_classic() 
          return(plotpoints)
  })
  
  
  # reactive to subset data to brushed points
  # and fit error function across this region
  # returns NLS fit
  fitData <- reactive({
          
          peakdata <- subData()
          

                  

          brushedinterface <- brushedPoints(peakdata, input$plotpoints_brush, xvar="time", yvar="intensity")
          start <- min(brushedinterface$time) #min(input$range)
          end <- max(brushedinterface$time) #max(input$range)
          interface <- brushedinterface %>% 
                  #subset(peak == "X69") %>%
                  #subset(time<=end & time >= start) %>%
                  select(one_of(c("time", "intensity"))) %>%
                  transmute(t = time, logIntensity=log10(intensity))
          
          
          # Write the function
          error.func <- function(t, B, sigma, i, A) {
                  0.5 * (B-A) * (1 + erf( (t - i) / (sigma *sqrt(2) ) ) ) + A
                  #erf(x)v
          }
          
          
          # Generate fitted data
          #t <- seq(start,end, by=0.01)
          #logIntensity <- error.func(seq(start, end, by=0.01), 13, 5, 45, 6)
          
          # Optimize fit

          nls_fit <-nls(logIntensity ~ error.func(t, B, sigma, i, A), 
                        data = interface, 
                        start = list(B = input$B, #10, # max intensity 
                                     A = input$A, #3# baseline intensity
                                     sigma = input$sigma, # sigma
                                     #i = 120
                                     i = ((end-start)/2)+start # interface position
                        ), 
                        trace = F)

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
          t <- seq(start ,end, by=10/(end-start))
          logIntensity <- as.numeric(predict(nls_fit, list(t = t)))
          
          # Make a data frame
          fit <- cbind(t,logIntensity) %>% as.data.frame()
          colnames(fit) <- c("t", "logIntensity")
          return(fit)
          
  })
  
  # plots fitted data over points
  output$plotfit <- renderPlot({ 
          peakdata  <- subData()
          fit <- predictData()
          plotpoints <- ggplot() + 
                  geom_point(data = peakdata,
                             aes(time,log10(intensity)), colour = "blue") +
                  geom_line(data = peakdata,
                            aes(time,log10(intensity)), colour = "blue") +  
                  #    geom_point(data = subset(peakdata, peak == "X1175.8"),
                  #               aes(time,log(intensity)), colour = "green") +
                  #    geom_line(data = subset(peakdata, peak == "X1175.8"),
                  #              aes(time,log(intensity)), colour = "green") + 
                  theme_classic() 
          
          # Overlay fit on 
          plotfit <- plotpoints + geom_line(data=fit, aes(t,logIntensity), colour="red", size=2) + theme_bw() #+ scale_x_continuous(breaks=seq(0, 500, by =  25))
          
          return(plotfit)

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
  
  

}) # closes shinyServer
