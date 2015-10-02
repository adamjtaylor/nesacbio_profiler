
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)
library(scales)
library(plyr)
library(pracma)

shinyUI(
        navbarPage("NESAC/BIO depth profile analysis GUI",
                   tabPanel("Find interfaces",
        fluidPage(

     

  # Application title
 # titlePanel("NESAC/BIO depth profile interface finder"),
#  helpText("Adam J. Taylor | ", 
 #          a("tayloraj@uw.edu", href="mailto:tayloraj@uw.edu")
  #         ),

 
  fluidRow(
    column(4, wellPanel(
            h4("Data input"),
            helpText("Either export as ASCII from Measurment Explorer Profiles program or use file must be tab-seperated .txt files with \"time\" as first column ", a("Example depth profile file.", href="https://www.dropbox.com/s/rbbwszjya1gq9hg/bl6.txt?raw=1")),
            radioButtons("filetype", label = "File type",
                         choices = list("Import from Ion-ToF" = "iontof", 
                                        "Use own file" = "ownfile"
                         ), selected = "iontof"),
            fileInput('file1', 'Upload a dataset',
                      accept = c(
                                          'text/csv',
                                          'text/comma-separated-values',
                                          'text/tab-separated-values',
                                          'text/plain',
                                          '.csv',
                                          '.tsv'
                                  )
                      ),
            hr(),
            selectInput("dataset", "Choose a dataset:", 
                        choices = c("Example 1: 6% PS/PMMA bilayer", 
                                    "Example 2: 3% PS/PMMA bilayer", 
                                    "Example 3: VAMAS multilayer",
                                    "Test delta layer",
                                    "User uploaded file"
                        )),
           # uiOutput('rangeSelect'),
            uiOutput('peakList'),
           uiOutput('zoomrange'),
           checkboxInput("dispLog", label="Display log10 transformed data", value = TRUE),
           checkboxInput("fitLog", label="Fit using log10 transformed data", value = TRUE)
           ),
           radioButtons("interfacetype", "Search for:", 
                       choices = list("Interface" = "interface", 
                                   "Delta layer" = "deltalayer"
                       ), selected = "interface"),
           wellPanel(
                   h4("Fit parameters"),
                   withMathJax(helpText(
                        "Selected region fitted to error function of form: 
                        $$J_t = 0.5
                        \\left( J_B - J_A \\right)
                        \\left[
                        1 + 
                        \\text{erf}
                        \\left(
                        \\frac
                                {t-i}
                        	{\\sqrt{2} \\sigma}
                        \\right)
                        \\right]
                        - J_A$$")),
                   helpText('Adjust initial parameters if fit not found'),
                   
                  
            numericInput("A",
                         "Overlayer intensity, \\(J_A\\)",
                         10),
            numericInput("B",
                         withMathJax("Underlayer intensity, \\(J_B\\)"),
                         3),
            numericInput("sigma",
                         withMathJax("Interfacial width parameter, \\(\\sigma\\),"),
                         3)
    )),

    # Show a plot of the generated distribution
    column(5, 
           helpText('Brush over plot to select interfacial region'),
           hr(),
      plotOutput("plotpoints", height = "250px",
                 brush = brushOpts(id = "plotpoints_brush", direction = "x")),
      hr(),
      plotOutput("plotfit", height = "250px"),
      hr(),
      verbatimTextOutput("nls_summary")
    ),
    
    column(3, wellPanel(
            h4("Fitted coefficients and interface parameters"),
            helpText(''),
            tableOutput("fitsummary")
    ))
    
  ) # closes fluid row

  
                        ) # closes fluid page
                ), # closes tabPanel,
  tabPanel("Transform z-axis to depth",
    fluidPage(
     fluidRow(
        column(4, wellPanel(
            h4("Coming soon!")
              )
             )
          )
      )
    )# closes tabPanel
  ) #closes navbarPage
) # closes shiny UI