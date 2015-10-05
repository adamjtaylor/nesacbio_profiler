
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

 
fluidRow( # fluidrow format allows for fluid window resizing
        
# First column for data input and parameter setting
column(4,
  
  # First well panel is for data input
  wellPanel(
  h4("Data input"),
    
    # Help text guides user re file format and links to an example file
    helpText(
    "Either export as ASCII from Measurment Explorer Profiles program or use file must be tab-seperated .txt files with \"time\" as first column ",
    a("Example depth profile file.",
    href="https://www.dropbox.com/s/rbbwszjya1gq9hg/bl6.txt?raw=1")
    ),
   
    # Radio buttons allow user to select filetype for upload
    radioButtons("filetype",
                 label = "File type",
                 choices = list("Import from Ion-ToF" = "iontof",
                                "Use own file" = "ownfile"), 
                 selected = "iontof"),
   
   # File input allows for data set upload 
   fileInput('file1', 
             'Upload a dataset',
             accept = c('text/csv',
                        'text/comma-separated-values',
                        'text/tab-separated-values',
                        'text/plain',
                        '.csv',
                        '.tsv')
             ),
   
    hr(), # Horiziontal line
    
    # User selects data set from examples or uploaded file
    selectInput("dataset", 
                "Choose a dataset:",
                choices = c("Example 1: 6% PS/PMMA bilayer",
                            "Example 2: 3% PS/PMMA bilayer",
                            "Example 3: VAMAS multilayer",
                            "Test delta layer",
                            "User uploaded file")
                ),
    # uiOutput('rangeSelect'), # NOT IN USE IN CURRENT CODE
    
    # UI elements for peak selection and x-axis zoom called from server.R
    uiOutput('peakList'),
    uiOutput('zoomrange'),
  
    # Checboxes to select log10 transformation and fitting
    checkboxInput("dispLog", 
                  label="Display log10 transformed data", 
                  value = TRUE),
    checkboxInput("fitLog", 
                  label="Fit using log10 transformed data", 
                  value = TRUE),

    # Radio buttons to select interface type
    # Delta layer function not currently activated
    radioButtons("interfacetype", 
                 "Search for:",
                 choices = list("Interface" = "interface",
                                # UNCOMMENT TO ACTIVATE DELTA LAYER FUNCTION
                                # "Delta layer" = "deltalayer"), 
                                selected = "interface")
                 )
  ), # closes wellPanel
  
  # Well panel for fitting parameters
  wellPanel(
    h4("Fit parameters"),
    
    # Prints equation of error function
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
    
    # Prompt user to adjust initial parameters
    helpText('Adjust initial parameters if fit not found'),
                   
    # Initial estimate of overlayer intensity               
    numericInput("A",
                 "Overlayer intensity, \\(J_A\\)",
                 10),
    
    # Initial estimate of underlayer intensity               
    numericInput("B",
                 withMathJax("Underlayer intensity, \\(J_B\\)"),
                 3),
    
    # Initial estimate of sigma               
    numericInput("sigma",
                 withMathJax("Interfacial width parameter, \\(\\sigma\\),"),
                 3)
    
  ) # closes wellPanel
), # closes column

# 5th column shows graphs and result of fit    
column(5, 

  # Help text instructs user
  helpText('Brush over plot to select interface for fitting'),
  
  hr(), # Horizontal line
  
  # Outputs plot of raw data where user brushes to select interface region
  plotOutput("plotpoints", 
             height = "250px",
             brush = brushOpts(id = "plotpoints_brush", 
                               direction = "x")
             ),
  
  hr(), # Horizontal line
  
  # Outputs plot of fitted data overlaid on raw data
  plotOutput("plotfit", height = "250px"),
  
  hr(), # Horizontal line
  
  # Oputputs plain text summary of fit
  verbatimTextOutput("nls_summary")

), # Closes column
    
# Final column sumarizes fitted coefficiants and parameters
column(3,
  wellPanel(
    h4("Fitted coefficients and interface parameters"),
    helpText(''),
    tableOutput("fitsummary")
  ) # closes wellPanel
) # closes column
    
) # closes fluid row

) # closes fluid page

)#, # closes tabPanel,
  
## Uncomment to turn on z-axis transformation panel when completed
# tabPanel("Transform z-axis to depth",
#     fluidPage(
#      fluidRow(
#         column(4, wellPanel(
#             h4("Coming soon!")
#               )
#              )
#           )
#       )
#     )# closes tabPanel

) # closes navbarPage

) # closes shiny UI