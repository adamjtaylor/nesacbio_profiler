
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

shinyUI(fluidPage(

  # Application title
  titlePanel("NESAC/BIO depth profile interface finder"),
  helpText("Adam J. Taylor | ", 
           a("tayloraj@uw.edu", href="mailto:tayloraj@uw.edu")
           ),

  # Sidebar with a slider input for number of bins
  fluidRow(
    column(4, wellPanel(
            h4("Data input"),
            helpText("Must be tab-seperated .txt files with \"time\" as first column. ", a("Example depth profile file.", href="https://www.dropbox.com/s/rbbwszjya1gq9hg/bl6.txt?raw=1")),
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
                        choices = c("Example 1: 3% PS/PMMA bilayer", 
                                    "Example 2: 6% PS/PMMA bilayer", 
                                    "Example 3: VAMAS multilayer",
                                    "User uploaded file"
                        )),
           # uiOutput('rangeSelect'),
            uiOutput('peakList')),
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
    
  )
))
