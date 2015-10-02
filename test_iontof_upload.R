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

peaks <- read.table("6layer.txt", sep="\t", fill=TRUE, header=FALSE, nrows=1, skip=1) # imports masses (second row of df)

upload <- read.table("6layer.txt", sep="\t", 
                       fill=TRUE, header=FALSE, 
                       skip=3) %>% rename(peaks)

upload[,ncol(upload)] <- NULL # drops last column which should be full of NAs

colnames(upload) <- paste("X",colnames(upload), sep="")

upload <- upload %>% rename(c("XNA" = "time"))



#colnames(upload) <- colnames
#colnames(upload[1]) <- "time"
