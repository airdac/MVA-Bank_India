# Clear plots
if(!is.null(dev.list())) dev.off()

# Clean workspace
rm(list=ls())


#library of Association rules
library(arules)
library("arulesViz")
#some databases that are in form of transactions --> Read arules pdf users instructions


#setwd your own working directory to import the database
#How to work with associations rules with data matrix

dd <- read.table("2.Bank_India_preprocessed_data.csv",header=T, stringsAsFactors=TRUE, sep=",")