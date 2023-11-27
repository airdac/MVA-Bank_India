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

# FILTER BY TARGET = OVERDUE
dd <- dd[which(dd$target=='overdue'),]


#Selecting categorical variables
dcat<-dd[,sapply(dd, is.factor)]


#Transforming our table as a transactional database

dtrans<-as(dcat, "transactions")

foo<-function(x){length(levels(x))}
sum(sapply(dcat, foo))

#Checking our transactional database
dtrans
inspect(head(dtrans,10))
summary(dtrans)

itemFrequencyPlot(dtrans, topN=5)
itemFrequencyPlot(dtrans, support = 0.3)
# To start with, most individuals in the target=overdue category
# ask for a cash loan, don't own a car, have a secondary education,
# live in an apartment and ask for the loan unaccompanied.
