# Clear plots
if(!is.null(dev.list())) dev.off()

# Clean workspace
rm(list=ls())


#library of Association rules
library(arules)
library("arulesViz")
library(DataExplorer)
#some databases that are in form of transactions --> Read arules pdf users instructions


#setwd your own working directory to import the database
#How to work with associations rules with data matrix

dd <- read.table("2.Bank_India_preprocessed_data.csv",header=T, stringsAsFactors=TRUE, sep=",")


dd.overdue <- dd[which(dd$target=='overdue'),]

set.seed(123)
dd.target.original <- dd[which(dd$target=='payed'),]
dd.target <- dd.target.original[sample(nrow(dd.target.original), nrow(dd.overdue)), ]
# EDA of the sample
create_report(dd.target)
# EDA of the original dataset
create_report(dd.target.original)
# Distributions in the sample of dd.target are similar to the original ones, 
# so we can proceed with this data

dd.balanced <- rbind(dd.overdue, dd.target)
summary(dd.balanced)

dcat<-dd[,sapply(dd, is.factor)]
dtrans<-as(dcat, "transactions")

rules <- apriori(dtrans, parameter =
                   list(support = 0.01, confidence = 0.7, minlen=2, maxlen = 11))
good.rules <- subset(rules, subset = lift > 1)

inspect(head(good.rules, n=10, by = "lift"))
# Highest lift rules are trivial, since they relate people with the same
# occupation and job_type. This was expected because these variables tell a
# similar information

# Before doing any feature selection, let us remove redundant rules
nonredundant <- good.rules[!is.redundant(good.rules),]
summary(nonredundant)
inspect(head(nonredundant,n=10, by="lift"))

length(good.rules)
length(nonredundant)
good.rules <- nonredundant

inspect(head(good.rules, n=30, by="lift"))



# We will study now our target.
payedRules <- sort(subset(good.rules, subset = rhs %in% "target=payed"), by = "lift")
summary(payedRules)
inspect(head(payedRules, n=10, by="lift"))
# Too low lifts
payedRules2 <- sort(subset(good.rules, subset = lhs %in% "target=payed"), by = "lift")
inspect(head(payedRules2, n=10, by="lift"))
payedRules3 <- sort(subset(good.rules, subset = lhs %in% "target=payed" &
                             size(lhs) <= 2), by = "lift")
inspect(head(payedRules3, n=10, by="lift"))
overdueRules <- sort(subset(good.rules, subset = rhs %in% "target=overdue"), by = "lift")
summary(overdueRules)
overdueRules2 <- sort(subset(good.rules, subset = lhs %in% "target=overdue"), by = "lift")
summary(overdueRules2)
overdueRules3 <- sort(subset(good.rules, subset = lhs %in% "target=overdue" &
                               size(lhs) <= 2), by = "lift")
summary(overdueRules3)
inspect(head(overdueRules3, n=10, by="lift"))
# We have not found any good rule.
