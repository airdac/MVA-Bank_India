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

#Selecting categorical variables
dcat<-dd[,sapply(dd, is.factor)]


#Transforming our table as a transactional database

dtrans<-as(dcat, "transactions")

#[1] 5

foo<-function(x){length(levels(x))}
sum(sapply(dcat, foo))

#Checking our transactional database
dtrans
inspect(head(dtrans,10))
summary(dtrans)

itemFrequencyPlot(dtrans, topN=5)
itemFrequencyPlot(dtrans, support = 0.3)

#Apriori
#?apriori

#minlen : minimun number of items

rules <- apriori(dtrans, parameter =
                      list(support = 0.01, confidence = 0.7, minlen=2, maxlen = 11))
good.rules <- subset(rules, subset = lift > 1)
# Delete rules with a low support added multiple times
# good.rules <- unique(good.rules)

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

# Most rules have turned out to be redundant

# All rules with a lift higher than 17 are trivial
# The most relevant rule is that the set of pensioners is exactly the set
# of people with job type unknown. Indeed:

pensioners = which(dcat$job_stat=="Pensioner")
unk_jobtype = which(dcat$job_type=="Jobtype_Unknown")
setdiff(pensioners, unk_jobtype)
setdiff(unk_jobtype, pensioners)

inspect(sort(good.rules, by="lift")[9:30,])
# Many rules associate people with unknown occupation and pensioners. Let's
# look at their setdiff()

unk_occupation <- which(dcat$occupation == "Occupation_Unknown")
setdiff(pensioners, unk_occupation)
setdiff(unk_occupation, pensioners)
length(setdiff(unk_occupation, pensioners))
# All pensioners declared their occupation as unknown. However, 675 individuals
# didn't tell their occupation and weren't pensioners.

# Now we will analyze the category Occupation_Unknown.
unk.occup.not.pensioner <- subset(good.rules, subset =
                                    lhs %in% "occupation=Occupation_Unknown" & 
                                    !(rhs %in% "job_stat=Pensioner") &
                                    !(rhs %in% "job_type=Jobtype_Unknown"))
inspect(head(unk.occup.not.pensioner, n=10, by="lift"))
# Too complicated 


# WE WILL CONTINUE NEXT DAY
# - Balance target in the database
# - Study rules with rhs %in%% "target"
# - Check more rules with few items with different supports












# It looks like the set of Pensioners is the same as the set of people who
# didn't tell their job type. Indeed, it is true.
pensioners = which(dcat$job_stat=="Pensioner")
unk_jobtype = which(dcat$job_type=="Jobtype_Unknown")
setdiff(pensioners, unk_jobtype)
setdiff(unk_jobtype, pensioners)

payedRules <- sort(subset(rulesDtrans, subset = rhs %in% "target=payed"), by = "confidence")
summary(payedRules)   # lift is too low

overdueRules <- sort(subset(rulesDtrans, subset = rhs %in% "target=overdue"), by = "confidence")
summary(overdueRules)   # None found

# Non redundant rules
nonredundant <- rulesDtrans[!is.redundant(rulesDtrans),]
summary(nonredundant)
inspect(head(rulesDtrans,n=25, by="lift"))

payedRules <- sort(subset(rulesDtrans, subset = rhs %in% "target=payed"), by = "confidence")
summary(payedRules)   # lift is too low
inspect(head(payedRules, n=25, by="lift"))

overdueRules <- sort(subset(rulesDtrans, subset = rhs %in% "target=overdue"), by = "confidence")
summary(overdueRules)   # None found


# These are useless because lift is too low
rulesDtrans <- apriori(dtrans, parameter = list(support = 0.4, confidence = 0.8,  minlen=2))
summary(rulesDtrans)
inspect(head(rulesDtrans,n=25, by="lift"))


### Saving results
# write(rules.pruned, file = "rules.csv", sep = ",", col.names = NA)


#ECLAT (where are the itemsets with k>1?)

eclatDTrans<-eclat(dtrans)
eclatDTrans<-eclat(dtrans, parameter = list(support=0.4, minlen=2, maxlen=10))
inspect(eclatDTrans)

## Select a subset of rules using partial matching on the items 
## in the right-hand-side and a quality measure


## Mine frequent itemsets with Eclat.
eclatDTrans <- eclat(dtrans, parameter = list(supp = 0.5, confidence = 0.8))

#ruleInduction() !!!
## Display the 5 itemsets with the highest support.
orderedItemsets <- sort(eclatDTrans)
inspect(orderedItemsets)

top5 <- sort(eclatDTrans)[1:5]
inspect(top5)

## Get the itemsets as a list
as(items(top5), "list")

## Get the itemsets as a binary matrix
as(items(top5), "matrix")

## Get the itemsets as a sparse matrix, a ngCMatrix from package Matrix.
## Warning: for efficiency reasons, the ngCMatrix you get is transposed 
as(items(top5), "ngCMatrix")

###Visualizing Results
# inspect(rulesDtrans)
plot(rulesDtrans, measure = c("support", "lift"), shading = "confidence")
#order == number of items inside the rules
plot(rulesDtrans, method = "two-key plot")
plot(rulesDtrans, method = "grouped")
plot(rulesDtrans, method = "paracoord")

###Check the suggested links to get a friendly and visual analysis of your results