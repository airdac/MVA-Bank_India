#library of Association rules
library(arules)
library("arulesViz")
#some databases that are in form of transactions --> Read arules pdf users instructions

data("Epub")
data("Groceries")
data("Adult")

#setwd your own working directory to import the database
#How to work with associations rules with data matrix

dd <- read.table("credscoClean.csv",header=T, stringsAsFactors=TRUE, sep=";");

#Selecting categorical variables
dcat<-dd[,sapply(dd, is.factor)]


#Transforming our table as a transactional database

dtrans<-as(dcat, "transactions")

#Checking levels
length(levels(dcat$Dictamen))
#[1] 2
length(levels(dcat$Vivienda))
#[1] 7
length(levels(dcat$Estado.civil))
#[1] 6
length(levels(dcat$Registros))
#[1] 2
length(levels(dcat$Tipo.trabajo))
#[1] 5

foo<-function(x){length(levels(x))}
sum(sapply(dcat, foo))

#Checking our transactional database
dtrans
inspect(head(dtrans,10))
summary(dtrans)
dim(dtrans)
plot(size(dtrans))
plot(size(Groceries))
transactionInfo(Epub[1:10])

inspect(dtrans[1:10])

itemFrequencyPlot(Groceries, support=0.1, cex.names = 1)
itemFrequencyPlot(Groceries, support=0.05, cex.names = 1)
itemFrequencyPlot(Groceries, topN=5, cex.names = 1)
itemFrequencyPlot(dtrans, topN=5, cex.names = 1)
itemFrequencyPlot(dtrans, support=0.1, cex.names = 1)

#Apriori
?apriori

#minlen : minimun number of items

rulesDtrans <- apriori(dtrans, parameter = list(support = 0.1, confidence = 0.5,  minlen=2))
rulesEpub <- apriori(Epub, parameter = list(sup = 0.1, conf = 0.5, target="rules", minlen=1))
rulesGroceries <- apriori(Groceries, parameter = list(sup = 0.01, conf = 0.5,  minlen=2))
rulesGroceries
summary(rulesGroceries)
inspect(rulesGroceries)
summary(rulesDtrans)
inspect(rulesDtrans)
items<-apriori(dtrans, parameter = list(sup = 0.2,  target="frequent itemsets", minlen=2))
inspect(items)

rulesDtrans <- apriori(dtrans, parameter = list(support = 0.4, confidence = 0.8,  minlen=2))
inspect(rulesDtrans)

inspect(head(rulesDtrans,n=3, by="lift"))

data("Groceries")
mbarules<-apriori(Groceries, parameter = list (support=0.01, confidence=0.1, maxlen = 2))
inspect(head(mbarules,n=10, by="lift"))
inspect(head(mbarules,n=10, by="confidence"))
mbaitemsets<-apriori(Groceries, parameter = list (support=0.01, confidence=0.1, maxlen = 2, target="frequent"))
inspect(head(mbaitemsets,n=10, by="support"))

#Searching for redundant rules

#
subset.matrix <- is.subset(rulesDtrans,rulesDtrans,sparse = F)
subset.matrix[lower.tri(subset.matrix,diag=T)] <- NA
redundant <- colSums(subset.matrix,na.rm=T)>=1
which(redundant)

#Removing redundant rules

rules.pruned <- rulesDtrans[!redundant]
rules.pruned <- sort(rules.pruned,by="lift")
inspect(rules.pruned)

###You can use is.redundant and similar instructions to improve your analysis
### See the links of examples (English and Spanish Version)
### These links offers a way to summarize your Analysis
#### ********** https://rpubs.com/Buczman/AssociationRules
####*           https://rpubs.com/Joaquin_AR/397172
####These links is very useful to filter rules
##################################### according to your requirements, check "FILTRADO
##################################### DE REGLAS

###
### Saving results
write(rules.pruned, file = "rules.csv", sep = ",", col.names = NA)


#ECLAT

eclatDTrans<-eclat(dtrans)
eclatDTrans<-eclat(dtrans, parameter = list(support=0.4, minlen=1, maxlen=10))
inspect(eclatDTrans)

## Select a subset of rules using partial matching on the items 
## in the right-hand-side and a quality measure


## Mine frequent itemsets with Eclat.
eclatDTrans <- eclat(dtrans, parameter = list(supp = 0.5))

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
inspect(rulesDtrans)
plot(rulesDtrans, measure = c("support", "lift"), shading = "confidence")
#order == number of items inside the rules
plot(rulesDtrans, method = "two-key plot")
plot(rulesDtrans, method = "grouped")
plot(rulesDtrans, method = "paracoord")

###Check the suggested links to get a friendly and visual analysis of your results