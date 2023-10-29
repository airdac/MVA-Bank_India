## REVISING
# Libraries:
library(tidyverse)
library(memisc)
library(dplyr) 
library(recipes)
library(stats)
library(caret)
library(class)
library(MASS)
library(mvnmle)
library(devtools)
source("LittleMCAR function.R")
library(ggplot2)
library(misty)
library(mice)
library(skimr)
library(VIM)
library(cluster)
require(StatMatch)

#assume missings represented with NA
uncompleteVar<-function(vector){any(is.na(vector))}

# MIMMI
MiMMi <- function(data, priork=-1)
{
  #Identify columns without missings
  colsMiss<-which(sapply(data, uncompleteVar))
  if(length(colsMiss)==0){
    print("Non missing values found")
    out<-dd
  }else{
    K<-dim(data)[2]
    colsNoMiss<-setdiff(c(1:K),as.vector(colsMiss))
    
    #cluster with complete data
    dissimMatrix <- daisy(data[,colsNoMiss], metric = "gower", stand=TRUE)
    distMatrix<-dissimMatrix^2
    
    hcdata<-hclust(distMatrix, method = "ward.D2")
    plot(hcdata)
    nk<-2
    if(priork==-1){
      print("WARNING: See the dendrogramm and ZOOM if required")
      print("and enter a high number of clusters")
      nk<-readline("(must be a positive integer). k: ")
      nk<-as.integer(nk)
    }else{nk<-priork}
    
    partition<-cutree(hcdata, nk)
    
    CompleteData<-data
    #nomes cal per tenir tra?a de com s'ha fet la substituci?
    newCol<-K+1
    CompleteData[,newCol]<-partition
    names(CompleteData)[newCol]<-"ClassAux"
    
    setOfClasses<-as.numeric(levels(as.factor(partition)))
    imputationTable<-data.frame(row.names=setOfClasses)
    p<-1
    
    for(k in colsMiss)
    {
      #Files amb valors utils
      rowsWithFullValues<-!is.na(CompleteData[,k])
      
      #calcular valors d'imputacio
      if(is.numeric(CompleteData[,k]))
      {
        imputingValues<-aggregate(CompleteData[rowsWithFullValues,k], by=list(partition[rowsWithFullValues]), FUN=mean)
      }else{
        imputingValues<-aggregate(CompleteData[rowsWithFullValues,k], by=list(partition[rowsWithFullValues]), FUN=Mode)
      }
      
      #Impute
      
      for(c in setOfClasses)
      {
        CompleteData[is.na(CompleteData[,k]) & partition==c,k]<-imputingValues[c,2]
      }
      
      #Imputation Table
      imputationTable[,p]<-imputingValues[,2]
      names(imputationTable)[p]<-names(data)[k]
      p<-p+1
    }
    
    rownames(imputationTable)<-paste0("c", 1:nk)
    out<-new.env()
    out$imputedData<-CompleteData
    out$imputation<-imputationTable
  }
  return(out)
}


# LOAD DATA
data <- read.csv("1. Bank-India-raw-data.csv")
summary(data)

##### PREPROCESSING  #####
glimpse(data)
names(data) <- c("id", "target", "contract", "gender", "car", "n_child",
                 "income", "credit", "loan", "price", "job_stat", "studies",
                 "family", "house", "age", "job_duration", "occupation",
                 "job_type", "n_enquiries", "companion")
names(data)
str(data)

# recode missings to Unknown
data$job_duration[which(data$job_duration == 365243)] <- NA
data$job_type[which(data$job_type == "XNA")] <- "Jobtype_Unknown"
data <- data[!(data$gender == "XNA"), ]
data$occupation[which(data$occupation == "")] <- "Occupation_Unknown"
data$companion[which(data$companion == "")] <- "Companion_Unknown"

# recode modalities and group them
data$job_type <- sub(":.*", "", data$job_type)
data$companion[data$companion %in% c("Other_A", "Other_B", "Group_people")] <- "Other_companion"
data$family[data$family %in% c("Civil marriage")] <- "Married"
data$occupation[data$occupation %in% c("Accountants", "Secretaries","HR staff") ] <- "Administrative Staff"
data$occupation[data$occupation %in% c("High skill tech staff", "IT staff") ] <- "Tech Staff"
data$occupation[data$occupation %in% c("Low-skill Laborers", "Laborers") ] <- "Laborers"
data$occupation[data$occupation %in% c("Cleaning staff", "Chef","Drivers","Waiters") ] <- "Service Staff"

data$studies[data$studies %in% c("Incomplete higher","Lower secondary")] <- "Low education"
data$studies[data$studies %in% c("Secondary / secondary special")] <- "Secondary education"
data$studies[data$studies %in% c("Higher edu." = "Higher education","Academic degree")] <- "Higher education"

data$job_type[data$job_type %in% c("Business Entity Type 1", "Business Entity Type 2", "Business Entity Type 3")] <- "Business"
data$job_type[data$job_type %in% c("Advertising", "Bank", "Business") ] <- "Business"
data$job_type[data$job_type %in% c("Culture", "Electricity", "Emergency", "Hotel", "Industry","Legal Services", "Services", "Restaurant", "School", "Kindergarten", "Other") ] <- "Culture and Services"
data$job_type[data$job_type %in% c("Government", "Military", "Security Ministries", "Police", "Postal") ] <- "Government and Military"
data$job_type[data$job_type %in% c("Realtor", "Trade", "Telecom") ] <- "Real Estate and Trade"

data$companion[data$companion %in% c("Children", "Family")] <- "Family"

# convert as factor 
data$job_stat <- factor(data$job_stat)
data$job_type <- factor(data$job_type)
data$family <- factor(data$family)
data$house <- factor(data$house)
data$occupation <- factor(data$occupation)
data$companion <- factor(data$companion)
data$studies <- factor(data$studies)
data$target <- factor(data$target, levels = c(0,1), labels = c("payed","overdue"))

# check every modality
names(table(data$job_stat))
names(table(data$car))
names(table(data$family)) 
names(table(data$house)) # too centered 
names(table(data$occupation))
names(table(data$job_type))
names(table(data$companion))
names(table(data$studies)) 

# recode shorter name some modalities
data$job_stat <- fct_recode(data$job_stat, "Commer. Assoc." = "Commercial associate")
data$family <- fct_recode(data$family,  "single" = "Single / not married", "divorce" = "Separated")
data$house <- fct_recode(data$house, "apartment" = "House / apartment", "Municipal apart." = "Municipal apartment", "Rented apart."="Rented apartment", "Office apart."="Office apartment", "Co-op apart."="Co-op apartment")
data$occupation <- fct_recode(data$occupation, "High-tech stf" = "High skill tech staff", "Medic stf" = "Medicine staff", "Chef" = "Cooking staff", "Security" = "Security staff", "Waiters" = "Waiters/barmen staff", "Private ser." = "Private service staff")
data$companion <- fct_recode(data$companion, "Partner" = "Spouse, partner", "Unaccompan." = "Unaccompanied", "Group_people" = "Group of people")


char_cols <- which(sapply(data, is.character))
data[, char_cols] <- lapply(data[, char_cols], as.factor) 
data$age<-floor(abs(data$age) / 365)
data$job_duration <- (abs(data$job_duration) / 365)

# check transformations
glimpse(data)
str(data)
summary(data)
skim(data)

### NA
missing.values.data <-  as.data.frame(skimr::skim(data))
na_perc = sum(missing.values.data$n_missing) / (ncol(data) * nrow(data)) * 100
unknown_count = sum(data$job_type == "Unknown", na.rm = T) + sum(data$gender == "Unknown") +
  sum(data$occupation == "Unknown") + sum(data$companion == "Unknown")
unknown_perc = unknown_count / (ncol(data) * nrow(data)) * 100
missing_perc = na_perc + unknown_perc

# Little test (numerical)
little.test <- LittleMCAR(data)
#little.test$chi.square
little.test$p.value
#Data is NOT missing completely at random as p-value is 0. (numeric)
# We assume that is at RANDOM : then we have to impute
little.test$missing.patterns
little.test$amount.missing

numerical_data <- data %>%
  select_if(is.numeric)
md.pattern(numerical_data)

mplot <- aggr(numerical_data, col=c('navyblue','yellow'),
              numbers=TRUE, sortVars=TRUE,
              labels=names(numerical_data), cex.axis=.7,
              gap=3, ylab=c("Missing data","Pattern"))

#balanced data (low levels) Burtt tabel, ndim close 80. compare mca and pca

## sospecho q quan n_enquiries es NA  <-> job_type Unknown tmb
### na
rows_with_na <- data[which(is.na(data$n_enquiries)), ]
summary(rows_with_na)
summary(data)
rows_with_na[which(data$job_type == "Unknown"), ]

#contingency table??:
#result_table <- table(data$n_enquiries, data$occupation)
#result_table


# Imputation
# MICE
imputed_Data <- mice(numerical_data, m=5, maxit = 50, method = 'pmm', seed = 500)
summary(imputed_Data)

stripplot(imputed_Data, price, pch = 19, xlab = "Imputation number")
imputed_Data$imp$price
completeData <- mice::complete(imputed_Data, 3)
stripplot(imputed_Data, job_duration, pch = 19, xlab = "Imputation number")
stripplot(imputed_Data, n_enquiries, pch = 19)
stripplot(imputed_Data, pch = 10, cex = 1.2)
densityplot(imputed_Data)


#KNN
knn_imputed <- kNN(numerical_data)
marginplot(knn_imputed[, c("price", "job_duration")])


## MIMI
mimi_imputed<-MiMMi(numerical_data)
mimi_imputed$imputation
mimi_imputed$imputedData

# Density plot
par(mfrow=c(1,2))
# density plot mimi price
densityplot(data$price, col = "blue", main = "Density Plot of Price")
densityplot(mimi_imputed$imputedData$price, col = "blue", add = TRUE)
# density plot mimi job duration
densityplot(mimi_imputed$imputedData$job_duration, col = "blue", add = TRUE)
densityplot(data$job_duration, col = "blue", add = TRUE)

densityplot(mimi_imputed$imputedData$n_enquiries, col = "blue", add = TRUE)
densityplot(data$n_enquiries, col = "blue", add = TRUE)

#density plot job duration mice
densityplot(completeData$job_duration, col = "blue", add = TRUE)
densityplot(data$job_duration, col = "blue", add = TRUE)

#density plot n_enquiries mice
densityplot(completeData$n_enquiries, col = "blue", add = TRUE)
densityplot(data$n_enquiries, col = "blue", add = TRUE)

densityplot(completeData$price, col = "blue", add = TRUE)
densityplot(data$price, col = "blue", add = TRUE)

columns_to_replace <- c("price", "n_enquiries", "job_duration")
data[columns_to_replace] <- completeData[columns_to_replace]
head(data)
write.csv(data, "C:\\Users\\Airdac\\Documents\\GitHub\\bank_india\\2. Bank-India-preprocessed-data.csv", row.names = FALSE)
