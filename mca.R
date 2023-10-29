library(dplyr)
library(tidyverse)
library(skimr)
library(VIM)
library(mice)
library(cluster)
library(ggplot2)
library(car)
require(StatMatch)
library(FactoMineR)
library(Matrix)
library(factoextra)
library(corrplot)
library(devtools)
library(gridExtra)
library(Matrix)
library(factoextra)
library(corrplot)

# the data from the preprocess....
data

#################################### MCA
# Remember that MCA is only done with CATEGORICAL variables. 
summary(data)
sapply(data,class)

# separate numeric with categorical
numeriques<-which(sapply(data,is.numeric))
numeriques<-array(numeriques[-1])
ft<-which(sapply(data,is.factor))
categorical<-array(ft)
ft

## we dont consider in the analysis: job_stat , house, job_type, companion and id of course not. 
## we consider as extra information : target and occupation and the numerical variables
which(names(data)=="job_type");which(names(data)=="house");which(names(data)=="job_stat");which(names(data)=="companion")
data_w_used_categoricals<- data[c(-1,-18,-14,-11,-20)]
numeriques<-which(sapply(data_w_used_categoricals,is.numeric))
res.mca <- MCA(data_w_used_categoricals,quanti.sup =numeriques, quali.sup=c(1,14),method="Burt", graph = FALSE) #mca

#### summary of the dimensions
dimdesc(res.mca)

# The most influential modalities for the dimension 1 and 2:

res.mca$ind$contrib # contribution of the individuals (not very usful)
round(res.mca$var$contrib,2) # contribution of the modalities of each variable
(round(res.mca$var$contrib,2)[,1]) #Contributions of the variables for Dim1
plot(res.mca,invisible=c("var","quali.sup"),cex=0.7) # plot individuals in the dim1,2
fviz_contrib(res.mca, choice = "var", axes = 1:2, top = 15) # most influential modalities plot
fviz_screeplot(res.mca, addlabels = TRUE, ylim = c(0, 30))

fviz_mca_var(res.mca, col.var = "contrib",
             select.var = list(contrib = 11),
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, # avoid text overlapping (slow)
             ggtheme = theme_minimal()
)

# add extra information with occupation and target
fviz_mca_var(res.mca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, # avoid text overlapping (slow)
             ggtheme = theme_minimal()
)

# individuals based on target variable
fviz_mca_ind(res.mca, 
             label = "none", # hide individual labels
             habillage = "target", # color by groups 
             palette = c("#00AFBB", "#E7B800","#00AF00"),
             addEllipses = TRUE, ellipse.type = "confidence",
             ggtheme = theme_minimal()) 

# individuals in the plane (10 contributions)
fviz_mca_biplot(res.mca, repel = FALSE, select.var = list(contrib = 10), ggtheme = theme_minimal())

# center gravity for occupation in the 1st plane 
fviz_mca_ind(res.mca, 
             label = "none", # hide individual labels
             habillage = "occupation", # color by groups 
             addEllipses = TRUE, ellipse.type = "confidence",
             ggtheme = theme_minimal()) 

# center gravity for target in the 1st plane 
fviz_mca_ind(res.mca, 
             label = "none", # hide individual labels
             habillage = "target", # color by groups 
             palette = c("#00AFBB", "#E7B800","#00AF00"),
             addEllipses = TRUE, ellipse.type = "confidence",
             ggtheme = theme_minimal()) 

plot1 <- fviz_mca_ind(res.mca, 
                      label = "none",
                      habillage = "car",
                      palette = c("#00AFBB", "#E7B800"),
                      addEllipses = TRUE,
                      ellipse.type = "confidence",
                      ggtheme = theme_minimal())

# Plot btween categoricals that do MCA

plotellipses(res.mca,keepvar=c("quali"))


fviz_ellipses(res.mca, c("occupation", "target"),
              geom = "point")

fviz_ellipses(res.mca, c("contract", "target"),
              geom = "point")

# plot dim 1 with 3 (kind of similar conclusions)
fviz_mca_var(res.mca, axes = c(1, 3),col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, # avoid text overlapping (slow)
             ggtheme = theme_minimal()
)