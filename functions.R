LittleMCAR<-function(x){
  if (!require(mvnmle)) 
    stop("You must have mvnmle installed to use LittleMCAR")
  if (!(is.matrix(x) | is.data.frame(x))) 
    stop("Data should be a matrix or dataframe")
  if (is.data.frame(x)) 
    x <- data.matrix(x)
  # define variables        
  n.var<-ncol(x) # number of variables
  n<-nrow(x)  #number of respondents
  var.names<-colnames(x)
  r <- 1 * is.na(x)
  nmis <- as.integer(apply(r, 2, sum))  #number of missing data for each variable
  mdp <- (r %*% (2^((1:n.var - 1)))) + 1  #missing data patterns
  x.mp<-data.frame(cbind(x,mdp))
  colnames(x.mp)<-c(var.names,"MisPat")
  n.mis.pat<-length(unique(x.mp$MisPat)) # number of missing data patterns
  p<-n.mis.pat-1
  gmean<-mlest(x)$muhat #ML estimate of grand mean (assumes Normal dist)
  gcov<-mlest(x)$sigmahat #ML estimate of grand covariance (assumes Normal dist)
  colnames(gcov)<-rownames(gcov)<-colnames(x)
  
  #recode MisPat variable to go from 1 through n.mis.pat
  x.mp$MisPat2<-rep(NA,n)
  for (i in 1:n.mis.pat){
    x.mp$MisPat2[x.mp$MisPat == sort(unique(x.mp$MisPat), partial=(i))[i]]<- i
  }
  x.mp$MisPat<-x.mp$MisPat2
  x.mp<-x.mp[ , -which(names(x.mp) %in% "MisPat2")]
  
  #make list of datasets for each pattern of missing data
  datasets<-list() 
  for (i in 1:n.mis.pat){
    datasets[[paste("DataSet",i,sep="")]]<-x.mp[which(x.mp$MisPat==i),1:n.var]
  }
  
  #degrees of freedom
  kj<-0
  for (i in 1:n.mis.pat){	
    no.na<-as.matrix(1* !is.na(colSums(datasets[[i]])))
    kj<-kj+colSums(no.na)
  }
  df<-kj -n.var
  
  #Little's chi-square
  d2<-0
  cat("this could take a while")
  for (i in 1:n.mis.pat){	
    mean<-(colMeans(datasets[[i]])-gmean)
    mean<-mean[!is.na(mean)]
    keep<-1* !is.na(colSums(datasets[[i]]))
    keep<-keep[which(keep[1:n.var]!=0)]
    cov<-gcov 
    cov<-cov[which(rownames(cov) %in% names(keep)) , which(colnames(cov) %in% names(keep))]
    d2<-as.numeric(d2+(sum(x.mp$MisPat==i)*(t(mean)%*%solve(cov)%*%mean)))
  }
  
  #p-value for chi-square
  p.value<-1-pchisq(d2,df)
  
  #descriptives of missing data
  amount.missing<-matrix(nmis, 1, length(nmis))
  percent.missing<-amount.missing/n
  amount.missing<-rbind(amount.missing,percent.missing)
  colnames(amount.missing)<-var.names
  rownames(amount.missing)<-c("Number Missing", "Percent Missing")
  
  list(chi.square=d2, df=df, p.value=p.value, missing.patterns=n.mis.pat, amount.missing=amount.missing, data=datasets)
  
}


#assume missings represented with NA
uncompleteVar<-function(vector){any(is.na(vector))}

Mode <- function(x) 
{
  x<-as.factor(x)
  maxV<-which.max(table(x))
  return(levels(x)[maxV])
}



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
