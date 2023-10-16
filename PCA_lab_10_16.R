data <- read.csv("reduced_data_sample.csv")

names(data) <- c("id", "target", "contract", "gender", "car", "n_child",
                 "income", "credit", "loan", "price", "job_stat", "studies",
                 "family", "house", "age", "job_duration", "occupation",
                 "job_type", "n_enquiries", "companion")

data$target <- factor(data$target, levels = c(0,1), labels = c("payed","overdue"))
data$contract <- as.factor(data$contract)
for(i in c(4,5,11:14,17,18,20)) data[,i] <- as.factor(data[,i])

data[which(data$gender=="XNA")] <- 'Unknown'

data$target


library(mice)

data.mic <- mice(data)

summary(data.mic)
summary(data)

completedData <- complete(data.mic, 1)
summary(completedData)

completedData

dcon = completedData[,c(1,6:10,15,16,19)]

pc1 <- prcomp(dcon, scale=TRUE)
class(pc1)
attributes(pc1)

print(pc1)

str(pc1)


# WHICH PERCENTAGE OF THE TOTAL INERTIA IS REPRESENTED IN SUBSPACES?

pc1$sdev
inerProj<- pc1$sdev^2 
inerProj
totalIner<- sum(inerProj)
totalIner
pinerEix<- 100*inerProj/totalIner
pinerEix
barplot(pinerEix)

#Cummulated Inertia in subspaces, from first principal component to the 11th dimension subspace
barplot(100*cumsum(pc1$sdev[1:dim(dcon)[2]]^2)/dim(dcon)[2])
percInerAccum<-100*cumsum(pc1$sdev[1:dim(dcon)[2]]^2)/dim(dcon)[2]
percInerAccum

nd = 5
Psi = pc1$x[,1:nd]

iden = row.names(dcon)
etiq = names(dcon)
ze = rep(0,length(etiq)) # WE WILL NEED THIS VECTOR AFTERWARDS FOR THE GRAPHICS

#eje1<-2
eje1<-1
#eje2<-3
eje2<-2

# Here is were we create the shadows

plot(Psi[,eje1],Psi[,eje2])
text(Psi[,eje1],Psi[,eje2],labels=iden, cex=0.5)
axis(side=1, pos= 0, labels = F, col="cyan")
axis(side=3, pos= 0, labels = F, col="cyan")
axis(side=2, pos= 0, labels = F, col="cyan")
axis(side=4, pos= 0, labels = F, col="cyan")

plot(Psi[,eje1],Psi[,eje2], type="n")
text(Psi[,eje1],Psi[,eje2],labels=iden, cex=0.5)
axis(side=1, pos= 0, labels = F, col="cyan")
axis(side=3, pos= 0, labels = F, col="cyan")
axis(side=2, pos= 0, labels = F, col="cyan")
axis(side=4, pos= 0, labels = F, col="cyan")

Phi = cor(dcon,Psi)
View(Phi)

X<-Phi[,eje1]
Y<-Phi[,eje2]

plot(Psi[,eje1],Psi[,eje2],type="n",xlim=c(-1,1), ylim=c(-1,1))
axis(side=1, pos= 0, labels = F)
axis(side=3, pos= 0, labels = F)
axis(side=2, pos= 0, labels = F)
axis(side=4, pos= 0, labels = F)
arrows(ze, ze, X, Y, length = 0.07,col="blue")
text(X,Y,labels=etiq,col="darkblue", cex=0.7)

plot(Psi[,eje1],Psi[,eje2],type="n",xlim=c(-1,-0.5), ylim=c(-0.5,0.5))
axis(side=1, pos= 0, labels = F)
axis(side=3, pos= 0, labels = F)
axis(side=2, pos= 0, labels = F)
axis(side=4, pos= 0, labels = F)
arrows(ze, ze, X, Y, length = 0.07,col="blue")
text(X,Y,labels=etiq,col="darkblue", cex=0.7)
