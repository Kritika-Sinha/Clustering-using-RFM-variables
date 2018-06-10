##k-means for customer segmentation

getwd()
setwd("C:/Users/kriti/Documents")
data<-read.csv("Online Retail.csv")


datan<-read.table("Online Retail.xlsx",fill = TRUE)
##creating a backup

data2<-data


##checking number of customer IDs

length(unique(data$CustomerID))

##checking numbe of invoice IDs

length(unique(data$InvoiceNo))

sum(is.na(data$CustomerID))

##keeping records with customer IDs

data<-subset(data,!is.na(data$CustomerID))

data$InvoiceDate1<-as.Date(data$InvoiceDate,"%m/%d/%Y")


##checking date range

range(data$InvoiceDate1)

###subsetting data on invoice date

data<-subset(data,data$InvoiceDate1>="2010-12-09")

##subsetting on geography
data<-subset(data,data$Country=="United Kingdom")


##checking number of customer IDs

length(unique(data$CustomerID))

##checking numbe of invoice IDs

length(unique(data$InvoiceNo))

##removing returns
data$item.return<-grepl("C",data$InvoiceNo,fixed=TRUE)
data$purchase.invoice <- ifelse(data$item.return=="TRUE", 0, 1)




#####dataset is finally ready
##we create a unique customer level data

customers<-as.data.frame(unique(data$CustomerID))
names(customers)<-"CustomerID"

colnames(customers)

##recency##
data$recency<-as.Date("2011-12-10")-as.Date(data$InvoiceDate1)

##removing data with orders returned

temp<-subset(data,data$purchase.invoice==1)
recency <- aggregate(recency ~ CustomerID, data=temp, FUN=min, na.rm=TRUE)
##OR
recency2<-aggregate(temp$recency,by=list(temp$CustomerID),FUN=min,na.rm=TRUE)


##adding recency to the customer data

customers<-merge(customers,recency,by="CustomerID",all= TRUE)

class(customers$recency)
##making it numeric

customers$recency<-as.numeric(customers$recency)
class(customers$recency)

##frequency##
##here we need to have data at customer invoice level
##subseting original data

customer.invoice<-data[,c(7,1,11)]
##removing duplicate records
customer.invoice<-customer.invoice[!duplicated(customer.invoice),]

row.names(customer.invoice)<-NULL

##getting total invoices for each customer

annual.invoices<-aggregate(customer.invoice$purchase.invoice,by=list(customer.invoice$CustomerID),FUN=sum,na.rm=TRUE)
colnames(annual.invoices)<-c("CustomerID","Frequency")

##merging with customer

customers<-merge(customers,annual.invoices,by="CustomerID",all=TRUE,na.rm=TRUE)

customers<-customers[,c(1,2,4)]


##generating the monetary value

data$Amount<-data$Quantity*data$UnitPrice
##aggregating sales for each customer
annual.sales<-aggregate(data$Amount,by=list(data$CustomerID),FUN=sum,na.rm=TRUE)

names(annual.sales)<-c("CustomerID","Monetary")

##mapping it with customers
customers<-merge(customers,annual.sales,by="CustomerID",all=TRUE,na.rm=TRUE)

##removing 0 freq

customers<-subset(customers,customers$Frequency>0)

##treating negative monetary##capping to 0

customers$Monetary<-ifelse(customers$Monetary<0,0,customers$Monetary)

##checking the pareto rule


##sorting data
customers<-customers[order(-customers$Monetary),]

##getting the top 20% customers

pareto.cutoff<-0.8*sum(customers$Monetary)
customers$pareto<-ifelse(cumsum(customers$Monetary)<=pareto.cutoff,"Top 20%","Bottom 80%")
class(customers$pareto)

##converting as a factor

customers$pareto<-factor(customers$pareto ,levels=c("Top 20%", "Bottom 80%"), ordered=TRUE)
levels(customers$pareto)
prop.table(table(customers$pareto))

##we see that technically 30% of customers are in top20% bucket

customers<-customers[order(customers$CustomerID),]

##pre processing the data

##log transform the data 

customers$recency.log<-log(customers$recency)
customers$Frequency.log<-log(customers$Frequency)
customers$Monetary.log<-log(customers$Monetary+0.1)


###now computing the z-scores

customers$recency.z<-scale(customers$recency.log,center = TRUE , scale=TRUE)
customers$frequency.z<-scale(customers$Frequency.log,center = TRUE , scale=TRUE)
customers$monetary.z<-scale(customers$Monetary.log,center = TRUE , scale=TRUE)


##visualizing the data


scatter.1<-ggplot(customers,aes(Frequency,Monetary))
scatter.1<-scatter.1+geom_point(aes(colour=recency,shape=pareto))
scatter.1 <- scatter.1 + scale_shape_manual(name = "80/20 Designation", values=c(17, 16))
scatter.1 <- scatter.1 + scale_colour_gradient(name="Recency\n(Days since Last Purchase))")
scatter.1 <- scatter.1 + scale_y_continuous(label=dollar)
scatter.1 <- scatter.1 + xlab("Frequency (Number of Purchases)")
scatter.1 <- scatter.1 + ylab("Monetary Value of Customer (Annual Sales)")
scatter.1

##we see that its tough to make out anything from this graph

##hence we use the scaled variables

# Log-transformed
scatter.2 <- ggplot(customers, aes(x = Frequency.log, y = Monetary.log))
scatter.2 <- scatter.2 + geom_point(aes(colour = recency.log, shape = pareto))
scatter.2 <- scatter.2 + scale_shape_manual(name = "80/20 Designation", values=c(17, 16))
scatter.2 <- scatter.2 + scale_colour_gradient(name="Log-transformed Recency")
scatter.2 <- scatter.2 + xlab("Log-transformed Frequency")
scatter.2 <- scatter.2 + ylab("Log-transformed Monetary Value of Customer")
scatter.2

##we see there are customers who have log value lesser than 0.This indicates that 
#they returned everything they bought.we exclude them.

unique(customers[customers$Monetary.log<0,])

delete <- subset(customers, Monetary.log < 0)
no.value.custs <- unique(delete$CustomerID)
delete2 <- subset(data, CustomerID %in% no.value.custs)
delete2 <- delete2[order(delete2$CustomerID, delete2$InvoiceDate),]
remove(delete, delete2, no.value.custs)

###after removing outliers we have the following

# Scaled variables
scatter.3 <- ggplot(customers, aes(x = frequency.z, y = monetary.z))
scatter.3 <- scatter.3 + geom_point(aes(colour = recency.z, shape = pareto))
scatter.3 <- scatter.3 + scale_shape_manual(name = "80/20 Designation", values=c(17, 16))
scatter.3 <- scatter.3 + scale_colour_gradient(name="Z-scored Recency")
scatter.3 <- scatter.3 + xlab("Z-scored Frequency")
scatter.3 <- scatter.3 + ylab("Z-scored Monetary Value of Customer")
scatter.3


##plotting the data as z variables or log variables..both give the same shape of distribution


##using the log transformed z scores

preprocessed<-customers[,9:11]
j<-10
models<-data.frame(k=numeric(),tots.withinss=numeric(),betweenss=numeric(),totss=numeric(),rsquared=numeric())


##trying

tot=array()

for (k in 1:j)
{
  print(k)
  model1<-kmeans(preprocessed,k,nstart =20)
  print(model1$centers)
  print(model1$withinss)
  print(model1$betweenss)
  tot[k]=model1$tot.withinss
}

tot

check=data.frame(c(1:10),tot)


ggplot(check , aes(check$c.1.10.,check$tot))+geom_point()


##totss-compute a global mean and then compute distance of each point from this mean
#betweenss-compute the distance between cluster mean and global mean
#within ss-compute distance between cluster mean and the distance of each data point from
#this mean
#rsquared-betweenss/totss



##this library automatically computes the best clusters
library(NbClust)
set.seed(1)
nc <- NbClust(preprocessed, min.nc=2, max.nc=7, method="kmeans")
table(nc$Best.n[1,])

nc$All.index # estimates for each number of clusters on 26 different metrics of model fit

