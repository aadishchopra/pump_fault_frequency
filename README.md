---
title: "Pumps"
author: "Aadish Chopra"
date: "January 23, 2018"
output:
  word_document: default
  html_document: default
---

```{r setup, include=TRUE,autodep=TRUE,cache=FALSE}
knitr::opts_chunk$set(echo = TRUE,cache = TRUE,autodep = TRUE)
```

## R Markdown

R version 3.4.1 (2017-06-30)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows >= 8 x64 (build 9200)

We are using R Studio as an IDE for R. All the packages attached are listed with the code

```{r warning=FALSE,message=FALSE}

setwd("D:/UIC/MyResearch")



library(readxl)
library(magrittr)
library(dplyr)

```

**Attaching f in prefix because loading the name in dataset**

```{r}


path="D:/UIC/MyResearch/Pumps/KIRLOSKER PUMP BREAKDOWN DETAILS (1).xlsx"
for(i in 1:length(excel_sheets(path))){
assign(x =paste0("f",excel_sheets(path)[i]), value =read_excel(path = path,sheet = excel_sheets(path)[i]))  
}
# drop a column 

fP301A<-within(fP301A,rm("System status"))

Kirlosker<-rbind(fAGA951,fAGA952A,fAGA955,fCGA201A,fCGA302A,f11P19A,fP301A)

```


Let's identify the equipments notifn 

```{r}

knitr::kable(table(Kirlosker %>% group_by(`Notifictn type` ) %>% select(`Notifictn type`,Equipment) ,dnn = c("Notification Type","Freq equipment")),caption="Matrix for Pumps and Notification")

```

Z1 category is preventice maintenance. Thus we see that if a pump is maintained preventively chances are
less of suffering a fault.


# Extracting data from the second excel sheet

```{r}

# To automate the extraction of sheets from excel


path="D:/UIC/MyResearch/Pumps/KSB PUMPS BREAKDOWN RECORD (2).xlsx"
for(i in 1:length(excel_sheets(path))){
assign(x =excel_sheets(path)[i], value =read_excel(path = path,sheet = excel_sheets(path)[i]))  
  }

# Rbinding difficult due to different names

`11P24A`<-within(`11P24A`,rm("System status"))
P701A<-within(P701A,rm("System status"))
UGA1107A<-within(UGA1107A,rm("System status"))

colnames(P901A)<-names(P701A)
# Now again aggregating all the data from different sheets and putting them in a KSB dataset

KSB<-rbind(`11P24A`,P701A,P702A,P2001A,P901A,UGA1107A,UGA2201A)


knitr::kable(table(KSB %>% group_by(`Notifictn type` ) %>% select(`Notifictn type`,Equipment) ,dnn = c("Notification Type","Freq equipment")),caption="Matrix for Pumps and Notification")


```

# Extracting data from the third excel sheet

```{r results="asis"}


path="D:/UIC/MyResearch/Pumps/MAINTENANCE HISTORY - compressor and turbile- K1&TK1.xlsx"
for(i in 1:length(excel_sheets(path))){
  assign(x =paste0("c",excel_sheets(path)[i]), value =read_excel(path = path,sheet = excel_sheets(path)[i]))  
}


# All these pumps have extra atrributes which 1st are not common across all pumps and secondly 
# may not be relevant in analyzing the data





# What is important is the notification date because that might help us in knowing the frequency

# The third data set is of compressor and turbine. We will be stripping of unwanted columns 


# Let's us change the names of columns so they are in a readable format 
# X_3 is useful in 11ko1. We ll rename the column to Execution date

c11K01<-rename(c11K01,Execution.date=X__3)
cP607C<-rename(cP607C,Execution.date=X__1)

# In the equipment the equipment column should have a constant name throughout the dataset

c11K01$Equipment<-rep("11K01",nrow(c11K01))
cK431$Equipment<-rep("K431",nrow(cK431))


Compar<-data.frame(Ko1=colnames(c11K01))
Compar[14,]<-rep(x = NA,1)
Compar1<-data.frame(TKo1=colnames(c11TK01))
Compar1[11:14,]<-rep(x = NA,4)
Compar2<-data.frame(P607C=colnames(cP607C))
Compar3<-data.frame(K431=colnames(cK431))
Compar3[13:14,]<-rep(x = NA,2)

NetNames<-cbind(Compar,Compar1,Compar2,Compar3)


knitr::kable(NetNames,caption = "column names")


rm(Compar,Compar1,Compar2,Compar3)




```


```{r}

# We will delete the unwanted variables from the dataset
# From the netnames we see that first 8 variables are common along the dataset. We will delete the rest


for(i in 1:length(excel_sheets(path))){
assign(x=paste0("c",excel_sheets(path)[i]),value=get(paste0("c",excel_sheets(path)[i]))[,1:8])
}


# They have different date formats so we need to convert to a common date format

#The below code is not working. The dates format are not proper 

# ?c11K01$Notif.date<-as.Date(c11K01$Notif.date,format = "%d.%m.%y")
# ?c11TK01$Notif.date<-as.Date(c11TK01$Notif.date,format = "%d.%m.%y")

c11K01$Notif.date<-lubridate::dmy(c11K01$Notif.date)
c11TK01$Notif.date<-lubridate::dmy(c11TK01$Notif.date)
cP607C$Notif.date<-as.Date(cP607C$Notif.date,format = "%d.%m.%y")
cK431$Notif.date<-as.Date(cK431$Notif.date,format = "%d.%m.%y")

# Comnbining for analysis


CompTurbine<-rbind(get(paste0("c",excel_sheets(path)[1])),get(paste0("c",excel_sheets(path)[2])),get(paste0("c",excel_sheets(path)[3])),get(paste0("c",excel_sheets(path)[4])))


knitr::kable(table(CompTurbine %>% group_by(`Notifictn type` ) %>% select(`Notifictn type`,Equipment) ,dnn = c("Notification Type","Freq equipment")),caption="Matrix for Pumps and Notification")



```

```{r}

CompTurbine %>% group_by(`Notifictn type`,Equipment ) %>% select(`Notifictn type`,Equipment) %>% tally()


```

-----------------------------------------------------------------------------------------------------------


It is assumed that installation date of every pump is to be considered the previous year's date in April

4 dates were formatted in excel itself before loading. Their format was changed from 
%m/%d/%y and %m.%d.y

We will arrange the dates in <span color="green">descending order </span> ( in the order in which they are listed ) and then group them according to the <span color="green">notification type </span>



```{r}

Kirlosker$Notif.date<-lubridate::dmy(Kirlosker$Notif.date)

KSB$Notif.date<-lubridate::dmy(KSB$Notif.date)


```



After converting all the dates in a common format, we will 

Analyzing the Kirlosker dataset first

###### We know that start date of any pump is the previous year and april 1 which would be 



```{r}

dates_from_subtd<-Kirlosker %>% group_by(Equipment) %>% summarise(Date=min(Notif.date))

#For every pump we got the min date and hence subtracting one year from the date we can get our date for analysis 


date_installed<-as.data.frame(paste((lubridate::year(dates_from_subtd$Date)-1),"04-01",sep="-"))
colnames(date_installed)<-"Date Installed"

dates_from_subtd<-cbind(dates_from_subtd,date_installed)
rm(date_installed)

dates_from_subtd<-dates_from_subtd[,c(1,3)]


Kirlosker<-left_join(Kirlosker,dates_from_subtd)

rm(dates_from_subtd)

dates_from_subtd<-Kirlosker %>% select(Equipment,Notif.date,`Date Installed`)  

dates_from_subtd$`Date Installed`<-as.Date(dates_from_subtd$`Date Installed`)

dates_from_subtd<-dates_from_subtd %>% mutate(diff_date=(Notif.date - `Date Installed`))

dates_from_subtd %>% group_by(Equipment) %>% summarise(Date=min(diff_date))


```

The above table gives us the the days between which the pump had fault for the first time 

**Can use %in% operator to selectively filter some of the values **

```{r}

Kirlosker<-Kirlosker %>% select(`Notifictn type`,Equipment,Notif.date,`Date Installed`)
Kirlosker<-Kirlosker %>% group_by(Equipment,Notif.date) %>% arrange(Equipment,desc(Notif.date))
Kirlosker<-as.data.frame(Kirlosker)

Kirlosker<-Kirlosker %>% mutate(diff_days=Notif.date -lead(Notif.date))

Kirlosker$diff_days<-as.numeric(Kirlosker$diff_days)

for(i in 1:(nrow(Kirlosker)-1)){
if(Kirlosker$diff_days[i]<0)
{
Kirlosker$diff_days[i]<--1  
}
else{
Kirlosker$diff_days[i]<-Kirlosker$diff_days[i]
}
    
}


```



We need to manually put the dates which are set to zero because it has to be subtracted from the data installed 

**Although we can easily replace it with few rows I want to write code which is generic** 
For this we have to know how many rows each category has and will then replace them will the last one because they are the ones which are zero or NA 

```{r}

# We get the count of each equipment and we know that last we have to calculate manually


Kirlosker[is.na(Kirlosker[,5]),5]<--1

# Now wherever there are zeroes we can replace them with the date installed subtracted from the first fault occurence

Kirlosker$`Date Installed`<-as.Date(Kirlosker$`Date Installed`)

for(i in 1:nrow(Kirlosker)){
  
  if(Kirlosker[i,5]==-1){
    Kirlosker[i,5]<-Kirlosker[i,3]-Kirlosker[i,4]
    
  }
}




```


#We will be doing the same for KSB and CompTurbine 


```{r}

dates_from_subtd<-KSB%>% group_by(Equipment) %>% summarise(Date=min(Notif.date))

#For every pump we got the min date and hence subtracting one year from the date we can get our date for analysis 


date_installed<-as.data.frame(paste((lubridate::year(dates_from_subtd$Date)-1),"04-01",sep="-"))
colnames(date_installed)<-"Date Installed"

dates_from_subtd<-cbind(dates_from_subtd,date_installed)
rm(date_installed)

dates_from_subtd<-dates_from_subtd[,c(1,3)]


KSB<-left_join(KSB,dates_from_subtd)

rm(dates_from_subtd)

dates_from_subtd<-KSB %>% select(Equipment,Notif.date,`Date Installed`)  

dates_from_subtd$`Date Installed`<-as.Date(dates_from_subtd$`Date Installed`)

dates_from_subtd<-dates_from_subtd %>% mutate(diff_date=(Notif.date - `Date Installed`))

dates_from_subtd %>% group_by(Equipment) %>% summarise(Date=min(diff_date))



```



The above table gives us the the days between which the pump had fault for the first time 

**Can use %in% operator to selectively filter some of the values **

```{r}

KSB<-KSB %>% select(`Notifictn type`,Equipment,Notif.date,`Date Installed`)
KSB<-KSB %>% group_by(Equipment,Notif.date) %>% arrange(Equipment,desc(Notif.date))

KSB<-as.data.frame(KSB)

# to calculate difference between successive defaults

KSB<-KSB %>% mutate(diff_days=Notif.date -lead(Notif.date))

KSB$diff_days<-as.numeric(KSB$diff_days)

# We are running the loop max-1 because there is no lead for the last item which generates NA's and we know NA's cause lot of troubles in R

for(i in 1:(nrow(KSB)-1)){
if(KSB$diff_days[i] < 0)
{
KSB$diff_days[i]<--1  
}
else{
KSB$diff_days[i]<-KSB$diff_days[i]
}
    
}


```


We need to manually put the dates which are set to zero because it has to be subtracted from the data installed 

**Although we can easily replace it with few rows I want to write code which is generic** 
For this we have to know how many rows each category has and will then replace them will the last one because they are the ones which are zero or NA 






```{r}
# setting the last NA to zero

KSB[is.na(KSB[,5]),5]<--1

# Now wherever there are zeroes we can replace them with the date installed subtracted from the first fault occurence

KSB$`Date Installed`<-as.Date(KSB$`Date Installed`)

for(i in 1:nrow(KSB)){
  
  if(KSB[i,5]==-1){
    KSB[i,5]<-KSB[i,3]-KSB[i,4]
    
  }
}






```


**NOTE: It might be a wrong practice to carry the dates installed as it is carrying the same data.**


#CompTurbine 

We see that CompTurbine the dates are not arranged in a strictly decreasing order within a category.



```{r}

dates_from_subtd<-CompTurbine%>% group_by(Equipment) %>% summarise(Date=min(Notif.date))

#For every pump we got the min date and hence subtracting one year from the date we can get our date for analysis 


date_installed<-as.data.frame(paste((lubridate::year(dates_from_subtd$Date)-1),"04-01",sep="-"))
colnames(date_installed)<-"Date Installed"

dates_from_subtd<-cbind(dates_from_subtd,date_installed)
rm(date_installed)

dates_from_subtd<-dates_from_subtd[,c(1,3)]


CompTurbine<-left_join(CompTurbine,dates_from_subtd)

rm(dates_from_subtd)

dates_from_subtd<-CompTurbine %>% select(Equipment,Notif.date,`Date Installed`)  

dates_from_subtd$`Date Installed`<-as.Date(dates_from_subtd$`Date Installed`)

dates_from_subtd<-dates_from_subtd %>% mutate(diff_date=(Notif.date - `Date Installed`))

dates_from_subtd %>% group_by(Equipment) %>% summarise(Date=min(diff_date))



```



The above table gives us the the days between which the pump had fault for the first time 

**Can use %in% operator to selectively filter some of the values **

```{r}

CompTurbine<-CompTurbine %>% select(`Notifictn type`,Equipment,Notif.date,`Date Installed`)
CompTurbine<-CompTurbine %>% group_by(Equipment,Notif.date) %>% arrange(Equipment,desc(Notif.date))
CompTurbine<-as.data.frame(CompTurbine)

# to calculate difference between successive defaults

CompTurbine<-CompTurbine %>% mutate(diff_days=Notif.date -lead(Notif.date))

CompTurbine$diff_days<-as.numeric(CompTurbine$diff_days)

# We are running the loop max-1 because there is no lead for the last item which generates NA's and we know NA's cause lot of troubles in R

for(i in 1:(nrow(CompTurbine)-1)){
if(CompTurbine$diff_days[i] < 0)
{
CompTurbine$diff_days[i]<--1  
}
else{
CompTurbine$diff_days[i]<-CompTurbine$diff_days[i]
}
    
}


```


We need to manually put the dates which are set to zero because it has to be subtracted from the data installed 

**Although we can easily replace it with few rows I want to write code which is generic** 
For this we have to know how many rows each category has and will then replace them will the last one because they are the ones which are zero or NA 






```{r}
# setting the last NA to zero

CompTurbine[is.na(CompTurbine[,5]),5]<--1

# Now wherever there are zeroes we can replace them with the date installed subtracted from the first fault occurence

CompTurbine$`Date Installed`<-as.Date(CompTurbine$`Date Installed`)

for(i in 1:nrow(CompTurbine)){
  
  if(CompTurbine[i,5]==-1){
    CompTurbine[i,5]<-CompTurbine[i,3]-CompTurbine[i,4]
    
  }
}






```


**NOTE: It might be a wrong practice to carry the dates installed as it is carrying the same data.**

After calculating the fault frequency in terms of days for all the three equipments let us put the output in a nice format



```{r results='asis'}

knitr::kable(Kirlosker,caption = 'Kirlosker')
knitr::kable(KSB,caption = 'KSB')
knitr::kable(CompTurbine,caption = 'CompTurbine')
```


#Statistical Analysis 
 
We will first observe the five number summary for Kirlosker, KSB and Compressor and Turbine

For some readers who do not know about Kirlosker and KSB following are the links https://kirloskar.com and https://www.ksb.com/ksb-en/


```{r}
summary(Kirlosker$diff_days)
summary(KSB$diff_days)
summary(CompTurbine$diff_days)


```

Using the Notification Type to analyze

```{r}

Kirlosker %>% group_by(`Notifictn type`) %>% summarise(mean(diff_days),median(diff_days))

KSB %>% group_by(`Notifictn type`) %>% summarise(mean(diff_days),median(diff_days))

CompTurbine %>% group_by(`Notifictn type`) %>% summarise(mean(diff_days),median(diff_days))

```


As we can see that Z is preventive maintainance and M is natural fault that occurs 
We see that Z category has a mean difference greater than M



Converting into factor variables 

```{r}

Kirlosker$`Notifictn type`<-as.factor(Kirlosker$`Notifictn type`)
KSB$`Notifictn type`<-as.factor(KSB$`Notifictn type`)
CompTurbine$`Notifictn type`<-as.factor(CompTurbine$`Notifictn type`)

```


Now putting linear regression

Y(dependent variable) = Future date of failure , calculated based on the historical analysis . However dates which are quite close to each other having diff_days=0 have to be left out since MTBF will be wrong if we include those

X (independent variable) = Can be a factor variable which in this case can be notification type. 


```{r}

kker<-Kirlosker %>% filter(diff_days>0)

kker_model<-lm(formula=kker$diff_days~kker$`Notifictn type`,data=kker)
summary(kker_model)


```





```{r}
ksber<-KSB %>% filter(diff_days>0)

ksber_model<-lm(formula=ksber$diff_days~ksber$`Notifictn type`,data=ksber)
summary(ksber_model)



```



```{r}
CompTur<-CompTurbine %>% filter(diff_days>0)

CompTur_model<-lm(formula=CompTur$diff_days~CompTur$`Notifictn type`,data=CompTur)
summary(CompTur_model)

```



```{r}

library(ggplot2)
g1<-ggplot(aes(diff_days,`Notifictn type`),data = kker)+
geom_col(aes(`Notifictn type`,diff_days))
  plot(g1)

```

```{r}
g3<-ggplot(aes(diff_days,`Notifictn type`),data = kker)+
geom_point(aes(diff_days,`Notifictn type`))
  plot(g3)



```



```{r}
library(ggplot2)
g2<-ggplot(aes(diff_days,`Notifictn type`),data = ksber)+
geom_count(aes(`Notifictn type`,diff_days))
plot(g2)


```

```{r}
g4<-ggplot(aes(diff_days,`Notifictn type`),data = ksber)+
geom_point(aes(diff_days,`Notifictn type`))
  plot(g4)



```

```{r}

g5<-ggplot(aes(diff_days,`Notifictn type`),data = CompTur)+
geom_point(aes(diff_days,`Notifictn type`))
  plot(g5)

  

```

```{r}

g6<-ggplot(aes(diff_days,`Notifictn type`),data = CompTur)+
geom_col(aes(`Notifictn type`,diff_days))
  plot(g6)


```

Let us bind all the datasets and see if we are able to predcit


```{r}

Total<-rbind(Kirlosker %>% select(Equipment,diff_days,`Notifictn type`),KSB %>% select(Equipment,diff_days,`Notifictn type`),CompTurbine %>% select(Equipment,diff_days,`Notifictn type`))


g7<-ggplot(aes(diff_days,`Notifictn type`),data = Total)+
geom_point(aes(`Notifictn type`,diff_days))
  plot(g7)


```


If we try to group them based on the categories of the notification then we see that there is no feature in the dataset which can disect this dataset into three categories 




```{r}
Total<-Total %>% filter(diff_days >0)

results<-kmeans(Total$diff_days,3)

table(Total$`Notifictn type`,results$cluster)



```


How the overall data is behaving irrespective of any pumps.An attempt to figure out something from the plot 

```{r}

plot(Total$diff_days,type='o')

```


Are there any outliers 


```{r}

boxplot(Total$diff_days)

```


From the boxplot we see that observations that are below the median don't have quite a variance but above the median the observations are quite far apart as illustrated from the box plot. 

What is causing such behavior I don't know. We may have to slice the data from different angles so that we can get a better perspective




We are not able to get good clusters as well.

After applying different techniques, I am thinking that a pump's fault would be based on what kind/when repairing has been done.If pump has been repaired yesterday chances are less that it will need a repair within a month


Let us apply time series model 


```{r warning=FALSE,message=FALSE}

library(forecast)
mydata.arima101<-forecast::Arima(y = Total$diff_days,order = c(1,0,1))
mydata.pred1 <- predict(mydata.arima101, n.ahead=100)
plot (Total$diff_days)
lines(mydata.pred1$pred, col="blue")
lines(mydata.pred1$pred+2*mydata.pred1$se, col="red")
lines(mydata.pred1$pred-2*mydata.pred1$se, col="red")








```

Using the Arima 1,0,1 model and using the standard error , we are calculating a confidence interval for our prediction


```{r}


mydata.pred1$pred+2*mydata.pred1$se
mydata.pred1$pred-2*mydata.pred1$se



```


As we can see the red part is in negative days so we will have to adjust our prediction 

In time series based approach we don't have to calculate the differences between successive observations. Times series methods inherently take the differences and the differences can be of first order or second order or more depending on the stationariness of the data

There are various tests to check the stationarity of the data 

I know of Dickey-Fuller test so I will be using it


```{r}

Total<-Total %>% filter(diff_days>0)



tseries::adf.test(Total$diff_days,alternative = "stationary",k = 0)
acf(Total$diff_days)
pacf(Total$diff_days)



```

```{r}

ggplot(data = Total,aes(diff_days,Equipment))+
geom_line(aes(diff_days,color=Equipment))+
geom_point(aes(diff_days,color=Equipment))+
geom_vline(xintercept = 320)
 




```


The point of drawing such a plot is to see the distribution of the differences in the successive two faults. Drawing a line at h=320 because this line intersects with every other pump.It means that for every pumps there will probably be one observation 

In conclusion, probabilty of occurence is maximum 


We can take a reference point for dates. For every pump there is a date installed 

```{r}
ggplot(Kirlosker,aes(diff_days,Equipment))+
geom_point(aes(diff_days,Equipment),data = Kirlosker)  


```

We will be plotting the dates as a function of index


```{r}
ggplot(Kirlosker,aes(diff_days))+
geom_line(aes(y=diff_days,x=seq(1:67)),data = Kirlosker)


ggplot(Kirlosker,aes(diff_days))+
geom_line(aes(y=diff_days,x=seq(1:61)),data = KSB)


ggplot(Kirlosker,aes(diff_days))+
geom_line(aes(y=diff_days,x=seq(1:129)),data = CompTurbine)

```




