Pumps
Aadish Chopra
February 6, 2018
R Markdown
This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see http://rmarkdown.rstudio.com.
Loading the datasets into a statistical programming language. In this case we are using R programming language.
R version 3.4.1 (2017-06-30) Platform: x86_64-w64-mingw32/x64 (64-bit) Running under: Windows >= 8 x64 (build 9200)
We are using R Studio as an IDE for R. All the packages attached are listed with the code
setwd("D:/UIC/MyResearch")



library(readxl)
library(magrittr)
library(dplyr)
Attaching f in prefix because loading the name in dataset
path="D:/UIC/MyResearch/Pumps/KIRLOSKER PUMP BREAKDOWN DETAILS (1).xlsx"
for(i in 1:length(excel_sheets(path))){
assign(x =paste0("f",excel_sheets(path)[i]), value =read_excel(path = path,sheet = excel_sheets(path)[i]))  
}
# drop a column 

fP301A<-within(fP301A,rm("System status"))

Kirlosker<-rbind(fAGA951,fAGA952A,fAGA955,fCGA201A,fCGA302A,f11P19A,fP301A)
Let's identify the equipments notifn
knitr::kable(table(Kirlosker %>% group_by(`Notifictn type` ) %>% select(`Notifictn type`,Equipment) ,dnn = c("Notification Type","Freq equipment")),caption="Matrix for Pumps and Notification")
Matrix for Pumps and Notification
	11P19A	AGA951	AGA952A	AGA955	CGA201A	P301A	UGA302A
M1	12	5	12	11	1	3	0
Z1	0	0	0	0	7	8	8
Z1 category is preventice maintenance. Thus we see that if a pump is maintained preventively chances are less of it getting corrupted
Extracting data from the second excel sheet
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
Matrix for Pumps and Notification
	21P24A	AGA901A	P2001A	P701A	P702A	UGA1107A	UGA2201A
M1	1	4	4	2	3	0	9
Z1	0	3	0	0	9	8	7
Z3	6	0	0	5	0	0	0
Extracting data from the second excel sheet
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
column names
Ko1	TKo1	P607C	K431
Notifictn type	Notifictn type	Notifictn type	Notifictn type
Notification	Notification	Notification	Notification
Notif.date	Notif.date	Notif.date	Notif.date
Order	Order	Order	Order
Equipment	Equipment	Equipment	Equipment
Description	Description	Description	Description
Functional loc.	Functional loc.	Functional loc.	Functional loc.
System status	System status	System status	System status
X__1	User status	Execution.date	User status
X__2	Main WorkCtr	X__2	Execution Date
Execution.date	NA	User status	Job Details
X__4	NA	PartnerResp.	Main WorkCtr
PartnerResp.	NA	DescEmpl.Resp.	NA
NA	NA	Main WorkCtr	NA
rm(Compar,Compar1,Compar2,Compar3)
# We will delete the unwanted variables from the dataset
# From the netnames we see that first 8 variables are common along the dataset. We will delete the rest


for(i in 1:length(excel_sheets(path))){
assign(x=paste0("c",excel_sheets(path)[i]),value=get(paste0("c",excel_sheets(path)[i]))[,1:8])
}


# They have different date formats so we need to convert to a common date format

c11K01$Notif.date<-as.Date(c11K01$Notif.date,format = "%d.%m.%y")
c11TK01$Notif.date<-as.Date(c11TK01$Notif.date,format = "%d.%m.%y")
cP607C$Notif.date<-as.Date(cP607C$Notif.date,format = "%d.%m.%y")
cK431$Notif.date<-as.Date(cK431$Notif.date,format = "%d.%m.%y")

# Comnbining for analysis


CompTurbine<-rbind(get(paste0("c",excel_sheets(path)[1])),get(paste0("c",excel_sheets(path)[2])),get(paste0("c",excel_sheets(path)[3])),get(paste0("c",excel_sheets(path)[4])))


knitr::kable(table(CompTurbine %>% group_by(`Notifictn type` ) %>% select(`Notifictn type`,Equipment) ,dnn = c("Notification Type","Freq equipment")),caption="Matrix for Pumps and Notification")
Matrix for Pumps and Notification
	11K01	11TK01	K431	P607C
M1	9	50	16	10
Z1	0	0	0	8
Z3	6	30	0	0
CompTurbine %>% group_by(`Notifictn type`,Equipment ) %>% select(`Notifictn type`,Equipment) %>% tally()
## # A tibble: 7 x 3
## # Groups:   Notifictn type [?]
##   `Notifictn type` Equipment     n
##              <chr>     <chr> <int>
## 1               M1     11K01     9
## 2               M1    11TK01    50
## 3               M1      K431    16
## 4               M1     P607C    10
## 5               Z1     P607C     8
## 6               Z3     11K01     6
## 7               Z3    11TK01    30
________________________________________
It is assumed that installation date of every pump is to be considered the previous year's date in April
4 dates were formatted in excel itself before loading. Their format was changed from %m/%d/y and %m.%d.y
We will arrange the dates in descending order  ( in the order in which they are listed ) and then group them according to the notification type 
Kirlosker$Notif.date<-lubridate::dmy(Kirlosker$Notif.date)

KSB$Notif.date<-lubridate::dmy(KSB$Notif.date)
After converting all the dates in a common format, we will
Analyzing the Kirlosker dataset first
We know that start date of any pump is the previous year and april 1 which would be
dates_from_subtd<-Kirlosker %>% group_by(Equipment) %>% summarise(Date=min(Notif.date))

#For every pump we got the min date and hence subtracting one year from the date we can get our date for analysis 


date_installed<-as.data.frame(paste((lubridate::year(dates_from_subtd$Date)-1),"04-01",sep="-"))
colnames(date_installed)<-"Date Installed"

dates_from_subtd<-cbind(dates_from_subtd,date_installed)
rm(date_installed)

dates_from_subtd<-dates_from_subtd[,c(1,3)]


Kirlosker<-left_join(Kirlosker,dates_from_subtd)
## Joining, by = "Equipment"
rm(dates_from_subtd)

dates_from_subtd<-Kirlosker %>% select(Equipment,Notif.date,`Date Installed`)  

dates_from_subtd$`Date Installed`<-as.Date(dates_from_subtd$`Date Installed`)

dates_from_subtd<-dates_from_subtd %>% mutate(diff_date=(Notif.date - `Date Installed`))

dates_from_subtd %>% group_by(Equipment) %>% summarise(Date=min(diff_date))
## # A tibble: 7 x 2
##   Equipment     Date
##       <chr>   <time>
## 1    11P19A 330 days
## 2    AGA951 322 days
## 3   AGA952A 414 days
## 4    AGA955 365 days
## 5   CGA201A 556 days
## 6     P301A 308 days
## 7   UGA302A 576 days
The above table gives us the the days between which the pump had fault for the first time
Can use %in% operator to selectively filter some of the values 
Kirlosker<-Kirlosker %>% select(`Notifictn type`,Equipment,Notif.date,`Date Installed`)

Kirlosker<-Kirlosker %>% mutate(diff_days=Notif.date -lead(Notif.date))

Kirlosker$diff_days<-as.numeric(Kirlosker$diff_days)

for(i in 1:(nrow(Kirlosker)-1)){
if(Kirlosker$diff_days[i] < 0)
{
Kirlosker$diff_days[i]<-0  
}
else{
Kirlosker$diff_days[i]<-Kirlosker$diff_days[i]
}
    
}
We need to manually put the dates which are set to zero because it has to be subtracted from the data installed
Although we can easily replace it with few rows I want to write code which is generic For this we have to know how many rows each category has and will then replace them will the last one because they are the ones which are zero or NA
insertionM<-(Kirlosker %>% group_by(Equipment) %>% tally())

# We get the count of each equipment and we know that last we have to calculate manually


Kirlosker[is.na(Kirlosker[,5]),5]<-0

# Now wherever there are zeroes we can replace them with the date installed subtracted from the first fault occurence

Kirlosker$`Date Installed`<-as.Date(Kirlosker$`Date Installed`)

for(i in 1:nrow(Kirlosker)){
  
  if(Kirlosker[i,5]==0){
    Kirlosker[i,5]<-Kirlosker[i,3]-Kirlosker[i,4]
    
  }
}
We will be doing the same for KSB and CompTurbine
dates_from_subtd<-KSB%>% group_by(Equipment) %>% summarise(Date=min(Notif.date))

#For every pump we got the min date and hence subtracting one year from the date we can get our date for analysis 


date_installed<-as.data.frame(paste((lubridate::year(dates_from_subtd$Date)-1),"04-01",sep="-"))
colnames(date_installed)<-"Date Installed"

dates_from_subtd<-cbind(dates_from_subtd,date_installed)
rm(date_installed)

dates_from_subtd<-dates_from_subtd[,c(1,3)]


KSB<-left_join(KSB,dates_from_subtd)
## Joining, by = "Equipment"
rm(dates_from_subtd)

dates_from_subtd<-KSB %>% select(Equipment,Notif.date,`Date Installed`)  

dates_from_subtd$`Date Installed`<-as.Date(dates_from_subtd$`Date Installed`)

dates_from_subtd<-dates_from_subtd %>% mutate(diff_date=(Notif.date - `Date Installed`))

dates_from_subtd %>% group_by(Equipment) %>% summarise(Date=min(diff_date))
## # A tibble: 7 x 2
##   Equipment     Date
##       <chr>   <time>
## 1    21P24A 332 days
## 2   AGA901A 380 days
## 3    P2001A 377 days
## 4     P701A 330 days
## 5     P702A 287 days
## 6  UGA1107A 448 days
## 7  UGA2201A 321 days
The above table gives us the the days between which the pump had fault for the first time
Can use %in% operator to selectively filter some of the values 
KSB<-KSB %>% select(`Notifictn type`,Equipment,Notif.date,`Date Installed`)

# to calculate difference between successive defaults

KSB<-KSB %>% mutate(diff_days=Notif.date -lead(Notif.date))

KSB$diff_days<-as.numeric(KSB$diff_days)

# We are running the loop max-1 because there is no lead for the last item which generates NA's and we know NA's cause lot of troubles in R

for(i in 1:(nrow(KSB)-1)){
if(KSB$diff_days[i] < 0)
{
KSB$diff_days[i]<-0  
}
else{
KSB$diff_days[i]<-KSB$diff_days[i]
}
    
}
We need to manually put the dates which are set to zero because it has to be subtracted from the data installed
Although we can easily replace it with few rows I want to write code which is generic For this we have to know how many rows each category has and will then replace them will the last one because they are the ones which are zero or NA
# setting the last NA to zero

KSB[is.na(KSB[,5]),5]<-0

# Now wherever there are zeroes we can replace them with the date installed subtracted from the first fault occurence

KSB$`Date Installed`<-as.Date(KSB$`Date Installed`)

for(i in 1:nrow(KSB)){
  
  if(KSB[i,5]==0){
    KSB[i,5]<-KSB[i,3]-KSB[i,4]
    
  }
}
NOTE: It might be a wrong practice to carry the dates installed as it is carrying the same data.
CompTurbine
dates_from_subtd<-CompTurbine%>% group_by(Equipment) %>% summarise(Date=min(Notif.date))

#For every pump we got the min date and hence subtracting one year from the date we can get our date for analysis 


date_installed<-as.data.frame(paste((lubridate::year(dates_from_subtd$Date)-1),"04-01",sep="-"))
colnames(date_installed)<-"Date Installed"

dates_from_subtd<-cbind(dates_from_subtd,date_installed)
rm(date_installed)

dates_from_subtd<-dates_from_subtd[,c(1,3)]


CompTurbine<-left_join(CompTurbine,dates_from_subtd)
## Joining, by = "Equipment"
rm(dates_from_subtd)

dates_from_subtd<-CompTurbine %>% select(Equipment,Notif.date,`Date Installed`)  

dates_from_subtd$`Date Installed`<-as.Date(dates_from_subtd$`Date Installed`)

dates_from_subtd<-dates_from_subtd %>% mutate(diff_date=(Notif.date - `Date Installed`))

dates_from_subtd %>% group_by(Equipment) %>% summarise(Date=min(diff_date))
## # A tibble: 4 x 2
##   Equipment     Date
##       <chr>   <time>
## 1     11K01 329 days
## 2    11TK01 282 days
## 3      K431 365 days
## 4     P607C 440 days
The above table gives us the the days between which the pump had fault for the first time
Can use %in% operator to selectively filter some of the values 
CompTurbine<-CompTurbine %>% select(`Notifictn type`,Equipment,Notif.date,`Date Installed`)

# to calculate difference between successive defaults

CompTurbine<-CompTurbine %>% mutate(diff_days=Notif.date -lead(Notif.date))

CompTurbine$diff_days<-as.numeric(CompTurbine$diff_days)

# We are running the loop max-1 because there is no lead for the last item which generates NA's and we know NA's cause lot of troubles in R

for(i in 1:(nrow(CompTurbine)-1)){
if(CompTurbine$diff_days[i] < 0)
{
CompTurbine$diff_days[i]<-0  
}
else{
CompTurbine$diff_days[i]<-CompTurbine$diff_days[i]
}
    
}
We need to manually put the dates which are set to zero because it has to be subtracted from the data installed
Although we can easily replace it with few rows I want to write code which is generic For this we have to know how many rows each category has and will then replace them will the last one because they are the ones which are zero or NA
# setting the last NA to zero

CompTurbine[is.na(CompTurbine[,5]),5]<-0

# Now wherever there are zeroes we can replace them with the date installed subtracted from the first fault occurence

CompTurbine$`Date Installed`<-as.Date(CompTurbine$`Date Installed`)

for(i in 1:nrow(CompTurbine)){
  
  if(CompTurbine[i,5]==0){
    CompTurbine[i,5]<-CompTurbine[i,3]-CompTurbine[i,4]
    
  }
}
NOTE: It might be a wrong practice to carry the dates installed as it is carrying the same data.
After calculating the fault frequency in terms of days for all the three equipments let us put the output in a nice format
knitr::kable(Kirlosker,caption = 'Kirlosker')
Kirlosker
Notifictn type	Equipment	Notif.date	Date Installed	diff_days
M1	AGA951	2017-01-11	2010-04-01	391
M1	AGA951	2015-12-17	2010-04-01	576
M1	AGA951	2014-05-20	2010-04-01	664
M1	AGA951	2012-07-25	2010-04-01	524
M1	AGA951	2011-02-17	2010-04-01	322
M1	AGA952A	2016-10-28	2008-04-01	27
M1	AGA952A	2016-10-01	2008-04-01	90
M1	AGA952A	2016-07-03	2008-04-01	122
M1	AGA952A	2016-03-03	2008-04-01	629
M1	AGA952A	2014-06-13	2008-04-01	314
M1	AGA952A	2013-08-03	2008-04-01	9
M1	AGA952A	2013-07-25	2008-04-01	562
M1	AGA952A	2012-01-10	2008-04-01	113
M1	AGA952A	2011-09-19	2008-04-01	767
M1	AGA952A	2009-08-13	2008-04-01	73
M1	AGA952A	2009-06-01	2008-04-01	12
M1	AGA952A	2009-05-20	2008-04-01	414
M1	AGA955	2017-06-07	2008-04-01	300
M1	AGA955	2016-08-11	2008-04-01	246
M1	AGA955	2015-12-09	2008-04-01	195
M1	AGA955	2015-05-28	2008-04-01	114
M1	AGA955	2015-02-03	2008-04-01	159
M1	AGA955	2014-08-28	2008-04-01	1179
M1	AGA955	2011-06-06	2008-04-01	298
M1	AGA955	2010-08-12	2008-04-01	285
M1	AGA955	2009-10-31	2008-04-01	211
M1	AGA955	2009-04-03	2008-04-01	2
M1	AGA955	2009-04-01	2008-04-01	365
Z1	CGA201A	2017-01-02	2008-04-01	621
M1	CGA201A	2015-04-22	2008-04-01	194
Z1	CGA201A	2014-10-10	2008-04-01	240
Z1	CGA201A	2014-02-12	2008-04-01	544
Z1	CGA201A	2012-08-17	2008-04-01	302
Z1	CGA201A	2011-10-20	2008-04-01	367
Z1	CGA201A	2010-10-18	2008-04-01	374
Z1	CGA201A	2009-10-09	2008-04-01	556
Z1	UGA302A	2016-05-24	2008-04-01	320
Z1	UGA302A	2015-07-09	2008-04-01	351
Z1	UGA302A	2014-07-23	2008-04-01	392
Z1	UGA302A	2013-06-26	2008-04-01	347
Z1	UGA302A	2012-07-14	2008-04-01	369
Z1	UGA302A	2011-07-11	2008-04-01	373
Z1	UGA302A	2010-07-03	2008-04-01	247
Z1	UGA302A	2009-10-29	2008-04-01	576
M1	11P19A	2017-05-18	2009-04-01	1074
M1	11P19A	2014-06-09	2009-04-01	154
M1	11P19A	2014-01-06	2009-04-01	113
M1	11P19A	2013-09-15	2009-04-01	52
M1	11P19A	2013-07-25	2009-04-01	172
M1	11P19A	2013-02-03	2009-04-01	85
M1	11P19A	2012-11-10	2009-04-01	101
M1	11P19A	2012-08-01	2009-04-01	14
M1	11P19A	2012-07-18	2009-04-01	65
M1	11P19A	2012-05-14	2009-04-01	389
M1	11P19A	2011-04-21	2009-04-01	420
M1	11P19A	2010-02-25	2009-04-01	330
M1	P301A	2017-02-16	2009-04-01	49
Z1	P301A	2016-12-29	2009-04-01	324
Z1	P301A	2016-02-09	2009-04-01	288
Z1	P301A	2015-04-27	2009-04-01	2217
M1	P301A	2015-04-27	2009-04-01	441
Z1	P301A	2014-02-10	2009-04-01	368
Z1	P301A	2013-02-07	2009-04-01	372
M1	P301A	2012-02-01	2009-04-01	16
Z1	P301A	2012-01-16	2009-04-01	349
Z1	P301A	2011-02-01	2009-04-01	363
Z1	P301A	2010-02-03	2009-04-01	308
knitr::kable(KSB,caption = 'KSB')
KSB
Notifictn type	Equipment	Notif.date	Date Installed	diff_days
Z3	21P24A	2016-10-25	2008-04-01	976
Z3	21P24A	2014-02-22	2008-04-01	676
Z3	21P24A	2012-04-17	2008-04-01	353
Z3	21P24A	2011-04-30	2008-04-01	387
Z3	21P24A	2010-04-08	2008-04-01	382
M1	21P24A	2009-03-22	2008-04-01	23
Z3	21P24A	2009-02-27	2008-04-01	332
Z3	P701A	2016-10-18	2008-04-01	711
M1	P701A	2014-11-07	2008-04-01	242
M1	P701A	2014-03-10	2008-04-01	703
Z3	P701A	2012-04-06	2008-04-01	343
Z3	P701A	2011-04-29	2008-04-01	384
Z3	P701A	2010-04-10	2008-04-01	409
Z3	P701A	2009-02-25	2008-04-01	330
Z1	P702A	2017-02-24	2008-04-01	263
M1	P702A	2016-06-06	2008-04-01	2988
M1	P702A	2016-07-07	2008-04-01	26
M1	P702A	2016-06-11	2008-04-01	130
Z1	P702A	2016-02-02	2008-04-01	376
Z1	P702A	2015-01-22	2008-04-01	365
Z1	P702A	2014-01-22	2008-04-01	330
Z1	P702A	2013-02-26	2008-04-01	473
Z1	P702A	2011-11-11	2008-04-01	330
Z1	P702A	2010-12-16	2008-04-01	315
Z1	P702A	2010-02-04	2008-04-01	387
Z1	P702A	2009-01-13	2008-04-01	287
M1	P2001A	2017-05-29	2010-04-01	24
M1	P2001A	2017-05-05	2010-04-01	710
M1	P2001A	2015-05-26	2010-04-01	1504
M1	P2001A	2011-04-13	2010-04-01	377
Z1	AGA901A	2017-04-07	2008-04-01	367
Z1	AGA901A	2016-04-05	2008-04-01	780
M1	AGA901A	2014-02-15	2008-04-01	304
M1	AGA901A	2013-04-17	2008-04-01	489
M1	AGA901A	2011-12-15	2008-04-01	358
M1	AGA901A	2010-12-22	2008-04-01	615
Z1	AGA901A	2009-04-16	2008-04-01	380
Z1	UGA1107A	2017-04-12	2008-04-01	190
Z1	UGA1107A	2016-10-04	2008-04-01	599
Z1	UGA1107A	2015-02-13	2008-04-01	663
Z1	UGA1107A	2013-04-21	2008-04-01	408
Z1	UGA1107A	2012-03-09	2008-04-01	283
Z1	UGA1107A	2011-05-31	2008-04-01	354
Z1	UGA1107A	2010-06-11	2008-04-01	353
Z1	UGA1107A	2009-06-23	2008-04-01	448
M1	UGA2201A	2017-06-01	2008-04-01	415
Z1	UGA2201A	2016-04-12	2008-04-01	357
Z1	UGA2201A	2015-04-21	2008-04-01	313
M1	UGA2201A	2014-06-12	2008-04-01	23
M1	UGA2201A	2014-05-20	2008-04-01	13
Z1	UGA2201A	2014-05-07	2008-04-01	332
M1	UGA2201A	2013-06-09	2008-04-01	16
Z1	UGA2201A	2013-05-24	2008-04-01	252
M1	UGA2201A	2012-09-14	2008-04-01	136
M1	UGA2201A	2012-05-01	2008-04-01	705
Z1	UGA2201A	2010-05-27	2008-04-01	5
Z1	UGA2201A	2010-05-22	2008-04-01	167
M1	UGA2201A	2009-12-06	2008-04-01	74
M1	UGA2201A	2009-09-23	2008-04-01	127
Z1	UGA2201A	2009-05-19	2008-04-01	92
M1	UGA2201A	2009-02-16	2008-04-01	321
knitr::kable(CompTurbine,caption = 'CompTurbine')
CompTurbine
Notifictn type	Equipment	Notif.date	Date Installed	diff_days
M1	P607C	2016-10-13	2008-04-01	104
M1	P607C	2016-07-01	2008-04-01	88
Z1	P607C	2016-04-04	2008-04-01	365
Z1	P607C	2015-04-05	2008-04-01	43
Z1	P607C	2015-02-21	2008-04-01	688
Z1	P607C	2013-04-04	2008-04-01	401
Z1	P607C	2012-02-28	2008-04-01	988
Z1	P607C	2009-06-15	2008-04-01	440
Z1	P607C	2011-02-05	2008-04-01	1040
M1	P607C	2014-08-16	2008-04-01	184
M1	P607C	2014-02-13	2008-04-01	188
M1	P607C	2013-08-09	2008-04-01	119
M1	P607C	2013-04-12	2008-04-01	8
Z1	P607C	2013-04-04	2008-04-01	336
M1	P607C	2012-05-03	2008-04-01	162
M1	P607C	2011-11-23	2008-04-01	156
M1	P607C	2011-06-20	2008-04-01	90
M1	P607C	2011-03-22	2008-04-01	1085
M1	11K01	2020-09-29	2019-04-01	183
M1	11K01	2020-03-30	2019-04-01	35
M1	11K01	2020-02-24	2019-04-01	329
M1	11K01	2020-12-12	2019-04-01	170
Z3	11K01	2020-06-25	2019-04-01	46
Z3	11K01	2020-05-10	2019-04-01	405
M1	11K01	2020-12-04	2019-04-01	244
Z3	11K01	2020-04-04	2019-04-01	369
Z3	11K01	2020-04-04	2019-04-01	369
M1	11K01	2020-08-30	2019-04-01	96
M1	11K01	2020-05-26	2019-04-01	22
M1	11K01	2020-05-04	2019-04-01	3
M1	11K01	2020-05-01	2019-04-01	33
Z3	11K01	2020-03-29	2019-04-01	363
Z3	11K01	2020-03-29	2019-04-01	1606
M1	K431	2015-11-05	2009-04-01	622
M1	K431	2014-02-21	2009-04-01	155
M1	K431	2013-09-19	2009-04-01	24
M1	K431	2013-08-26	2009-04-01	65
M1	K431	2013-06-22	2009-04-01	78
M1	K431	2013-04-05	2009-04-01	228
M1	K431	2012-08-20	2009-04-01	31
M1	K431	2012-07-20	2009-04-01	1206
M1	K431	2012-07-20	2009-04-01	86
M1	K431	2012-04-25	2009-04-01	44
M1	K431	2012-03-12	2009-04-01	265
M1	K431	2011-06-21	2009-04-01	205
M1	K431	2010-11-28	2009-04-01	606
M1	K431	2010-11-28	2009-04-01	146
M1	K431	2010-07-05	2009-04-01	95
M1	K431	2010-04-01	2009-04-01	365
Z3	11TK01	2020-02-02	2019-04-01	307
Z3	11TK01	2020-02-02	2019-04-01	307
M1	11TK01	2020-11-25	2019-04-01	9
M1	11TK01	2020-11-16	2019-04-01	4
M1	11TK01	2020-11-12	2019-04-01	115
M1	11TK01	2020-07-20	2019-04-01	37
Z3	11TK01	2020-06-13	2019-04-01	2
M1	11TK01	2020-06-11	2019-04-01	80
Z3	11TK01	2020-03-23	2019-04-01	357
Z3	11TK01	2020-03-23	2019-04-01	357
Z3	11TK01	2020-03-23	2019-04-01	5
Z3	11TK01	2020-03-18	2019-04-01	352
Z3	11TK01	2020-03-18	2019-04-01	12
Z3	11TK01	2020-03-06	2019-04-01	340
M1	11TK01	2020-07-11	2019-04-01	146
M1	11TK01	2020-02-16	2019-04-01	30
Z3	11TK01	2020-01-17	2019-04-01	291
Z3	11TK01	2020-12-09	2019-04-01	74
M1	11TK01	2020-09-26	2019-04-01	4
M1	11TK01	2020-09-22	2019-04-01	24
M1	11TK01	2020-08-29	2019-04-01	64
M1	11TK01	2020-06-26	2019-04-01	9
M1	11TK01	2020-06-17	2019-04-01	1
M1	11TK01	2020-06-16	2019-04-01	9
M1	11TK01	2020-06-07	2019-04-01	10
M1	11TK01	2020-05-28	2019-04-01	3
M1	11TK01	2020-05-25	2019-04-01	420
M1	11TK01	2020-05-25	2019-04-01	19
Z3	11TK01	2020-05-06	2019-04-01	1
Z3	11TK01	2020-05-05	2019-04-01	38
M1	11TK01	2020-03-28	2019-04-01	3
M1	11TK01	2020-03-25	2019-04-01	73
M1	11TK01	2020-01-12	2019-04-01	286
Z3	11TK01	2020-12-01	2019-04-01	33
Z3	11TK01	2020-10-29	2019-04-01	577
Z3	11TK01	2020-10-29	2019-04-01	46
M1	11TK01	2020-09-13	2019-04-01	531
Z3	11TK01	2020-09-13	2019-04-01	32
M1	11TK01	2020-08-12	2019-04-01	18
Z3	11TK01	2020-07-25	2019-04-01	26
Z3	11TK01	2020-06-29	2019-04-01	4
Z3	11TK01	2020-06-25	2019-04-01	19
Z3	11TK01	2020-06-06	2019-04-01	54
M1	11TK01	2020-04-13	2019-04-01	96
Z3	11TK01	2020-01-08	2019-04-01	282
Z3	11TK01	2020-01-08	2019-04-01	282
M1	11TK01	2020-10-31	2019-04-01	38
M1	11TK01	2020-09-23	2019-04-01	68
M1	11TK01	2020-07-17	2019-04-01	33
M1	11TK01	2020-06-14	2019-04-01	43
M1	11TK01	2020-05-02	2019-04-01	10
M1	11TK01	2020-04-22	2019-04-01	72
Z3	11TK01	2020-02-10	2019-04-01	9
M1	11TK01	2020-02-01	2019-04-01	23
Z3	11TK01	2020-01-09	2019-04-01	283
M1	11TK01	2020-12-05	2019-04-01	92
Z3	11TK01	2020-09-04	2019-04-01	55
M1	11TK01	2020-07-11	2019-04-01	467
M1	11TK01	2020-07-11	2019-04-01	20
Z3	11TK01	2020-06-21	2019-04-01	1
Z3	11TK01	2020-06-20	2019-04-01	25
M1	11TK01	2020-05-26	2019-04-01	7
M1	11TK01	2020-05-19	2019-04-01	10
M1	11TK01	2020-05-09	2019-04-01	39
Z3	11TK01	2020-03-31	2019-04-01	34
M1	11TK01	2020-02-26	2019-04-01	331
M1	11TK01	2020-09-28	2019-04-01	4
M1	11TK01	2020-09-24	2019-04-01	121
M1	11TK01	2020-05-26	2019-04-01	25
M1	11TK01	2020-05-01	2019-04-01	396
M1	11TK01	2020-05-01	2019-04-01	4
M1	11TK01	2020-04-27	2019-04-01	24
M1	11TK01	2020-04-03	2019-04-01	59
M1	11TK01	2020-02-04	2019-04-01	309
M1	11TK01	2020-12-12	2019-04-01	30
Z3	11TK01	2020-11-12	2019-04-01	111
M1	11TK01	2020-07-24	2019-04-01	15
M1	11TK01	2020-07-09	2019-04-01	76
M1	11TK01	2020-04-24	2019-04-01	95
M1	11TK01	2020-01-20	2019-04-01	294

