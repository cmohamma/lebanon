library(dplyr)
library(lubridate)
library(ggplot2)
library(scales)
library(gridExtra)
library(zoo)
library(xts)
library(rgdal)
library(stringr)
library(tidyr)
library(reshape2)
library(scales)

refugeedata<-read.csv('refugee.databb.csv')
refugee_data<-refugeedata %>%
  select(Caza_Name, YearMonthDay, Syrian_Ref) %>%
  mutate(date=ymd(refugeedata$YearMonthDay))
refugee_data$date<-str_replace_all(refugee_data$date, 
                                   "2013-06-10", "2013-06-30") #One off date

colnames(refugee_data)[1]<-"district"

refugee_data$month<-month(refugee_data$date, label=T)
refugee_data$year<-year(refugee_data$date)
refugee_data$month.year<-paste0(refugee_data$month, sep = "-", refugee_data$year)
refugee_data<-filter(refugee_data, month.year!="Mar-2013")

#Line up district names ID for merging later
refugee_data$district<-as.character(refugee_data$district)
refugee_data$district<-str_replace_all(refugee_data$district, "Bcharre", "Bcharré")
refugee_data$district<-str_replace_all(refugee_data$district, "El Meten", "Matn")
refugee_data$district<-str_replace_all(refugee_data$district, "El Nabatieh", "Nabatiyeh")
refugee_data$district<-str_replace_all(refugee_data$district, "Bent Jbeil", "Bent Jbayl")
refugee_data$district<-str_replace_all(refugee_data$district, "El Minieh Dennie", "Minie-Danniyeh")
refugee_data$district<-str_replace_all(refugee_data$district, "Jbeil", "Jbayl")
refugee_data$district<-str_replace_all(refugee_data$district, "Kesrwane", "Kesrouan")
refugee_data$district<-str_replace_all(refugee_data$district, "West Bekaa", "Bekaa-West")
refugee_data$district<-str_replace_all(refugee_data$district, "El Batroun", "Batroun")
refugee_data$district<-str_replace_all(refugee_data$district, "El Hermel", "Hermel")
refugee_data$district<-str_replace_all(refugee_data$district, "El Koura", "Koura")
refugee_data$district<-str_replace_all(refugee_data$district, "Zahle", "Zahlé")

refugee_data<-arrange(refugee_data, date)

##interpolation
Akkar<-refugee_data %>%
  filter(district=='Akkar')
Akkar$count<-na.approx(zoo(Akkar$Syrian_Ref))

Aley<-refugee_data %>%
  filter(district=='Aley')
Aley$count<-na.approx(zoo(Aley$Syrian_Ref))

Batroun<-refugee_data %>%
  filter(district=='Batroun')
Batroun$count<-na.approx(zoo(Batroun$Syrian_Ref))

Baabda<-refugee_data %>%
  filter(district=='Baabda')
Baabda$count<-na.approx(zoo(Baabda$Syrian_Ref))

Baalbek<-refugee_data %>%
  filter(district=='Baalbek')
Baalbek$count<-na.approx(zoo(Baalbek$Syrian_Ref))

Bcharré<-refugee_data %>%
  filter(district=='Bcharré')
Bcharré$count<-na.approx(zoo(Bcharré$Syrian_Ref))

Beirut<-refugee_data %>%
  filter(district=='Beirut')
Beirut$count<-na.approx(zoo(Beirut$Syrian_Ref))

BentJbayl<-refugee_data %>%
  filter(district=='Bent Jbayl')
BentJbayl$count<-na.approx(zoo(BentJbayl$Syrian_Ref))

Chouf<-refugee_data %>%
  filter(district=='Chouf')
Chouf$count<-na.approx(zoo(Chouf$Syrian_Ref))

Hermel<-refugee_data %>%
  filter(district=='Hermel')
Hermel$count<-na.approx(zoo(Hermel$Syrian_Ref))

Koura<-refugee_data %>%
  filter(district=='Koura')
Koura$count<-na.approx(zoo(Koura$Syrian_Ref))

Matn<-refugee_data %>%
  filter(district=='Matn')
Matn$count<-na.approx(zoo(Matn$Syrian_Ref))

MinieDanniyeh<-refugee_data %>%
  filter(district=='Minie-Danniyeh')
MinieDanniyeh$count<-na.approx(zoo(MinieDanniyeh$Syrian_Ref))

Nabatiyeh<-refugee_data %>%
  filter(district=='Nabatiyeh')
Nabatiyeh$count<-na.approx(zoo(Nabatiyeh$Syrian_Ref))

Hasbaya<-refugee_data %>%
  filter(district=='Hasbaya')
Hasbaya$count<-na.approx(zoo(Hasbaya$Syrian_Ref))

Jbayl<-refugee_data %>%
  filter(district=='Jbayl')
Jbayl$count<-na.approx(zoo(Jbayl$Syrian_Ref))

Jezzine<-refugee_data %>%
  filter(district=='Jezzine')
Jezzine$count<-na.approx(zoo(Jezzine$Syrian_Ref))

Kesrouan<-refugee_data %>%
  filter(district=='Kesrouan')
Kesrouan$count<-na.approx(zoo(Kesrouan$Syrian_Ref))

Marjayoun<-refugee_data %>%
  filter(district=='Marjayoun')
Marjayoun$count<-na.approx(zoo(Marjayoun$Syrian_Ref))

Rachaya<-refugee_data %>%
  filter(district=='Rachaya')
Rachaya$count<-na.approx(zoo(Rachaya$Syrian_Ref))

Saida<-refugee_data %>%
  filter(district=='Saida')
Saida$count<-na.approx(zoo(Saida$Syrian_Ref))

Sour<-refugee_data %>%
  filter(district=='Sour')
Sour$count<-na.approx(zoo(Sour$Syrian_Ref))

Tripoli<-refugee_data %>%
  filter(district=='Tripoli')
Tripoli$count<-na.approx(zoo(Tripoli$Syrian_Ref))

BekaaWest<-refugee_data %>%
  filter(district=='Bekaa-West')
BekaaWest$count<-na.approx(zoo(BekaaWest$Syrian_Ref))

Zahle<-refugee_data %>%
  filter(district=='Zahlé')
Zahle$count<-na.approx(zoo(Zahle$Syrian_Ref))

Zgharta<-refugee_data %>%
  filter(district=='Zgharta')
Zgharta$count<-na.approx(zoo(Zgharta$Syrian_Ref))

refugee_data<-rbind(Akkar, Aley, Baabda, Baalbek, Bcharré, Beirut, BentJbayl, Chouf, Batroun, Hermel, Koura, Matn, MinieDanniyeh, Nabatiyeh, Hasbaya, Jbayl, Jezzine, Kesrouan, Marjayoun, Rachaya, Saida, Sour, Tripoli, BekaaWest, Zahle,Zgharta)

refugee_data$Syrian_Ref<-as.numeric(refugee_data$count)
##refugee_data<-arrange(refugee_data, date)##maybe

ref_graph<-refugee_data %>%
  group_by(month.year, district, date) %>%
  summarise(count=sum(Syrian_Ref))
ref_graph<-as.data.frame(ref_graph)
ref_graph<-arrange(ref_graph, date)

lebmap <- readOGR(dsn = "LBN_adm-2/", "LBN_adm2")
lebmap.f <- fortify(lebmap, region = "ID_2")
lebmap.f <- merge(lebmap.f, lebmap@data, by.x = "id", by.y = "ID_2")
lebmap.f$district<-lebmap.f$NAME_2

#Population data/district
##Population data needs to be fixed

refugee_data<-arrange(refugee_data, date, district)

p<-c(198000,105000,502222,167000,50000,97000,433000,56000,53000,81000,19000,
     43000,82500,15000,123600,70000,41000,702000,96000,92000,51000,
     207500,238000,228000,253000,70000)
pop<-c(p,p,p,p,p,p,p,p,p,p,p,p,p,p,p,p,p,p,p,p,p,p,p,p,p)
refugee_data$pop<-pop

refugee_data$date<-as.Date(refugee_data$date)

refugee_data$month<-month(refugee_data$date, label=T)
refugee_data$year<-year(refugee_data$date)
refugee_data$month.year<-paste0(refugee_data$month, sep = "-", refugee_data$year)

refugee_data$month.year <- factor(refugee_data$month.year, levels = c("Apr-2013", "May-2013", "Jun-2013", "Jul-2013", "Aug-2013", "Sep-2013","Oct-2013","Nov-2013","Dec-2013","Jan-2014","Feb-2014","Mar-2014","Apr-2014", "May-2014", "Jun-2014","Jul-2014","Aug-2014","Sep-2014","Oct-2014","Nov-2014","Dec-2014","Jan-2015","Feb-2015","Mar-2015","Apr-2015"))

refugee_data$month<-month(refugee_data$date, label=T)
refugee_data$year<-year(refugee_data$date)
refugee_data$month.year<-paste0(refugee_data$month, sep = "-", refugee_data$year)

refugee_data$month.year <- factor(refugee_data$month.year, levels = c("Apr-2013", "May-2013", "Jun-2013", "Jul-2013", "Aug-2013", "Sep-2013","Oct-2013","Nov-2013","Dec-2013","Jan-2014","Feb-2014","Mar-2014","Apr-2014", "May-2014", "Jun-2014","Jul-2014","Aug-2014","Sep-2014","Oct-2014","Nov-2014","Dec-2014","Jan-2015","Feb-2015","Mar-2015","Apr-2015"))

refugee_data$count<-as.numeric(refugee_data$count)


lebmap <- readOGR(dsn = "LBN_adm-2/", "LBN_adm2",verbose = FALSE)
lebmap$district<-lebmap$NAME_2
dissdata<-read.csv('lebanondata.april.csv')

data<-dissdata %>%
  select(date, actor, actor_group, 
         target, target_group, category,
         classification, arrest, injury, dead, 
         district, latitude, longitude) %>%
  filter(date != '1-May-15') %>% 
  mutate(date=dmy(date))

#Line up district names ID for merging later
data$district<-as.character(data$district)
data$district<-str_replace_all(data$district, "Bcharre", "Bcharré")
data$district<-str_replace_all(data$district, "El Meten", "Matn")
data$district<-str_replace_all(data$district, "El Nabatieh", "Nabatiyeh")
data$district<-str_replace_all(data$district, "Bent Jbeil", "Bent Jbayl")
data$district<-str_replace_all(data$district, "El Minieh-Dennie", "Minie-Danniyeh")
data$district<-str_replace_all(data$district, "Jbeil", "Jbayl")
data$district<-str_replace_all(data$district, "Kesrwane", "Kesrouan")
data$district<-str_replace_all(data$district, "West Bekaa", "Bekaa-West")
data$district<-str_replace_all(data$district, "Marjaayoun", "Marjayoun")
data$district<-str_replace_all(data$district, "El Batroun", "Batroun")
data$district<-str_replace_all(data$district, "El Hermel", "Hermel")
data$district<-str_replace_all(data$district, "El Koura", "Koura")
data$district<-str_replace_all(data$district, "Zahle", "Zahlé")

data$month<-month(data$date, label=T)
data$year<-year(data$date)
data$month.year<-paste0(data$month, sep = "-", data$year)

refugee_data$Syrian_Ref<-as.numeric(refugee_data$Syrian_Ref)
##refugee_data<-arrange(refugee_data, date)##maybe
##refugee_data<-filter(refugee_data, month.year!="Mar-2013")##maybe

data<-data %>%
  group_by(district, month.year)  %>%
  summarize(count=n())
data<-as.data.frame(data)

data2<-merge(data,refugee_data, by=c("month.year","district"), all=T) 
data2[is.na(data2)] <- 0
data2$count.x<-as.numeric(data2$count.x)

lebmap <- readOGR(dsn = "LBN_adm-2/", "LBN_adm2")
lebmap.f <- ggplot2::fortify(lebmap, region = "ID_2")
lebmap.f <- merge(lebmap.f, lebmap@data, by.x = "id", by.y = "ID_2")
lebmap.f$district<-lebmap.f$NAME_2

plot.data<- merge(lebmap.f, data2, by.x = "district", by.y = "district")
plot.data$conflict<-plot.data$count.x



sectdata<-read.csv('lebsect.csv')
sectdata$district<-as.character(sectdata$district)
sectdata[6,1]<-'Bcharré'
sectdata[25,1]<-'Zahlé'


plot.data.sect <- merge(lebmap.f, sectdata, by.x = "district", by.y = "district")

refugeedata<-read.csv('refugee.databb.csv')
refugee_data<-refugeedata %>%
  select(Caza_Name, YearMonthDay, Syrian_Ref) %>%
  mutate(date=ymd(refugeedata$YearMonthDay))
refugee_data$date<-str_replace_all(refugee_data$date, 
                                   "2013-06-10", "2013-06-30") #One off date

colnames(refugee_data)[1]<-"district"

refugee_data$month<-month(refugee_data$date, label=T)
refugee_data$year<-year(refugee_data$date)
refugee_data$month.year<-paste0(refugee_data$month, sep = "-", refugee_data$year)
refugee_data<-filter(refugee_data, month.year!="Mar-2013")

#Line up district names ID for merging later
refugee_data$district<-as.character(refugee_data$district)
refugee_data$district<-str_replace_all(refugee_data$district, "Bcharre", "Bcharré")
refugee_data$district<-str_replace_all(refugee_data$district, "El Meten", "Matn")
refugee_data$district<-str_replace_all(refugee_data$district, "El Nabatieh", "Nabatiyeh")
refugee_data$district<-str_replace_all(refugee_data$district, "Bent Jbeil", "Bent Jbayl")
refugee_data$district<-str_replace_all(refugee_data$district, "El Minieh Dennie", "Minie-Danniyeh")
refugee_data$district<-str_replace_all(refugee_data$district, "Jbeil", "Jbayl")
refugee_data$district<-str_replace_all(refugee_data$district, "Kesrwane", "Kesrouan")
refugee_data$district<-str_replace_all(refugee_data$district, "West Bekaa", "Bekaa-West")
refugee_data$district<-str_replace_all(refugee_data$district, "El Batroun", "Batroun")
refugee_data$district<-str_replace_all(refugee_data$district, "El Hermel", "Hermel")
refugee_data$district<-str_replace_all(refugee_data$district, "El Koura", "Koura")
refugee_data$district<-str_replace_all(refugee_data$district, "Zahle", "Zahlé")

refugee_data<-arrange(refugee_data, date)

##interpolation
Akkar<-refugee_data %>%
  filter(district=='Akkar')
Akkar$count<-na.approx(zoo(Akkar$Syrian_Ref))

Aley<-refugee_data %>%
  filter(district=='Aley')
Aley$count<-na.approx(zoo(Aley$Syrian_Ref))

Batroun<-refugee_data %>%
  filter(district=='Batroun')
Batroun$count<-na.approx(zoo(Batroun$Syrian_Ref))

Baabda<-refugee_data %>%
  filter(district=='Baabda')
Baabda$count<-na.approx(zoo(Baabda$Syrian_Ref))

Baalbek<-refugee_data %>%
  filter(district=='Baalbek')
Baalbek$count<-na.approx(zoo(Baalbek$Syrian_Ref))

Bcharré<-refugee_data %>%
  filter(district=='Bcharré')
Bcharré$count<-na.approx(zoo(Bcharré$Syrian_Ref))

Beirut<-refugee_data %>%
  filter(district=='Beirut')
Beirut$count<-na.approx(zoo(Beirut$Syrian_Ref))

BentJbayl<-refugee_data %>%
  filter(district=='Bent Jbayl')
BentJbayl$count<-na.approx(zoo(BentJbayl$Syrian_Ref))

Chouf<-refugee_data %>%
  filter(district=='Chouf')
Chouf$count<-na.approx(zoo(Chouf$Syrian_Ref))

Hermel<-refugee_data %>%
  filter(district=='Hermel')
Hermel$count<-na.approx(zoo(Hermel$Syrian_Ref))

Koura<-refugee_data %>%
  filter(district=='Koura')
Koura$count<-na.approx(zoo(Koura$Syrian_Ref))

Matn<-refugee_data %>%
  filter(district=='Matn')
Matn$count<-na.approx(zoo(Matn$Syrian_Ref))

MinieDanniyeh<-refugee_data %>%
  filter(district=='Minie-Danniyeh')
MinieDanniyeh$count<-na.approx(zoo(MinieDanniyeh$Syrian_Ref))

Nabatiyeh<-refugee_data %>%
  filter(district=='Nabatiyeh')
Nabatiyeh$count<-na.approx(zoo(Nabatiyeh$Syrian_Ref))

Hasbaya<-refugee_data %>%
  filter(district=='Hasbaya')
Hasbaya$count<-na.approx(zoo(Hasbaya$Syrian_Ref))

Jbayl<-refugee_data %>%
  filter(district=='Jbayl')
Jbayl$count<-na.approx(zoo(Jbayl$Syrian_Ref))

Jezzine<-refugee_data %>%
  filter(district=='Jezzine')
Jezzine$count<-na.approx(zoo(Jezzine$Syrian_Ref))

Kesrouan<-refugee_data %>%
  filter(district=='Kesrouan')
Kesrouan$count<-na.approx(zoo(Kesrouan$Syrian_Ref))

Marjayoun<-refugee_data %>%
  filter(district=='Marjayoun')
Marjayoun$count<-na.approx(zoo(Marjayoun$Syrian_Ref))

Rachaya<-refugee_data %>%
  filter(district=='Rachaya')
Rachaya$count<-na.approx(zoo(Rachaya$Syrian_Ref))

Saida<-refugee_data %>%
  filter(district=='Saida')
Saida$count<-na.approx(zoo(Saida$Syrian_Ref))

Sour<-refugee_data %>%
  filter(district=='Sour')
Sour$count<-na.approx(zoo(Sour$Syrian_Ref))

Tripoli<-refugee_data %>%
  filter(district=='Tripoli')
Tripoli$count<-na.approx(zoo(Tripoli$Syrian_Ref))

BekaaWest<-refugee_data %>%
  filter(district=='Bekaa-West')
BekaaWest$count<-na.approx(zoo(BekaaWest$Syrian_Ref))

Zahle<-refugee_data %>%
  filter(district=='Zahlé')
Zahle$count<-na.approx(zoo(Zahle$Syrian_Ref))

Zgharta<-refugee_data %>%
  filter(district=='Zgharta')
Zgharta$count<-na.approx(zoo(Zgharta$Syrian_Ref))

refugee_data<-rbind(Akkar, Aley, Baabda, Baalbek, Bcharré, Beirut, BentJbayl, Chouf, Batroun, Hermel, Koura, Matn, MinieDanniyeh, Nabatiyeh, Hasbaya, Jbayl, Jezzine, Kesrouan, Marjayoun, Rachaya, Saida, Sour, Tripoli, BekaaWest, Zahle,Zgharta)

refugee_data<-arrange(refugee_data, date, district)

p<-c(198000,105000,502222,167000,50000,97000,433000,56000,53000,81000,19000,
     43000,82500,15000,123600,70000,41000,702000,96000,92000,51000,
     207500,238000,228000,253000,70000)
pop<-c(p,p,p,p,p,p,p,p,p,p,p,p,p,p,p,p,p,p,p,p,p,p,p,p,p)
refugee_data$pop<-pop

refugee_data$date<-as.Date(refugee_data$date)

refugee_data$month<-month(refugee_data$date, label=T)
refugee_data$year<-year(refugee_data$date)
refugee_data$month.year<-paste0(refugee_data$month, sep = "-", refugee_data$year)

refugee_data$month.year <- factor(refugee_data$month.year, levels = c("Apr-2013", "May-2013", "Jun-2013", "Jul-2013", "Aug-2013", "Sep-2013","Oct-2013","Nov-2013","Dec-2013","Jan-2014","Feb-2014","Mar-2014","Apr-2014", "May-2014", "Jun-2014","Jul-2014","Aug-2014","Sep-2014","Oct-2014","Nov-2014","Dec-2014","Jan-2015","Feb-2015","Mar-2015","Apr-2015"))

refugee_data$month<-month(refugee_data$date, label=T)
refugee_data$year<-year(refugee_data$date)
refugee_data$month.year<-paste0(refugee_data$month, sep = "-", refugee_data$year)

refugee_data$month.year <- factor(refugee_data$month.year, levels = c("Apr-2013", "May-2013", "Jun-2013", "Jul-2013", "Aug-2013", "Sep-2013","Oct-2013","Nov-2013","Dec-2013","Jan-2014","Feb-2014","Mar-2014","Apr-2014", "May-2014", "Jun-2014","Jul-2014","Aug-2014","Sep-2014","Oct-2014","Nov-2014","Dec-2014","Jan-2015","Feb-2015","Mar-2015","Apr-2015"))

refugee_data$count<-as.numeric(refugee_data$count)

#__________

dissdata<-read.csv('lebanondata.april.csv')
conflict_data<-dissdata %>%
  select(date, actor, actor_group, 
         target, target_group, category,
         classification, arrest, injury, dead, 
         district, latitude, longitude) %>%
  filter(date != '1-May-15') %>% 
  mutate(date=dmy(date))

viol.domestic.events<-conflict_data %>%
  filter(category=='exect'| category=='bombard'| category=='crossclash' |
           category=='assault'|category=='shooting'| 
           category=='clashes'|category=='suicbatt'|
           category=='bombatt' | category=='aerbom' | 
           injury > 0 | dead > 0) %>%
  filter(classification!='isrcrosscon') 

viol.domestic.events$month<-month(viol.domestic.events$date, label=T)
viol.domestic.events$year<-year(viol.domestic.events$date)
viol.domestic.events$month.year<-paste0(viol.domestic.events$month, sep = "-", viol.domestic.events$year)

viol.domestic.events<-arrange(viol.domestic.events, date)

data<-viol.domestic.events %>%
  group_by(district, month.year)  %>%
  summarize(count=n())
data<-as.data.frame(data)

data_wide <- spread(data, month.year, count)
data_wide[is.na(data_wide)] <- 0

data_wide$district<-as.character(data_wide$district)
data_wide[25,1]<-"Bcharré"
data_wide[26,1]<-"BentJbayl"
data_wide[25,2:26]<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
data_wide[26,2:26]<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)

data_wide$district<-str_replace_all(data_wide$district, "Bcharre", "Bcharré")
data_wide$district<-str_replace_all(data_wide$district, "El Meten", "Matn")
data_wide$district<-str_replace_all(data_wide$district, "El Nabatieh", "Nabatiyeh")
data_wide$district<-str_replace_all(data_wide$district, "Bent Jbeil", "BentJbayl")
data_wide$district<-str_replace_all(data_wide$district, "El Minieh-Dennie", "MinieDanniyeh")
data_wide$district<-str_replace_all(data_wide$district, "Jbeil", "Jbayl")
data_wide$district<-str_replace_all(data_wide$district, "Kesrwane", "Kesrouan")
data_wide$district<-str_replace_all(data_wide$district, "West Bekaa", "BekaaWest")
data_wide$district<-str_replace_all(data_wide$district, "Marjaayoun", "Marjayoun")
data_wide$district<-str_replace_all(data_wide$district, "El Batroun", "Batroun")
data_wide$district<-str_replace_all(data_wide$district, "El Hermel", "Hermel")
data_wide$district<-str_replace_all(data_wide$district, "El Koura", "Koura")
data_wide$district<-str_replace_all(data_wide$district, "Zahle", "Zahlé")

data_wide<-arrange(data_wide, district)
data_wide <- data_wide[ , c(1,2,19,15,13,5,25,23,21,7,11,9,17,3,20,16,14,6,26,24,22,8,12,10,18,4)]

data.melt.viol <- melt(data_wide, id = c("district"))
colnames(data.melt.viol)[2]<-'month.year'



lebmap <- readOGR(dsn = "LBN_adm-2/", "LBN_adm2")
lebmap.f <- fortify(lebmap, region = "ID_2")
lebmap.f <- merge(lebmap.f, lebmap@data, by.x = "id", by.y = "ID_2")
lebmap.f$district<-lebmap.f$NAME_2
plot.data.viol <- merge(lebmap.f, data.melt.viol, by.x = "district", by.y = "district")

#_____________________________________________

dissdata<-read.csv('lebanondata.april.csv')
conflict_data<-dissdata %>%
  select(date, actor, actor_group, 
         target, target_group, category,
         classification, arrest, injury, dead, 
         district, latitude, longitude) %>%
  filter(date != '1-May-15') %>% 
  mutate(date=dmy(date))

conflict_data$month<-month(conflict_data$date, label=T)
conflict_data$year<-year(conflict_data$date)
conflict_data$month.year<-paste0(conflict_data$month, sep = "-", conflict_data$year)

arrests.graph<-conflict_data %>%
  filter(arrest>0)  %>%
  group_by(district, month.year) %>%
  summarize(arrest.sum=sum(arrest))
arrests.graph<-as.data.frame(arrests.graph)

data_wide <- spread(arrests.graph, month.year, arrest.sum)
data_wide[is.na(data_wide)] <- 0

data_wide$district<-as.character(data_wide$district)
data_wide[25,1]<-"Bcharré"
data_wide[26,1]<-"BentJbayl"
data_wide[25,2:26]<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
data_wide[26,2:26]<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)

data_wide$district<-str_replace_all(data_wide$district, "Bcharre", "Bcharré")
data_wide$district<-str_replace_all(data_wide$district, "El Meten", "Matn")
data_wide$district<-str_replace_all(data_wide$district, "El Nabatieh", "Nabatiyeh")
data_wide$district<-str_replace_all(data_wide$district, "Bent Jbeil", "BentJbayl")
data_wide$district<-str_replace_all(data_wide$district, "El Minieh-Dennie", "MinieDanniyeh")
data_wide$district<-str_replace_all(data_wide$district, "Jbeil", "Jbayl")
data_wide$district<-str_replace_all(data_wide$district, "Kesrwane", "Kesrouan")
data_wide$district<-str_replace_all(data_wide$district, "West Bekaa", "BekaaWest")
data_wide$district<-str_replace_all(data_wide$district, "Marjaayoun", "Marjayoun")
data_wide$district<-str_replace_all(data_wide$district, "El Batroun", "Batroun")
data_wide$district<-str_replace_all(data_wide$district, "El Hermel", "Hermel")
data_wide$district<-str_replace_all(data_wide$district, "El Koura", "Koura")
data_wide$district<-str_replace_all(data_wide$district, "Zahle", "Zahlé")

data_wide<-arrange(data_wide, district)
data_wide <- data_wide[ , c(1,2,19,15,13,5,25,23,21,7,11,9,17,3,20,16,14,6,26,24,22,8,12,10,18,4)]

data.melt.arr <- melt(data_wide, id = c("district"))
colnames(data.melt.arr)[2]<-'month.year'

lebmap <- readOGR(dsn = "LBN_adm-2/", "LBN_adm2")
lebmap.f <- fortify(lebmap, region = "ID_2")
lebmap.f <- merge(lebmap.f, lebmap@data, by.x = "id", by.y = "ID_2")
lebmap.f$district<-lebmap.f$NAME_2
plot.data.arrest <- merge(lebmap.f, data.melt.arr, by.x = "district", by.y = "district")

#__________________________________________________

dissdata<-read.csv('lebanondata.april.csv')
conflict_data<-dissdata %>%
  select(date, actor, actor_group, 
         target, target_group, category,
         classification, arrest, injury, dead, 
         district, latitude, longitude) %>%
  filter(date != '1-May-15') %>% 
  mutate(date=dmy(date))

conflict_data$month<-month(conflict_data$date, label=T)
conflict_data$year<-year(conflict_data$date)
conflict_data$month.year<-paste0(conflict_data$month, sep = "-", conflict_data$year)

injur.graph<-conflict_data %>%
  filter(injury>0)  %>%
  group_by(district, month.year) %>%
  summarize(injur.sum=sum(injury))
injur.graph<-as.data.frame(injur.graph)

data_wide <- spread(injur.graph, month.year, injur.sum)
data_wide[is.na(data_wide)] <- 0


data_wide$district<-as.character(data_wide$district)

data_wide[25,1]<-"Bcharré"
data_wide[26,1]<-"Nabatiyeh"
data_wide[25,2:26]<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
data_wide[26,2:26]<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)


data_wide$district<-str_replace_all(data_wide$district, "Bcharre", "Bcharré")
data_wide$district<-str_replace_all(data_wide$district, "El Meten", "Matn")
data_wide$district<-str_replace_all(data_wide$district, "El Nabatieh", "Nabatiyeh")
data_wide$district<-str_replace_all(data_wide$district, "Bent Jbeil", "BentJbayl")
data_wide$district<-str_replace_all(data_wide$district, "El Minieh-Dennie", "MinieDanniyeh")
data_wide$district<-str_replace_all(data_wide$district, "Jbeil", "Jbayl")
data_wide$district<-str_replace_all(data_wide$district, "Kesrwane", "Kesrouan")
data_wide$district<-str_replace_all(data_wide$district, "West Bekaa", "BekaaWest")
data_wide$district<-str_replace_all(data_wide$district, "Marjaayoun", "Marjayoun")
data_wide$district<-str_replace_all(data_wide$district, "El Batroun", "Batroun")
data_wide$district<-str_replace_all(data_wide$district, "El Hermel", "Hermel")
data_wide$district<-str_replace_all(data_wide$district, "El Koura", "Koura")
data_wide$district<-str_replace_all(data_wide$district, "Zahle", "Zahlé")

data_wide<-arrange(data_wide, district)
data_wide <- data_wide[ , c(1,2,19,15,13,5,25,23,21,7,11,9,17,3,20,16,14,6,26,24,22,8,12,10,18,4)]

data.melt.inj <- melt(data_wide, id = c("district"))
colnames(data.melt.inj)[2]<-'month.year'

lebmap <- readOGR(dsn = "LBN_adm-2/", "LBN_adm2")
lebmap.f <- fortify(lebmap, region = "ID_2")
lebmap.f <- merge(lebmap.f, lebmap@data, by.x = "id", by.y = "ID_2")
lebmap.f$district<-lebmap.f$NAME_2
plot.data.inj <- merge(lebmap.f, data.melt.inj, by.x = "district", by.y = "district")

#_____________________________________________

dissdata<-read.csv('lebanondata.april.csv')
conflict_data<-dissdata %>%
  select(date, actor, actor_group, 
         target, target_group, category,
         classification, arrest, injury, dead, 
         district, latitude, longitude) %>%
  filter(date != '1-May-15') %>% 
  mutate(date=dmy(date))

conflict_data$month<-month(conflict_data$date, label=T)
conflict_data$year<-year(conflict_data$date)
conflict_data$month.year<-paste0(conflict_data$month, sep = "-", conflict_data$year)

dead.graph<-conflict_data %>%
  filter(dead>0)  %>%
  group_by(district, month.year) %>%
  summarize(dead.sum=sum(dead))
dead.graph<-as.data.frame(dead.graph)


data_wide <- spread(dead.graph, month.year, dead.sum)
data_wide[is.na(data_wide)] <- 0

data_wide$district<-as.character(data_wide$district)
data_wide[23,1]<-"Bcharré"
data_wide[24,1]<-"BentJbayl"
data_wide[25,1]<-"Nabatiyeh"
data_wide[26,1]<-"Hasbaya"
data_wide[23,2:26]<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
data_wide[24,2:26]<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
data_wide[25,2:26]<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
data_wide[26,2:26]<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)

data_wide<-filter(data_wide, district!='NA')

data_wide$district<-str_replace_all(data_wide$district, "Bcharre", "Bcharré")
data_wide$district<-str_replace_all(data_wide$district, "El Meten", "Matn")
data_wide$district<-str_replace_all(data_wide$district, "El Nabatieh", "Nabatiyeh")
data_wide$district<-str_replace_all(data_wide$district, "Bent Jbeil", "BentJbayl")
data_wide$district<-str_replace_all(data_wide$district, "El Minieh-Dennie", "MinieDanniyeh")
data_wide$district<-str_replace_all(data_wide$district, "Jbeil", "Jbayl")
data_wide$district<-str_replace_all(data_wide$district, "Kesrwane", "Kesrouan")
data_wide$district<-str_replace_all(data_wide$district, "West Bekaa", "BekaaWest")
data_wide$district<-str_replace_all(data_wide$district, "Marjaayoun", "Marjayoun")
data_wide$district<-str_replace_all(data_wide$district, "El Batroun", "Batroun")
data_wide$district<-str_replace_all(data_wide$district, "El Hermel", "Hermel")
data_wide$district<-str_replace_all(data_wide$district, "El Koura", "Koura")
data_wide$district<-str_replace_all(data_wide$district, "Zahle", "Zahlé")

data_wide<-arrange(data_wide, district)
data_wide <- data_wide[ , c(1,2,19,15,13,5,25,23,21,7,11,9,17,3,20,16,14,6,26,24,22,8,12,10,18,4)]

data.melt.dea <- melt(data_wide, id = c("district"))
colnames(data.melt.dea)[2]<-'month.year'

lebmap <- readOGR(dsn = "LBN_adm-2/", "LBN_adm2")
lebmap.f <- fortify(lebmap, region = "ID_2")
lebmap.f <- merge(lebmap.f, lebmap@data, by.x = "id", by.y = "ID_2")
lebmap.f$district<-lebmap.f$NAME_2
plot.data.dea <- merge(lebmap.f, data.melt.dea, by.x = "district", by.y = "district")

data.melt.dea$dead<-data.melt.dea$value
data.melt.viol$viol<-data.melt.viol$value
data.melt.inj$injury<-data.melt.inj$value
data.melt.arr$arrest<-data.melt.arr$value

data.melt.dea$value<-NULL
data.melt.viol$value<-NULL
data.melt.inj$value<-NULL
data.melt.arr$value<-NULL


con<-merge(data.melt.dea, data.melt.inj, by=c("month.year", "district"))
con2<-merge(con, data.melt.arr, by=c("month.year", "district"))
conflict<-merge(data.melt.viol,con2, by=c("month.year", "district"))

conflict<-arrange(conflict, month.year, district)
conflict<-group_by(conflict, district, month.year)

conflict.sprd<-select(conflict, 1,2,4)
conflict.sprd<-spread(conflict.sprd, district, dead)

conflict.sprd.viol<-select(conflict, 1,2,3)
conflict.sprd.viol<-spread(conflict.sprd.viol, district, viol)

conflict.sprd.arrest<-select(conflict, 1,2,6)
conflict.sprd.arrest<-spread(conflict.sprd.arrest, district, arrest)

conflict.sprd.injury<-select(conflict, 1,2,5)
conflict.sprd.injury<-spread(conflict.sprd.injury, district, injury)


#_______NEW________#

dissdata<-read.csv('lebanondata.april.csv')
conflict_data<-dissdata %>%
  select(date, actor, actor_group, 
         target, target_group, category,
         classification, arrest, injury, dead, 
         district, latitude, longitude) %>%
  filter(date != '1-May-15') %>% 
  mutate(date=dmy(date))

viol.domestic.events<-conflict_data %>%
  filter(category=='exect'| category=='bombard'| 
           category=='assault'|category=='shooting'| 
           category=='clashes'|category=='suicbatt'|
           category=='bombatt' | category=='aerbom' | 
           injury > 0 | dead > 0) %>%
  filter(classification!='isrcrosscon') 

viol.domestic.events$month<-month(viol.domestic.events$date, label=T)
viol.domestic.events$year<-year(viol.domestic.events$date)
viol.domestic.events$month.year<-paste0(viol.domestic.events$month, sep = "-", viol.domestic.events$year)

viol.domestic.events<-arrange(viol.domestic.events, date)

data<-viol.domestic.events %>%
  group_by(district, month.year)  %>%
  summarize(count=n())
data<-as.data.frame(data)

data_wide <- spread(data, month.year, count)
data_wide[is.na(data_wide)] <- 0

data_wide$district<-as.character(data_wide$district)
data_wide[25,1]<-"Bcharré"
data_wide[26,1]<-"Bent Jbayl"
data_wide[25,2:26]<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
data_wide[26,2:26]<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)

data_wide$district<-str_replace_all(data_wide$district, "Bcharre", "Bcharré")
data_wide$district<-str_replace_all(data_wide$district, "El Meten", "Matn")
data_wide$district<-str_replace_all(data_wide$district, "El Nabatieh", "Nabatiyeh")
data_wide$district<-str_replace_all(data_wide$district, "Bent Jbeil", "Bent Jbayl")
data_wide$district<-str_replace_all(data_wide$district, "El Minieh-Dennie", "Minie-Danniyeh")
data_wide$district<-str_replace_all(data_wide$district, "Jbeil", "Jbayl")
data_wide$district<-str_replace_all(data_wide$district, "Kesrwane", "Kesrouan")
data_wide$district<-str_replace_all(data_wide$district, "West Bekaa", "Bekaa-West")
data_wide$district<-str_replace_all(data_wide$district, "Marjaayoun", "Marjayoun")
data_wide$district<-str_replace_all(data_wide$district, "El Batroun", "Batroun")
data_wide$district<-str_replace_all(data_wide$district, "El Hermel", "Hermel")
data_wide$district<-str_replace_all(data_wide$district, "El Koura", "Koura")
data_wide$district<-str_replace_all(data_wide$district, "Zahle", "Zahlé")

data_wide<-arrange(data_wide, district)
data_wide <- data_wide[ , c(1,2,19,15,13,5,25,23,21,7,11,9,17,3,20,16,14,6,26,24,22,8,12,10,18,4)]

data.melt.viol <- melt(data_wide, id = c("district"))
colnames(data.melt.viol)[2]<-'month.year'


lebmap <- readOGR(dsn = "LBN_adm-2/", "LBN_adm2")
lebmap.f <- fortify(lebmap, region = "ID_2")
lebmap.f <- merge(lebmap.f, lebmap@data, by.x = "id", by.y = "ID_2")
lebmap.f$district<-lebmap.f$NAME_2
plot.data.viol <- merge(lebmap.f, data.melt.viol, by.x = "district", by.y = "district")

#_____________________________________________

dissdata<-read.csv('lebanondata.april.csv')
conflict_data<-dissdata %>%
  select(date, actor, actor_group, 
         target, target_group, category,
         classification, arrest, injury, dead, 
         district, latitude, longitude) %>%
  filter(date != '1-May-15') %>% 
  mutate(date=dmy(date))

conflict_data$month<-month(conflict_data$date, label=T)
conflict_data$year<-year(conflict_data$date)
conflict_data$month.year<-paste0(conflict_data$month, sep = "-", conflict_data$year)

arrests.graph<-conflict_data %>%
  filter(arrest>0)  %>%
  group_by(district, month.year) %>%
  summarize(arrest.sum=sum(arrest))
arrests.graph<-as.data.frame(arrests.graph)

data_wide <- spread(arrests.graph, month.year, arrest.sum)
data_wide[is.na(data_wide)] <- 0

data_wide$district<-as.character(data_wide$district)
data_wide[25,1]<-"Bcharré"
data_wide[26,1]<-"Bent Jbayl"
data_wide[25,2:26]<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
data_wide[26,2:26]<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)

data_wide$district<-str_replace_all(data_wide$district, "Bcharre", "Bcharré")
data_wide$district<-str_replace_all(data_wide$district, "El Meten", "Matn")
data_wide$district<-str_replace_all(data_wide$district, "El Nabatieh", "Nabatiyeh")
data_wide$district<-str_replace_all(data_wide$district, "Bent Jbeil", "Bent Jbayl")
data_wide$district<-str_replace_all(data_wide$district, "El Minieh-Dennie", "Minie-Danniyeh")
data_wide$district<-str_replace_all(data_wide$district, "Jbeil", "Jbayl")
data_wide$district<-str_replace_all(data_wide$district, "Kesrwane", "Kesrouan")
data_wide$district<-str_replace_all(data_wide$district, "West Bekaa", "Bekaa-West")
data_wide$district<-str_replace_all(data_wide$district, "Marjaayoun", "Marjayoun")
data_wide$district<-str_replace_all(data_wide$district, "El Batroun", "Batroun")
data_wide$district<-str_replace_all(data_wide$district, "El Hermel", "Hermel")
data_wide$district<-str_replace_all(data_wide$district, "El Koura", "Koura")
data_wide$district<-str_replace_all(data_wide$district, "Zahle", "Zahlé")

data_wide<-arrange(data_wide, district)
data_wide <- data_wide[ , c(1,2,19,15,13,5,25,23,21,7,11,9,17,3,20,16,14,6,26,24,22,8,12,10,18,4)]

data.melt.arr <- melt(data_wide, id = c("district"))
colnames(data.melt.arr)[2]<-'month.year'

lebmap <- readOGR(dsn = "LBN_adm-2/", "LBN_adm2")
lebmap.f <- fortify(lebmap, region = "ID_2")
lebmap.f <- merge(lebmap.f, lebmap@data, by.x = "id", by.y = "ID_2")
lebmap.f$district<-lebmap.f$NAME_2
plot.data.arrest <- merge(lebmap.f, data.melt.arr, by.x = "district", by.y = "district")

#__________________________________________________

dissdata<-read.csv('lebanondata.april.csv')
conflict_data<-dissdata %>%
  select(date, actor, actor_group, 
         target, target_group, category,
         classification, arrest, injury, dead, 
         district, latitude, longitude) %>%
  filter(date != '1-May-15') %>% 
  mutate(date=dmy(date))

conflict_data$month<-month(conflict_data$date, label=T)
conflict_data$year<-year(conflict_data$date)
conflict_data$month.year<-paste0(conflict_data$month, sep = "-", conflict_data$year)

injur.graph<-conflict_data %>%
  filter(injury>0)  %>%
  group_by(district, month.year) %>%
  summarize(injur.sum=sum(injury))
injur.graph<-as.data.frame(injur.graph)

data_wide <- spread(injur.graph, month.year, injur.sum)
data_wide[is.na(data_wide)] <- 0


data_wide$district<-as.character(data_wide$district)

data_wide[25,1]<-"Bcharré"
data_wide[26,1]<-"Nabatiyeh"
data_wide[25,2:26]<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
data_wide[26,2:26]<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)


data_wide$district<-str_replace_all(data_wide$district, "Bcharre", "Bcharré")
data_wide$district<-str_replace_all(data_wide$district, "El Meten", "Matn")
data_wide$district<-str_replace_all(data_wide$district, "El Nabatieh", "Nabatiyeh")
data_wide$district<-str_replace_all(data_wide$district, "Bent Jbeil", "Bent Jbayl")
data_wide$district<-str_replace_all(data_wide$district, "El Minieh-Dennie", "Minie-Danniyeh")
data_wide$district<-str_replace_all(data_wide$district, "Jbeil", "Jbayl")
data_wide$district<-str_replace_all(data_wide$district, "Kesrwane", "Kesrouan")
data_wide$district<-str_replace_all(data_wide$district, "West Bekaa", "Bekaa-West")
data_wide$district<-str_replace_all(data_wide$district, "Marjaayoun", "Marjayoun")
data_wide$district<-str_replace_all(data_wide$district, "El Batroun", "Batroun")
data_wide$district<-str_replace_all(data_wide$district, "El Hermel", "Hermel")
data_wide$district<-str_replace_all(data_wide$district, "El Koura", "Koura")
data_wide$district<-str_replace_all(data_wide$district, "Zahle", "Zahlé")

data_wide<-arrange(data_wide, district)
data_wide <- data_wide[ , c(1,2,19,15,13,5,25,23,21,7,11,9,17,3,20,16,14,6,26,24,22,8,12,10,18,4)]

data.melt.inj <- melt(data_wide, id = c("district"))
colnames(data.melt.inj)[2]<-'month.year'

lebmap <- readOGR(dsn = "LBN_adm-2/", "LBN_adm2")
lebmap.f <- fortify(lebmap, region = "ID_2")
lebmap.f <- merge(lebmap.f, lebmap@data, by.x = "id", by.y = "ID_2")
lebmap.f$district<-lebmap.f$NAME_2
plot.data.inj <- merge(lebmap.f, data.melt.inj, by.x = "district", by.y = "district")

#_____________________________________________

dissdata<-read.csv('lebanondata.april.csv')
conflict_data<-dissdata %>%
  select(date, actor, actor_group, 
         target, target_group, category,
         classification, arrest, injury, dead, 
         district, latitude, longitude) %>%
  filter(date != '1-May-15') %>% 
  mutate(date=dmy(date))

conflict_data$month<-month(conflict_data$date, label=T)
conflict_data$year<-year(conflict_data$date)
conflict_data$month.year<-paste0(conflict_data$month, sep = "-", conflict_data$year)

dead.graph<-conflict_data %>%
  filter(dead>0)  %>%
  group_by(district, month.year) %>%
  summarize(dead.sum=sum(dead))
dead.graph<-as.data.frame(dead.graph)


data_wide <- spread(dead.graph, month.year, dead.sum)
data_wide[is.na(data_wide)] <- 0

data_wide$district<-as.character(data_wide$district)
data_wide[23,1]<-"Bcharré"
data_wide[24,1]<-"Bent Jbayl"
data_wide[25,1]<-"Nabatiyeh"
data_wide[26,1]<-"Hasbaya"
data_wide[23,2:26]<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
data_wide[24,2:26]<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
data_wide[25,2:26]<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
data_wide[26,2:26]<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)

data_wide<-filter(data_wide, district!='NA')

data_wide$district<-str_replace_all(data_wide$district, "Bcharre", "Bcharré")
data_wide$district<-str_replace_all(data_wide$district, "El Meten", "Matn")
data_wide$district<-str_replace_all(data_wide$district, "El Nabatieh", "Nabatiyeh")
data_wide$district<-str_replace_all(data_wide$district, "Bent Jbeil", "Bent Jbayl")
data_wide$district<-str_replace_all(data_wide$district, "El Minieh-Dennie", "Minie-Danniyeh")
data_wide$district<-str_replace_all(data_wide$district, "Jbeil", "Jbayl")
data_wide$district<-str_replace_all(data_wide$district, "Kesrwane", "Kesrouan")
data_wide$district<-str_replace_all(data_wide$district, "West Bekaa", "Bekaa-West")
data_wide$district<-str_replace_all(data_wide$district, "Marjaayoun", "Marjayoun")
data_wide$district<-str_replace_all(data_wide$district, "El Batroun", "Batroun")
data_wide$district<-str_replace_all(data_wide$district, "El Hermel", "Hermel")
data_wide$district<-str_replace_all(data_wide$district, "El Koura", "Koura")
data_wide$district<-str_replace_all(data_wide$district, "Zahle", "Zahlé")

data_wide<-arrange(data_wide, district)
data_wide <- data_wide[ , c(1,2,19,15,13,5,25,23,21,7,11,9,17,3,20,16,14,6,26,24,22,8,12,10,18,4)]

data.melt.dea <- melt(data_wide, id = c("district"))
colnames(data.melt.dea)[2]<-'month.year'

lebmap <- readOGR(dsn = "LBN_adm-2/", "LBN_adm2")
lebmap.f <- fortify(lebmap, region = "ID_2")
lebmap.f <- merge(lebmap.f, lebmap@data, by.x = "id", by.y = "ID_2")
lebmap.f$district<-lebmap.f$NAME_2
plot.data.dea <- merge(lebmap.f, data.melt.dea, by.x = "district", by.y = "district")

data.melt.dea$dead<-data.melt.dea$value
data.melt.viol$viol<-data.melt.viol$value
data.melt.inj$injury<-data.melt.inj$value
data.melt.arr$arrest<-data.melt.arr$value

data.melt.dea$value<-NULL
data.melt.viol$value<-NULL
data.melt.inj$value<-NULL
data.melt.arr$value<-NULL

con<-merge(data.melt.dea, data.melt.inj, by=c("month.year", "district"))
con2<-merge(con, data.melt.arr, by=c("month.year", "district"))
conflict<-merge(data.melt.viol,con2, by=c("month.year", "district"))

lebmap <- readOGR(dsn = "LBN_adm-2/", "LBN_adm2",verbose = FALSE)
lebmap$district<-lebmap$NAME_2
lebmap.f <- fortify(lebmap, region = "ID_2")
lebmap.f <- merge(lebmap.f, lebmap@data, by.x = "id", by.y = "ID_2")
conflict.plot <- merge(lebmap.f, conflict, by.x = "district", by.y = "district")

library(shiny)
library(rgdal)
library(maptools)

eventdata<-dissdata
colnames(eventdata)[1]<-"DayMonthYear"

#_____Barplots

dissdata<-read.csv('lebanondata.april.csv')
data<-dissdata %>%
  select(date, actor, actor_group, 
         target, target_group, category,
         classification, arrest, injury, dead, 
         district, latitude, longitude) %>%
  filter(date != '1-May-15') %>% 
  mutate(date=dmy(date))

#Line up district names ID for merging later
data$district<-as.character(data$district)
data$district<-str_replace_all(data$district, "Bcharre", "Bcharré")
data$district<-str_replace_all(data$district, "El Meten", "Matn")
data$district<-str_replace_all(data$district, "El Nabatieh", "Nabatiyeh")
data$district<-str_replace_all(data$district, "Bent Jbeil", "Bent Jbayl")
data$district<-str_replace_all(data$district, "El Minieh-Dennie", "Minie-Danniyeh")
data$district<-str_replace_all(data$district, "Jbeil", "Jbayl")
data$district<-str_replace_all(data$district, "Kesrwane", "Kesrouan")
data$district<-str_replace_all(data$district, "West Bekaa", "Bekaa-West")
data$district<-str_replace_all(data$district, "Marjaayoun", "Marjayoun")
data$district<-str_replace_all(data$district, "El Batroun", "Batroun")
data$district<-str_replace_all(data$district, "El Hermel", "Hermel")
data$district<-str_replace_all(data$district, "El Koura", "Koura")
data$district<-str_replace_all(data$district, "Zahle", "Zahlé")

conflict_data<-data

viol.domestic.events<-conflict_data %>%
  filter(category=='exect'| category=='bombard'| 
           category=='assault'|category=='shooting'| 
           category=='clashes'|category=='suicbatt'|
           category=='bombatt' | category=='aerbom' | 
           injury > 0 | dead > 0) %>%
  filter(classification!='isrcrosscon') %>%
  select(date, district, actor, actor_group, target, 
         target_group, arrest, injury, dead, latitude, longitude)


viol.district<-viol.domestic.events %>%
  group_by(district)  %>%
  summarize(viol.count=n())
viol.district<-as.data.frame(viol.district)

viol.district[25,1]<-"Bcharré"
viol.district[26,1]<-"Bent Jbayl"
viol.district[25,2]<-0
viol.district[26,2]<-0

injur.district<-conflict_data %>%
  filter(injury>0)  %>%
  group_by(district) %>%
  summarize(injur.sum=sum(injury))
injur.district<-as.data.frame(injur.district)

injur.district[25,1]<-"Bcharré"
injur.district[26,1]<-"Nabatiyeh"
injur.district[25,2]<-0
injur.district[26,2]<-0

dead.district<-conflict_data %>%
  filter(dead>0)  %>%
  group_by(district) %>%
  summarize(dead.sum=sum(dead))
dead.district<-as.data.frame(dead.district)

dead.district[23,1]<-"Bcharré"
dead.district[24,1]<-"Bent Jbayl"
dead.district[25,1]<-"Nabatiyeh"
dead.district[26,1]<-"Hasbaya"
dead.district[23,2]<-0
dead.district[24,2]<-0
dead.district[25,2]<-0
dead.district[26,2]<-0

arrest.district<-conflict_data %>%
  filter(arrest>0)  %>%
  group_by(district) %>%
  summarize(arrest.sum=sum(arrest))
arrest.district<-as.data.frame(arrest.district)

arrest.district[25,1]<-"Bcharré"
arrest.district[26,1]<-"Bent Jbayl"
arrest.district[25,2]<-0
arrest.district[26,2]<-0

barA<-merge(dead.district, arrest.district, by.x='district', by.y='district')
barB<-merge(viol.district, injur.district, by.x='district', by.y='district')
bar<-merge(barA, barB, by.x='district', by.y='district')

refugeedata<-read.csv('refugee.databb.csv')
refugee_data<-refugeedata %>%
  select(Caza_Name, YearMonthDay, Syrian_Ref) %>%
  mutate(date=ymd(refugeedata$YearMonthDay))
refugee_data$date<-str_replace_all(refugee_data$date, 
                                   "2013-06-10", "2013-06-30") #One off date

colnames(refugee_data)[1]<-"district"

refugee_data$month<-month(refugee_data$date, label=T)
refugee_data$year<-year(refugee_data$date)
refugee_data$month.year<-paste0(refugee_data$month, sep = "-", refugee_data$year)
refugee_data<-filter(refugee_data, month.year!="Mar-2013")

#Line up district names ID for merging later
refugee_data$district<-as.character(refugee_data$district)
refugee_data$district<-str_replace_all(refugee_data$district, "Bcharre", "Bcharré")
refugee_data$district<-str_replace_all(refugee_data$district, "El Meten", "Matn")
refugee_data$district<-str_replace_all(refugee_data$district, "El Nabatieh", "Nabatiyeh")
refugee_data$district<-str_replace_all(refugee_data$district, "Bent Jbeil", "Bent Jbayl")
refugee_data$district<-str_replace_all(refugee_data$district, "El Minieh Dennie", "Minie-Danniyeh")
refugee_data$district<-str_replace_all(refugee_data$district, "Jbeil", "Jbayl")
refugee_data$district<-str_replace_all(refugee_data$district, "Kesrwane", "Kesrouan")
refugee_data$district<-str_replace_all(refugee_data$district, "West Bekaa", "Bekaa-West")
refugee_data$district<-str_replace_all(refugee_data$district, "El Batroun", "Batroun")
refugee_data$district<-str_replace_all(refugee_data$district, "El Hermel", "Hermel")
refugee_data$district<-str_replace_all(refugee_data$district, "El Koura", "Koura")
refugee_data$district<-str_replace_all(refugee_data$district, "Zahle", "Zahlé")

refugee_data<-arrange(refugee_data, date)

##interpolation
Akkar<-refugee_data %>%
  filter(district=='Akkar')
Akkar$count<-na.approx(zoo(Akkar$Syrian_Ref))

Aley<-refugee_data %>%
  filter(district=='Aley')
Aley$count<-na.approx(zoo(Aley$Syrian_Ref))

Batroun<-refugee_data %>%
  filter(district=='Batroun')
Batroun$count<-na.approx(zoo(Batroun$Syrian_Ref))

Akkar<-refugee_data %>%
  filter(district=='Akkar')
Akkar$count<-na.approx(zoo(Akkar$Syrian_Ref))

Baabda<-refugee_data %>%
  filter(district=='Baabda')
Baabda$count<-na.approx(zoo(Baabda$Syrian_Ref))

Baalbek<-refugee_data %>%
  filter(district=='Baalbek')
Baalbek$count<-na.approx(zoo(Baalbek$Syrian_Ref))

Bcharré<-refugee_data %>%
  filter(district=='Bcharré')
Bcharré$count<-na.approx(zoo(Bcharré$Syrian_Ref))

Beirut<-refugee_data %>%
  filter(district=='Beirut')
Beirut$count<-na.approx(zoo(Beirut$Syrian_Ref))

BentJbayl<-refugee_data %>%
  filter(district=='Bent Jbayl')
BentJbayl$count<-na.approx(zoo(BentJbayl$Syrian_Ref))

Chouf<-refugee_data %>%
  filter(district=='Chouf')
Chouf$count<-na.approx(zoo(Chouf$Syrian_Ref))

Hermel<-refugee_data %>%
  filter(district=='Hermel')
Hermel$count<-na.approx(zoo(Hermel$Syrian_Ref))

Koura<-refugee_data %>%
  filter(district=='Koura')
Koura$count<-na.approx(zoo(Koura$Syrian_Ref))

Matn<-refugee_data %>%
  filter(district=='Matn')
Matn$count<-na.approx(zoo(Matn$Syrian_Ref))

MinieDanniyeh<-refugee_data %>%
  filter(district=='Minie-Danniyeh')
MinieDanniyeh$count<-na.approx(zoo(MinieDanniyeh$Syrian_Ref))

Nabatiyeh<-refugee_data %>%
  filter(district=='Nabatiyeh')
Nabatiyeh$count<-na.approx(zoo(Nabatiyeh$Syrian_Ref))

Hasbaya<-refugee_data %>%
  filter(district=='Hasbaya')
Hasbaya$count<-na.approx(zoo(Hasbaya$Syrian_Ref))

Jbayl<-refugee_data %>%
  filter(district=='Jbayl')
Jbayl$count<-na.approx(zoo(Jbayl$Syrian_Ref))

Jezzine<-refugee_data %>%
  filter(district=='Jezzine')
Jezzine$count<-na.approx(zoo(Jezzine$Syrian_Ref))

Kesrouan<-refugee_data %>%
  filter(district=='Kesrouan')
Kesrouan$count<-na.approx(zoo(Kesrouan$Syrian_Ref))

Marjayoun<-refugee_data %>%
  filter(district=='Marjayoun')
Marjayoun$count<-na.approx(zoo(Marjayoun$Syrian_Ref))

Rachaya<-refugee_data %>%
  filter(district=='Rachaya')
Rachaya$count<-na.approx(zoo(Rachaya$Syrian_Ref))

Saida<-refugee_data %>%
  filter(district=='Saida')
Saida$count<-na.approx(zoo(Saida$Syrian_Ref))

Sour<-refugee_data %>%
  filter(district=='Sour')
Sour$count<-na.approx(zoo(Sour$Syrian_Ref))

Tripoli<-refugee_data %>%
  filter(district=='Tripoli')
Tripoli$count<-na.approx(zoo(Tripoli$Syrian_Ref))

BekaaWest<-refugee_data %>%
  filter(district=='Bekaa-West')
BekaaWest$count<-na.approx(zoo(BekaaWest$Syrian_Ref))

Zahle<-refugee_data %>%
  filter(district=='Zahlé')
Zahle$count<-na.approx(zoo(Zahle$Syrian_Ref))

Zgharta<-refugee_data %>%
  filter(district=='Zgharta')
Zgharta$count<-na.approx(zoo(Zgharta$Syrian_Ref))

refugee_data<-rbind(Akkar, Aley, Baabda, Baalbek, Bcharré, Beirut, BentJbayl, Chouf, Batroun, Hermel, Koura, Matn, MinieDanniyeh, Nabatiyeh, Hasbaya, Jbayl, Jezzine, Kesrouan, Marjayoun, Rachaya, Saida, Sour, Tripoli, BekaaWest, Zahle,Zgharta)

refugee_data$Syrian_Ref<-as.numeric(refugee_data$count)

ref_bar<-refugee_data %>%
  group_by(district) %>%
  summarise(count=sum(Syrian_Ref))
ref_bar<-as.data.frame(ref_graph)
ref_bar<-arrange(ref_graph, count)

#____start____

shinyServer(function(input, output, session) {
  
  output$plot <- renderPlot({
    sub.data<-subset(plot.data, month.year==input$monthyear)
    ggplot(sub.data, aes(x = long, y = lat, fill = Syrian_Ref, group = group)) + 
      geom_polygon() + geom_path(color="black",linetype=1) + 
      coord_equal() +
      scale_fill_gradient(limits = c(0, max(plot.data$Syrian_Ref)), low = "lightgrey", high = "darkred", 
                          name=input$variable) + xlab("") + ylab("")+
      theme(axis.text = element_blank(), 
            axis.title = element_blank(), 
            axis.ticks = element_blank()) + 
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            panel.background = element_blank(), axis.line = element_blank()) + 
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            panel.background = element_blank(), axis.line = element_blank())
    
  })
  
  output$plot2 <- renderPlot({
    sub.plot<-subset(conflict.plot, month.year==input$monthyear)
    ggplot(data = sub.plot, aes_string(x = 'long', y = 'lat', fill = input$coninput, group = 'group')) + 
      geom_polygon() + geom_path(color="black",linetype=1) + 
      coord_equal() +
      scale_fill_gradient(limits = c(0, max(conflict.plot[input$coninput])), low = "lightgrey",high = "darkred", name= input$coninput) +xlab("")+ylab("") + 
      theme(axis.text = element_blank(), 
            axis.title = element_blank(), 
            axis.ticks = element_blank()) + 
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            panel.background = element_blank(), axis.line = element_blank()) + 
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            panel.background = element_blank(), axis.line = element_blank())
  })
  output$plot3 <- renderPlot({
    ggplot(plot.data, aes(x = long, y = lat, fill = pop, group = group)) + 
      geom_polygon() + geom_path(color="black",linetype=1) + 
      coord_equal() +
      scale_fill_gradient(low = "lightgrey", high = "darkred", 
                          name=input$variable) + xlab("") + ylab("")+
      theme(axis.text = element_blank(), 
            axis.title = element_blank(), 
            axis.ticks = element_blank()) + 
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            panel.background = element_blank(), axis.line = element_blank()) + 
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            panel.background = element_blank(), axis.line = element_blank())
  })
  
  output$plot4 <- renderPlot({
    ggplot(plot.data.sect, aes_string(x = 'long', y = 'lat', fill = input$sectinput, group = 'group')) + 
      geom_polygon() + geom_path(color="black",linetype=1) + 
      coord_equal() +
      scale_fill_gradient(low = "lightgrey", high = "darkred", 
                          name=input$variable) + xlab("") + ylab("")+
      theme(axis.text = element_blank(), 
            axis.title = element_blank(), 
            axis.ticks = element_blank()) + 
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            panel.background = element_blank(), axis.line = element_blank()) + 
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            panel.background = element_blank(), axis.line = element_blank())
  })
  
  output$plot5 <- renderPlot({
    green<-'#2ca25f'
    ggplot(conflict.sprd, aes_string(x='month.year', y=input$disinput, group=1, color=Beirut)) +
      geom_line() + 
      scale_y_continuous(limits = c(0,66)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            panel.background = element_blank(), axis.line = element_line(colour = "black")) +
      xlab("") + theme(legend.position = "none") +
      theme(axis.text=element_text(size=14), axis.title=element_text(size=18,face="bold")) +
      scale_color_manual(values="darkgreen")
  })
  
  output$plot6 <- renderPlot({
    green<-'#2ca25f'
    ggplot(conflict.sprd.viol, aes_string(x='month.year', y=input$disinput, group=1, color=Beirut)) +
      geom_line() + 
      scale_y_continuous(limits = c(0,25)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            panel.background = element_blank(), axis.line = element_line(colour = "black")) +
      xlab("") + theme(legend.position = "none") +
      theme(axis.text=element_text(size=14), axis.title=element_text(size=18,face="bold")) +
      scale_color_manual(values="darkgreen")
  })
  
  output$plot7 <- renderPlot({
    green<-'#2ca25f'
    ggplot(conflict.sprd.arrest, aes_string(x='month.year', y=input$disinput, group=1, color=Beirut)) +
      geom_line() + 
      scale_y_continuous(limits = c(0,138)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            panel.background = element_blank(), axis.line = element_line(colour = "black")) +
      xlab("") + theme(legend.position = "none") +
      theme(axis.text=element_text(size=14), axis.title=element_text(size=18,face="bold")) +
      scale_color_manual(values="darkgreen")
  })
  
  output$plot8 <- renderPlot({
    green<-'#2ca25f'
    ggplot(conflict.sprd.injury, aes_string(x='month.year', y=input$disinput, group=1, color=Beirut)) +
      geom_line() + 
      scale_y_continuous(limits = c(0,503)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            panel.background = element_blank(), axis.line = element_line(colour = "black")) +
      xlab("") + theme(legend.position = "none") +
      theme(axis.text=element_text(size=14), axis.title=element_text(size=18,face="bold")) +
      scale_color_manual(values="darkgreen")
  })
  
  output$plot9 <- renderPlot({
    bar$district<- reorder(bar$district, -bar[,input$eventinput])
    ggplot(bar, aes_string(x='district', y=input$eventinput)) +
      geom_bar(stat="identity", fill='darkgreen') +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      theme(legend.position = "none") +
      xlab('') + ylab("") + 
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            panel.background = element_blank(), axis.line = element_line(colour = "black"))
  })
  
  output$plot10 <- renderPlot({
    ggplot(ref_bar, aes(reorder(district, count),count))+
      geom_bar(stat="identity", fill='darkgreen') +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      theme(legend.position = "none") +
      xlab('') + ylab("") + 
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            panel.background = element_blank(), axis.line = element_line(colour = "black")) +
      scale_y_continuous(labels=comma)
  })
  
  output$mytable = renderDataTable({
    eventdata[, input$show_vars, drop = FALSE]
  }, options = list(lengthMenu = c(5, 30, 50), pageLength = 5))
  
  output$mytable2 = renderDataTable({
    refugee_data
  }, options = list(lengthMenu = c(5, 30, 50), pageLength = 5))
  
})



