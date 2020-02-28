
# run everything! 

setwd("C:\\Dropbox\\Insect Biomass Trends\\greenland arthropods\\View_BioBasis_Zackenberg_Data_Arthropods_Arthropod_emergence230120191043238647.xlsx\\data/")
setwd("C:\\Users\\roelv\\Dropbox\\Insect Biomass Trends\\greenland arthropods\\View_BioBasis_Zackenberg_Data_Arthropods_Arthropod_emergence230120191043238647.xlsx\\data")
library(tidyverse)
library(reshape2)

dat<-     read.csv("View_BioBasis_Zackenberg_Data_Arthropods_Arthropod_emergence230120191043238647.csv", sep = "\t")
old.dat<- read.csv("C:\\Dropbox\\Dropbox\\Insect Biomass Trends\\greenland arthropods\\View_BioBasis_Zackenberg_Data_Arthropod.xlsx\\data/View_BioBasis_Zackenberg_Data_Arthropods_Arthropod_phenology010320180935370448.csv", sep = "\t")
dim(old.dat)
dim(dat) # dims differ 

sum(duplicated(dat))
head(dat[duplicated(dat), ])
tail(dat[duplicated(dat), ])




#Check dates
unique(dat[,1])  #414 dates
dat$date<- as.Date(dat[,1])
dat$Year<- format (dat$date, "%Y")
dat$Period<- format (dat$date, "%m")

old.dat$date<- as.Date(old.dat[,1])
old.dat$Year<- format (old.dat$date, "%Y")


# data from 2011 is corrupted in newest file, exclude and hope they can fix it : 
dat<- subset(dat, Year != 2011)


head(dat)

# art1 = windowtrap 
# rest are yellow pitfalls in different vegetation zones
# coordinates: 74o28'N, 20o34'W

dat[dat == -9999]<- NA
dat[dat == -999]<- NA







#check Species

dat$uniq.sp<- apply(dat[,13:17],MARGIN = 1,  function(x) x[max(which(!is.na(x)))])
old.dat$uniq.sp<- apply(old.dat[,22:26],MARGIN = 1,  function(x) x[max(which(!is.na(x)))])
as.data.frame(unique(dat$uniq.sp) )  # 91, but messy 
as.data.frame(unique(dat[, c(13:17, 32)]) )

subset(dat, uniq.sp == "#N/A") # negligible 
sample_n(subset(dat, Year == 2011), 25)


subset(old.dat, uniq.sp == "#N/A") # not there!



# Best to take mean of all active traps at each date. 
# this should then also somehow be standardized for trapping days 

dat$uniq.sp[dat$uniq.sp == "unidentified"]<- "Araneae" # fuckup in names


# first calc number ind per trap per day
dat$mn.A<- dat$A / dat$Days.A
dat$mn.B<- dat$B / dat$Days.B
dat$mn.C<- dat$C / dat$Days.C
dat$mn.D<- dat$D / dat$Days.D
dat$mn.E<- dat$E / dat$Days.E
dat$mn.F<- dat$F / dat$Days.F
dat$mn.G<- dat$G / dat$Days.G
dat$mn.H<- dat$H / dat$Days.H


# then calc mean value of all traps per plot: 
dat$all.mn<- apply(dat[,  c("mn.A","mn.B","mn.C", "mn.D", "mn.E", "mn.F","mn.G","mn.H")], MARGIN = 1, mean, na.rm = T) # seems fine 
dim( subset(dat, all.mn != "NaN"))
dat<- subset(dat, all.mn != "NaN") # remove not-sampled dates

# there are Inf values here! 
subset(dat, all.mn == Inf)
dat[dat == Inf]   <- NA  # remove Inf values, perhaps stng wrong 
# seem to beall gone now 2019






# selection of good data

dat<- subset(dat, Plot.ID != "Art6")  # remove Art6 (only sampled 96-98)
window<-subset(dat, Plot.ID == "Art1") # separate window trap from rest 
dat<- subset(dat, Plot.ID != "Art1") # exclude window trap


sum(duplicated(dat)) #21
sum(duplicated(window)) # 115




dim(dat) # 30.000 lost 

dat$Plot_ID<-NA
dat$Plot_ID[dat$Plot.ID == "Art2"]<-1054
dat$Plot_ID[dat$Plot.ID == "Art3"]<-1055
dat$Plot_ID[dat$Plot.ID == "Art4"]<-1056
dat$Plot_ID[dat$Plot.ID == "Art5"]<-1057
dat$Plot_ID[dat$Plot.ID == "Art7"]<-1058



# calc mean dates
# not possible, becaue Days is actually more a reflection of trapping effort (also correcting for number of pooled traps)
# just take date trap was emptied



# make file ready for use 

Datasource_name <- "Greenland arthropods"
Unit<- "abundance"
Transformed_number<- NA;    Sex <- NA
Error <- NA;               
Sample_ID<-292

Greenland<-data.frame(Datasource_name, 
                             Plot_ID = dat$Plot_ID, 
                             Plot_name = dat$Plot.ID, 
                             Sample_ID, 
                             Year = dat$Year,
                             Period = dat$Period,
                             Date = dat$date,
                             Taxon = dat$uniq.sp, 
                             Sex, 
                             Unit, 
                             Original_number  = dat$all.mn, 
                             Transformed_number, 
                             Number = dat$all.mn, 
                             Error
)

dim(subset(Greenland, Number == 0)) # 50.000 zeros
dim(Greenland) # 85537 on 19-12-19

# duplicates?
sum(duplicated(Greenland)) # 69
subset(Greenland[duplicated(Greenland),], Number !=0) # 4 

dim(Greenland)
Greenland<- subset(Greenland, !duplicated(Greenland) | Number >0)


# windowtrap (sum of all partial traps , in stead of mean )

window$Plot_ID<-NA
window$Plot_ID[window$Plot.ID == "Art1"]<-1053

window$w.sum<- apply(window[,  c("mn.A","mn.B","mn.C", "mn.D")], MARGIN = 1, sum, na.rm = T)

Greenland.window<-data.frame(Datasource_name, 
                      Plot_ID = window$Plot_ID, 
                      Plot_name = window$Plot.ID, 
                      Sample_ID = 291, 
                      Year = window$Year,
                      Period = window$Period,
                      Date = window$date,
                      Taxon = window$uniq.sp, 
                      Sex, 
                      Unit, 
                      Original_number  = window$w.sum, 
                      Transformed_number, 
                      Number = window$w.sum, 
                      Error
)

dim(Greenland.window) # 12683 on 19-12-19
dim(subset(Greenland.window, Number == 0)) # 7900 zeros

# duplicates
sum(duplicated(Greenland.window)) # yes! 119 
subset(Greenland.window[duplicated(Greenland.window),], Number !=0)
# all zero's -> remove

Greenland.window<- Greenland.window[!duplicated(Greenland.window),]





Greenland.all<- rbind(Greenland.window, Greenland)


# remove 2010 because moste samples were lost 

Greenland.all<- subset(Greenland.all, Year != 2010) 


subset(Greenland.all, Number == Inf)
dim(subset(Greenland.all, Number == 0)) # 50.000 zeros
dim(Greenland.all)

Greenland.all$Taxon<-gsub(" ", "_", Greenland.all$Taxon)


dcast(Greenland, Year ~ Plot_ID, value.var = "Number", sum, na.rm = T)

sum(duplicated(Greenland.all)) # 188

write.csv(Greenland.all, "C:\\Dropbox\\Insect Biomass Trends\\csvs/Greenland2020.csv")

















#run test
sub.gl<- subset(Greenland, Taxon != "Acari" & Taxon != "Collembola" & Number != Inf)
test<- dcast(sub.gl, Plot_name + Year + Period  + Date ~ "Number", value.var = "Number", sum    )
test$Year<-as.numeric(as.character(test$Year))


ggplot(test, aes(x=Date, y = Number, color = Plot_name)) + 
  geom_point() + #geom_line() +  # lines don;t really help
   scale_y_log10() + ggtitle("excluding mites and springtails")

summary(lmer(Number ~ Year + Plot_name + (1|Period),    data = test)) # actually a sign increase! 

# same but including mites and springtails
test<- dcast(Greenland, Plot_name + Year + Period  + Date ~ "Number", value.var = "Number", sum    )
test$Year<-as.numeric(as.character(test$Year))


ggplot(test, aes(x=Date, y = Number, color = Plot_name)) + 
  geom_point() + #geom_line() +  # lines don;t really help
  scale_y_log10() + ggtitle("including mites and springtails")
summary(lmer((Number) ~ Year + Plot_name + (1|Period),    data = test)) # actually a sign increase! 



# fix up windowtrap 


























#check plots 

unique(dat$Plot.ID)  # 7 plots 

# plots over time
dat$all<- rowSums(dat[,19:26], na.rm = T)

dcast(dat, Year  ~ Plot.ID, value.var = "all" , sum , na.rm = T)

# were these all sampled consistently over time? 

# E-H should have been closed since 2007. 
# also likely that sometimes traps were lost 
mdat<-melt(dat[, -(33)], id.vars = names(dat)[ c(1:18, 27:32)]  , variable.name = "TrapID", value.name = "Number")
dim(mdat)

dcast(mdat, Year+ TrapID  ~ Plot.ID, value.var = "Number" , sum , na.rm = T)

# 1996: traps A-D are combined
# 1999 - 2006 only years for traps E-H
# Art1 2006-2013 windowtrap suddenly has 4 (in 2017 2)
# 2010 has very low numbers



dim(mdat) # lost half of all obs... 

dcast(mdat, Year+ TrapID  ~ Plot.ID, value.var = "Number" , sum , na.rm = T)
# better

# now look for missing samples

d97<- subset(mdat, Year >= 1997 & Year <2005)
dim(d97)
cd97<-dcast(d97,   date ~ Plot.ID+ TrapID, value.var = "Number" , sum , na.rm = T )

ggplot(cd97, aes(x=date, y = Art5_B)) + 
  geom_point()


cd10<-dcast(subset(mdat, Year == 2010),   date ~ Plot.ID+ TrapID, value.var = "Number" , sum , na.rm = T )
# only september data! 


subset(d97, date == "1997-07-01" & Plot.ID == "Art2" & TrapID == "B")[, -(2:10)] # almost 1000 acari 

subset(d97, Number >0 & Order == "Acari")$Number # sometimes very highnumbers. best to be omitted 



