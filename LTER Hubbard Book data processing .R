

library(reshape2)
library(tidyverse)


leps<-read.table ("C:\\Dropbox\\Insect Biomass Trends\\LTER hubbard brook/leps.txt", sep = ",", header = T)
leps






# These tables are from website 

trees<- data.frame( 
tree.spec = c(1,2,3,4),
Tree = c("Beech", "Sugar maple", "Striped maple","Viburnum")
)

# Hubbard
#West bounding coordinate: -71.804985
#East bounding coordinate: -71.697350
#North bounding coordinate: 43.962128
#South bounding coordinate: 43.915932
mean(c(-71.804985, -71.697350))
mean(c(43.962128, 43.915932))

#Mount Moosilauke bird inventory plot, Woodstock NH.
#West bounding coordinate: -71.773936
#East bounding coordinate: -71.773936
#North bounding coordinate: 43.990567
#South bounding coordinate: 43.990567
mean(c(-71.773936, -71.773936))
mean(c(43.990567, 43.990567))

#Stinson Mountain bird inventory plot, Rumney NH.
#West bounding coordinate: -71.773936
#East bounding coordinate: -71.773936
#North bounding coordinate: 43.83500
#South bounding coordinate: 43.83500



#Russell Pond bird inventory plot, Thornton NH.
#West bounding coordinate: -71.645864
#East bounding coordinate: -71.645864
#North bounding coordinate: 44.004677
#South bounding coordinate: 44.004677




plot<- data.frame(
  plot = c(1,2,3,4), 
  Location = c("Hubbard Brook", "Moosilauke", "Russell", "Stinson"), 
  Plot_ID = c( 639, 640, 641, 642)
  )

Taxon<- data.frame(
  lep.species = c(2,3,4,5,6),
  Taxon = c("Geometrid", "Noctuid", "Notodontid", "Pyraloid, Tortricoid, Coliophorid, Psychid", "Other species"))

merge1<- merge(leps, trees, by = "tree.spec")
merge2<- merge(merge1, Taxon , by = "lep.species")
merge3<- merge(merge2, plot, by = "plot")

# deselect 2 tree species that were only sampled in 96,97
leps.sel<-subset(merge3, tree.spec <3 & count !=6)


#each plot has 4 grid letters each year. Thus these can be summed up
# sample period 6 doesnt have data  in all years->  excluded
# in 96-97 only Hubbard brooke was sampled



test<- dcast(leps.sel,  Location + count + date + Tree + Taxon ~ "Number", value.var = "number.lep", sum )


# still not clear iof there are true 0's in the data


# first make date format and separate months and years 
test$date<- as.Date(test$date, format = "%m/%d/%y")

test$Year<-as.numeric(format(test$date,"%Y")) 



arrange(unique(test[, c(1,7, 2,4)]), Location, count, Year)
# some missing data which should be 0's 


# final aggregation: ignore tree species
LTERhubbard <- dcast(leps.sel,  Plot_ID + Location + count + date + Taxon ~ "Number", value.var = "number.lep", sum )
LTERhubbard$date<- as.Date(LTERhubbard$date, format = "%m/%d/%y")
LTERhubbard$Year<-as.numeric(format(LTERhubbard$date,"%Y")) 
LTERhubbard$Month<-as.numeric(format(LTERhubbard$date,"%m") )

# same fo biomass
LTERhubbardBIOMASS <- dcast(leps.sel,  Plot_ID + Location + count + date + Taxon ~ "Number", value.var = "lepbio.mass.mg.", sum )
LTERhubbardBIOMASS$date<- as.Date(LTERhubbardBIOMASS$date, format = "%m/%d/%y")
LTERhubbardBIOMASS$Year<-as.numeric(format(LTERhubbardBIOMASS$date,"%Y")) 
LTERhubbardBIOMASS$Month<-as.numeric(format(LTERhubbardBIOMASS$date,"%m") )



write.csv(LTERhubbard, file = "C:\\Dropbox\\Dropbox\\Insect Biomass Trends\\LTER hubbard brook/leps.aggr.csv" )
write.csv(LTERhubbardBIOMASS, file = "C:\\Dropbox\\Dropbox\\Insect Biomass Trends\\LTER hubbard brook/leps.aggr.biomass.csv" )




