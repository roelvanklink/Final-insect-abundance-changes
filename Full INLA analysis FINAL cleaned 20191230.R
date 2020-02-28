#
# This script contains all analyses performed in the paper by van Klink et al. (within reasonable limitations)
#
#
## WARNING:: The INLA models require ~200 gb of RAM. A high performance computer is required. The objects are up to 400 mb,
#  and can thus also not all be loaded in a standard R environment

rm(list=ls()) 

library(INLA)
library(brinla)
library(ggplot2)
library(tidyverse)
library(reshape2)


completeData<- readRDS("DataS1.rds")
completeDataAB <- readRDS("DataS2.rds")



theme_clean<- theme_grey() + theme(panel.grid.major = element_blank(), 
                                 panel.grid.minor = element_blank(),
                                 panel.background = element_blank(), 
                                 axis.line = element_line(colour = "black") , 
                                 legend.key=element_blank())

col.scheme.realm<- c(  "Freshwater"  = "dodgerblue2", "Terrestrial" = "chocolate4", "Both realms" = "grey50")  #coral4
shps<- c("Freshwater" = 24, "Terrestrial" = 21, "Both realms" = 22)

col.scheme.global<- c(  "Global"  = "grey10", "Observed" = "grey70")  #
col.scheme.black<- c(  "Global"  = "black", "Observed" = "black")  #

sz = 0.5
brks<- c(-0.02, -0.01, 0, 0.01, 0.02, 0.03, 0.04)
perc<-(10^(brks )  *100) - 100
l<- paste(brks, paste0(round(perc,1), "%"),sep = "\n")
e<- c("","","","","","","")
lims<- c(-0.03,0.036)

mypalett<- colorRampPalette  (c("#CC0000", "#FF6666", "cornsilk2", "dodgerblue2", "dodgerblue4"), space = "rgb")


metadata_per_dataset<-  completeData %>% 
  group_by(Datasource_ID) %>%
  summarise(
    Datasource_name = unique(Datasource_name), 
    Place = unique(Country_State),
    Country = unique(Country),
    Start = min(Year, na.rm = T),
    End = max(Year, na.rm = T),
    Duration = (max(Year, na.rm = T) - min(Year, na.rm = T))+1, 
    NUMBER_OF_PLOTS =  length(unique(Plot_ID)), 
    NUMBER_OF_YEARS = length(unique(Year)), 
    Continent = unique(Continent), 
    Realm = unique(Realm)
  )

metadata_per_plot<- completeData %>% 
  group_by(Plot_ID) %>%
  summarise(
    Datasource_ID = unique(Datasource_ID),
    Datasource_name = unique(Datasource_name), 
    Duration = (max(Year, na.rm = T) - min(Year, na.rm = T))+1, 
    Start_year = min(Year, na.rm = T),
    End_year = max(Year, na.rm = T),
    Continent = unique(Continent), 
    Country_State = unique(Country_State),
    Country = unique(Country),
    Realm = unique(Realm),
    Stratum = length(unique(Stratum)),
    NUMBER_OF_PLOTS =  length(unique(Plot_ID)), # should be 1
    NUMBER_OF_SAMPLES = length(unique(paste(Year, Period))),
    NUMBER_OF_YEARS = length(unique(Year)),
    TOTAL_N = sum(Number, na.rm = T),
    PA = unique(PA)#, 
    )



# descriptive statistics #####
median(metadata_per_plot$Duration) #15yrs median duration plot level 
median(metadata_per_dataset$Duration) #20 yrs median duration dataset level 
median(metadata_per_plot$Start_year) #1996 median start year plots
median(metadata_per_dataset$Start) #1987 median start year datasets

max(metadata_per_dataset$NUMBER_OF_PLOTS) #264
max(metadata_per_plot$Duration) #81 


sum(metadata_per_dataset$Realm == "Terrestrial") # 104
sum(metadata_per_dataset$Realm == "Freshwater") # 63
sum(metadata_per_plot$Realm == "Terrestrial") # 1066
sum(metadata_per_plot$Realm == "Freshwater") # 615

# percentage plots in protected Areas
sum(metadata_per_plot$PA == "yes")/nrow(metadata_per_plot)

# of full dataset
median(completeData$Year ) #2003 median year of all data
hist(completeData$Year, las = 1)

# slope trends: 
sum(RandEfDataset$`DataID_Slope_ mean`>0) / 167 #51% positive
sum(RandEfDataset$`DataID_Slope_ mean`<0) / 167 #49% negative

sum(RandEfDataset$`DataID_Slope_ 0.025quant`>0)/167 # 10 datasets 6.5% positive 
RandEfDataset[RandEfDataset$`DataID_Slope_ 0.025quant`>0, c(18:21,25, 30)]

sum(RandEfDataset$`DataID_Slope_ 0.975quant`<0)/167 # 15datasets,  9.5% positive 
RandEfDataset[RandEfDataset$`DataID_Slope_ 0.975quant`<0, c(18:21,25, 30)]


#Histograms ####

sums<- dcast(subset(completeData, !is.na(Number)), Realm + Datasource_ID + Year ~"SumNumber", value.var = "Number" , sum)

hist(sums$Year)
ggplot(sums, aes(x=Year)) + 
  geom_histogram(binwidth=1, color = "grey30", fill = "grey30") +
  ylab ("Number of\nDatasets")+
  xlim(1920, 2020)+
  facet_wrap(.~Realm, scales = "free") +
  theme_clean +
  theme( strip.background = element_blank(), plot.margin=unit(c(5.5,12, 5.5, 5.5),"points"))

#outline only 
hist1<- ggplot(sums, aes(x=Year, fill = Realm)) + 
  geom_histogram(binwidth=1, position="dodge")
df <- ggplot_build(hist1)$data[[1]][ , c("xmin", "y", "group")]
ggplot(data = df, aes(x = xmin, y = y)) +
  geom_step()+ 
  facet_wrap(.~group, scales = "free") +
  theme_clean


ggplot(subset(sums, Realm == "Freshwater"), aes(x=Year)) + geom_histogram(binwidth=1)






# MODEL: Overall model #####

inla1 <- inla(log10(Number+1) ~ cYear + 
                f(Period_4INLA,model='iid')+
                f(Plot_ID_4INLA,model='iid')+
                f(Location_4INLA,model='iid')+
                f(Datasource_ID_4INLA,model='iid')+
                 f(Plot_ID_4INLAR,iYear,model='iid')+
                 f(Location_4INLAR,iYear,model='iid')                      +
                 f(Datasource_ID_4INLAR,iYear,model='iid')+
                f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
              control.compute = list(dic=TRUE, waic=TRUE, cpo = TRUE),
              quantiles=c(0.01, 0.025, 0.05, 0.1, 0.3, 0.5, 0.7, 0.9, 0.95, 0.975, 0.99)  ,                       
              control.predictor = list(link = 1, compute = T) , verbose = F,
              data=completeData) # 

#calculate percentage change per year and per decade
inla.pmarginal(0, inla1$marginals.fixed[[2]])
data.frame(
  var =   c("1 yr" ,"Terr 10 yr"), 
  CI2.5 =  c((10^(inla1$summary.fixed [2,4] )-1 )  *100, (10^(inla1$summary.fixed [2,4] *10)-1)  *100),#0.025 CI
  mean =   c((10^(inla1$summary.fixed [2,1] )-1)   *100, (10^(inla1$summary.fixed [2,1] *10)-1)  *100), # proportional changes per year
  CI97.5 = c((10^(inla1$summary.fixed [2,12] )-1 ) *100, (10^(inla1$summary.fixed [2,12] *10)-1)  *100)# 0.975
)


# MODEL: Confounding factors####


inlaConfoundEndYr  <- inla(log10(Number+1) ~  cYear+   cEndYear +  cYear: cEndYear +  #cYear: cDuration   +   cDuration  + #cYear: cStartYear + cStartYear +    
                           f(Plot_ID_4INLA,model='iid')+
                           f(Location_4INLA,model='iid')+
                           f(Datasource_ID_4INLA,model='iid')+
                           f(Plot_ID_4INLAR,iYear,model='iid')+
                           f(Location_4INLAR,iYear,model='iid')                      +
                           f(Datasource_ID_4INLAR,iYear,model='iid')+
                           f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                           control.compute = list(dic=TRUE, waic=TRUE, cpo = TRUE),
                           quantiles=c(0.01, 0.025, 0.05, 0.1, 0.3, 0.5, 0.7, 0.9, 0.95, 0.975, 0.99)  ,                       
                           control.predictor = list(link = 1, compute = T) , verbose = F,
                           data=completeData) # 

inlaConfounDuration  <- inla(log10(Number+1) ~ cYear+    cYear: cDuration   +   cDuration +  #cEndYear +  cYear: cEndYear + + #cYear: cStartYear + cStartYear +    
                             f(Plot_ID_4INLA,model='iid')+
                             f(Location_4INLA,model='iid')+
                             f(Datasource_ID_4INLA,model='iid')+
                             f(Plot_ID_4INLAR,iYear,model='iid')+
                             f(Location_4INLAR,iYear,model='iid')                      +
                             f(Datasource_ID_4INLAR,iYear,model='iid')+
                             f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                             control.compute = list(dic=TRUE, waic=TRUE, cpo = TRUE),
                             quantiles=c(0.01, 0.025, 0.05, 0.1, 0.3, 0.5, 0.7, 0.9, 0.95, 0.975, 0.99)  ,                       
                             control.predictor = list(link = 1, compute = T) , verbose = F,
                             data=completeData) # 

inlaConfoundStartYr  <- inla(log10(Number+1) ~  cYear+  cYear: cStartYear + cStartYear + #cEndYear +  cYear: cEndYear +  #cYear: cDuration   +   cDuration  + #    
                             f(Plot_ID_4INLA,model='iid')+
                             f(Location_4INLA,model='iid')+
                             f(Datasource_ID_4INLA,model='iid')+
                             f(Plot_ID_4INLAR,iYear,model='iid')+
                             f(Location_4INLAR,iYear,model='iid')                      +
                             f(Datasource_ID_4INLAR,iYear,model='iid')+
                             f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                             control.compute = list(dic=TRUE, waic=TRUE, cpo = TRUE),
                             quantiles=c(0.01, 0.025, 0.05, 0.1, 0.3, 0.5, 0.7, 0.9, 0.95, 0.975, 0.99)  ,                       
                             control.predictor = list(link = 1, compute = T) , verbose = F,
                             data=completeData) # 


# 

read_rds("inlaConfoundEndSUMMARY.rds") # no effect 
read_rds("inlaConfoundStartSUMMARY.rds")# no effect 
read_rds("inlaConfoundDurSUMMARY.rds") # no effect

confEnd<- read_rds("inlaConfoundEndTEST.rds")
inla.pmarginal(0, confEnd$marginals.fixed[[4]]) # for start year: 0.68

confStart<- read_rds("inlaConfoundStartTEST.rds")
inla.pmarginal(0, confStart$marginals.fixed[[4]]) # for Duration: 0.226

confDur<- read_rds("inlaConfoundDurTEST.rds")
inla.pmarginal(0, confDur$marginals.fixed[[4]]) # for End year: 0.4852








# Get random slopes #####
inla1sum<- inla1$summary.fixed

#Pull out the random effects and slopes from the overall model

#get list of unique plots and datasourceID
summary_df <- unique(completeData[,c("Plot_ID","Datasource_ID",
                                     "Plot_ID_4INLA","Datasource_ID_4INLA",
                                     "Plot_ID_4INLAR","Datasource_ID_4INLAR")])

RandEfDataset<-unique(completeData[,c("Datasource_ID", "Datasource_ID_4INLA", "Datasource_ID_4INLAR")])
#pull out random intercepts and slopes:

#data source ID
intercepts     <- inla1$summary.random$Datasource_ID_4INLA
slopes         <- inla1$summary.random$Datasource_ID_4INLAR
slopes_Location<-inla1$summary.random$Location_4INLAR
slopes_plot    <-inla1$summary.random$Plot_ID_4INLAR
names(intercepts)[2:ncol(intercepts)]      <- paste("DataID_Intercept_", names(intercepts)[2:ncol(intercepts)]) # names for dataset intercepts
names(slopes)[2:ncol(intercepts)]          <- paste("DataID_Slope_", names(slopes)[2:ncol(intercepts)])             # names for dataset slopes
names(slopes_Location)[2:ncol(intercepts)] <- paste("Loc_slp_", names(slopes_Location)[2:ncol(intercepts)]) # names for Location slopes
names(slopes_plot)[2:ncol(intercepts)]     <- paste("Plot_slp_", names(slopes_plot)[2:ncol(intercepts)])        # names for plot slopes

# datasource level slopes for Fig 1
RandEfDataset <- merge(RandEfDataset,intercepts, by.x="Datasource_ID_4INLA", by.y="ID")
RandEfDataset <- merge(RandEfDataset,slopes, by.x="Datasource_ID_4INLAR", by.y="ID")

# add up fixed slope and random slopes
load("metadata_per_dataset.RData")
RandEfDataset<- merge(RandEfDataset, metadata_per_dataset, by = "Datasource_ID")
fx<-data.frame( # df for fixed effects. because we use the moel with only 'year' no differences between realms
               fixedSlp = inla1$summary.fixed$mean[2], 
               fixedIntercept = (inla1$summary.fixed$mean[1]  ) )
RandEfDataset<- merge(RandEfDataset, fx )
RandEfDataset$slope <- RandEfDataset$'DataID_Slope_ mean'+ RandEfDataset$fixedSlp # sum of fixed and random slopes  
RandEfDataset$intercept <- RandEfDataset$'DataID_Intercept_ mean'+ RandEfDataset$fixedIntercept # sum of fixed and random slopes  
saveRDS(RandEfDataset, file = "RandEfDataset.rds")


# plot level random effects 
RandEfPlot<-unique(completeData[,c("Datasource_ID", "Datasource_ID_4INLA", "Datasource_ID_4INLAR",
                                   "Location",       "Location_4INLA",      "Location_4INLAR", 
                                   "Plot_ID",        "Plot_ID_4INLA",       "Plot_ID_4INLAR" )])
#RandEfPlot <- merge(RandEfPlot,intercepts, by.x="Datasource_ID_4INLA", by.y="ID") # not really needed here
RandEfPlot <- merge(RandEfPlot,slopes,          by.x="Datasource_ID_4INLAR", by.y="ID")
RandEfPlot <- merge(RandEfPlot,slopes_Location, by.x="Location_4INLAR", by.y="ID")
RandEfPlot <- merge(RandEfPlot,slopes_plot, by.x="Plot_ID_4INLAR", by.y="ID")
RandEfPlot <- merge(metadata_per_plot, RandEfPlot )
fx<-data.frame( # df for fixed effects
               fixedSlp = inla1$summary.fixed$mean[2], 
               fixedIntercept = c(inla1$summary.fixed$mean[1]) )
RandEfPlot<- merge(RandEfPlot, fx )
# add up fixed slope, dataset random + location Random, + plot random 
RandEfPlot$slope <- RandEfPlot$fixedSlp +  RandEfPlot$'DataID_Slope_ mean'  + RandEfPlot$'Plot_slp_ mean' +RandEfPlot$'Loc_slp_ mean' 
saveRDS(RandEfPlot, file = "RandEfPlot.rds")





# Fig 1 maps #####
  library(rgdal)
  library(sp)
  library(broom)

pts.wgs <- RandEfDataset
pts.wgs$slope.lim<- pts.wgs$slope
pts.wgs$slope.scal<-scale(pts.wgs$slope) # rescale slopes

pts.wgs$slope.lim[pts.wgs$slope.lim<(-0.02)]<- -0.02 # 
pts.wgs$slope.lim[pts.wgs$slope.lim>(0.02)]<- 0.02
pts.wgs <- SpatialPointsDataFrame(coords = data.frame(lon = pts.wgs$mean_long,
                                                      lat = pts.wgs$mean_lat),
                                  proj4string = CRS(WGS84),
                                  data = pts.wgs)

# This can be plotted on any map 






# MODEL: Realm ####

#FINAL model for realms (no correlation slope and interceps)
InlaRealm <- inla(log10(Number+1) ~  cYear:Realm+ Realm + 
                    f(Period_4INLA,model='iid')+
                    f(Location_4INLA,model='iid')+
                    f(Plot_ID_4INLA,model='iid')+
                    f(Datasource_ID_4INLA,model='iid')+
                    f(Plot_ID_4INLAR,iYear,model='iid')+
                    f(Location_4INLAR,iYear,model='iid') +
                    f(Datasource_ID_4INLAR,iYear,model='iid')+
                    f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                  control.compute = list(dic=TRUE, waic=TRUE, cpo = TRUE),
                  quantiles=c(0.01, 0.025, 0.05, 0.1, 0.3, 0.5, 0.7, 0.9, 0.95, 0.975, 0.99)  ,                       
                  control.predictor = list(link = 1, compute = T) , verbose = F,
                  data=completeData) # has lower WAIC and DIC than InlaRealm2

inlaRealmSum<- inlaRealm$summary.fixed

# percentage change per year and per decade
data.frame(
  var =   c("FW 1 yr" ,"Terr 1 yr", "FW 10 yr", "Terr 10 yr"), 
  CI2.5 =  c((10^(inlaRealmSum  [3:4,4]  )-1 ) *100, (10^(inlaRealmSum  [3:4,4] *10)-1 )  *100),#0.025 CI
  mean =   c((10^(inlaRealmSum  [3:4,1]  )-1)  *100, (10^(inlaRealmSum  [3:4,1] *10)-1 )  *100), # proportional changes per year
  CI97.5 = c((10^(inlaRealmSum  [3:4,12] )-1 ) *100, (10^(inlaRealmSum  [3:4,12] *10)-1)  *100)# 0.975
)

# get probabilities of including 0 , one sided test
ps<- NULL
for(i in 3: nrow(inlaRealmSum)){
  p<-inla.pmarginal(0, inlaRealm$marginals.fixed[[i]])
  ps<- c(ps, p) };ps

# fitted vs predicted values (figure not in paper)    
completeData$preds <- inlaRealm$summary.fitted.values$mean  #need to have control.predictor = list(link = 1) in model
ggplot(completeData,aes(x=preds,y=log10(Number+1)))+
  geom_point (aes(color = Realm), size =sz)+
  geom_abline(slope=1,intercept=0)+
  scale_color_manual(values = col.scheme.realm, guide = FALSE)+
  xlab ("Predicted values") + ylab ("Observed values")+
  facet_wrap(.~Realm, scales = "free")+
 theme_clean + 
  theme( strip.background = element_blank())

metadata_realm<-  completeData %>% 
  group_by(Realm) %>%
  summarise(
    Datasources = length(unique(Datasource_ID)),
    Plots =  length(unique(Plot_ID))) 

metadata_realm$Unit<- "Abundance &\n biomass"

Rslope<- inlaRealmSum[3:4,]
varsR<- data.frame(do.call(rbind, strsplit(rownames(Rslope), split = ":")))
Rslope<-cbind(Rslope, varsR)
Rslope$X3<- Rslope$X2
Rslope$Realm<-gsub("Realm", "", Rslope$X1)
Rslope$Unit<- "Abundance &\n biomass"
Rslope$AB <-Rslope$Unit
Rslope<- merge(Rslope, metadata_realm)
Rslope$P<- ps



table(inlaRealm$cpo$failure)
cpo = inlaRealm$cpo$cpo
ind <- 1:length(cpo)
df = data.frame(ind,cpo,
                realm=completeData$Realm,
                unit=completeData$Unit,
                dataset=completeData$Datasource_ID)

qplot(ind,cpo,data=subset(df,cpo<0.05))+
  geom_point(aes(colour=factor(dataset)))+
  facet_wrap(realm~unit) # 

pit <- inlaRealm$cpo$pit
n = length(sort(pit))
samples <- (1:n)/(n+1)
plot(samples, sort(pit), xlab="uniform quantiles", ylab="Sorted PIT values")
abline(0,1)



# Table S3 #####

#pull out variance

inla1<-inlaRealm
tauPlot <-inla1$marginals.hyperpar$`Precision for Plot_ID_4INLA`
tauDatasource <-inla1$marginals.hyperpar$`Precision for Datasource_ID_4INLA`
tauPeriod <-inla1$marginals.hyperpar$`Precision for Period_4INLA`
tauLocation <-inla1$marginals.hyperpar$`Precision for Location_4INLA`
tauPlotR <-inla1$marginals.hyperpar$`Precision for Plot_ID_4INLAR`
tauLocationR<-inla1$marginals.hyperpar$`Precision for Location_4INLAR`
tauDatasourceR <-inla1$marginals.hyperpar$`Precision for Datasource_ID_4INLAR`

#convert to standard deviations
myfun <- function(x){1/sqrt(x)}
sigmaPlot <- inla.emarginal(myfun,tauPlot)
sigmaDatasource <- inla.emarginal(myfun,tauDatasource)
sigmaPeriod <- inla.emarginal(myfun,tauPeriod)
sigmaLocation <- inla.emarginal(myfun,tauLocation)
sigmaPlotR <- inla.emarginal(myfun,tauPlotR)
sigmaLocationR <- inla.emarginal(myfun,tauLocationR)
sigmaDatasourceR <- inla.emarginal(myfun,tauDatasourceR)

data.frame(Plot_intercept = sigmaPlot, 
           Dataset_intercept = sigmaDatasource,
           Location_intercept = sigmaLocation,
           Period_intercept = sigmaPeriod,
           Plot_slope = sigmaPlotR,
           Location_slope = sigmaLocationR,
           Dataset_slope = sigmaDatasourceR)









# MODEL: Biomass vs abundance  #####
#(uses dataframe with both Units for datasets that have both) 
metadata_AB<-  completeDataAB %>% 
  group_by( Realm, Unit) %>%
  summarise(
    Datasources = length(unique(Datasource_ID)),
    Plots =  length(unique(Plot_ID)))
metadata_AB


inlaABrealm <- inla(log10(Number+1) ~ cYear: Realm: Unit +  Unit +Realm +
                        f(Period_4INLA,model='iid')+
                        f(Plotunit_4INLA,model='iid')+
                        f(Locunit_4INLA,model='iid')+
                        f(DSunit_4INLA,model='iid')+
                        f(Plotunit_4INLAR,iYear,model='iid')+
                        f(Locunit_4INLAR,iYear,model='iid')                      +
                        f(DSunit_4INLAR,iYear,model='iid')+
                        f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                    control.compute = list(dic=TRUE, waic=TRUE, cpo = TRUE),
                    quantiles=c(0.01, 0.025, 0.05, 0.1, 0.3, 0.5, 0.7, 0.9, 0.95, 0.975, 0.99)  ,                       
                    control.predictor = list(link = 1, compute = T) , verbose = F,
                    data=completeDataAB) # 


inlaABrealmsum<-inlaABrealm$summary.fixed


ps<- NULL
for(i in 4: nrow(inlaABrealm)){
  p<-inla.pmarginal(0, inlaABrealm$marginals.fixed[[i]])
  ps<- c(ps, p) }; ps


data.frame(
  var =   rownames(inlaABrealmSum)[4:7], 
  mean = (10^(inlaABrealmSum[4:7,1] )-1)  *100, # proportional changes per year
  CI2.5 = (10^(inlaABrealmSum[4:7,3] )-1 ) *100,#0.025 CI
  CI97.5 = (10^(inlaABrealmSum[4:7,5] )-1 ) *100# 0.975
)
10^(inlaABrealmSum$mean[4:7] *10)-1 # proportional changes per decade
10^(inlaABrealmSum$mean[4:7] *30)-1 # proportional changes per 30 yrs



ABSlope<- inlaABrealmSum[4:7,]
vars<-data.frame(do.call(rbind, strsplit(rownames(ABSlope), split = ":")))
ABSlope<-cbind(ABSlope, vars)
ABSlope$Realm<-gsub("Realm", "", ABSlope$X2)
ABSlope$Unit<-gsub("Unit", "", ABSlope$X1)
ABSlope$AB <-ABSlope$Unit
ABSlope$P<- ps
ABSlope<- merge(ABSlope, metadata_AB)
ABSlope<- rbind(Rslope, ABSlope)


ABSlope$text = paste0("(", ABSlope$Datasources, " | ", ABSlope$Plots, ") ")
ABSlope$Unit[ABSlope$Unit == "abundance"]<- "Abundance" ; ABSlope$Unit[ABSlope$Unit == "biomass"] <- "Biomass" 
ABSlope$Unit<- ordered(ABSlope$Unit, levels = rev(c("Abundance &\n biomass", "Abundance", "Biomass" ,  "Abundance &\n  biomass")))


ABSlope<- rbind(ABSlope, 
  data.frame(Realm = "Both realms", # add results from Inla1 to create Fig 2A
             Unit = "Abundance &\n  biomass",
             inla1$summary.fixed[2,], 
             X1 = "RealmFreshwater",
             X2 = "cYear", 
             X3 = "cYear", 
             AB = "Abundance &\n  biomass",  
             Datasources = 67,   
             Plots = 1681, 
             text = "(67 | 1681)" , 
             P = inla.pmarginal(0, inla1$marginals.fixed[[2]])))
ABSlope$Realm<- ordered(as.factor(ABSlope$Realm), levels = rev(c("Both realms",  "Terrestrial", "Freshwater" )))



# Fig 2A ####
ABplot<- ggplot(data.frame(ABSlope))+
  geom_errorbar(aes(x=Unit,ymin=X0.025quant,ymax= X0.975quant, color = Realm),alpha = 0.5,
                size = 1, width=0,   position=position_dodge(width= 0.7))+  
  geom_errorbar(aes(x=Unit,ymin=X0.05quant,ymax= X0.95quant, color = Realm),alpha = 0.75,
                size = 2, width = 0,  position=position_dodge(width= 0.7))+  
  geom_errorbar(aes(x=Unit,ymin=X0.1quant,ymax= X0.9quant, color = Realm), alpha = 1,
                size = 3, width=0, position=position_dodge(width= 0.7))+  
  geom_point(aes(x=Unit,   y=mean,  shape = Realm), 
             size = 2.5, position=  position_dodge(width = 0.7), color = "black", fill = "black",  alpha=1 )+
  scale_color_manual(values = col.scheme.realm)+
  scale_fill_manual(values = col.scheme.realm)+
  scale_shape_manual(values = shps)+
  geom_hline(yintercept=0,linetype="dashed")+
  coord_flip()+
  scale_y_continuous(breaks = brks,labels = l, limits=c(-0.030,  0.034))+
  xlab ("")+ ylab("")+  #Trend slope  \n % change per year
  geom_text(aes(x = Unit , y = 0.028, label = text, fill = Realm),  
            position = position_dodge(width = 0.7), size = 3, color = 1) +
  theme_clean +
  theme(legend.key=element_blank(), 
        legend.position="bottom", 
        axis.text.x=element_blank()) +
  geom_text(aes(x = 4.3 , y = -0.025, label = "A"),  
            size = 6, color = 1) 


png("./figures/AB plot.png", width=2000, height=700, res = 360)
ABplot
dev.off()





#MODEL: Strata ####

metadata_strata<-  completeData %>% 
  group_by(Stratum) %>%
  summarise(
    Datasources = length(unique(Datasource_ID)),
    Plots =  length(unique(Plot_ID)),
    Start_year = min(Year, na.rm = T),
    End_year = max(Year, na.rm = T)) 
metadata_strata

inlaFstratTEST <- inla(log10(Number+1) ~ cYear:Stratum + Stratum + 
                     f(Period_4INLA,model='iid')+
                     f(Location_4INLA,model='iid')+
                     f(Plot_ID_4INLA,model='iid')+
                     f(Datasource_ID_4INLA,model='iid')+
                     f(Plot_ID_4INLAR,iYear,model='iid')+
                     f(Location_4INLAR,iYear,model='iid')                      +
                     f(Datasource_ID_4INLAR,iYear,model='iid')+
                     f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                   control.compute = list(dic=TRUE, waic=TRUE, cpo = TRUE),
                   quantiles=c(0.01, 0.025, 0.05, 0.1, 0.3, 0.5, 0.7, 0.9, 0.95, 0.975, 0.99)  ,                       
                   control.predictor = list(link = 1, compute = T) , verbose = F,
                   data=completeData)


stratSlope<- inlaFstratTEST$summary.fixed[7:12,]
ps<- NULL
for(i in 7: nrow(inlaFstrat$summary.fixed )){
  p<-inla.pmarginal(0, inlaFstrat$marginals.fixed[[i]])
  ps<- c(ps, p) } ; ps


vars<-data.frame(do.call(rbind, strsplit(rownames(stratSlope), split = ":")))
stratSlope<-cbind(stratSlope, vars)
stratSlope$X1<-gsub("Stratum", "", stratSlope$X1)
stratSlope$P <- round(ps, 3)
stratSlope$ptxt<- paste0("p = ", round(stratSlope$P, 3))
stratSlope<- merge(stratSlope, metadata_strata, by.x = "X1", by.y = "Stratum")
stratSlope$text = paste0("(", stratSlope$Datasources, " | ", stratSlope$Plots, ")")
  
# reorder for graph
stratSlope$X1[stratSlope$X1 == "Underground"]<- "Below ground"
stratSlope$X1<- ordered(stratSlope$X1, levels = c("Water", "Below ground" , "Soil surface", "Herb layer", "Trees", "Air" ))
rownames(stratSlope)<-stratSlope$X1

brks<- c(-0.02, -0.01, 0, 0.01, 0.02)
perc<-(10^(brks )  *100) - 100
l<- paste(brks, paste0(round(perc,1), "%"),sep = "\n")

# Fig S2 strata #####
strataplot<- ggplot(data.frame(stratSlope))+
  geom_errorbar(aes(x=X1,ymin=X0.025quant,ymax=X0.975quant), alpha = 0.5,
                size = 1, width=0, position=position_dodge(width= 0.7), color = "grey50")+  
  geom_errorbar(aes(x=X1,ymin=X0.05quant,ymax=X0.95quant), alpha = 0.75,
                size = 2,width=0, position=position_dodge(width= 0.7), color = "grey50")+  
  geom_errorbar(aes(x=X1,ymin=X0.1quant,ymax= X0.9quant),
                size = 3, width=0, position=position_dodge(width= 0.7), color = "grey50")+  
  geom_point(aes(x=X1,   y=mean), shape = 16, color = "black", fill = "black",  alpha=1,
             size = 2.5, position=  position_dodge(width = 0.7))+
  coord_flip()+
  xlab ("")+ ylab("Trend slope  \n % change per year")+ #
  geom_hline(yintercept=0,linetype="dashed")+
  geom_text(aes(x = X1 , y = 0.028, label = text), size = 3) +
  geom_text(aes(x = X1 , y = 0.037, label = ptxt), size = 2.5) +
  scale_y_continuous(breaks = brks,labels = l, limits=c(-0.030,  0.04))+
  theme_clean
  
png("./figures/Strata plot.png", width=2000, height=1500, res = 360)
strataplot
dev.off()



# MODEL: continents #####

inlaFcont <- inla(log10(Number+1) ~ cYear: Realm:Continent + Realm + Continent + 
                    f(Period_4INLA,model='iid')+
                    f(Location_4INLA,model='iid')+
                    f(Plot_ID_4INLA,model='iid')+
                    f(Datasource_ID_4INLA,model='iid')+
                    f(Plot_ID_4INLAR,iYear,model='iid')+
                    f(Location_4INLAR,iYear,model='iid')                      +
                    f(Datasource_ID_4INLAR,iYear,model='iid')+
                    f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                  control.compute = list(dic=TRUE, waic=TRUE, cpo = TRUE),
                  quantiles=c(0.01, 0.025, 0.05, 0.1, 0.3, 0.5, 0.7, 0.9, 0.95, 0.975, 0.99)  ,                       
                  control.predictor = list(link = 1, compute = T) , verbose = F,
                  data=completeData)


#Fig S7 ####
completeData$preds <- inlaFcont$summary.fitted.values$mean  #need to have control.predictor = list(link = 1) in model
ggplot(completeData,aes(x=preds,y=log10(Number+1)))+
  geom_point (aes(color = Realm), size =sz)+
  geom_abline(slope=1,intercept=0)+
  scale_color_manual(values = col.scheme.realm, guide = FALSE)+
  xlab ("Predicted values") + ylab ("Observed values")+
  facet_wrap(Continent~Realm, scales = "free")+
  theme_clean + 
  theme( strip.background = element_blank())





#model check: 
plot(inlaFcont$cpo$cpo, main = "CPO")

pit <- inlaFcont$cpo$pit
n = nrow(completeData)
samples <- (1:n)/(n+1)
plot(samples[1:length(sort(pit))], sort(pit), xlab="uniform quantiles", ylab="Sorted PIT values")
abline(0,1)



metadata_cont<-  completeData %>% 
  group_by(Continent, Realm) %>%
  summarise(
    Datasources = length(unique(Datasource_ID)),
    Plots =  length(unique(Plot_ID)),
    Start_year = min(Year, na.rm = T),
    End_year = max(Year, na.rm = T)) 
metadata_cont


ps<- NULL
for(i in 8: nrow(inlaFcont$summary.fixed )){
  p<-inla.pmarginal(0, inlaFcont$marginals.fixed[[i]])
  ps<- c(ps, p) }

contSlope<- inlaFcont$summary.fixed[8:19,]

# africa
data.frame(
  var =   c("FW 1 yr" ,"Terr 1 yr"), 
  CI2.5 =  c((10^(contSlope  [3:4,4]  )-1 ) *100),#0.025 CI
  mean =   c((10^(contSlope  [3:4,1]  )-1)  *100), # proportional changes per year
  CI97.5 = c((10^(contSlope  [3:4,12] )-1 ) *100)# 0.975
)


vars<-data.frame(do.call(rbind, strsplit(rownames(contSlope), split = ":")))
contSlope<-cbind(contSlope, vars)
contSlope$Realm<-gsub("Realm", "", contSlope$X1);  contSlope$Continent<-gsub("Continent", "", contSlope$X2)
contSlope$P <- ps
contSlope<- merge(contSlope, metadata_cont)
contSlope$text = paste0("(", contSlope$Datasources, " | ", contSlope$Plots, ")")
contSlope$Continent<- ordered(contSlope$Continent, levels = rev(c("Europe", "North America" , "Asia", "Latin America", "Australia", "Africa" )))




# Fig 2b continents #####


contPlot<- 
ggplot(data.frame(subset(contSlope, Continent != "Africa"   )))+ # exclude africa,bc it has too wide CI's 
  geom_errorbar(aes(x=Continent,ymin=X0.025quant,ymax= X0.975quant, color = Realm),alpha = 0.5,
                size = 1, width=0,   position=position_dodge(width= 0.7))+  
  geom_errorbar(aes(x=Continent ,ymin=X0.05quant,ymax= X0.95quant, color = Realm),alpha = 0.75,
                size = 2, width = 0,  position=position_dodge(width= 0.7))+  
  geom_errorbar(aes(x=Continent ,ymin=X0.1quant,ymax= X0.9quant, color = Realm), alpha = 1,
                size = 3, width=0, position=position_dodge(width= 0.7))+  
  geom_point(aes(x=Continent ,   y=mean,  shape = Realm), 
             size = 2.5, position=  position_dodge(width = 0.7), color = "black", fill = "black",  alpha=1 )+
  scale_color_manual(values = col.scheme.realm)+
  scale_fill_manual(values = col.scheme.realm)+
  scale_shape_manual(values = shps)+
  geom_hline(yintercept=0,linetype="dashed") +
  geom_text(aes(x = Continent , y = 0.03, label = text, fill = Realm),  
            position = position_dodge(width = 0.7), size = 3, color = 1) +
  coord_flip()+
  scale_y_continuous(breaks = brks,labels = l, limits=c(-0.03,0.034))+
  xlab ("")+ ylab("")+ 
  theme_clean +
  theme(legend.key=element_blank(),
        legend.position='none', 
        axis.text.x=element_blank()) +
  geom_text(aes(x = 5.3 , y = -0.025, label = "B"),  
            size = 6, color = 1) 
#
png("continent plot.png", width=2000, height=1500, res = 360)
contPlot
dev.off()





# MODEL: Regions ##### 

inlaFregions <- inla(log10(Number+1) ~ cYear: Realm:Region + Realm + Region + 
                       f(Period_4INLA,model='iid')+
                       f(Location_4INLA,model='iid')+
                       f(Plot_ID_4INLA,model='iid')+
                       f(Datasource_ID_4INLA,model='iid')+
                        f(Plot_ID_4INLAR,iYear,model='iid')+
                        f(Location_4INLAR,model='iid')+   
                        f(Datasource_ID_4INLAR,iYear,model='iid')+
                       f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                     control.compute = list(dic=TRUE, waic=TRUE, cpo = TRUE),
                     quantiles=c(0.01, 0.025, 0.05, 0.1, 0.3, 0.5, 0.7, 0.9, 0.95, 0.975, 0.99)  ,                       
                     control.predictor = list(link = 1, compute = T) , verbose = F,
                     data=completeData)

metadata_region<-  completeData %>% 
  group_by(Region, Realm) %>%
  summarise(
    Datasources = length(unique(Datasource_ID)),
    Plots =  length(unique(Plot_ID)),
    Start_year = min(Year, na.rm = T),
    End_year = max(Year, na.rm = T)) 
print(metadata_region, n = Inf)


ps<- NULL
for(i in 30: nrow(inlaFregions$summary.fixed )){
  p<-inla.pmarginal(0, inlaFregions$marginals.fixed[[i]])
  ps<- c(ps, p) }


regionSlope<- inlaFregions$summary.fixed[30:79,]
vars<-data.frame(do.call(rbind, strsplit(rownames(regionSlope), split = ":")))
regionSlope<-cbind(regionSlope, vars)
regionSlope$Realm<-gsub("Realm", "", regionSlope$X1);  regionSlope$Region<-gsub("Region", "", regionSlope$X2)
regionSlope$P<- ps
regionSlope$ptxt<- paste0("p = ", format(round(regionSlope$P, 3), nsmall = 3))
regionSlope<- merge(regionSlope, metadata_region)
regionSlope$text = paste0("(", regionSlope$Datasources, " | ", regionSlope$Plots, ")")
regionSlope$Region<- ordered(regionSlope$Region, 
          levels = rev(c("United Kingdom", "Germany" , "Europe rest West",
                         "Sweden", "Russia Northwest","Europe rest North", #
                         "Russia Central & Volga",   "Europe rest East", 
                         "Europe rest South", "Asia East", 
                         "USA West", "USA Midwest"  , "USA Northeast","USA South", 
                         "Central America",
                         "Australia","New Zealand" ,
                          "Asia Southeast"  ,   "Asia Central", "Africa", 
                         "Canada" , "High Arctic" , "Middle east", "South America",    "Russia Ural & Siberia" )))
regionSlope$plots.ok<- regionSlope$Plots > 20
regionSlope$dataset.ok<- regionSlope$Datasources >4
regionSlope$ok <- regionSlope$plots.ok + regionSlope$dataset.ok 

brks<- c(-0.02, -0.01, 0, 0.01, 0.02, 0.03, 0.04)
perc<-(10^(brks )  *100) - 100
l<- paste(brks, paste0(round(perc,1), "%"),sep = "\n")


# include NA for missing levels: 
allVars <-expand.grid(Realm = unique(regionSlope$Realm), Region = unique(regionSlope$Region))
regionSlope<- merge(allVars, regionSlope, all.x = T)
test<-NULL
for (i in (1: length(unique(regionSlope$Region)))){
regs<-  unique(regionSlope$Region)
df<-subset(regionSlope, Region == regs[i])

if (any(is.na (df$ok))) {
  df$ok[is.na(df$ok)]<-   df$ok[!is.na(df$ok)]}
if (sum(df$ok ==0 ) ==1) {
  df[df$ok == 0 , c(3:17, 22,  27)] <- NA     # but values need to be NA, or else they'll be included
df$ok[df$ok == 0 ] <-   df$ok[df$ok != 0 ]} # needs number >0 to be included 
 test<- rbind(test, df) 
}
regionSlope<- test

# Fig S1 Regions ####
ggplot(data.frame(subset(regionSlope, ok >0 )))+ # only use >20plots or >4 datasets 
  geom_errorbar(aes(x=Region,ymin=X0.025quant,ymax= X0.975quant, color = Realm),alpha = 0.5,
                size = 1, width=0,   position=position_dodge(width= 0.7))+  
  geom_errorbar(aes(x=Region ,ymin=X0.05quant,ymax= X0.95quant, color = Realm),alpha = 0.75,
                size = 2, width = 0,  position=position_dodge(width= 0.7))+  
  geom_errorbar(aes(x=Region ,ymin=X0.1quant,ymax= X0.9quant, color = Realm), alpha = 1,
                size = 3, width=0, position=position_dodge(width= 0.7))+  
  geom_point(aes(x=Region ,   y=mean,  shape = Realm), 
             size = 2.5, position=  position_dodge(width = 0.7), color = "black", fill = "black",  alpha=1 )+
  scale_color_manual(values = col.scheme.realm)+
  scale_fill_manual(values = col.scheme.realm)+
  scale_shape_manual(values = shps)+
  geom_hline(yintercept=0,linetype="dashed") +
  xlab ("")+ ylab("Trend slope  \n % change per year")+ #
  geom_text(aes(x = Region , y = 0.04, label = text, fill = Realm),  
            position = position_dodge(width = 0.7), size = 3, color = 1) +
  geom_text(aes(x = Region , y = 0.052, label = ptxt, fill = Realm),  
            position = position_dodge(width = 0.7), size = 2.5, color = 1) +
  coord_flip()+
  scale_y_continuous(breaks = brks,labels = l, limits=c(-0.03,0.055))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(legend.key=element_blank())
# ignre warning about 10 resp 4  missing values  

  



# MODEL: Climatic zones  #####
#(tropical/ drylands / temperate / boreal-alpine )
inlaFbiom <- inla(log10(Number+1) ~ cYear: Realm:BiomeCoarse + Realm + BiomeCoarse + 
                    f(Period_4INLA,model='iid')+
                    f(Location_4INLA,model='iid')+
                    f(Plot_ID_4INLA,model='iid')+
                    f(Datasource_ID_4INLA,model='iid')+
                    f(Plot_ID_4INLAR,iYear,model='iid')+
                    f(Location_4INLAR,iYear,model='iid')                      +
                    f(Datasource_ID_4INLAR,iYear,model='iid')+
                    f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                  control.compute = list(dic=TRUE, waic=TRUE, cpo = TRUE),
                  quantiles=c(0.01, 0.025, 0.05, 0.1, 0.3, 0.5, 0.7, 0.9, 0.95, 0.975, 0.99)  ,                       
                  control.predictor = list(link = 1, compute = T) , verbose = F,
                  data=completeData)


metadata_biom<-  completeData %>% 
  group_by(BiomeCoarse, Realm) %>%
  summarise(
    Datasources = length(unique(Datasource_ID)),
    Plots =  length(unique(Plot_ID)),
    Start_year = min(Year, na.rm = T),
    End_year = max(Year, na.rm = T)) 
metadata_biom


ps<- NULL
for(i in 6: nrow(inlaFbiom$summary.fixed )){
  p<-inla.pmarginal(0, inlaFbiom$marginals.fixed[[i]])
  ps<- c(ps, p) }

biomSlope<- inlaFbiom$summary.fixed[6:13,]
vars<-data.frame(do.call(rbind, strsplit(rownames(biomSlope), split = ":")))
biomSlope<-cbind(biomSlope, vars)
biomSlope$Realm<-gsub("Realm", "", biomSlope$X1);  biomSlope$BiomeCoarse<-gsub("BiomeCoarse", "", biomSlope$X2)
biomSlope$Biome <-biomSlope$BiomeCoarse
biomSlope$P<- ps
biomSlope<- merge(biomSlope, metadata_biom)
biomSlope$text = paste0("(", biomSlope$Datasources, " | ", biomSlope$Plots, ") ")
biomSlope$BiomeCoarse<- ordered(biomSlope$BiomeCoarse, levels = rev(c("Boreal/Alpine", "Temperate" , "Drylands", "Tropical"  )))


# Fig 2c biomes ####
biomPlot<- ggplot(data.frame(biomSlope))+
  geom_errorbar(aes(x=BiomeCoarse,ymin=X0.025quant,ymax= X0.975quant, color = Realm),alpha = 0.5,
                size = 1, width=0,   position=position_dodge(width= 0.7))+  
  geom_errorbar(aes(x=BiomeCoarse ,ymin=X0.05quant,ymax= X0.95quant, color = Realm),alpha = 0.75,
                size = 2, width = 0,  position=position_dodge(width= 0.7))+  
  geom_errorbar(aes(x=BiomeCoarse ,ymin=X0.1quant,ymax= X0.9quant, color = Realm), alpha = 1,
                size = 3, width=0, position=position_dodge(width= 0.7))+  
  geom_point(aes(x=BiomeCoarse ,   y=mean,  shape = Realm), 
             size = 2.5, position=  position_dodge(width = 0.7), color = "black", fill = "black",  alpha=1 )+
  scale_color_manual(values = col.scheme.realm)+
  scale_fill_manual(values = col.scheme.realm)+
  scale_shape_manual(values = shps)+
  geom_hline(yintercept=0,linetype="dashed") +
  geom_text(aes(x = Biome , y = 0.030, label = text, fill = Realm),  
            position = position_dodge(width = 0.7), size = 3, color = 1) +
  coord_flip()+
  scale_y_continuous(breaks = brks,labels = l, limits=c(-0.03,0.034))+ 
  xlab ("")+ ylab("Trend slope  \n % change per year")+
  theme_clean +
  theme(legend.key=element_blank(), 
        legend.position="none")  + 
  geom_text(aes(x = 4.3 , y = -0.025, label = "C"),  
          size = 6, color = 1) 
  
png("biome plot.png", width=2000, height=1300, res = 360)
biomPlot
dev.off()


library(gridExtra)



gA <- ggplotGrob(ABplot)
gB <- ggplotGrob(contPlot)
gC <- ggplotGrob(biomPlot)

grid::grid.newpage()
grid::grid.draw(rbind(gA, gB, gC))







# MODELS: time slices 1960-2005 #####
metadata_full<-  completeData %>%   group_by(Continent, Realm) %>%
  summarise(    Datasources = length(unique(Datasource_ID)),
                Plots =  length(unique(Plot_ID)), 
                slice = min(Year) )

# grab data after certain years, exclude datasets with less than 10 years data in said period


cD1960 <- subset(completeData, Year >1959); dim(cD1960) # lost 345 observations
dim(cD1960); dim(completeData) # 8000 difference
metadata_1960<-  subset(cD1960, !is.na(Number )) %>% 
  group_by( Plot_ID) %>%
  summarise(  Duration = (max(Year, na.rm = T) - min(Year, na.rm = T))+1)
too.short<- subset(metadata_1960, Duration< 9)
cD1960<- cD1960[! cD1960$Plot_ID  %in% too.short$Plot_ID  , ]
length(unique(cD1960$Datasource_ID)) #165    # lost 2
length(unique(cD1960$Plot_ID)) # 1667 # lost 11
metadata_1960<-  cD1960 %>%   group_by(Continent, Realm) %>%
  summarise(    Datasources = length(unique(Datasource_ID)),
                Plots =  length(unique(Plot_ID)), 
                slice = min(Year) )



cD1970 <- subset(completeData, Year >1969)
dim(cD1970); dim(completeData) # 8000 difference
metadata_1970<-  subset(cD1970, !is.na(Number )) %>% 
  group_by( Plot_ID) %>%
  summarise(  Duration = (max(Year, na.rm = T) - min(Year, na.rm = T))+1)
too.short<- subset(metadata_1970, Duration< 9)
cD1970<- cD1970[! cD1970$Plot_ID  %in% too.short$Plot_ID  , ]
length(unique(cD1970$Datasource_ID)) #160
length(unique(cD1970$Plot_ID)) #1599
metadata_1970<-  cD1970 %>%   group_by(Continent, Realm) %>%
  summarise(    Datasources = length(unique(Datasource_ID)),
                Plots =  length(unique(Plot_ID)), 
                slice = min(Year) )


cD1980 <- subset(completeData, Year >1979)
dim(cD1980); dim(completeData) # 8000 difference
metadata_1980<-  subset(cD1980, !is.na(Number )) %>% 
  group_by( Plot_ID) %>%
  summarise(  Duration = (max(Year, na.rm = T) - min(Year, na.rm = T))+1)
too.short<- subset(metadata_1980, Duration< 9)
cD1980<- cD1980[! cD1980$Plot_ID  %in% too.short$Plot_ID  , ] ; dim(cD1980)
length(unique(cD1980$Datasource_ID)) #154
length(unique(cD1980$Plot_ID)) #1553
metadata_1980<-  cD1980 %>%   group_by(Continent, Realm) %>%
  summarise(    Datasources = length(unique(Datasource_ID)),
                Plots =  length(unique(Plot_ID)),
                slice = min(Year) )

cD1990 <- subset(completeData, Year >1989)
dim(cD1990); dim(completeData) # 10000 difference
metadata_1990<-  subset(cD1990, !is.na(Number )) %>% 
  group_by( Plot_ID) %>%
  summarise(  Duration = (max(Year, na.rm = T) - min(Year, na.rm = T))+1)
too.short<- subset(metadata_1990, Duration< 9)
cD1990<- cD1990[! cD1990$Plot_ID  %in% too.short$Plot_ID  , ] ; dim(cD1990)
length(unique(cD1990$Datasource_ID)) #123
length(unique(cD1990$Plot_ID)) #1428
metadata_1990<-  cD1990 %>%   group_by(Continent, Realm) %>%
  summarise(    Datasources = length(unique(Datasource_ID)),
                Plots =  length(unique(Plot_ID)), 
                slice = min(Year) )

cD1995 <- subset(completeData, Year >1994)
dim(cD1995); dim(completeData) # 10000 difference
metadata_1995<-  subset(cD1995, !is.na(Number )) %>% 
  group_by( Plot_ID) %>%
  summarise(  Duration = (max(Year, na.rm = T) - min(Year, na.rm = T))+1)
too.short<- subset(metadata_1995, Duration< 9)
cD1995<- cD1995[! cD1995$Plot_ID  %in% too.short$Plot_ID  , ] ; dim(cD1995)
length(unique(cD1995$Datasource_ID)) #123
length(unique(cD1995$Plot_ID)) #1428
metadata_1995<-  cD1995 %>%   group_by(Continent, Realm) %>%
  summarise(    Datasources = length(unique(Datasource_ID)),
                Plots =  length(unique(Plot_ID)), 
                slice = min(Year) )


cD2000 <- subset(completeData, Year >1999)
dim(cD2000)- dim(completeData) # 26625 difference
metadata_2000<-  subset(cD2000, !is.na(Number )) %>% 
  group_by( Plot_ID) %>%
  summarise(  Duration = (max(Year, na.rm = T) - min(Year, na.rm = T))+1)
too.short<- subset(metadata_2000, Duration< 9)
cD2000<- cD2000[! cD2000$Plot_ID  %in% too.short$Plot_ID  , ] ; dim(cD2000)
length(unique(cD2000$Datasource_ID)) #73
length(unique(cD2000$Plot_ID)) #1054
metadata_2000<-  cD2000 %>%   group_by(Continent, Realm) %>%
  summarise(    Datasources = length(unique(Datasource_ID)),
    Plots =  length(unique(Plot_ID)), 
    slice = min(Year) )

cD2005 <- subset(completeData, Year >2004)
dim(cD2005)- dim(completeData) # 38519 difference
metadata_2005<-  subset(cD2005, !is.na(Number )) %>% 
  group_by( Plot_ID) %>%
  summarise(  Duration = (max(Year, na.rm = T) - min(Year, na.rm = T))+1)
too.short<- subset(metadata_2005, Duration< 9)
cD2005<- cD2005[! cD2005$Plot_ID  %in% too.short$Plot_ID  , ] ; dim(cD2005)
length(unique(cD2005$Datasource_ID)) #44
length(unique(cD2005$Plot_ID)) #827
metadata_2005<-  cD2005 %>%   group_by(Continent, Realm) %>%
  summarise(    Datasources = length(unique(Datasource_ID)),
                Plots =  length(unique(Plot_ID)), 
                slice = min(Year) )



#example models for timeslice since 2005

inlaRealm2005 <- inla(log10(Number+1) ~ cYear: Realm + Realm +  
                       f(Period_4INLA,model='iid')+
                       f(Location_4INLA,model='iid')+
                       f(Plot_ID_4INLA,model='iid')+
                       f(Datasource_ID_4INLA,model='iid')+
                       f(Plot_ID_4INLAR,iYear,model='iid')+
                       f(Location_4INLAR,iYear,model='iid')+
                       f(Datasource_ID_4INLAR,iYear,model='iid')+
                       f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                     control.compute = list(dic=TRUE, waic=TRUE, cpo = TRUE),
                     control.predictor = list(link = 1) ,
                     quantiles=c(0.01, 0.025, 0.05, 0.1, 0.3, 0.5, 0.7, 0.9, 0.95, 0.975, 0.99)  ,                       
                     verbose = F,
                     num.threads = threads,
                     data=cD2005)
inlaRealm2005SUMMARY<- inlaRealm2005$summary.fixed # global trends



inlaCont2005 <- inla(log10(Number+1) ~ cYear: Realm:Continent + Realm + Continent + 
                f(Period_4INLA,model='iid')+
                f(Location_4INLA,model='iid')+
                f(Plot_ID_4INLA,model='iid')+
                f(Datasource_ID_4INLA,model='iid')+
                f(Plot_ID_4INLAR,iYear,model='iid')+
                f(Location_4INLAR,iYear,model='iid')+
                f(Datasource_ID_4INLAR,iYear,model='iid')+
                f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
              control.compute = list(dic=TRUE, waic=TRUE, cpo = TRUE),
              control.predictor = list(link = 1) ,
              quantiles=c(0.01, 0.025, 0.05, 0.1, 0.3, 0.5, 0.7, 0.9, 0.95, 0.975, 0.99)  ,                       
              verbose = F,
              data=cD2005)
inlaCont2005SUMMARY<- inlaCont2005$summary.fixed

# run models for all timeslices and load them:  

# realm
inlaRealm<- as.data.frame(readRDS("InlaRealmSUMMARY.rds"))[3:4, ]
inlaRealm$slice<- "Full"
inlaRealm1960<- as.data.frame(readRDS("inlaRealm1960SUMMARY.rds"))[3:4, ]
inlaRealm1960$slice<- "1960"
inlaRealm1970<- as.data.frame(readRDS("inlaRealm1970SUMMARY.rds"))[3:4, ]
inlaRealm1970$slice<- "1970"
inlaRealm1980<- as.data.frame(readRDS("inlaRealm1980SUMMARY.rds"))[3:4, ]
inlaRealm1980$slice<- "1980"
inlaRealm1990<- as.data.frame(readRDS("inlaRealm1990SUMMARY.rds"))[3:4, ]
inlaRealm1990$slice<- "1990"
inlaRealm2000<- as.data.frame(readRDS("inlaRealm2000SUMMARY.rds"))[3:4, ]
inlaRealm2000$slice<- "2000"
inlaRealm2005<- as.data.frame(readRDS("inlaRealm2005SUMMARY.rds"))[3:4, ]
inlaRealm2005$slice<- "2005"


RealmSlices<- rbind(inlaRealm1960, inlaRealm1970, inlaRealm1980, inlaRealm1990, inlaRealm2000, inlaRealm2005)
RealmSlices$Realm<- c("Freshwater", "Terrestrial")

cds<- list(cD1960, cD1970, cD1980, cD1990, cD2000, cD2005)
metadata_realm <- NULL
for (i in 1: length(cds)) {
meta<-   as.data.frame(cds[i]) %>%   group_by( Realm) %>%
  summarise(    Datasources = length(unique(Datasource_ID)),
                Plots =  length(unique(Plot_ID)), 
                slice = min(Year)         )
  
metadata_realm <- rbind(metadata_realm, meta)
  
}
metadata_realm$Continent<- as.factor("Global")


metadata_1960$slice<- 1960
metadata_1970$slice<- 1970
metadata_1980$slice<- 1980
metadata_1990$slice<- 1990
metadata_2000$slice<- 2000
metadata_2005$slice<- 2005

metadata<- bind_rows(metadata_1960, metadata_1970, metadata_1980, metadata_1990,
                       metadata_2000, metadata_2005, 
                      metadata_realm[, c("Continent", "Realm", "Datasources", "Plots", "slice") ])
# ignore warning


inlaCont1960<- as.data.frame(readRDS("inlaCont1960SUMMARY.rds"))[8:19, ]
inlaCont1970<- as.data.frame(readRDS("inlaCont1970SUMMARY.rds"))[8:19, ]
inlaCont1980<- as.data.frame(readRDS("inlaCont1980SUMMARY.rds"))[8:19, ]
inlaCont1990<- as.data.frame(readRDS("inlaCont1990SUMMARY.rds"))[8:19, ]
inlaCont2000<- as.data.frame(readRDS("inlaCont2000SUMMARY.rds"))[8:19, ]
inlaCont2005<- as.data.frame(readRDS("inlaCont2005SUMMARY.rds"))[8:19, ]

inlaCont1960$slice<- "1960"
inlaCont1970$slice<- "1970"
inlaCont1980$slice<- "1980"
inlaCont1990$slice<- "1990"
inlaCont2000$slice<- "2000"
inlaCont2005$slice<- "2005"
ContSlices<- rbind(inlaCont1960,   inlaCont1970,  inlaCont1980,  inlaCont1990,   inlaCont2000,  inlaCont2005)

vars<-data.frame(do.call(rbind, strsplit(rownames(ContSlices), split = ":")))
ContSlices<-cbind(ContSlices, vars)
ContSlices$Realm<-gsub("Realm", "", ContSlices$X1);  ContSlices$Continent<-gsub("Continent", "", ContSlices$X2)

varsR<- data.frame(do.call(rbind, strsplit(rownames(RealmSlices), split = ":")))
RealmSlices<-cbind(RealmSlices, varsR)
RealmSlices$X3<- RealmSlices$X2;  RealmSlices$Realm<-gsub("Realm", "", RealmSlices$X1);   RealmSlices$X2<-"Global"; RealmSlices$Continent<-"Global" 

slices<- rbind(RealmSlices, ContSlices)

slices<- merge(metadata, slices) ; dim(slices) #should be 59 rows


slices$minmax<- NA 
slices$minmax[slices$Continent == "Global"]       <- c(-0.01258, 0.02)
slices$minmax[slices$Continent == "Europe"]       <- c(-0.022, 0.035)
slices$minmax[slices$Continent == "North America"]<- c(-0.022, 0.035)
slices$minmax[slices$Continent == "Asia"]<- c(-0.015, 0.05)
slices$minmax[slices$Continent == "Latin America"]<- c(-0.035, 0.030)
slices$minmax[slices$Continent == "Australia"]<- c(-0.035, 0.030)     #ignore warnings
slices$minmax[slices$Continent == "Africa"]<- c(-0.025, 0.02)


slices$Continent<- ordered(slices$Continent, levels = (c("Global", "Europe", "North America" , "Asia", "Latin America", "Australia", "Africa" )))
slices$plots.ok<- slices$Plots > 20
slices$dataset.ok<- slices$Datasources >3
slices$ok <- slices$plots.ok + slices$dataset.ok 


slicePlot<- ggplot(subset(slices, ok >0 & sd < 1))+
  geom_hline(yintercept=0,linetype="dashed") +
  geom_errorbar(aes(x=as.factor(slice) ,ymin=X0.1quant, ymax=X0.9quant, color = Realm),
                size = 1, width=0,  position=position_dodge(width= 0.5))+  
  geom_errorbar(aes(x=as.factor(slice),ymin=X0.025quant,ymax=X0.975quant, color = Realm),
                width=0, alpha = 0.7, position=position_dodge(width= 0.5))+  
  geom_point(aes(x=as.factor(slice),   y=mean, shape = Realm),
             size = 1, position=  position_dodge(width = 0.5), alpha=1 ,  fill = "black", color = "black")+
  geom_blank(aes(y=minmax)) +
  scale_color_manual(values = col.scheme.realm)+
  scale_fill_manual(values = col.scheme.realm)+
  scale_shape_manual(values = shps)+
  xlab ("Start year") + ylab("Trend slope")+
facet_wrap(.~Continent, scales = "free_y")+  #)+#
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf)+
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf)+
    theme_clean +
  theme(strip.background =element_rect(fill="white"), 
        axis.line=element_line() ,
        axis.text.x  = element_text(angle=45, vjust=1, hjust = 1), 
        legend.position = "bottom")   










# MODEL: protected areas #####
# how many pa's changes status during the sampling period? and how many have missing data?
metadata_pa<-  completeData %>% 
  group_by( Realm,PA) %>%
  summarise(
    Datasources = length(unique(Datasource_ID)),
    Plots =  length(unique(Plot_ID)))
metadata_pa


inlaFpaInt <- inla(log10(Number+1) ~ cYear: PA:Realm + PA + Realm +
                     f(Period_4INLA,model='iid')+
                     f(Location_4INLA,model='iid')+
                     f(Plot_ID_4INLA,model='iid')+
                     f(Datasource_ID_4INLA,model='iid')+
                     f(Plot_ID_4INLAR,iYear,model='iid')+
                     f(Location_4INLAR,iYear,model='iid')                      +
                     f(Datasource_ID_4INLAR,iYear,model='iid')+
                     f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                   control.compute = list(dic=TRUE, waic=TRUE, cpo = TRUE),
                   control.predictor = list(link = 1) ,
                   quantiles=c(0.01, 0.025, 0.05, 0.1, 0.3, 0.5, 0.7, 0.9, 0.95, 0.975, 0.99)  ,                       
                   verbose = F, num.threads = 2)
inlaFpaSUMMARY<-inlaFpaInt$summary.fixed
inlaFpaTEST<-inlaFpaInt


ps<- NULL
for(i in 4: nrow(inlaFpaTEST$summary.fixed )){
  p<-inla.pmarginal(0, inlaFpaTEST$marginals.fixed[[i]])
  ps<- c(ps, p) }


data.frame(
var =   rownames(inlaFpa)[4:7], 
mean = (10^(inlaFpa[4:7,1] )-1)  *100, # proportional changes per year
CI2.5 = (10^(inlaFpa[4:7,4] )-1 ) *100,#0.025 CI
CI97.5 = (10^(inlaFpa[4:7,12] )-1 ) *100# 0.975
)
10^(inlaFpa[4:7,1] *10)-1 # proportional changes per decade



paSlope<- inlaFpa[4:7,]
vars<-data.frame(do.call(rbind, strsplit(rownames(paSlope), split = ":")))
paSlope<-cbind(paSlope, vars)
paSlope$Realm<-gsub("Realm", "", paSlope$X2);  paSlope$PA<-gsub("PA", "", paSlope$X1)
paSlope$factor4<-rownames(paSlope) 
paSlope$PA <-paSlope$PA
paSlope$P <- ps
paSlope<- merge(paSlope, metadata_pa)
paSlope$text = paste0("(", paSlope$Datasources, " | ", paSlope$Plots, ") ")
paSlope$PA<- factor(paSlope$PA, levels = rev(c( "no", "yes"))) 

brks<- c(-0.010, -0.005, 0, 0.005, 0.01)
perc<-(10^(brks )  *100) - 100
l<- paste(brks, paste0(round(perc,1), "%"),sep = "\n")
e<- c("","","","","","","")


PA.alph<- c(  "yes"  = 1, "no" = 0.7)
PA.col<- c("PAno:RealmFreshwater:cYear" = "dodgerblue4",  "PAyes:RealmFreshwater:cYear" = "dodgerblue2", 
           "PAno:RealmTerrestrial:cYear" =  "chocolate4", "PAyes:RealmTerrestrial:cYear" =  "chocolate3" )

saveRDS(paSlope, file = "paSlope.rds")

paSlope<- read_rds("paSlope.rds")

  
 
PAplot <- 
ggplot(data.frame(paSlope))+
  geom_errorbar(aes(x=Realm,ymin=X0.025quant,ymax= X0.975quant, color = factor4),alpha = 0.5,
              size = 1, width=0,   position=position_dodge(width= 0.7))+  
  geom_errorbar(aes(x=Realm ,ymin=X0.05quant,ymax= X0.95quant, color = factor4),alpha = 0.75,
                size = 2, width = 0,  position=position_dodge(width= 0.7))+  
  geom_errorbar(aes(x=Realm ,ymin=X0.1quant,ymax= X0.9quant, color = factor4), alpha = 1,
                size = 3, width=0, position=position_dodge(width= 0.7))+  
  geom_point(aes(x=Realm ,   y=mean,  shape = Realm, group  = factor4), 
             size = 2.5, position=  position_dodge(width = 0.7), color = "black", fill = "black",  alpha=1 )+
  geom_hline(yintercept=0,linetype="dashed")+
  coord_flip()+
  scale_y_continuous(breaks = brks,labels = l, limits=c(-0.01, 0.015))+
  xlab ("")+ ylab("Trend slope  \n % change per year")+
  geom_text(aes(x = Realm , y = 0.0125, group = PA,  label = text), 
            position = position_dodge(width = 0.7), size = 3, color = 1) +
  scale_fill_manual(values = PA.col)+
  scale_shape_manual(values = shps)+
  scale_color_manual(name="Protection status",
                    breaks=rev(c("PAno:RealmFreshwater:cYear", "PAyes:RealmFreshwater:cYear", 
                             "PAno:RealmTerrestrial:cYear", "PAyes:RealmTerrestrial:cYear")),
                    labels=rev(c("Unprotected", "Protected", "Unprotected", "Protected")), 
                    values = PA.col) + 
  guides(#fill = guide_legend(reverse = TRUE), 
        fill = FALSE,
  shape =  FALSE)+
  theme_clean +
  theme(legend.position="bottom") 
 




#  LAND USE  #####

#  USing LUH2: cover of Urban and cropland in the surrounding of the sites
# resolution : ~25km

# MODEL land use Landscape scale at end of sampling period #####  
############################################### #
 

inlaFlanduseT<- inla(log10(Number+1) ~  cYear + cYear: scale(End_cropArea)+ cYear: scale(End_urbanArea) + scale(End_cropArea) +  scale(End_urbanArea)+
                     f(Period_4INLA,model='iid')+
                     f(Location_4INLA,model='iid')+
                     f(Plot_ID_4INLA,model='iid')+
                     f(Datasource_ID_4INLA,model='iid')+
                     f(Plot_ID_4INLAR,iYear,model='iid')+
                     f(Location_4INLAR,iYear,model='iid')                      +
                     f(Datasource_ID_4INLAR,iYear,model='iid')+
                     f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                     control.compute = list(dic=TRUE, waic=TRUE, cpo = TRUE),
                     control.predictor = list(link = 1) ,
                     quantiles=c(0.01, 0.025, 0.05, 0.1, 0.3, 0.5, 0.7, 0.9, 0.95, 0.975, 0.99)  ,                        
                     data= subset(completeData, Realm == "Terrestrial"), 
                   num.threads = 2) #verbose = T,
  
 res<- inlaFlanduseT$summary.fixed[5:6,]
 
 ps<- NULL
 for(i in 5: nrow(inlaFlanduseT$summary.fixed )){
   p<-inla.pmarginal(0, inlaFlanduseT$marginals.fixed[[i]])
   ps<- c(ps, p) } ; ps # 
 cover25kmTer   <- cbind(Realm = "Terrestrial", res, variable = rownames(res), ps)
 
 
 
 inlaFlanduseFW<- inla(log10(Number+1) ~  cYear + cYear: scale(End_cropArea)+ cYear: scale(End_urbanArea) + scale(End_cropArea) +  scale(End_urbanArea)+
                        f(Period_4INLA,model='iid')+
                        f(Location_4INLA,model='iid')+
                        f(Plot_ID_4INLA,model='iid')+
                        f(Datasource_ID_4INLA,model='iid')+
                        f(Plot_ID_4INLAR,iYear,model='iid')+
                        f(Location_4INLAR,iYear,model='iid')                      +
                        f(Datasource_ID_4INLAR,iYear,model='iid')+
                        f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                       control.compute = list(dic=TRUE, waic=TRUE, cpo = TRUE),
                       control.predictor = list(link = 1) ,
                       quantiles=c(0.01, 0.025, 0.05, 0.1, 0.3, 0.5, 0.7, 0.9, 0.95, 0.975, 0.99)  ,                        
                       data= subset(completeData, Realm == "Freshwater"), 
                      num.threads = 2) #verbose = T,
 
 res<- inlaFlanduseFW$summary.fixed[5:6,] # ok

 ps<- NULL
 for(i in 5: nrow(inlaFlanduseFW$summary.fixed )){
   p<-inla.pmarginal(0, inlaFlanduseFW$marginals.fixed[[i]])
   ps<- c(ps, p) } ; ps # 
 cover25kmFW    <- cbind(Realm = "Freshwater",  res, variable = rownames(res), ps)
 
 
 


 # MODEL: Changes in land use at lanscape scale #####
#models (only use LUH2  = LANDSCAPE change) 
inlaFChangesTerr<- inla(log10(Number+1) ~ cYear + cYear :  scale(urbanization) + cYear : scale(cropification) + scale(urbanization) + scale(cropification)
                          f(Period_4INLA,model='iid')+
                          f(Location_4INLA,model='iid')+
                          f(Plot_ID_4INLA,model='iid')+
                          f(Datasource_ID_4INLA,model='iid')+
                          f(Plot_ID_4INLAR,iYear,model='iid')+
                          f(Location_4INLAR,iYear,model='iid')                      +
                          f(Datasource_ID_4INLAR,iYear,model='iid')+
                          f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                          control.compute = list(dic=TRUE, waic=TRUE, cpo = TRUE),
                          control.predictor = list(link = 1) ,
                          quantiles=c(0.01, 0.025, 0.05, 0.1, 0.3, 0.5, 0.7, 0.9, 0.95, 0.975, 0.99)  ,                        
                          data=subset(completeData, Realm == "Terrestrial"), verbose = T, num.threads = 2)
res<- inlaFChangesTerr$summary.fixed[5:6,] # fixed hessian


ps<- NULL
for(i in 5: nrow(inlaFChangesTerr$summary.fixed )){
  p<-inla.pmarginal(0, inlaFChangesTerr$marginals.fixed[[i]])
  ps<- c(ps, p) } ; ps #
LUchangeTer    <- cbind(Realm = "Terrestrial", res  , variable = rownames(res), ps)



inlaFChangesFW<- inla(log10(Number+1) ~ cYear + cYear *  urbanization + cYear * cropification + #cYear:PA +
                        f(Period_4INLA,model='iid')+
                        f(Location_4INLA,model='iid')+
                        f(Plot_ID_4INLA,model='iid')+
                        f(Datasource_ID_4INLA,model='iid')+
                        f(Plot_ID_4INLAR,iYear,model='iid')+
                        f(Location_4INLAR,iYear,model='iid')                      +
                        f(Datasource_ID_4INLAR,iYear,model='iid')+
                        f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                      control.compute = list(dic=TRUE, waic=TRUE, cpo = TRUE),
                      control.predictor = list(link = 1) ,
                      quantiles=c(0.01, 0.025, 0.05, 0.1, 0.3, 0.5, 0.7, 0.9, 0.95, 0.975, 0.99)  ,                        
                      data=subset(completeData, Realm == "Freshwater"), verbose = F, num.threads = 2)
 

res<- inlaFchangesFW$summary.fixed[5:6,] # fixed hessian

ps<- NULL
for(i in 5: nrow(inlaFChangesFW$summary.fixed )){
  p<-inla.pmarginal(0, inlaFChangesFW$marginals.fixed[[i]])
  ps<- c(ps, p) } ; ps # 0.2811565 0.1227597 

LUchangeFW     <- cbind(Realm = "Freshwater", res  , variable = rownames(res), ps)



# MODEL: landuse at local scale #####

inlaFlanduseESAterr<- inla(log10(Number+1) ~  cYear* frcCrop900m1992 + cYear* frcUrban900m1992 +# frcCrop900m + frcUrban900m +
                           f(Period_4INLA,model='iid')+
                             f(Location_4INLA,model='iid')+
                             f(Plot_ID_4INLA,model='iid')+
                             f(Datasource_ID_4INLA,model='iid')+
                             f(Plot_ID_4INLAR,iYear,model='iid')+
                             f(Location_4INLAR,iYear,model='iid')                      +
                             f(Datasource_ID_4INLAR,iYear,model='iid'), #+
                             f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                           control.compute = list(dic=TRUE, waic=TRUE, cpo = TRUE),
                           control.predictor = list(link = 1) ,
                           quantiles=c(0.01, 0.025, 0.05, 0.1, 0.3, 0.5, 0.7, 0.9, 0.95, 0.975, 0.99)  ,                        
                           data= subset(completeData, !is.na(completeData$frcCrop900m1992) & Realm == "Terrestrial"), 
                           num.threads = 2)#verbose = T,
res<- inlaFlanduseESAterr$summary.fixed[5:6,]

ps<- NULL
for(i in 5: nrow(inlaFlanduseESAterr$summary.fixed )){
  p<-inla.pmarginal(0, inlaFlanduseESAterr$marginals.fixed[[i]])
  ps<- c(ps, p) } ; ps # 

cover0.81Ter   <- cbind(Realm = "Terrestrial", res, variable = rownames(res), ps)



inlaFlanduseESAfw<- inla(log10(Number+1) ~   cYear* frcCrop900m1992 + cYear* frcUrban900m1992+
                         f(Period_4INLA,model='iid')+
                           f(Location_4INLA,model='iid')+
                           f(Plot_ID_4INLA,model='iid')+
                           f(Datasource_ID_4INLA,model='iid')+
                           f(Plot_ID_4INLAR,iYear,model='iid')+
                           f(Location_4INLAR,iYear,model='iid')                      +
                           f(Datasource_ID_4INLAR,iYear,model='iid')+
                           f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                         control.compute = list(dic=TRUE, waic=TRUE, cpo = TRUE),
                         control.predictor = list(link = 1) ,
                         quantiles=c(0.01, 0.025, 0.05, 0.1, 0.3, 0.5, 0.7, 0.9, 0.95, 0.975, 0.99)  ,                        
                         data= subset(completeData, !is.na(completeData$frcCrop900m) & Realm == "Freshwater"), 
                         num.threads = 2)#verbose = T,

res<- inlaFlanduseESAfw$summary.fixed[5:6,]


ps<- NULL
for(i in 5: nrow(inlaFlanduseESAfw$summary.fixed )){
  p<-inla.pmarginal(0, inlaFlanduseESAfw$marginals.fixed[[i]])
  ps<- c(ps, p) } ; ps # 

cover0.81FW    <- cbind(Realm = "Freshwater",  res, variable = rownames(res), ps)







# Fig S4 #####
# loop for creating figs S4 

# load 
ESAterr<- inlaFlanduseESAterr$summary.fixed[5:6,]
ESAfw<-   inlaFlanduseESAfw$summary.fixed[5:6,]
LUHterr<- inlaFlanduseT$summary.fixed[5:6,]
LUHfw<- inlaFlanduseFW$summary.fixed[5:6,]
deltaLUHterr<- inlaFChangesTerr$summary.fixed[5:6,] 
deltaLUHfw<- inlaFchangesFW$summary.fixed[5:6,]


objects<- list(deltaLUHterr, deltaLUHfw, LUHterr, LUHfw, ESAterr, ESAfw  )

realms<- c("Terrestrial", "Freshwater","Terrestrial", "Freshwater","Terrestrial", "Freshwater")  

drivers<- c("Delta cover (Landscape scale)", "Delta cover (Landscape scale)", 
            "Land cover (Landscape scale)","Land cover (Landscape scale)", 
            "Land cover (local scale)", "Land cover (local scale)")


fulldf<- NULL
for (k in 1:6) {
  mod<- objects[[k]]
  rownames(mod)<- sub("Crop", "crop", rownames(mod) )
  rownames(mod)<- sub("Urban", "urban", rownames(mod) )
  names(completeData)<- sub("Crop", "crop", names(completeData) )
  names(completeData)<- sub("Urban", "urban", names(completeData) )
  
  
  urbanVariable<- rownames(mod)[grep("urban",  rownames(mod)[3:4])+2]
  urbanVariable<- gsub("scale(","" , urbanVariable, fixed = T)
  urbanVariable<- gsub(")","" , urbanVariable, fixed = T)
  cropVariable<- rownames(mod)[grep("crop",  rownames(mod)[3:4])+2]
  cropVariable<- gsub("scale(","" , cropVariable, fixed = T)
  cropVariable<- gsub(")","" , cropVariable, fixed = T)
  
  
  minCrop       = min(completeData[completeData$Realm == realms[k]  , names(completeData) == cropVariable], na.rm = T)    
  maxCrop       = max(completeData[completeData$Realm == realms[k]  , names(completeData) == cropVariable], na.rm = T)    
  minUrb       = min(completeData[completeData$Realm == realms[k]  , names(completeData) == urbanVariable], na.rm = T)    
  maxUrb       = max(completeData[completeData$Realm == realms[k]  , names(completeData) == urbanVariable], na.rm = T)    
  sdCrop       = sd(completeData[completeData$Realm == realms[k]  , names(completeData) == cropVariable], na.rm = T)    
  meanCrop       = mean(completeData[completeData$Realm == realms[k]  , names(completeData) == cropVariable], na.rm = T)    
  meanUrb       = mean(completeData[completeData$Realm == realms[k]  , names(completeData) == urbanVariable], na.rm = T)    
  sdUrb       = sd(completeData[completeData$Realm == realms[k]  , names(completeData) == urbanVariable], na.rm = T)    
  
  
  
  crop_value<- seq(minCrop,maxCrop, length.out = 101 )
  scCrop_value<- (crop_value-meanCrop) / sdCrop
  urb_value<- seq(minUrb,maxUrb, length.out = 101 )
  scUrb_value<- (urb_value-meanUrb) / sdUrb
  
  
  SlpYrmean<- mod["cYear" , "mean"]   
  SlpYrSd <-  mod["cYear" , "sd"]
  SlpYrCropmean <-mod[grep("crop",  rownames(mod)[5:6])+4 , "mean"] 
  SlpYrCropsd    <-mod[grep("crop",  rownames(mod)[5:6])+4, "sd"]
  SlpYrUrbmean   <-mod[grep("urban", rownames(mod)[5:6])+4 , "mean"] 
  SlpYrUrbsd     <-mod[grep("urban", rownames(mod)[5:6])+4 , "sd"]
  
  
  cropEstimate = matrix(data=NA,ncol=10000,nrow=length(scCrop_value))
  urbanEstimate = matrix(data=NA,ncol=10000,nrow=length(scUrb_value))
 # rownames(trendEstimate)<- scCrop_value
  for(c in 1: length (scCrop_value)){
    for(i in 1:10000){
      cropEstimate[c,i]  = rnorm(1,SlpYrmean, SlpYrSd) + scCrop_value[c] * rnorm(1,SlpYrCropmean,SlpYrCropsd)
      urbanEstimate[c,i] = rnorm(1,SlpYrmean, SlpYrSd) + scUrb_value[c]  * rnorm(1,SlpYrUrbmean, SlpYrUrbsd)
      
    }
  }
  
  mns<- apply(cropEstimate, 1, mean)  
  sds<- apply(cropEstimate, 1, sd)  
  upperCI<- apply(cropEstimate, 1, quantile, probs = c(0.025))
  lowerCI<- apply(cropEstimate, 1, quantile, probs = c(0.975))
  cropEsts<- data.frame(Realm = realms[k], Process = cropVariable, biotope = "Cropland", driver = drivers[k], mns, sds, cover = crop_value, upperCI, lowerCI)
  
  mns<- apply(urbanEstimate, 1, mean)  
  sds<- apply(urbanEstimate, 1, sd)  
  upperCI<- apply(urbanEstimate, 1, quantile, probs = c(0.025))
  lowerCI<- apply(urbanEstimate, 1, quantile, probs = c(0.975))
  urbanEsts<- data.frame(Realm = realms[k], Process = urbanVariable, biotope = "Urban", driver = drivers[k],mns, sds, cover = urb_value, upperCI, lowerCI)
  
  res<- rbind(cropEsts, urbanEsts)
  
  fulldf<- rbind(fulldf, res)
}


cropificationPlot<-  ggplot(subset(fulldf, Process == "cropification"), 
                            aes(x = cover, y = mns, color = Realm))+
  geom_line(size = 1.5)+
  geom_ribbon(aes(x = cover,  ymin = lowerCI, ymax = upperCI, fill = Realm, color = Realm), alpha = 0.5  )+
  scale_color_manual(values = col.scheme.realm, guide = FALSE)+
  scale_fill_manual(values = col.scheme.realm, guide = F)+
  geom_hline(yintercept=0,linetype="dashed")+
  geom_vline(xintercept=0,linetype="dashed")+
  ylab ("")+ xlab (expression(Delta* ' crop cover (landscape scale)') )+
  ylim(-0.025, 0.03)+
  ggtitle  ("B")+
  theme_clean + 
  theme(  strip.background = element_blank(), 
          plot.title = element_text(hjust = 0.5))#,

urbanizationPlot<-  ggplot(subset(fulldf, Process == "urbanization"), 
                           aes(x = cover, y = mns, color = Realm))+
  geom_line(size = 1.5)+
  geom_ribbon(aes(x = cover,  ymin = lowerCI, ymax = upperCI, fill = Realm, color = Realm), alpha = 0.5  )+
  scale_color_manual(values = col.scheme.realm, guide = FALSE)+
  scale_fill_manual(values = col.scheme.realm, guide = F)+
  geom_hline(yintercept=0,linetype="dashed")+
  geom_vline(xintercept=0,linetype="dashed")+
  ylab ("")+ xlab (expression(Delta* ' urban cover (landscape scale)') )+
  ylim(-0.025, 0.03)+
  ggtitle  ("A")+
  theme_clean + 
  theme(  strip.background = element_blank(), 
          plot.title = element_text(hjust = 0.5))#,

cropPlot<-  ggplot(subset(fulldf, Process == "End_cropArea"), 
                   aes(x = cover, y = mns, color = Realm))+
  geom_line(size = 1.5)+
  geom_ribbon(aes(x = cover,  ymin = lowerCI, ymax = upperCI, fill = Realm, color = Realm), alpha = 0.5  )+
  scale_color_manual(values = col.scheme.realm, guide = FALSE)+
  scale_fill_manual(values = col.scheme.realm, guide = F)+
  geom_hline(yintercept=0,linetype="dashed")+
  ylab ("")+ xlab ("Crop cover (landscape scale)" )+
  ylim(-0.025, 0.03)+
  ggtitle  ("D")+
  theme_clean + 
  theme(  strip.background = element_blank(), 
          plot.title = element_text(hjust = 0.5))#,

urbanPlot<-  ggplot(subset(fulldf, Process == "End_urbanArea"), 
                    aes(x = cover, y = mns, color = Realm))+
  geom_line(size = 1.5)+
  geom_ribbon(aes(x = cover,  ymin = lowerCI, ymax = upperCI, fill = Realm, color = Realm), alpha = 0.5  )+
  scale_color_manual(values = col.scheme.realm, guide = FALSE)+
  scale_fill_manual(values = col.scheme.realm, guide = F)+
  geom_hline(yintercept=0,linetype="dashed")+
  ylab ("Trend slope")+ xlab ("Urban cover (landscape scale)" )+
  ylim(-0.025, 0.03)+
  ggtitle  ("C")+
  theme_clean + 
  theme(  strip.background = element_blank(), 
          plot.title = element_text(hjust = 0.5))#,

crop900mPlot<-  ggplot(subset(fulldf, Process == "frccrop900m1992"), 
                       aes(x = cover, y = mns, color = Realm))+
  geom_line(size = 1.5)+
  geom_ribbon(aes(x = cover,  ymin = lowerCI, ymax = upperCI, fill = Realm, color = Realm), alpha = 0.5  )+
  scale_color_manual(values = col.scheme.realm, guide = FALSE)+
  scale_fill_manual(values = col.scheme.realm, guide = F)+
  geom_hline(yintercept=0,linetype="dashed")+
  ylab ("")+ xlab ("Crop cover (local scale)" )+
  ylim(-0.025, 0.03)+
  ggtitle  ("F")+
  theme_clean + 
  theme(  strip.background = element_blank(), 
          plot.title = element_text(hjust = 0.5))#,

urban900mPlot<-  ggplot(subset(fulldf, Process == "frcurban900m1992"), 
                        aes(x = cover, y = mns, color = Realm))+
  geom_line(size = 1.5)+
  geom_ribbon(aes(x = cover,  ymin = lowerCI, ymax = upperCI, fill = Realm, color = Realm), alpha = 0.5  )+
  scale_color_manual(values = col.scheme.realm, guide = FALSE)+
  scale_fill_manual(values = col.scheme.realm, guide = F)+
  geom_hline(yintercept=0,linetype="dashed")+
  ylab ("")+ xlab ("Urban cover (local scale)" )+
  ylim(-0.025, 0.03)+
  theme_clean + 
  ggtitle  ("E")+
  theme(   plot.title = element_text(hjust = 0.5)
  )#,strip.background = element_blank(),



library(gridExtra)
grid.arrange(urbanizationPlot, cropificationPlot, 
             urbanPlot,cropPlot, 
             urban900mPlot, crop900mPlot,
             nrow = 3)












# MODEL Climate change landscape scale CRU #####
# relative changes in climate mean T and precipitation 
#: CRU (whole period, low resolustion) & CHELSA 1979-2013 high resolution
# test for delta Tmean and delta Prec AND for RELATIVE delta Tmean and Delta Prec 


inlaFClimChangesTerr<- inla(log10(Number+1) ~ cYear + cYear *  relDeltaTmean + cYear * relDeltaPrec +# cYear*PA +
                              f(Period_4INLA,model='iid')+
                              f(Location_4INLA,model='iid')+
                              f(Plot_ID_4INLA,model='iid')+
                              f(Datasource_ID_4INLA,model='iid')+
                              f(Plot_ID_4INLAR,iYear,model='iid')+
                              f(Location_4INLAR,iYear,model='iid')                      +
                              f(Datasource_ID_4INLAR,iYear,model='iid')+
                              f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                            control.compute = list(dic=TRUE,waic=TRUE),
                            control.inla = list(tolerance = 1e-10),
                            data=subset(completeData, Realm == "Terrestrial"), verbose = T, num.threads = 2)
res<- inlaFClimChangesTerr$summary.fixed[5:6,] 

ps<- NULL
for(i in 5: nrow(inlaFClimChangesTerr$summary.fixed )){
  p<-inla.pmarginal(0, inlaFClimChangesTerr$marginals.fixed[[i]])
  ps<- c(ps, p) } ; ps #  0.1442737 0.1399039  prestty close to 0.1

deltaTerCRU    <- cbind(Realm = "Terrestrial", res, variable = rownames(res), ps)




inlaFClimChangesFW<- inla(log10(Number+1) ~ cYear + cYear *  relDeltaTmean + cYear * relDeltaPrec + #cYear:PA +
                            f(Period_4INLA,model='iid')+
                            f(Location_4INLA,model='iid')+
                            f(Plot_ID_4INLA,model='iid')+
                            f(Datasource_ID_4INLA,model='iid')+
                            f(Plot_ID_4INLAR,iYear,model='iid')+
                            f(Location_4INLAR,iYear,model='iid')                      +
                            f(Datasource_ID_4INLAR,iYear,model='iid')+
                            f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                          control.compute = list(dic=TRUE,waic=TRUE),
                          control.inla = list(tolerance = 1e-10),
                          data=subset(completeData, Realm == "Freshwater"), verbose = F, num.threads = 2)
res<- inlaFClimChangesFW$summary.fixed[5:6,] # ok

ps<- NULL
for(i in 5: nrow(inlaFClimChangesFW$summary.fixed )){
  p<-inla.pmarginal(0, inlaFClimChangesFW$marginals.fixed[[i]])
  ps<- c(ps, p) } ; ps #  0.8260536 0.2490158
deltaFwCRU     <- cbind(Realm = "Freshwater",  res, variable = rownames(res),ps)








# MODEL climate change at local scale CHELSA #####

inlaFCHELSAChangesTerr<- inla(log10(Number+1) ~ cYear + cYear *  CHELSArelDeltaTmean + cYear * CHELSArelDeltaPrec +# cYear*PA +
                                f(Period_4INLA,model='iid')+
                                f(Location_4INLA,model='iid')+
                                f(Plot_ID_4INLA,model='iid')+
                                f(Datasource_ID_4INLA,model='iid')+
                                f(Plot_ID_4INLAR,iYear,model='iid')+
                                f(Location_4INLAR,iYear,model='iid')                      +
                                f(Datasource_ID_4INLAR,iYear,model='iid')+
                                f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                              control.compute = list(dic=TRUE,waic=TRUE),
                              control.inla = list(tolerance = 1e-10),
                              data=subset(completeData, Realm == "Terrestrial" & !is.na(CHELSArelDeltaTmean) ), 
                              verbose = F, num.threads = 2)
res<-inlaFCHELSAChangesTerr$summary.fixed[5:6,] #ok

ps<- NULL
for(i in 5: nrow(inlaFCHELSAChangesTerr$summary.fixed )){
  p<-inla.pmarginal(0, inlaFCHELSAChangesTerr$marginals.fixed[[i]])
  ps<- c(ps, p) } ; ps #   

deltaTerCHELSA <- cbind(Realm = "Terrestrial", res, variable = rownames(res), ps)


inlaFCHELSAChangesFW<- inla(log10(Number+1) ~ cYear + cYear *  CHELSArelDeltaTmean + cYear * CHELSArelDeltaPrec + #cYear:PA +
                              f(Period_4INLA,model='iid')+
                              f(Location_4INLA,model='iid')+
                              f(Plot_ID_4INLA,model='iid')+
                              f(Datasource_ID_4INLA,model='iid')+
                              f(Plot_ID_4INLAR,iYear,model='iid')+
                              f(Location_4INLAR,iYear,model='iid')                      +
                              f(Datasource_ID_4INLAR,iYear,model='iid')+
                              f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                            control.compute = list(dic=TRUE,waic=TRUE),
                            control.inla = list(tolerance = 1e-10),
                            data=subset(completeData, Realm == "Freshwater" & !is.na(CHELSArelDeltaTmean)),
                            verbose = F, num.threads = 2)
res<- inlaFCHELSAChangesFW$fummary.fixed[5:6,] # 

ps<- NULL
for(i in 5: nrow(inlaFCHELSAChangesFW$summary.fixed )){
  p<-inla.pmarginal(0, inlaFCHELSAChangesFW$marginals.fixed[[i]])
  ps<- c(ps, p) } ; ps #   0.3283940 0.8353412

deltaFwCHELSA  <- cbind(Realm = "Freshwater",  res, variable = rownames(res), ps)

drivers<- rbind(LUchangeTer, LUchangeFW, cover25kmFW   , cover25kmTer, cover0.81Ter, cover0.81FW, 
                deltaTerCRU, deltaFwCRU, deltaTerCHELSA, deltaFwCHELSA)

saveRDS(drivers, file = "Results drivers.rds")
saveRDS(drivers, file = "Results drivers.rds")




# Fig S5 #####

inlaFClimChangesTerr<-    inlaFClimChangesTerr$summary.fixed[5:6,]
inlaFClimChangesFW<-      inlaFClimChangesFW$summary.fixed[5:6,]
inlaFCHELSAChangesTerr<-  inlaFCHELSAChangesTerr$summary.fixed[5:6,]
inlaFCHELSAChangesFW  <-  inlaFCHELSAChangesFW$fummary.fixed[5:6,]



objects<- list(inlaFClimChangesTerr, inlaFClimChangesFW,  inlaFCHELSAChangesTerr, inlaFCHELSAChangesFW  )

realms<- c("Terrestrial", "Freshwater","Terrestrial", "Freshwater","Terrestrial", "Freshwater")  

drivers<- c("CRU", "CRU", "CHELSA", "CHELSA")


# loop
climdf<- NULL
for (k in 1:4) {
  mod<- as.data.frame(objects[[k]])
  
  
  tempVariable<- rownames(mod)[grep("Tmean",  rownames(mod)[3:4])+2]
  tempVariable<- gsub("scale(","" , tempVariable, fixed = T)
  tempVariable<- gsub(")","" , tempVariable, fixed = T)
  precVariable<- rownames(mod)[grep("Prec",  rownames(mod)[3:4])+2]
  precVariable<- gsub("scale(","" , precVariable, fixed = T)
  precVariable<- gsub(")","" , precVariable, fixed = T)
  
  
  minPrec       = min(completeData[completeData$Realm == realms[k]  , names(completeData) == precVariable], na.rm = T)    
  maxPrec       = max(completeData[completeData$Realm == realms[k]  , names(completeData) == precVariable], na.rm = T)    
  minTemp       = min(completeData[completeData$Realm == realms[k]  , names(completeData) == tempVariable], na.rm = T)    
  maxTemp       = max(completeData[completeData$Realm == realms[k]  , names(completeData) == tempVariable], na.rm = T)    
  sdPrec       = sd(completeData[completeData$Realm == realms[k]  , names(completeData) == precVariable], na.rm = T)    
  meanPrec       = mean(completeData[completeData$Realm == realms[k]  , names(completeData) == precVariable], na.rm = T)    
  meanTemp       = mean(completeData[completeData$Realm == realms[k]  , names(completeData) == tempVariable], na.rm = T)    
  sdTemp       = sd(completeData[completeData$Realm == realms[k]  , names(completeData) == tempVariable], na.rm = T)    

  
  
  prec_value<- seq(minPrec,maxPrec, length.out = 101 )
  scprec_value<- (prec_value-meanPrec) / sdPrec
  temp_value<- seq(minTemp,maxTemp, length.out = 101 )
  sctemp_value<- (temp_value-meanTemp) / sdTemp
  
  SlpYrmean<- mod["cYear" , "mean"]   
  SlpYrSd <-  mod["cYear" , "sd"]
  SlpYrPrecmean <-mod[grep("Prec",  rownames(mod)[5:6])+4 , "mean"] 
  SlpYrPrecsd    <-mod[grep("Prec",  rownames(mod)[5:6])+4, "sd"]
  SlpYrTmean   <-mod[grep("Tmean", rownames(mod)[5:6])+4 , "mean"] 
  SlpYrTsd     <-mod[grep("Tmean", rownames(mod)[5:6])+4 , "sd"]
  
  
  precEstimate = matrix(data=NA,ncol=10000,nrow=length(scprec_value))
  tempEstimate = matrix(data=NA,ncol=10000,nrow=length(sctemp_value))
  #rownames(trendEstimate)<- scCrop_value
  for(c in 1: length (scprec_value)){
    for(i in 1:10000){
      precEstimate[c,i]  = rnorm(1,SlpYrmean, SlpYrSd) + scprec_value[c] * rnorm(1,SlpYrPrecmean,SlpYrPrecsd)
      tempEstimate[c,i] = rnorm(1,SlpYrmean, SlpYrSd) + sctemp_value[c]  * rnorm(1,SlpYrTmean, SlpYrTsd)
      
    }
  }
  
  mns<- apply(precEstimate, 1, mean)  
  sds<- apply(precEstimate, 1, sd)  
  upperCI<- apply(precEstimate, 1, quantile, probs = c(0.025))
  lowerCI<- apply(precEstimate, 1, quantile, probs = c(0.975))
  precEsts<- data.frame(Realm = realms[k], Process = precVariable, weather = "Precipitation", driver = drivers[k], mns, sds,
                        change = prec_value,   upperCI, lowerCI)
  
  mns<- apply(tempEstimate, 1, mean)  
  sds<- apply(tempEstimate, 1, sd)  
  upperCI<- apply(tempEstimate, 1, quantile, probs = c(0.025))
  lowerCI<- apply(tempEstimate, 1, quantile, probs = c(0.975))
  tempEsts<- data.frame(Realm = realms[k], Process = tempVariable, weather = "Mean temperature", driver = drivers[k],mns, sds, 
                        change = temp_value, upperCI, lowerCI)
  
  res<- rbind(tempEsts, precEsts)
  
  climdf<- rbind(climdf, res)
}

tempPlotCCI<-  ggplot(subset(climdf, Process == "relDeltaTmean"), 
                      aes(x = change, y = mns, color = Realm))+
  geom_line(size = 1.5)+
  geom_ribbon(aes(x = change,  ymin = lowerCI, ymax = upperCI, fill = Realm, color = Realm), alpha = 0.5  )+
  scale_color_manual(values = col.scheme.realm, guide = FALSE)+
  scale_fill_manual(values = col.scheme.realm, guide = F)+
  geom_hline(yintercept=0,linetype="dashed")+
  geom_vline(xintercept=0,linetype="dashed")+
  ylab ("Trend slope")+ 
  xlab( expression(atop('Relative ' *Delta* ' mean temperature', 'landscape scale ('*Delta*'T / ' *mu*'T (K))')))+
  ylim(-0.05, 0.05)+ xlim(-0.0032, 0.01)+
  ggtitle  ("A")+
  theme_clean + 
  theme(  strip.background = element_blank(), 
          plot.title = element_text(hjust = 0.5))#,

precPlotCCI<-  ggplot(subset(climdf, Process == "relDeltaPrec"), 
                      aes(x = change, y = mns, color = Realm))+
  geom_line(size = 1.5)+
  geom_ribbon(aes(x = change,  ymin = lowerCI, ymax = upperCI, fill = Realm, color = Realm), alpha = 0.5  )+
  scale_color_manual(values = col.scheme.realm, guide = FALSE)+
  scale_fill_manual(values = col.scheme.realm, guide = F)+
  geom_hline(yintercept=0,linetype="dashed")+
  geom_vline(xintercept=0,linetype="dashed")+
  ylab ("")+ 
  xlab( expression(atop('Relative ' *Delta* ' precipitation', 'landscape scale ('*Delta*'T / ' *mu*'T (K))')))+
  ylim(-0.05, 0.05)+ xlim(-0.8, 0.8)+
  ggtitle  ("B")+
  theme_clean + 
  theme(  strip.background = element_blank(), 
          plot.title = element_text(hjust = 0.5))#,


tempPlotCHEL<-  ggplot(subset(climdf, Process == "CHELSArelDeltaTmean"), 
                       aes(x = change, y = mns, color = Realm))+
  geom_line(size = 1.5)+
  geom_ribbon(aes(x = change,  ymin = lowerCI, ymax = upperCI, fill = Realm, color = Realm), alpha = 0.5  )+
  scale_color_manual(values = col.scheme.realm, guide = FALSE)+
  scale_fill_manual(values = col.scheme.realm, guide = F)+
  geom_hline(yintercept=0,linetype="dashed")+
  geom_vline(xintercept=0,linetype="dashed")+
  ylab ("Trend slope")+ 
  xlab( expression(atop('Relative ' *Delta* ' mean temperature', 'local scale ('*Delta*'T / ' *mu*'T (K))')))+
  ylim(-0.05, 0.05)+xlim(-0.003, 0.01)+
  ggtitle  ("C")+
  theme_clean + 
  theme(  strip.background = element_blank(), 
          plot.title = element_text(hjust = 0.5))#,


precPlotCHEL<-  ggplot(subset(climdf, Process == "CHELSArelDeltaPrec"), 
                       aes(x = change, y = mns, color = Realm))+
  geom_line(size = 1.5)+
  geom_ribbon(aes(x = change,  ymin = lowerCI, ymax = upperCI, fill = Realm, color = Realm), alpha = 0.5  )+
  scale_color_manual(values = col.scheme.realm, guide = FALSE)+
  scale_fill_manual(values = col.scheme.realm, guide = F)+
  geom_hline(yintercept=0,linetype="dashed")+
  geom_vline(xintercept=0,linetype="dashed")+
  ylab ("")+ 
  xlab( expression(atop('Relative ' *Delta* ' precipitation', 'local scale ('*Delta*'T / ' *mu*'T (K))')))+
  ylim(-0.05, 0.05)+ xlim(-0.8, 0.8)+
  ggtitle  ("D")+
  theme_clean + 
  theme(  strip.background = element_blank(), 
          plot.title = element_text(hjust = 0.5))#,


grid.arrange(tempPlotCCI, precPlotCCI, 
             tempPlotCHEL,precPlotCHEL, 
             nrow = 2)






# Fig S3 #####


drivers<- read_rds("Results drivers.rds")
drivers$ptxt<- paste('p =', format(round(drivers$ps, 3), nsmall = 3))

# reorder
drivers$variable<- factor(drivers$variable, levels = rev(c("cYear:scale(urbanization)"   , 
                                                           "cYear:scale(cropification)"   , 
                                                           "cYear:scale(End_urbanArea)"       , 
                                                           "cYear:scale(End_cropArea)"  ,
                                                           "cYear:scale(frcUrban900m1992)"   ,
                                                           "cYear:scale(frcCrop900m1992)"     ,
                                                           "cYear:scale(relDeltaTmean)"      ,
                                                           "cYear:scale(relDeltaPrec)"       ,
                                                           "cYear:scale(CHELSArelDeltaTmean)" ,
                                                           "cYear:scale(CHELSArelDeltaPrec)" )))                                              

labs<-  c('cYear:scale(urbanization)' =     expression(Delta* ' urban cover (landscape scale)'), 
          'cYear:scale(cropification)' =    expression(Delta* ' cropland cover (landscape scale)'),
          'cYear:scale(End_cropArea)' =    'Cropland cover (landscape scale)',
          'cYear:scale(End_urbanArea)' =   'Urban cover (landscape scale)',
          'cYear:scale(frcCrop900m1992)' = 'Cropland cover (local scale)',
          'cYear:scale(frcUrban900m1992)' ='Urban cover (local scale)',
          'cYear:scale(relDeltaTmean)' =    expression('Relative ' *Delta* ' mean temperature (landscape scale)'),
          'cYear:scale(relDeltaPrec)' =     expression('Relative ' *Delta* ' monthly precipitation (landscape scale)'),
          'cYear:scale(CHELSArelDeltaTmean)' = expression('Relative ' *Delta* ' mean temperature (local scale)'),
          'cYear:scale(CHELSArelDeltaPrec)' = expression('Relative ' *Delta* ' montly precipitation (local scale)'))


drivers$Realm <- factor(drivers$Realm, levels = rev(c("Terrestrial", "Freshwater" )))
brks<- c(-0.006, -0.004, -0.002, 0, 0.002, 0.004, 0.006)



driverEffects<- ggplot( drivers   ) + 
  xlab ("")+ ylab ("")+geom_hline(yintercept=0,linetype="dashed")+
  geom_errorbar(aes(x=variable,ymin=X0.025quant,ymax= X0.975quant, color = Realm),alpha = 0.5,
                size = 1, width=0,   position=position_dodge(width= 0.7))+  
  geom_errorbar(aes(x=variable ,ymin=X0.05quant,ymax= X0.95quant, color = Realm),alpha = 0.75,
                size = 2, width = 0,  position=position_dodge(width= 0.7))+  
  geom_errorbar(aes(x=variable ,ymin=X0.1quant,ymax= X0.9quant, color = Realm), alpha = 1,
                size = 3, width=0, position=position_dodge(width= 0.7))+  
  geom_point(aes(x=variable ,   y=mean,  shape = Realm), 
             size = 2.5, position=  position_dodge(width = 0.7), color = "black", fill = "black",  alpha=1 )+  scale_x_discrete("", labels = labs)+
  scale_color_manual(values = col.scheme.realm)+
  scale_y_continuous("Parameter estimate",  breaks = brks, limits=c(-0.007,0.007))+
  geom_text(aes(x = variable , y = 0.0068, label = ptxt, fill = Realm),  
            position = position_dodge(width = 0.7), size = 2.5, color = 1) +
  coord_flip()+
  theme_clean + 
  theme(legend.position = "", 
        strip.text = element_blank(), 
        strip.background = element_blank())





# SM MODEL analysis excluding North America #####

inlaFrealmExclNAm <- inla(log10(Number+1) ~ cYear: Realm+ Realm  +
                            f(Period_4INLA,model='iid')+
                            f(Location_4INLA,model='iid')+
                            f(Plot_ID_4INLA,model='iid')+
                            f(Datasource_ID_4INLA,model='iid')+
                            f(Plot_ID_4INLAR,iYear,model='iid')+
                            f(Location_4INLAR,iYear,model='iid')                      +
                            f(Datasource_ID_4INLAR,iYear,model='iid')+
                            f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                          control.compute = list(dic=TRUE, waic=TRUE, cpo = TRUE),
                          quantiles=c(0.01, 0.025, 0.05, 0.1, 0.3, 0.5, 0.7, 0.9, 0.95, 0.975, 0.99)  ,                       
                          control.predictor = list(link = 1, compute = T) , verbose = F,
                          data=subset(completeData, Continent != "North America"))

inlaFrealmExclNAmSUMMARY<-  inlaFrealmExclNAm$summary.fixed


# get probabilities of including 0 , one sided test
ps<- NULL
for(i in 3: nrow(inlaRealmExclNAm$summary.fixed)){
  p<-inla.pmarginal(0, inlaRealmExclNAm$marginals.fixed[[i]])
  ps<- c(ps, p) }; ps

data.frame(
  var =   c("FW 1 yr" ,"Terr 1 yr", "FW 10 yr", "Terr 10 yr"), 
  CI2.5 =  c((10^(inlaFrealmExclNAmSUMMARY  [3:4,4] )-1 ) *100, (10^(inlaFrealmExclNAmSUMMARY  [3:4,4] *10)-1)  *100),#0.025 CI
  mean =   c((10^(inlaFrealmExclNAmSUMMARY  [3:4,1] )-1)  *100, (10^(inlaFrealmExclNAmSUMMARY  [3:4,1] *10)-1)  *100), # proportional changes per year
  CI97.5 = c((10^(inlaFrealmExclNAmSUMMARY  [3:4,12] )-1 ) *100, (10^(inlaFrealmExclNAmSUMMARY  [3:4,12] *10)-1)  *100)# 0.975
)




# SM MODEL Analyses excluding outliers #####

RandEfDataset<- readRDS("RandEfDataset.rds")
hist(RandEfDataset$slope)

# definition: values outside the 1.5* the interquantile distance above or below the 0.25 and 0.75 quantiles

lowOutl<-  RandEfDataset$Datasource_ID[RandEfDataset$slope < 
                                         quantile(RandEfDataset$slope, 0.25)- 1.5*IQR(RandEfDataset$slope)]
hiOutl<-  RandEfDataset$Datasource_ID[RandEfDataset$slope >
                                        quantile(RandEfDataset$slope, 0.75)+ 1.5*IQR(RandEfDataset$slope)]
outliers<- c(lowOutl, hiOutl)
sort(outliers)
completeData5<- completeData[! completeData$Datasource_ID %in% outliers, ]


realmExclOTEST <- inla(log10(Number+1) ~  cYear:Realm+ Realm + 
                    f(Period_4INLA,model='iid')+
                    f(Location_4INLA,model='iid')+
                    f(Plot_ID_4INLA,model='iid')+
                    f(Datasource_ID_4INLA,model='iid')+
                    f(Plot_ID_4INLAR,iYear,model='iid')+
                    f(Location_4INLAR,iYear,model='iid') +
                    f(Datasource_ID_4INLAR,iYear,model='iid')+
                    f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                  control.compute = list(dic=TRUE, waic=TRUE, cpo = TRUE),
                  quantiles=c(0.01, 0.025, 0.05, 0.1, 0.3, 0.5, 0.7, 0.9, 0.95, 0.975, 0.99)  ,                       
                  control.predictor = list(link = 1, compute = T) , verbose = F,
                  data=completeData5) #



realmExclOsum<- realmExclOTEST$summary.fixed

ps<- NULL
for(i in 3: nrow(realmExclO$summary.fixed )){
  p<-inla.pmarginal(0, realmExclO$marginals.fixed[[i]])
  ps<- c(ps, p) };ps

# check if the outliers were correctly excluded
test<- merge(RandEfDataset,realmExclO$summary.random$Datasource_ID_4INLAR, by.x="Datasource_ID_4INLAR", by.y="ID")
sort(test$Datasource_ID) # correctly all missing 


data.frame(
  var =   c("FW 1 yr" ,"Terr 1 yr", "FW 10 yr", "Terr 10 yr"), 
  CI2.5 =  c((10^(realmExclOsum  [3:4,4]  )-1 ) *100, (10^(realmExclOsum [3:4,4] *10)-1 )  *100),#0.025 CI
  mean =   c((10^(realmExclOsum  [3:4,1]  )-1)  *100, (10^(realmExclOsum  [3:4,1] *10)-1 )  *100), # proportional changes per year
  CI97.5 = c((10^(realmExclOsum  [3:4,12] )-1 ) *100, (10^(realmExclOsum  [3:4,12] *10)-1)  *100)# 0.975
)

# continents
contExclOTEST <- inla(log10(Number+1) ~ cYear: Realm:Continent + Realm + Continent + 
                    f(Period_4INLA,model='iid')+
                    f(Location_4INLA,model='iid')+
                    f(Plot_ID_4INLA,model='iid')+
                    f(Datasource_ID_4INLA,model='iid')+
                    f(Plot_ID_4INLAR,iYear,model='iid')+
                    f(Location_4INLAR,iYear,model='iid')                      +
                    f(Datasource_ID_4INLAR,iYear,model='iid')+
                    f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                  control.compute = list(dic=TRUE, waic=TRUE, cpo = TRUE),
                  quantiles=c(0.01, 0.025, 0.05, 0.1, 0.3, 0.5, 0.7, 0.9, 0.95, 0.975, 0.99)  ,                       
                  control.predictor = list(link = 1, compute = T) , verbose = F,
                  data=completeData5)


contExclOsum<- contExclOTEST$summary.fixed
ps<- NULL
for(i in 8: nrow(contExclO$summary.fixed )){
  p<-inla.pmarginal(0, contExclO$marginals.fixed[[i]])
  ps<- c(ps, p) }; ps

cbind(contExclO$summary.fixed [8: nrow(contExclO$summary.fixed ),], ps)
# for north America
data.frame(
  var =   c("FW 1 yr" ,"Terr 1 yr"), 
  CI2.5 =  c((10^(contExclOsum  [18:19,4]  )-1 ) *100),#0.025 CI
  mean =   c((10^(contExclOsum  [18:19,1]  )-1)  *100), # proportional changes per year
  CI97.5 = c((10^(contExclOsum  [18:19,12] )-1 ) *100)# 0.975
)

# australia
data.frame(
  var =   c("FW 1 yr" ,"Terr 1 yr" ), 
  CI2.5 =  c((10^(contExclOsum  [14:15,4]  )-1 ) *100),#0.025 CI
  mean =   c((10^(contExclOsum  [14:15,1]  )-1)  *100), # proportional changes per year
  CI97.5 = c((10^(contExclOsum  [14:15,12] )-1 ) *100)# 0.975
)


cont<- inlaFcont$summary.fixed[8:19,]  # compare to original continent model 
names(contExclOsum)<- paste0(names(contExclOsum), "exclO")
cbind(contExclOsum[8:19,], ps)
cont<- cbind(cont, contExclOsum)
cont<- cont[8:19,]
vars<-data.frame(do.call(rbind, strsplit(rownames(cont), split = ":")))
cont<-cbind(cont, vars)
cont$Realm<-gsub("Realm", "", cont$X1)
cont$Continent<-gsub("Continent", "", cont$X2)

cont.correlation <- ggplot(subset(cont, Continent != "Africa"), aes(x = mean, y = meanexclO, shape = Realm , color = Continent)) + 
  
  geom_errorbarh(aes(y=meanexclO ,xmin = X0.025quant, xmax=X0.975quant))+
  geom_errorbarh(aes(y=meanexclO ,xmin=X0.1quant, xmax=X0.9quant), size =1.5)+
  
  geom_errorbar(aes(x=mean ,ymin=X0.025quantexclO, ymax=X0.975quantexclO))+
  geom_errorbar(aes(x=mean ,ymin=X0.1quantexclO, ymax=X0.9quantexclO), size = 1.5)+
  
  geom_point( size = 3, color = "black")+
  geom_abline(aes(intercept = 0, slope = 1))+
  geom_hline(aes(yintercept = 0))+
  geom_vline(xintercept = 0,linetype="dashed")+
  scale_color_manual(values = col.scheme.cont)+
  xlab ("full model estimates") + ylab("esimates of model excluding outliers") +
  theme_clean



# Climatic zones
biomExclOTEST <- inla(log10(Number+1) ~ cYear: Realm:BiomeCoarse + Realm + BiomeCoarse + 
                    f(Period_4INLA,model='iid')+
                    f(Location_4INLA,model='iid')+
                    f(Plot_ID_4INLA,model='iid')+
                    f(Datasource_ID_4INLA,model='iid')+
                    f(Plot_ID_4INLAR,iYear,model='iid')+
                    f(Location_4INLAR,iYear,model='iid')                      +
                    f(Datasource_ID_4INLAR,iYear,model='iid')+
                    f(iYear,model='ar1', replicate=as.numeric(Plot_ID_4INLA)),
                  control.compute = list(dic=TRUE, waic=TRUE, cpo = TRUE),
                  quantiles=c(0.01, 0.025, 0.05, 0.1, 0.3, 0.5, 0.7, 0.9, 0.95, 0.975, 0.99)  ,                       
                  control.predictor = list(link = 1, compute = T) , verbose = F,
                  data=completeData5)




biomExclOsum<- biomExclOTEST$summary.fixed[6: nrow(biomExclO$summary.fixed),]

ps<- NULL
for(i in 6: nrow(biomExclOTEST$summary.fixed )){
  p<-inla.pmarginal(0, biomExclOTEST$marginals.fixed[[i]])
  ps<- c(ps, p) }; ps

cbind( biomExclOsum, ps)
# for temperate
data.frame(
  var =   c(c('RealmFreshwater:BiomeCoarseTemperate:cYear', 'RealmTerrestrial:BiomeCoarseTemperate:cYear')), 
  CI2.5 =  c((10^(biomExclOsum  [c('RealmFreshwater:BiomeCoarseTemperate:cYear', 'RealmTerrestrial:BiomeCoarseTemperate:cYear'),4]  )-1 ) *100),#0.025 CI
  mean =   c((10^(biomExclOsum  [c('RealmFreshwater:BiomeCoarseTemperate:cYear', 'RealmTerrestrial:BiomeCoarseTemperate:cYear'),1]  )-1)  *100), # proportional changes per year
  CI97.5 = c((10^(biomExclOsum  [c('RealmFreshwater:BiomeCoarseTemperate:cYear', 'RealmTerrestrial:BiomeCoarseTemperate:cYear'),12] )-1 ) *100)# 0.975
)

# for drylands
data.frame(
  var =   c('RealmFreshwater:BiomeCoarseDrylands:cYear' , 'RealmTerrestrial:BiomeCoarseTemperate:cYear'), 
  CI2.5 =  c((10^(biomExclOsum  [c('RealmFreshwater:BiomeCoarseDrylands:cYear' , 'RealmTerrestrial:BiomeCoarseTemperate:cYear'),'X0.025quant']  )-1 ) *100),#0.025 CI
  mean =   c((10^(biomExclOsum  [c('RealmFreshwater:BiomeCoarseDrylands:cYear' , 'RealmTerrestrial:BiomeCoarseTemperate:cYear'),'mean']  )-1)  *100), # proportional changes per year
  CI97.5 = c((10^(biomExclOsum  [c('RealmFreshwater:BiomeCoarseDrylands:cYear' , 'RealmTerrestrial:BiomeCoarseTemperate:cYear'),'X0.975quant'] )-1 ) *100)# 0.975
)


biom<- inlaFbiom$summary.fixed[6:13,] # load original biome model


names(biomExclOsum)<- paste0(names(biomExclOsum), "exclO")
cbind(biomExclOsum[6:13, ], ps)
biom<- cbind(biom, biomExclOsum)
vars<-data.frame(do.call(rbind, strsplit(rownames(biom), split = ":")))
biom<-cbind(biom, vars)
biom$Realm<-gsub("Realm", "", biom$X1)
biom$biome<-gsub("BiomeCoarse", "", biom$X2)

biom.correlation <- ggplot(biom, aes(x = mean, y = meanexclO, shape = Realm , color = biome)) + 
  geom_errorbarh(aes(y=meanexclO ,xmin = X0.025quant, xmax=X0.975quant))+
  geom_errorbarh(aes(y=meanexclO ,xmin=X0.1quant, xmax=X0.9quant), size =1.5)+
  geom_errorbar(aes(x=mean ,ymin=X0.025quantexclO, ymax=X0.975quantexclO))+
  geom_errorbar(aes(x=mean ,ymin=X0.1quantexclO, ymax=X0.9quantexclO), size = 1.5)+
  geom_point( size = 3, color = 1)+
  
  geom_abline(aes(intercept = 0, slope = 1))+
  geom_hline(aes(yintercept = 0))+
  geom_vline(xintercept = 0,linetype="dashed")+
  #  scale_color_manual(values = col.scheme.cont)+
  xlab ("full model estimates") + ylab("esimates of model excluding outliers") +
  theme_clean





















