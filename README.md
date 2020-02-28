# Final-insect-abundance-changes
This is the repository for the final analysis used in Van Klink et al 2020 Meta-analysis reveals declines in terrestrial but increases in freshwater insect abundances. With this code all analyses in the paper can be reproduced, and the graphs, except the maps, can be remade. Code to calculate the values of the dots on the map is provided. These values can then be plotted on any map.  

<hr>
##WARNING:  

Each INLA model requires some 200GB RAM to run, and produces an object of 100 - 400MB in size. Analyses should thus be performed on a high performance computer, and the processing of the results is limited by the capacity of the local machine. In our experience at least 10 models can be loaded in R at a time.  

<hr>
###The files in this repository are:  

Full INLA analysis FINAL cleaned 20191230 - this will reproduce the analayses using the file DataS1 and DataS2 from the online supplementary material of the paper.  

For processing 5 datasets that precluded publication of the derived numbers in DataS1 and DataS2:  

dataSource_ID 1339: These data are avialable at https://belovskylab.nd.edu/assets/231833/nbr_grasshopper_density.pdf The column 'DENSITY' of each site was used as input  

DataSource_ID 1416: These data are available at: http://data.hubbardbrook.org/data/dataset.php?id=82 to process these data use: 'LTER Hubbard Book data processing.r'  

DataSource_ID 1404: the original data can be downloaded here: https://data.g-e-m.dk/ use the Zackenberg insect monitoring data. To process this file use the code 'Greenland data processing revised dec 2019.r'. Only data until including 2017 were used. The years 2010 and 2011 were excuded.  

DataSource_ID's 1474 and 1475: The derived numbers for are available at https://knb.ecoinformatics.org/view/urn:uuid:4ac97c2d-d3af-4430-96d4-0873b98e8aa8
