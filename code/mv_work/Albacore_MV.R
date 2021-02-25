################# Multivariate statistics for Albacore prey #################

getwd()
setwd("/Users/natasha/Documents/POSTDOC/TUNA DIETS/TUNASTATS/data")

install.packages("ggplot2")
install.packages("vegan")
install.packages("viridis")
install.packages("RColorBrewer")
install.packages("wesanderson")
library(vegan)
library(ggplot2)
library(viridis)
library(RColorBrewer)
library(wesanderson)
library(BiodiversityR)
library(here)
library(readr)

# From GitHub
install_github("MoBiodiv/mobr")
library(mobr)
devtools::install_github('bbc/bbplot')
library(bbplot)

#### MV PREY SPECIES Present/Absence ####


## Loading the Albacore prey presence/absence data
PreyDat <- read.csv("Albacore_preyPA.csv",header=TRUE)
# A good function for checking the dataframe is saved properly/things are in the right place
str(PreyDat)

# For downstream analyses I am going to "carve out" different parts of the dataframe, here I'm
# labelling the prey species columns for later use in analyses, (so those are columns 4 to 484).
PreyPA <- PreyDat[,4:467]
str(PreyDat)

# Make sure or convert to presence/absence for the SP this should already be the case
PreyPA[PreyPA>0] = 1

# Labelling the explanatory variable columns, here the only two things I might look at are 
# OceanBasinQ and YearSample, so columns 1 and 2.
PreyPASite <- PreyDat[,1:3]

# Normally the different levels of an explanatory variable are going to load in alphabetically, for example:
PreyPASite$OceanBasinQ
# That prints all the levels of OceanBasinQ and that doesn't always make sense, so this piece of code to label them in the 
# particular order that I think makes sense in a graph. See below.
PreyPASite$OceanBasinQ <- factor(PreyPASite$OceanBasinQ, levels = c("Mediterranean", "NE Atlantic",
                                                                    "NW Atlantic", "SW Atlantic",
                                                                    "NE Indian", "NE Pacific", 
                                                                    "SW Pacific"))
## Summary stats:
SpO <- specnumber(PreyPA, PreyPASite$OceanBasinQ)
SpO
#Mediterranean   NE Atlantic   NW Atlantic   SW Atlantic     NE Indian    NE Pacific    SW Pacific 
#77           128            68            15            13           264            62 

# Colour vector: vector holding the colours. We have 7 locations in the OceanBasinQ variable, so
# we need 7 colours named. For vegan nMDS plots, the colours aren't great and I need to maybe improve them.
cols <- c("red", "pink", "orange", "dark orange", "darkgreen", "blue", "light blue")

## NMDS plot of the different assemblage composition of prey species for each study/year/ocean basin
# NMDS with bray-curtis distance
PreyPAOrd <- metaMDS(PreyPA, distance="bray", k=3)
# Here you want to check the output of this code, and the "stress" values. It basically needs to be < 0.2, 
# because above that means that your data is actually very disparate and thus difficult to plot, it will mean that your 
# nMDS is unreliable. Here our stress is < 0.1 and so low that the scaling model actually stopped and you get a 
#"*** Solution reached" message. This meant here that we had low stress and that the plot is good!

# Below script makes an empty plot
plot(PreyPAOrd, type = "n")
# Add points colored by Environmental Variable Management
points(PreyPAOrd, col = cols[PreyPASite$OceanBasinQ], pch = 16, cex = 2.5)
# Export this graph as is, or add more info
ordiellipse(PreyPAOrd, PreyPASite$OceanBasinQ)
ordihull(PreyPAOrd, PreyPASite$OceanBasinQ, draw = c("polygon"))

# Add legend, this code needs to get tweaked depending on your data, you need to make sure it does not cover up any data accidentally
# if it does, then we need to add code to manuall position the legend.
legend("bottomleft", legend=levels(PreyPASite$OceanBasinQ), col=cols, pch = 16)

# Sometimes for presentations, I just export the image without the legend, then export a separate figure with legend
# and mess around with the style in post-processing steps either in Abode Illustrator or by cropping things in powerpoint.
# This code prints a bigger legend which can be useful in presentations.
legend("bottomright", legend=levels(PreyPASite$OceanBasinQ), col=cols, pch = 16, cex = 2.5)

# Now we can make the same graph using PredLifeStage as an explanatory variable/colour for the dots:
# Check levels of that factor:
levels(PreyPASite$PredLifeStage)
# I'm going to back to the excel sheet and clean these up a little to just read: adult, juvenile, juvenile/adult and larvae.
# Checking levels again, looked good to use.
# Now we select four colours:
colL <- c("red", "pink", "dark orange", "blue")
plot(PreyPAOrd, type = "n")
points(PreyPAOrd, col = colL[PreyPASite$PredLifeStage], pch = 16, cex = 2.5)
legend("bottomleft", legend=levels(PreyPASite$PredLifeStage), col=colL, pch = 16, cex = 1.75)


#### RUN STATISTICAL TESTS ON MV SPECIES FOR PA

## PCA trial
## Data camp tutorial: https://www.datacamp.com/community/tutorials/pca-analysis-r

data(mtcars)
str(mtcars)
# remove non-numerical data
mtcars.pca <- prcomp(mtcars[,c(1:7,10,11)], center = TRUE,scale. = TRUE)
summary(mtcars.pca)
?prcomp
# results of PCA
str(mtcars.pca)

#### MV PREY SPECIES FO ####

## Loading the Albacore prey presence/absence data
PreyFODat <- read.csv("Albacore_preyFO.csv",header=TRUE)
# A good function for checking the dataframe is saved properly/things are in the right place
str(PreyFODat)

# Needed to label the prey species dataframe, but I updated the column number.
# See code just below, I had to clean out some empty data columns and rows, so there are few 
# rows to select here now.
PreyFO <- PreyFODat[,6:370]

# Now, we a little problem, that I suspect means that there are some empty columns and rows,
# that need to be deleted because there are species in the above data export that do not have %FO
# Code to omit species that were never observed in the dataset, 
# essentially deleting any columns and rows that sum to zero
PreyFO=PreyFO[,-which(colSums(PreyFO)==0)]
PreyFO=PreyFO[-which(rowSums(PreyFO)==0),]
# You'll notice this piece of code means that the dataframe now has 380 columns
# and we're going to need to save this so we can use it again rather than the original

# Labelling the explanatory variable columns, here the only two things I might look at are 
# OceanBasinQ and YearSample, so columns 1 and 2.
PreyFOSite <- PreyFODat[,1:5]

# Normally the different levels of an explanatory variable are going to load in alphabetically,
# and that doesn't always make sense, so this piece of code tells "it" to label them in the 
# particular order that I think makes sense in a graph. See below.
PreyFOSite$OceanBasinQ <- factor(PreyFOSite$OceanBasinQ, levels = c("Mediterranean", "NE Atlantic",
                                                                "NW Atlantic", "SW Atlantic",
                                                                "SE Indian", "NE Pacific", 
                                                                "SW Pacific"))


# Colour vector: vector holding the colours. We have 7 locations in the OceanBasinQ variable, so
# we need 7 colours named. For vegan nMDS plots, the colours aren't great and I need to maybe
# improve them.
cols <- c("red", "pink", "orange", "purple", "dark grey", "blue", "light blue")

# NMDS with bray-curtis distance
PreyFOOrd <- metaMDS(PreyFO, distance="bray", k=3)

# Below script makes an empty plot
plot(PreyFOOrd, type = "n")
# Add points colored by Environmental Variable Management
points(PreyPAOrd, col = cols[PreySite$OceanBasinQ], pch = 16, cex = 3)
legend("right", legend=levels(PreySite$OceanBasinQ), col=cols, pch = 16, cex = 1.5)
par(mfrow=c(1,1))


#### MV PREY FAMILIES PA ####

## Loading the Albacore prey presence/absence data
FamPADat <- read.csv("Albacore_preyfamPA.csv",header=TRUE)
# A good function for checking the dataframe is saved properly/things are in the right place
str(FamPADat)

# Prey families data.
FamPA <- FamPADat[,6:186]

# Labelling the explanatory variable columns, here the only two things I might look at are 
# OceanBasinQ and YearSample, so columns 1 and 2.
FamPADatSite <- FamPADat[,1:5]

# Normally the different levels of an explanatory variable are going to load in alphabetically,
# and that doesn't always make sense, so this piece of code tells "it" to label them in the 
# particular order that I think makes sense in a graph. See below.
FamPADatSite$OceanBasinQ <- factor(FamPADat$OceanBasinQ, levels = c("Mediterranean", "NE Atlantic",
                                                                    "NW Atlantic", "SW Atlantic",
                                                                    "SE Indian", "NE Pacific", 
                                                                    "SW Pacific"))


# Colour vector: vector holding the colours. We have 7 locations in the OceanBasinQ variable, so
# we need 7 colours named. For vegan nMDS plots, the colours aren't great and I need to maybe
# improve them.
cols <- c("red", "pink", "orange", "dark orange", "green", "blue", "light blue")

# NMDS with bray-curtis distance
FamPAOrd <- metaMDS(FamPA, distance="bray", k=3)

# Below script makes an empty plot
plot(FamPAOrd, type = "n")
# Add points colored by Environmental Variable Management
points(FamPAOrd, col = cols[FamPADatSite$OceanBasinQ], pch = 16, cex = 3)
legend("right", legend=levels(FamPADatSite$OceanBasinQ), col=cols, pch = 16, cex = 1.5)
par(mfrow=c(1,1))


#### MV PREY FAMILIES FO ####

## Loading the Albacore prey presence/absence data
FamFODat <- read.csv("Albacore_preyfamFO.csv",header=TRUE)
# A good function for checking the dataframe is saved properly/things are in the right place
str(FamFODat)

# Name prey families data.
FamFO <- FamFODat[,4:166]

# No need to do the weeding exercise below, because there were no zero sum columns or rows,
# so when I ran the code it just deleted everything.
#FamFO=FamFO[,-which(colSums(FamFO)==0)]
#FamFO=FamFO[-which(rowSums(FamFO)==0),]
# You'll notice this piece of code means that the dataframe now has 380 columns
# and we're going to need to save this so we can use it again rather than the original


# Labelling the explanatory variable columns, here the only two things I might look at are 
# OceanBasinQ and YearSample, so columns 1 and 2.
FamFODatSite <- FamFODat[,1:2]

# Normally the different levels of an explanatory variable are going to load in alphabetically,
# and that doesn't always make sense, so this piece of code tells "it" to label them in the 
# particular order that I think makes sense in a graph. See below.
FamFODatSite$OceanBasinQ <- factor(FamFODat$OceanBasinQ, levels = c("Mediterranean", "NE Atlantic",
                                                                    "NW Atlantic", "SW Atlantic",
                                                                    "SE Indian", "NE Pacific", 
                                                                    "SW Pacific"))


# Colour vector: vector holding the colours. We have 7 locations in the OceanBasinQ variable, so
# we need 7 colours named. For vegan nMDS plots, the colours aren't great and I need to maybe
# improve them.
cols <- c("red", "pink", "orange", "dark orange", "green", "blue", "light blue")

# NMDS with bray-curtis distance
FamFOOrd <- metaMDS(FamFO, distance="bray", k=3)

# Below script makes an empty plot
plot(FamFOOrd, type = "n")
# Add points colored by Environmental Variable Management
points(FamFOOrd, col = cols[FamFODatSite$OceanBasinQ], pch = 16, cex = 3)
legend("right", legend=levels(FamFODatSite$OceanBasinQ), col=cols, pch = 16, cex = 1.6)
par(mfrow=c(1,1))




#### Rarefaction ####

## Albacore prey species P/A data
# data
PreyPA
str(PreyPA)
Ocean <- PreyPASite$OceanBasinQ

# 2 dataset!! subsetted out the SW Atlantic and NE Indian for cumulative species richness by collector method
PreyDat2 <- read.csv("Albacore_preyPA3.csv",header=TRUE)
str(PreyDat2)
PreyPA2 = PreyDat2[,6:ncol(PreyDat2)]
PreySite2 = PreyDat2[,1:5]

# number of species and the factor that I want to group them by
Sp <- specnumber(PreyPA) #, PreySite$OceanBasinQ)
Sp

SpO <- specnumber(PreyPA, PreyPASite$OceanBasinQ)
SpO
# Result
#Mediterranean   NE Atlantic   NW Atlantic   SW Atlantic     SE Indian    NE Pacific    SW Pacific 
#       77           128            68           15              13           264            62 

# Acummresult function
Alb.accum <- accumresult(PreyPA, y=Ocean, method='exact', conditioned=TRUE)
Alb.accum
accumplot(Alb.accum)

Alb.accum2 <- accumresult(PreyPA, y=Ocean, method='collector', conditioned=TRUE)
Alb.accum2
accumplot(Alb.accum2)

Plot <- accumcomp(PreyPA, y=PreyPASite, factor='OceanBasinQ', method='exact', legend=FALSE, conditioned=TRUE)
str(PreyPASite)

#save and export that data

accum.data <- read.csv("Accum_plot.csv", header=TRUE)

accum.data$Ocean <- factor(accum.data$Ocean, levels = c("Mediterranean", "NE Atlantic",
                                                                    "NW Atlantic", "SW Atlantic",
                                                                    "NE Indian", "NE Pacific", 
                                                                    "SW Pacific"))

Accum.plot <- ggplot(data = accum.data, aes(y=Richness, x=Study, colour=Ocean)) +
  geom_point(size=2) +
  geom_line(size=1.5) + 
  theme_classic() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15))+
  theme(axis.title=element_text(size=18)) +
  theme(axis.text=element_text(size=18)) +
  theme(legend.text=element_text(size=18)) +
  theme(legend.title=element_text(size=18)) +
  labs(x="Years sampled", y="Species cumulative richness")+
  scale_colour_manual(values=wes_palette("Zissou1", 7, type="continuous"), name="Ocean Basin")+
  geom_errorbar(data = accum.data, aes(ymin=Richness-SD, ymax=Richness+SD), width=.2)
Accum.plot

## method = collector plot using 2nd dataset


Plot2 <- accumcomp(PreyPA2, y=PreySite2, factor='OceanBasinQ', method='collector', legend=FALSE, conditioned=TRUE)

# save and export the accumcomp data in order to add SW back in with just the one point

write.csv(Plot2, "Accum_plot2.csv")

accum.data2 <- read.csv("Accum_plot2.csv", header=TRUE)

View(accum.data2)

accum.data2$Ocean <- factor(accum.data2$Ocean, levels = c("Mediterranean", "NE Atlantic",
                                                        "NW Atlantic", "SW Atlantic",
                                                        "NE Indian", "NE Pacific", 
                                                        "SW Pacific"))

Accum.plot <- ggplot(data = accum.data2, aes(y=Richness, x=Year, colour=Ocean)) +
  geom_point(size=2) +
  geom_line(size=1.5) + 
  theme_classic() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  #scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15))+
  #geom_text(aes(x=Year, label=Stomachs), size=4, hjust=0.5, col="black") +
  geom_text_repel(data = accum.data2, aes(x=Year, label=Stomachs),
                  nudge_x = -10, nudge_y = 10) +
  theme(axis.title=element_text(size=18)) +
  theme(axis.text=element_text(size=18)) +
  theme(legend.text=element_text(size=18)) +
  theme(legend.title=element_text(size=18)) +
  labs(x="Years sampled", y="Species cumulative richness")+
  scale_colour_manual(values=wes_palette("Zissou1", 7, type="continuous"), name="Ocean Basin")
Accum.plot

#Note no error bar for pure accumulation plot