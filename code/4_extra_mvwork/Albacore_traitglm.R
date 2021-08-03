################# Trait-based analyses for Albacore prey #################

## Working Environment ----
getwd()
#setwd("/Users/natasha/Documents/POSTDOC/TUNA DIETS/TUNASTATS/data")

# general
library(tidyverse)
library(readxl)
library(plyr)
library(dplyr)
library(devtools)
library(reshape2)
library(here)
"%notin%" = Negate('%in%')
here::here()
library(pander)

# graphics
library(ggplot2)
library(lattice)
library(graphics)
library(RColorBrewer)
library(gridExtra)
library(captioner)

#modelling
library(Hmisc)
library(mgcv)
library(unmarked)
library(lme4)
library(mvabund)
library(tweedie)

## mvabund bug
source("residuals.glm1path.R")

## GLOBAL REVIEW DATA ####
#Albacore diet review data as %FO for > 200 species of prey
#For OceanBasinQ: Mediterranean, NE Atlantic, NE Pacific and SW Pacific

### Diet Use Data (L) ----
Diet <- read.csv(paste(here("data/Trait_species_env2.csv", sep="")), header=TRUE, check.names=FALSE) %>%
  dplyr::rename(Ocean = `ï»¿Ocean`)
#Env data are characters and species are integers
str(Diet)

## Define prey assemblage
PreySpecies = Diet[,7:251] 
# delete zero sum columns
PreySpecies=PreySpecies[,-which(colSums(PreySpecies)==0)]
#228 species left
# check dimensions and structure
ncol(PreySpecies) #228
nrow(PreySpecies) #62
str(PreySpecies) #data are integers
#dim(PreySpecies)

## For presence/absence data, ensure all values are 0 or 1:
PreySpecies[PreySpecies>0] = 1

#Check dataframe - ncol for the L matrix (species) needs to match nrow for the Q matrix (traits)

### Environmental variables (R) ----
##Maybe need to force retaining the header: 
SiteDat = Diet[,1:3]
nrow(SiteDat) #62
SiteDat = lapply(SiteDat, as.factor)
str(SiteDat)

summary(SiteDat)
summary(SiteDat$OceanBasinQ)
#Mediterranean   NE Atlantic    NE Pacific    SW Pacific 
#9            20            27             6 

is.factor(SiteDat$OceanBasinQ) # checking that Ocean is being treated as a factor.
#TRUE

levels(SiteDat$OceanBasinQ) #"Mediterranean" "NE Atlantic"   "NE Pacific"    "SW Pacific"
SiteDat$OceanBasinQ <- factor(SiteDat$OceanBasinQ, levels = c("SW Pacific", "NE Pacific", "NE Atlantic",
                                                  "Mediterranean"))

## Traits matrix (Q) ----

Traits <- read.csv(paste(here("data/Trait_list2.csv", sep="")), header=TRUE, row.names = 1)
str(Traits)
summary(Traits)
Traits2 = Traits[colnames(PreySpecies),1:2] #takes the trait values that equal the remaining cleaned sp in prey data

#If including length someday
#is.na(Traits$Length) #no NAs! #needed to take out Length because model would not converge with continuous variable

# Need to make sure mvabund treats traits as factors for analyses
Traits2 = lapply(Traits2[,1:2], as.factor)
Traits2$Habitat = as.factor(Traits2$Habitat)
Traits2$BodyShape = as.factor(Traits2$BodyShape)
#Subsetted for Habitat and BodyShape
#Or use: df[col_names] <- lapply(df[col_names] , factor) to convert to factors

# checking dimensions match
nrow(Traits2) #228
View(Traits2)
str(Traits2)

# check levels, and can set reference level for different traits
levels(Traits2$Habitat)
Traits2$Habitat <- factor(Traits2$Habitat, levels = c("unknown", "pelagic", "coastal epipelagic",
                                                          "benthopelagic", "reef-associated", "demersal", "benthic",
                                                          "bathypelagic", "bathydemersal"))

## Make sure species names match abund matrix
rownames(Traits2) == colnames(PreySpecies) #awesome!

## Global Review Trait analyses ----

## Run trial manyglm() to check for problems
is.mvabund(PreySpecies)
PreySpecies2 <- as.mvabund(PreySpecies)
Prey.M1 <-manyglm(PreySpecies2 ~ SiteDat$OceanBasinQ, data=Diet, family="binomial")
plot(Prey.M1)

Prey.M1.anova <- anova.manyglm(Prey.M1)
# Get anova result
Prey.M1.anova

#chose to set composition=FALSE since sampling intensity across different samples is constant

#chose to use method="glm1path" so use LASSO penalty
## test run for SiteDat dataset
Trait_OQ <- traitglm(PreySpecies,
                      SiteDat$OceanBasinQ,
                      Traits2,
                      family="binomial",
                      #method="manyglm",
                      method="glm1path",
                      composition=FALSE)



Trait_ocean <- traitglm(PreySpecies,
                      SiteDat$Ocean,
                      Traits2,
                      family="binomial",
                      method="manyglm",
                      #method="glm1path",
                      composition=FALSE)

#### Global trait model outputs

# Check residuals TEST
qqnorm(residuals(Trait_OQ)[which(residuals(Trait_OQ)<10000)]); abline(c(0,1),col="red")

#omit non-finite residuals FULL MODEL
resids.full = residuals(Trait_OQ)[is.finite((residuals(Trait_OQ)))]
plot(resids.full)
abline(c(0,0),col="red")

# Check output TEST

Trait_OQ$fourth.corner
# SW Pacific included this time, once binomial data were converted to P/A 
# But we still have an X variable issue

#### Global plot 4th corner ----

a        = max( abs(Trait_OQ$fourth.corner) )
colort   = colorRampPalette(c("blue","white","red")) 
temp = t(as.matrix(Trait_OQ$fourth.corner))

#rownames(temp) = c("Region: Ocean Basin")
#colnames(temp) = c("Trophic group: Benthic Invertivore",
#                   "Trophic group: Browsing Herbivore",
#                   "Trophic group: Mesopredator",
#                   "Trophic group: Planktivore",
#                   "Solitary",
#                   "Schooling",
#                   "Pairs/groups",
#                   "Known prey",
#                   "Potential prey",
#                   "Bathypelagic")
plot.4th = levelplot(temp, 
                     xlab=list("Environmental Variables", cex=1.5),
                     ylab=list("Species traits", cex=1.5), 
                     col.regions=colort(100), 
                     at=seq(-a, a,length=100),
                     scales = list(x=list(rot = 45, cex=1.3), y=list(cex=1.3)), 
                     colorkey=list(labels=list(cex=1.3)))

print(plot.4th)

#### North Hemisphere Sites ####

# Select data (L)
DietNorth <- Diet[1:58,]
DietNorthSP <- DietNorth[,7:251]
nrow(DietNorthSP) #58
View(DietNorthSP)

#omit species that were never observed in the region
DietNorthSP=DietNorthSP[,-which(colSums(DietNorthSP)==0)]
ncol(DietNorthSP) #226

# Select sites (R)
SiteNorth = DietN[,1:2]
nrow(SiteNorth) #56

levels(SiteNorth$OceanBasinQ)
SiteNorth$OceanBasinQ <- factor(SiteNorth$OceanBasinQ, levels = c("NE Pacific", "NE Atlantic", 
                                                                  "Mediterranean"))


North <- cbind(SiteNorth, DietNorthSP)
str(North)
View(North)
write.csv(North, "Trait_north.csv")

DietN <- read.csv(paste("Trait_north.csv", sep=""), header=TRUE, check.names=FALSE)
DietNSP <- DietN[,3:228]
DietNSP=DietNSP[,-which(colSums(DietNSP)==0)]
ncol(DietNSP) #222


# Select traits (Q)
#omit species not detected in each region
TraitsN = Traits[colnames(DietNSP),]
nrow(TraitsN) #222
View(TraitsN)
TraitsNT = TraitsN[,1:2]



# Trait GLM model
Trait_N <- traitglm(DietNSP,
                    SiteNorth$OceanBasinQ,
                    TraitsNT,
                    family="binomial",
                    method="manyglm",
                    #method="glm1path",
                    composition=FALSE)

## Check residuals TEST
qqnorm(residuals(Trait_N)[which(residuals(Trait_N)<10000)]); abline(c(0,1),col="red")

#omit non-finite residuals FULL MODEL
resids.full = residuals(Trait_N)[is.finite((residuals(Trait_N)))]

plot(resids.full)
abline(c(0,0),col="red")

# Check output TEST

Trait_N$fourth.corner

# ...

## Plot 4th corner
a        = max( abs(Trait_N$fourth.corner) )
colort   = colorRampPalette(c("blue","white","red")) 
temp = t(as.matrix(Trait_N$fourth.corner))

rownames(temp) = c("Region: Ocean Basin")
colnames(temp) = c("Trophic group: Benthic Invertivore",
                   "Trophic group: Browsing Herbivore",
                   "Trophic group: Mesopredator",
                   "Trophic group: Planktivore",
                   "Solitary",
                   "Schooling",
                   "Pairs/groups",
                   "Known prey",
                   "Potential prey",
                   "Not prey")
plot.4th = levelplot(temp, 
                     xlab=list("Environmental Variables", cex=1.5),
                     ylab=list("Species traits", cex=1.5), 
                     col.regions=colort(100), 
                     at=seq(-a, a,length=100),
                     scales = list(x=list(rot = 45, cex=1.3), y=list(cex=1.3)), 
                     colorkey=list(labels=list(cex=1.3)))

print(plot.4th)


#### NE Pacific ####

## Note not currently possible to analyse the data for a continuous explanatory variable.

# Select data (L)
DietPNW <- Diet[Diet$OceanBasinQ=="NE Pacific",]
DietPNWsp <- DietPNW[,7:251]
nrow(DietPNWsp) #27

#omit species that were never observed in the region
DietPNWsp=DietPNWsp[,-which(colSums(DietPNWsp)==0)]
ncol(DietPNWsp) #127

# Select sites (R)
SitePNW = DietPNW[,2:3]
nrow(SitePNW) #27

# Select traits (Q)
#omit species not detected in each region
TraitsPNW = Traits[colnames(DietPNWsp),]
nrow(TraitsPNW) #127
View(TraitsPNW)
TraitsPNWtest = TraitsPNW[,1:2]

# Trait GLM model
Trait_PNW <- traitglm(DietPNWsp,
                    SitePNW$YearSample,
                    TraitsPNWtest,
                    family="binomial",
                    method="manyglm",
                    #method="glm1path",
                    composition=FALSE)

## Check residuals TEST
qqnorm(residuals(Trait_PNW)[which(residuals(Trait_PNW)<10000)]); abline(c(0,1),col="red")

#omit non-finite residuals FULL MODEL
resids.full = residuals(Trait_PNW)[is.finite((residuals(Trait_PNW)))]

plot(resids.full)
abline(c(0,0),col="red")

# Check output TEST

Trait_PNW$fourth.corner

# ...

## Plot 4th corner
a        = max( abs(Trait_PNW$fourth.corner) )
colort   = colorRampPalette(c("blue","white","red")) 
temp = t(as.matrix(Trait_PNW$fourth.corner))

rownames(temp) = c("Region: Ocean Basin")
colnames(temp) = c("Trophic group: Benthic Invertivore",
                   "Trophic group: Browsing Herbivore",
                   "Trophic group: Mesopredator",
                   "Trophic group: Planktivore",
                   "Solitary",
                   "Schooling",
                   "Pairs/groups",
                   "Known prey",
                   "Potential prey",
                   "Not prey")
plot.4th = levelplot(temp, 
                     xlab=list("Environmental Variables", cex=1.5),
                     ylab=list("Species traits", cex=1.5), 
                     col.regions=colort(100), 
                     at=seq(-a, a,length=100),
                     scales = list(x=list(rot = 45, cex=1.3), y=list(cex=1.3)), 
                     colorkey=list(labels=list(cex=1.3)))

print(plot.4th)

#### Fish Subset ####

## Import
DietFish <- read.csv(paste("Trait_fishspecies.csv", sep=""), header=TRUE, check.names=FALSE)
str(DietFish)
View(DietFish)

## Define prey assemblage
FishSpecies = DietFish[,6:129] 

## For presence/absence data, ensure all values are 0 or 1:
FishSpecies[PreySpecies>0] = 1

FishSpecies=FishSpecies[,-which(colSums(FishSpecies)==0)]

# Check dataframe - ncol for the L matrix (species) needs to match nrow for the Q matrix (traits)
View(FishSpecies)
nrow(FishSpecies) #49
ncol(FishSpecies) #116

## Define environmental variables
## Maybe need to force retaining the header: 
SiteFish = DietFish[,1:3]
nrow(SiteFish) #49
str(SiteFish)

summary(SiteFish)
str(SiteFish)
View(SiteDat)
is.factor(SiteFish$OceanBasinQ) # checking that Ocean is being treated as a factor.

#relevel sites

levels(SiteFish$OceanBasinQ)
SiteFish$OceanBasinQ <- factor(SiteFish$OceanBasinQ, levels = c("NE Pacific", "NE Atlantic",
                                                                "Mediterranean"))

## Import traits:

TraitFish <- read.csv(paste("Trait_fish.csv", sep=""), header=TRUE, row.names = 1)
TraitFish = data.frame(apply(TraitFish, 2, as.factor))
View(TraitFish)

# Take fish species traits that = the fish species in the fish subset
TraitF = TraitFish[colnames(FishSpecies),]
nrow(TraitF) #116
View(TraitF)

TraitFT <- TraitF[,1:2]
str(TraitFT)
summary(TraitFT)

nrow(TraitFT) #116

## Make sure species names match abund matrix
rownames(TraitF) == colnames(FishSpecies) #awesome!
rownames(TraitFT) == colnames(FishSpecies) #awesome!


# Trait GLM model
Trait_Fish <- traitglm(FishSpecies,
                      SiteFish$OceanBasinQ,
                      TraitFT,
                      family="binomial",
                      #method="manyglm",
                      method="glm1path",
                      composition=TRUE)

# Trait GLM model with body length variable
Trait_FishL <- traitglm(FishSpecies,
                       SiteFish$OceanBasinQ,
                       TraitF,
                       family="binomial",
                       #method="manyglm",
                       method="glm1path",
                       composition=TRUE)


## Check residuals TEST
qqnorm(residuals(Trait_Fish)[which(residuals(Trait_Fish)<10000)]); abline(c(0,1),col="red")

#omit non-finite residuals FULL MODEL
resids.full = residuals(Trait_Fish)[is.finite((residuals(Trait_Fish)))]

plot(resids.full)
abline(c(0,0),col="red")

# Check fourth corner output for Fish Subset
Trait_Fish$fourth.corner

## Plot 4th corner
a        = max( abs(Trait_Fish$fourth.corner) )
colort   = colorRampPalette(c("blue","white","red")) 
temp = t(as.matrix(Trait_Fish$fourth.corner))

rownames(temp) = c("Region: Ocean Basin")
colnames(temp) = c("Trophic group: Benthic Invertivore",
                   "Trophic group: Browsing Herbivore",
                   "Trophic group: Mesopredator",
                   "Trophic group: Planktivore",
                   "...")
plot.4th = levelplot(temp, 
                     xlab=list("Ocean Basin", cex=1.5),
                     ylab=list("Species traits", cex=1.5), 
                     col.regions=colort(100), 
                     at=seq(-a, a,length=100),
                     scales = list(x=list(rot = 45, cex=1.3), y=list(cex=1.3)), 
                     colorkey=list(labels=list(cex=1.3)))

print(plot.4th)


## Plot 2nd

TCoeffs <- Trait_Fish$coefficients
View(TCoeffs)
a        = max( abs(Trait_Fish$coefficients[10:20,]))
second = t(as.matrix(Trait_Fish$coefficients[229:200,]))
View(second)
rownames(second) = " "
colnames(second) = c("...")
plot.2nd = levelplot(second, 
                     xlab=list("Coefficients", cex=1.5),
                     ylab=list("Main Effects", cex=1.5), 
                     col.regions=colort(100), 
                     at=seq(-a, a,length=100),
                     scales = list(x=list(rot = 45, cex=1.3, tck=0, tick.number=1),
                                   y=list(cex=1.2)), colorkey=list(labels=list(cex=1.3)))
print(plot.2nd)


#### Fish %FO NE subset ####

#### Fish Subset ####

## Import
DietFishFO <- read.csv(paste("Trait_fishspeciesNE.csv", sep=""), header=TRUE, check.names=FALSE)
str(DietFishFO)
View(DietFishFO)

## Define prey assemblage
FishFOSpecies = DietFishFO[,5:108] 
# delete empty columns
FishFOSpecies=FishFOSpecies[,-which(colSums(FishFOSpecies)==0)]
# delete >100% FO
FishFOSpecies=FishFOSpecies[,-which(colSums(FishFOSpecies)>100)]

write.csv(FishFOSpecies, "Trait_fishenv2.csv")

View
# Check dataframe - ncol for the L matrix (species) needs to match nrow for the Q matrix (traits)
View(FishFOSpecies)
nrow(FishFOSpecies) #38 observations
ncol(FishFOSpecies) #74 fish species

## Define environmental variables
## Maybe need to force retaining the header: 
SiteFishFO = DietFishFO[,1:3]
nrow(SiteFishFO) #38 observations for 3 env variables
str(SiteFishFO) #all factors

summary(SiteFishFO)
str(SiteFishFO)
View(SiteDat)
is.factor(SiteFishFO$OceanBasinQ) # checking that Ocean is being treated as a factor.

#relevel sites

levels(SiteFishFO$OceanBasinQ)
SiteFish$OceanBasinQ <- factor(SiteFish$OceanBasinQ, levels = c("NE Pacific", "NE Atlantic",
                                                                "Mediterranean"))

## Import traits:

TraitFish <- read.csv(paste("Trait_fish.csv", sep=""), header=TRUE, row.names = 1)
TraitFish = data.frame(apply(TraitFish, 2, as.factor))
View(TraitFish)

# Take fish species traits that = the fish species in the fish subset
TraitFO = TraitFish[colnames(FishFOSpecies),]
nrow(TraitFO) #74
View(TraitF)

TraitFOT <- TraitFO[,1:2]
str(TraitFOT)
summary(TraitFOT)

levels(TraitFOT$BodyShape)
TraitFOT$BodyShape <- factor(TraitFOT$BodyShape, levels = c("fusiform", "elongated", "eel-like",
                                                            "compressiform", "depressiform", "globiform"))

levels(TraitFOT$Habitat)
nrow(TraitFOT) #74

## Update traits
write.csv(TraitFOT, "Trait_fish2.csv")
TraitFish2 <- read.csv(paste("Trait_fish2.csv", sep=""), header=TRUE, row.names = 1)
TraitFish2 = data.frame(apply(TraitFish2, 2, as.factor))
View(TraitFish2)

levels(TraitFOT$BodyShape)
TraitFOT$BodyShape <- factor(TraitFOT$BodyShape, levels = c("fusiform", "elongated", "eel-like",
                                                            "compressiform", "depressiform", "globiform"))

levels(TraitFOT$Habitat)
TraitFOT$BodyShape <- factor(TraitFOT$BodyShape, levels = c("benthopelagic", "coastal demersal", "coastal epipelagic",
                                                            "continental demersal", "mesopelagic", "oceanic epipelagic",  
                                                            "reef-associated", "bathydemersal", "bathypelagic"))

## Make sure species names match abund matrix
rownames(TraitFO) == colnames(FishFOSpecies) #awesome!
rownames(TraitFOT) == colnames(FishFOSpecies) #awesome!
rownames(TraitFish2) == colnames(FishFOSpecies) #awesome!

# Trait GLM model
Trait_FO <- traitglm(FishFOSpecies,
                       SiteFishFO$OceanBasinQ,
                       TraitFish2,
                       family="negative.binomial",
                       method="manyglm",
                       #method="glm1path",
                       composition=FALSE)

## Poisson distribution not good!

## Check residuals TEST
qqnorm(residuals(Trait_FO)[which(residuals(Trait_FO)<10000)]); abline(c(0,1),col="red")

#omit non-finite residuals FULL MODEL
resids.full = residuals(Trait_FO)[is.finite((residuals(Trait_FO)))]
plot(resids.full)
abline(c(0,0),col="red")

# Check fourth corner output for Fish Subset
Trait_FO$fourth.corner
# This has the Xvariable issue
#                            XNE Pacific
#Habitatbathypelagic         -0.03325814
#Habitatbenthopelagic         1.09197992
#Habitatcoastal epipelagic    2.15896787
#Habitatcontinental demersal  1.84368487
#Habitatmesopelagic           3.24438221
#Habitatoceanic epipelagic    1.76282430
#Habitatreef-associated       1.32194899
#BodyShapeeel-like           -0.40958117
#BodyShapeelongated          -1.14798058
#BodyShapefusiform           -0.61624357

Trait_FO$coefficients

## Plot 4th corner
a        = max( abs(Trait_FO$fourth.corner) )
colort   = colorRampPalette(c("blue","white","red")) 
temp = t(as.matrix(Trait_FO$fourth.corner))

#rownames(temp) = c("Region: Ocean Basin")
#colnames(temp) = c("Trophic group: Benthic Invertivore",
#                   "Trophic group: Browsing Herbivore",
#                   "Trophic group: Mesopredator",
#                   "Trophic group: Planktivore",
#                   "...")
plot.4th = levelplot(temp, 
                     xlab=list("Ocean Basin", cex=1.5),
                     ylab=list("Species traits", cex=1.5), 
                     col.regions=colort(100), 
                     at=seq(-a, a,length=100),
                     scales = list(x=list(rot = 45, cex=1.3), y=list(cex=1.3)), 
                     colorkey=list(labels=list(cex=1.3)))

print(plot.4th)
