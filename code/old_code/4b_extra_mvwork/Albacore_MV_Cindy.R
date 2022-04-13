################# Multivariate statistics for Albacore prey #################

getwd()
setwd("/Users/natasha/Documents/POSTDOC/TUNA DIETS/TUNASTATS/data")
ls
install.packages("ggplot2")
install.packages("vegan")
install.packages("mvabund")
install.packages("viridis")
install.packages("RColorBrewer")
install.packages("wesanderson")
library(vegan)
library(mvabund)
library(ggplot2)
library(viridis)
library(RColorBrewer)
library(wesanderson)

#We're also going to need to need some packages from GitHub and not from CRAN (all the above packages come from CRAN, the founder and website of R)

library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)

# We will also need this add on for vegan for further analyses from: https://github.com/pmartinezarbizu/pairwiseAdonis
install_github("pmartinezarbizu/pairwiseAdonis/pairwiseAdonis")
library(pairwiseAdonis)

#### DATA EXPORT INSTRUCTIONS ####

## For the bigger multivariate data exports, I have had to take it offline because the Google spreadsheet just won't do it
## Download the DB
# Create pivot table from the selection of all rows 2 to 1962 (our last row of data)
# Filters, selected ""Include" and select all "Yes/Y/Maybes", and deselect "No/N"
# Row select: OceanBasinQ, YearSample, StudyID, PredLifeStage # I also added just the broad OceanBasin for graphing purposes
# Column select: PreySP OR PreyFam for whichever data you want
# Values select: "PresAb" and make sure it says "sum" or that it is returning 1/0 basically OR %FO
# Note: for %FO you will need to also include "%FO" as a filter and delect any "0", or text values because we don't want to run analyses on those
# Now copy and paste special > values into a new excel document and save as .csv file
# Then highlight all the data, and we need to fill in all the blanks so that R knows what to do with those.
# Click on Edit > Find > Go To > Special > then click on "blanks" > this will highlight all the blanks,
# You then just type "0" and hit Command + Enter, and will fill all the blanks with "0"
# The offline version of excel usually gives you a row and column "grand total", it's time to check if any rows or columns are 
# empty. If so, it could be a mistake. I found one where a species column summed to zero, and I looked it up in the DB, and found
# that it occurred once but that its value in PresAb column was zero.
# Once you've checked grand totals, you can delete those if you have them in the data.
# We should now have a sheet that contains columns with OceanBasinQ, YearSample, etc, in columns and columns containing the presence
# or absence of each species for each study, year sampled, ocean basin and included the life history of the albacore which we will
# use for graphing.


#### MV PREY SPECIES Present/Absence ####

## Loading the Albacore prey presence/absence data
PreyDat <- read.csv("Albacore_preyPA.csv",header=TRUE)
# A good function for checking the dataframe is saved properly/things are in the right place
str(PreyDat)

# For downstream analyses I am going to "carve out" different parts of the dataframe, here I'm
# labelling the prey species columns for later use in analyses, (so those are columns 4 to 484).
PreyPA <- PreyDat[,6:467]
# Note that this is to select the 6th to 467th columns of just the species data, and later we select the "factors" columns 1 to 5

# Make sure or convert to presence/absence for the SP this should already be the case
PreyPA[PreyPA>0] = 1

# Labelling the explanatory variable columns, here the only two things I might look at are 
# OceanBasinQ and YearSample, so columns 1 and 2.
PreyPASite <- PreyDat[,1:5]

# Normally the different levels of an explanatory variable are going to load in alphabetically, for example:
levels(PreyPASite$OceanBasinQ)
# That prints all the levels of OceanBasinQ and that doesn't always make sense, so this piece of code to label them in the 
# particular order that I think makes sense in a graph. See below.
PreyPASite$OceanBasinQ <- factor(PreyPASite$OceanBasinQ, levels = c("Mediterranean", "NE Atlantic",
                                                                "NW Atlantic", "SW Atlantic",
                                                                "NE Indian", "NE Pacific", 
                                                                "SW Pacific"))

# Colour vector: vector holding the colours. We have 7 locations in the OceanBasinQ variable, so
# we need 7 colours named. For vegan nMDS plots, the colours aren't great and I need to maybe improve them.
cols <- c("red", "dark orange", "pink", "purple", "darkgrey", "blue", "light blue")

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
# This graph clearly shows some clustering of different ocean basin regions. The Mediterranean and NE Atlantic are quite similar,
# So are the NE Pacific and surprisingly the SW Pacific; the SW Atlantic is completely out on it's own, and the NW Atlantic also
# clusters in an area that overlaps the Pacific and Atlantic ellipses, if you were to manually draw those ellipses.
# This means that we need to run some stats soon to find out what could be driving these patterns. See further on.
# Export this graph as is, or add more info

# for example a polygon for which you could use either OceanBasinQ or Ocean. I used Ocean because that I would personally want to
# tell a story of overlap or not. But try both and see what you think.
ordihull(PreyPAOrd, PreyPASite$Ocean, draw = "polygon", col = cols)

# Add legend, this code needs to get tweaked depending on your data, you need to make sure it does not cover up any data accidentally
# if it does, then we need to add code to manuall position the legend.
legend("bottomleft", legend=levels(PreyPASite$OceanBasinQ), col=cols, pch = 16, cex = 2.5)

# Sometimes for presentations, I just export the image without the legend, then export a separate figure with legend
# and mess around with the style in post-processing steps either in Abode Illustrator or by cropping things in powerpoint.
# This code prints a bigger legend which can be useful in presentations.
legend("bottomleft", legend=levels(PreyPASite$OceanBasinQ), col=cols, pch = 16, cex = 2.5)

## Predator Life Stage colouring of nMDS
# Now we can make the same graph using PredLifeStage as an explanatory variable/colour for the dots:
# Check levels of that factor:
levels(PreyPASite$PredLifeStage)
# I'm going to back to the excel sheet and clean these up a little to just read: adult, juvenile, juvenile/adult and larvae.
# Checking levels again, looked good to use. Just need to relevel the levels of the factor PredLifeStage:
PreyPASite$PredLifeStage <- factor(PreyPASite$PredLifeStage, levels = c("adult", "juvenile/adult",
                                                                    "juvenile", "larvae"))
# Now we select four colours:
colL <- c("red", "pink", "lightblue", "blue")
plot(PreyPAOrd, type = "n")
points(PreyPAOrd, col = colL[PreyPASite$PredLifeStage], pch = 16, cex = 2.5)
legend("bottomleft", legend=levels(PreyPASite$PredLifeStage), col=colL, pch = 16, cex = 2.5)
ordihull(PreyPAOrd, PreyPASite$PredLifeStage, draw = "polygon", col = colL)

# This is awesome, it clearly shows the difference in species composition of prey for larval albacore, and how juvenile to adult
# albacore diets start to merge. We're going to go into the larval diets in a little bit too.



#### MV Prey Species Analyses ####

### Permanova in R using adonis function in vegan package


# Here I just want to select the bits of the data that we are actually going to run some stats on
# This is because, for stats, we need fairly even numbers of replicates (studies), so we want to just analyse
# the Mediterranean, NE Atlantic and NE Pacific against each other. We therefore select rows [1:39,] and columns
# [, c(1,3:4,6:467)] and so the following code just puts that together. This is because we don't want all the explanatory
# variables, we just want OceanBasinQ and couple of others.
PreyDatST <- PreyDat[1:39,c(1,3:4,6:467)]
str(PreyDatST)


# we're going to have to take this offline and save a .csv and upload the subsetted .csv again
write.csv(PreyDatST, "Albacore_preySP_stat.csv")
PreyDatST2 <- read.csv("Albacore_preySP2_stat.csv", header=TRUE)
str(PreyDatST2)

## Alternative subsetting code 
## Don't use
#temp = PreyDat %>% 
#  filter(OceanBasinQ %in% c('Mediterranean', 'NE Atlantic', 'NE Pacific'))

library(dplyr)
unique(temp$OceanBasinQ)

# Something is wrong, here because we truncated the data such that we should have 3 levels of OceanBasinQ 
# (Med, NE Atl, NE Pac), yet the code from printing str() is returning 7 levels, and the code below confirms that.
# We need to remove that information somehow.

# For multivariate analyses, we just want the community matrix (species and their values only)
PreyDatST.species <- mvabund(PreyDatST2[,4:465])
PreyDatST.species[PreyDatST.species>0] = 1
is.mvabund(PreyDatST.species)

## Need to delete any zero sum columns or columns with a single species
PreyDatST.species=PreyDatST.species[,-which(colSums(PreyDatST.species)==0)]
PreyDatST.species=PreyDatST.species[,-which(colSums(PreyDatST.species)==1)]

# And we want to select our explanatory variables for analyses
PreyDatST.sites <- PreyDatST2[,1:3]

#Factors for analyses:
levels(PreyDatST.sites$OceanBasinQ)
# This output is weird, but let's just see how we go with downstream analyses
# Running off the new subsetted dataframe, we're good to go.

#Name the facotr we want to use 
Ocean <- PreyDatST.sites$OceanBasinQ

Perm.PreySP <- adonis(PreyDatST.species~Ocean,data=PreyDatST, permutations = 999, method = "bray",
       strata = NULL, contr.unordered = "contr.sum", contr.ordered = "contr.poly")

Perm.PreySP
#           Df SumsOfSqs MeanSqs F.Model     R2 Pr(>F)    
#Ocean       2    3.1106  1.5553  4.4886 0.1996  0.001 ***
#Residuals  36   12.4738  0.3465         0.8004           
#Total      38   15.5844                 1.0000       

## This was still significant even when deleting single species occurrences

pairwise.adonis(PreyDatST.species, Ocean, sim.method = "bray")
## Output of pairwise test for each level of "Ocean"
#                        pairs  Df SumsOfSqs  F.Model        R2 p.value p.adjusted sig
#1 Mediterranean vs NE Atlantic  1 0.9642912 2.831050 0.1140125   0.002      0.006   *
#2  Mediterranean vs NE Pacific  1 1.3289915 3.620129 0.1359922   0.001      0.003   *
#3    NE Atlantic vs NE Pacific  1 2.2394747 6.710505 0.1990627   0.001      0.003   *


# Prey Species ~ Ocean
MV.PreySP <-manyglm(PreyDatST.species~Ocean,data=PreyDatST,family="binomial")
plot(MV.PreySP)
summary.manyglm(MV.PreySP)

# output of summary call doesn't actually make sense for binomial data!!

MV.PreySP.anova <- anova.manyglm(MV.PreySP)
# Get anova result
MV.PreySP.anova

### So there's a very significant difference between the Mediterranean, NE Atlantic and NE Pacific. 
# So we need to find out, firstly if each of them differs from each other, and then how.

## pairwise comparison for manyglm
treatment <- as.character(PreyDatST.sites$Ocean)
MVp.PreySP <- anova(MV.PreySP, pairwise.comp = ~treatment, nBoot = 199)

# Could also run: anova(msolglm, pairwise.comp = treatment, nBoot = 199)

# Result:

# Analysis of Deviance Table

# Model: manyglm(formula = PreyDatST.species ~ Ocean, family = "binomial", data = PreyDatST)

#Multivariate test:
#               Res.Df Df.diff  Dev Pr(>Dev)   
#(Intercept)     38                         
#Ocean           36       2 1478    0.005 **
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Pairwise comparison results: 
#                                                      Observed statistic Free Stepdown Adjusted P-Value   
#treatment:NE Atlantic vs treatment:NE Pacific                1030.9                          0.005 **
#treatment:Mediterranean vs treatment:NE Pacific               539.3                          0.005 **
#treatment:Mediterranean vs treatment:NE Atlantic              393.4                          0.020 *
#Arguments:
#Test statistics calculated assuming uncorrelated response (for faster computation) 
#P-value calculated using 199 resampling iterations via PIT-trap resampling (to account for correlation in testing)

## Check the model is running well and not biased by any issues in the data:
# 
E1 <- resid(MV.PreySP)
F1 <- fitted(MV.PreySP)
#Check normality
qqnorm(rnorm(E1))
qqline(E1)
# Check normality is respected
hist(E1, main = "Normality", breaks=10)


#Check residuals against factors
boxplot(E1 ~ factor(Ocean), varwidth = TRUE)
abline(h = 0, lty = 2)
# Model looks good!

#Get the univariate p-values for univariate tests
MV.PreySP.an.uni <- anova.manyglm(MV.PreySP,p.uni="adjusted")
MV.PreySP.an.uni
#Significant:
#Site : G. tricuspidata (p = 0.026); O. lineolata (p = 0.037);
#Region: N. gymnogenis (p = 0.013); S. lineolata (p = 0.018);

#Get the direction of effect fof each species with the main effect
coef(MV.PreySP)

##Top species
MV_TopSP <- sort(MV.PreySP.anova$uni.test[2,],decreasing=T,index.return=T)[1:20]
# an$uni.test stores univariate test stats, 3rd row has change in deviance due to time
MV_TopSP$ix[1:20] # the column numbers of the "top 10" most impacted spp # so column number 25 had the strongest effect, most disappeared between the two time periods
dimnames(PreyDatST.species)[[2]][MV_TopSP$ix[1:20]] #the names of the top 10 spp

## Names of  most influencial species:
#[1] "Scomberesox.saurus"        "Euphausiidae.spp."         "Cololabis.saira"          
#[4] "Scyllarus.arctus"          "Sebastes.spp."             "Trachurus.trachurus"      
#[7] "Hyperiidae.spp."           "Myctophidae.spp."          "Maurolicus.muelleri"      
#[10] "Meganyctiphanes.norvegica" "Nematoscelis.megalops"     "Themisto.gaudichaudii"    
#[13] "Illex.coindetii"           "Engraulis.mordax"          "Gonatus.spp."             
#[16] "Todarodes.sagittatus"      "Engraulis.encrasicolus"    "Arctozenus.risso"         
#[19] "Polybius.henslowii"        "Argyropelecus.olfersii"   

plot(PreyDatST.species~Ocean,var.subset=MV_TopSP$ix[1:20])

#### MV PREY FAMILIES PA ####

#### How about running the same thing for the prey families ? using the above code, also using the previous Albacore_MV.R code
### and the updated data provided.
### Remember to update the columns selected for the families because the dimensions of this data frame will be slightly different 
### to the species one.


#### ANALYSES EXTRA ####

## SIMPER Analyses

PreySP_SIMP <- simper(PreyPA, PreyPASite$Ocean, permutations = 0, trace = FALSE)
summary(PreySP_SIMP, ordered = TRUE)

## PCA

# Here I just want to select the bits of the data that we are actually going to run some stats on
# This is because, for stats, we need fairly even numbers of replicates (studies), so we want to just analyse
# the Mediterranean, NE Atlantic and NE Pacific against each other. We therefore select rows [1:39,] and columns
# [, c(1,3:4,6:467)] and so the following code just puts that together. This is because we don't want all the explanatory
# variables, we just want OceanBasinQ and couple of others.
PreyDatPCA <- PreyDat[1:39,c(1,3:4,6:467)]
str(PreyDatPCA)


PreySP_PCA <- prcomp(PreyPA, center = TRUE, scale = TRUE)
# Now we request the summary or results of the PCA
summary(PreySP_PCA)
# Wow, we have 50 PC axes of explanation, that's not too surprising given that our inputs are over 400 species of prey

# Here you can add the result to refer back to later. I suggest adding "#" in front of every line of the results that
# you would copy in this document, so that when you some day go to run all this code again, it won't run lines that
# are not meant to be computed.

# What do the results mean?
# This means that PC1 explains 8.1% of the total variation in the diets of albacore at our three ocean basins
# Notice also how the cumulative proportion of variance explained changes - by the time we get to PC7 we have captured
# statistically speaking > 50% of the variation in our dataset.

str(PreySP_PCA)
ggbiplot(PreySP_PCA)

