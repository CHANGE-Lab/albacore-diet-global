################################### Prey Trait Typologies ###################################

#This code is for analysis of prey typologies using multivariate statistics 
#--> using both a trait set that corresponds to adult life stage only and to our estimated probable life stage consumed.
#The aim is to identify groupings of prey or "typologies" by clustering prey species that significantly
#share traits. We can also graphs this treating species similar to sites (as rows) and traits as 
#columns in an nMDS plot.

# Workspace setup ----
getwd()
setwd("/Users/natasha/Documents/POSTDOC/TUNA DIETS/TUNASTATS/data")

#The code used Friday 8th Nov mainly involved the vegan and klaR packages. So you should only need
#to load those out of the list below.

#library(devtools)

#install what needs to be installed
#install.packages("fpc")
#install.packages("labdsv")
#install.packages("MVPARTwrap") #notice that this didn't work
#install_github('kassambara/factoextra')
#install.packages("ggdendro")
#install.packages("circlize")

#some packages are on GitHub and not on the CRAN project yet
#install.packages("devtools") 
#library(devtools)
#install_github("cran/mvpart")
#install_github("cran/MVPARTwrap")
#devtools::install_github("jakelawlor/PNWColors") 

#load all packages required
library(tidyverse)
library(vegan)
library(klaR)
library(dplyr)
library(cluster)
library(fpc)
library(ggplot2)
library(reshape2)
library(purrr)
library("dendextend")
library("PNWColors")
library(factoextra)
library(ggdendro)
library(circlize)
library(plyr)
library(viridis)

#Not required, parking lot here
#library(ade4)
#library(gclus)
#library(RColorBrewer)
#library(labdsv)
#library(mvpart)
#library(MVPARTwrap)

######## DATASETS ########

#### Load data ----

#Load the adult trait data and probable life stage data

prey_adult = read.csv("prey_adults_traits.csv", header=TRUE) %>% #row.names = 1, #spe_traitord #./data/
  dplyr::select(-c(X, diel_migrant, refuge, season_migrant, l_max, trophic_level:standard_total, energy_density:percent_lipid)) %>%
  dplyr::rename(`vertical habitat` = `vert_habitat`, 
                `horizontal habitat` = `horz_habitat`,
                `diel migrant` = `diel_migrant_cat`, 
                `refuge use` = `refuge_cat`,
                `seasonal migrant` = `season_cat`, 
                `body shape` = `body_shape`,
                `physical defense` = `phys_defense`,
                `gregarious` = `gregarious_primary` #,
                #`energy density` = energy_density,
                #`percent protein` = `percent_protein`,
                #`percent lipid` = `percent_lipid`
                ) %>% #Relabel variables
  mutate(`physical defense`=if_else(prey_adult$`physical defense`>0, "defended", "undefended"), 
         `diel migrant`=case_when(prey_adult$`diel migrant` == "diel_no" ~ "diel (no)",
                                  prey_adult$`diel migrant` == "diel_UN" ~ "diel (unknown)",
                                  prey_adult$`diel migrant` == "diel_yes" ~ "diel (yes)"),
         `refuge use`=case_when(prey_adult$`refuge use` == "refuge_no" ~ "refuge (no)",
                                prey_adult$`refuge use` == "refuge_NA" ~ "refuge (unknown)",
                                prey_adult$`refuge use` == "refuge_yes" ~ "refuge (yes)"),
         `seasonal migrant`=case_when(prey_adult$`seasonal migrant` == "season_no" ~ "season (no)",
                                      prey_adult$`seasonal migrant` == "season_NA" ~ "season (unknown)",
                                      prey_adult$`seasonal migrant` == "season_yes" ~ "season (yes)")
         ) %>%
  filter(`diel migrant` != "diel (unknown)", `refuge use` != "refuge (unknown)", `seasonal migrant` != "season (unknown)") %>%
  drop_na() #reduced from ~308 taxa to ~267 and filtering further reduced to ~179 species

# gregarious=case_when(gregarious == NA ~ "unknown", gregarious == "pairing" ~ "pairing", gregarious == "schooling" ~ "schooling", gregarious == "shoaling" ~ "shoaling", gregarious == "solitary" ~ "solitary")  

#308 taxa and 15 traits
View(prey_adult)

prey_probable = read.csv("prey_probable_traits.csv", header=TRUE) %>% #./data/
  dplyr::select(-c(X, diel_migrant, refuge, season_migrant, l_max, trophic_level:standard_total, energy_density:percent_lipid)) %>%
  dplyr::rename(`vertical habitat` = `vert_habitat`, 
                `horizontal habitat` = `horz_habitat`,
                `diel migrant` = `diel_migrant_cat`, 
                `refuge use` = `refuge_cat`,
                `seasonal migrant` = `season_cat`, 
                `body shape` = `body_shape`,
                `physical defense` = `phys_defense`,
                `gregarious` = `gregarious_primary`#,
                #`energy density` = energy_density,
                #`percent protein` = `percent_protein`,
                #`percent lipid` = `percent_lipid`
  ) %>% #Relabel variables
  mutate(`physical defense`=if_else(`physical defense`>0, "defended", "undefended"), 
         `diel migrant`=case_when(`diel migrant` == "diel_no" ~ "diel (no)",
                                  `diel migrant` == "diel_UN" ~ "diel (unknown)",
                                  `diel migrant` == "diel_yes" ~ "diel (yes)"),
         `refuge use`=case_when(`refuge use` == "refuge_no" ~ "refuge (no)",
                                `refuge use` == "refuge_NA" ~ "refuge (unknown)",
                                `refuge use` == "refuge_yes" ~ "refuge (yes)"),
         `seasonal migrant`=case_when(`seasonal migrant` == "season_no" ~ "season (no)",
                                      `seasonal migrant` == "season_NA" ~ "season (unknown)",
                                      `seasonal migrant` == "season_yes" ~ "season (yes)")
         ) %>%
  filter(`diel migrant` != "diel (unknown)", `refuge use` != "refuge (unknown)", `seasonal migrant` != "season (unknown)") %>%
  drop_na() #reduced from ~299 taxa to ~250 and filtering further reduced to ~148

View(prey_probable)
#299 taxa and 16 traits
unique(prey_probable$gregarious)

#write.csv(spe_traitsO, "spe_traitsO.csv")


#speciesO = spe_traitsO$species
#ctraitsO = spe_traitsO[,4:9]


#### Data manip for cluster analysis ----

## Row name as column datasets
#For adult traits
prey_adult_row<- prey_adult
names <- rownames(prey_adult_row)
prey_adult <- cbind(names, prey_adult_row)
names(prey_adult)[names(prey_adult)=="names"] <- "dendlabs"
#Adult trait df subsets
adult_species = prey_adult$prey_sp
prey_adult[,6:13] = lapply(prey_adult[,6:13], as.factor)
adult_traits = as.data.frame(prey_adult[,6:13])

## Row name as column datasets
#For probable prey traits
prey_probable_row <- prey_probable
names <- rownames(prey_probable_row)
prey_probable <- cbind(names, prey_probable_row)
names(prey_probable)[names(prey_probable)=="names"] <- "dendlabs"
#Probable trait df subsets
probable_species = prey_probable$prey_sp
prey_probable[,7:14] = lapply(prey_probable[,7:14] , as.factor)
probable_traits = as.data.frame(prey_probable[,7:14])


######## ALB CLUSTER ANALYSES ########

#### ADULT TRAITS ----

#### Distance matrices ----

#select just the traits you want to contribute to ordination
adult.gower.dist <- daisy(adult_traits, metric = c("gower"))

#### Divisive cluster ----
adult.divisive.clust <- diana(as.matrix(adult.gower.dist), 
                            diss = TRUE, keep.diss = TRUE)
plot(adult.divisive.clust, main = "Divisive")


#### Agglomerative cluster ----
#Use "average" or "complete" linkage
#ADD NOTE ON AVE VS. COMPLETE LINKAGE
adult.aggl.clustc <- hclust(adult.gower.dist, method = "complete")
plot(adult.aggl.clustc, main = "Agglomerative, complete linkages")


#### Cluster method assessment ----

#Stats table for divisive method
adult.stats.df.divisive <- cstats.table(adult.gower.dist, adult.divisive.clust, 10)
adult.stats.df.divisive

#Stats table for agglomerative method
adult.stats.df.aggl <-cstats.table(adult.gower.dist, adult.aggl.clustc, 10) 
#complete linkages looks like the most balanced approach
adult.stats.df.aggl

#The average between metric is tighter for agglomerative whilst the average within is tighter 
#for divisive. # MEANING of above statement?
#Conceptually, we want to aggregate prey species and am thus selecting an agglomerative technique.


#### Clusters number selection ----

# Using "Elbow" and "Silhouette" methods to identify the best number of clusters

#Assessment of clustering analyses reveals the minimum optimal clusters = 7, and up to 15.
#Selected --> 10 because of cluster evenness
#I want to choose a reasonable number, based on which I will be able to see basic 
#differences between customer groups as a result

# Elbow method
# Divisive clustering
ggplot(data = data.frame(t(cstats.table(adult.gower.dist, adult.divisive.clust, 20))), 
       aes(x=cluster.number, y=within.cluster.ss)) + 
  geom_point()+
  geom_line()+
  ggtitle("Divisive clustering") +
  labs(x = "Num.of clusters", y = "Within clusters sum of squares (SS)") +
  theme(plot.title = element_text(hjust = 0.5))
# 6 clusters was potentially enough, but even went up to ~15

# Agglomerative clustering,provides a more ambiguous picture
ggplot(data = data.frame(t(cstats.table(adult.gower.dist, adult.aggl.clustc, 20))), 
       aes(x=cluster.number, y=within.cluster.ss)) + 
  geom_point()+
  geom_line()+
  ggtitle("Agglomerative clustering") +
  labs(x = "Num.of clusters", y = "Within clusters sum of squares (SS)") +
  theme(plot.title = element_text(hjust = 0.5))
# Less clear, but 5 and 10 were separate inflection points

## Silhouette
#When it comes to silhouette assessment, the rule is you should choose the number that maximizes the 
#silhouette coefficient because you want clusters that are distinctive (far) enough to be considered separate.

# Divisive
ggplot(data = data.frame(t(cstats.table(adult.gower.dist, adult.divisive.clust, 20))), 
       aes(x=cluster.number, y=avg.silwidth)) + 
  geom_point()+
  geom_line()+
  ggtitle("Divisive clustering") +
  labs(x = "Num.of clusters", y = "Average silhouette width") +
  theme(plot.title = element_text(hjust = 0.5))
## Looks like ~12 clusters are optimal

# Agglomerative
ggplot(data = data.frame(t(cstats.table(adult.gower.dist, adult.aggl.clustc, 20))), 
       aes(x=cluster.number, y=avg.silwidth)) + 
  geom_point()+
  geom_line()+
  ggtitle("Agglomerative clustering") +
  labs(x = "Num.of clusters", y = "Average silhouette width") +
  theme(plot.title = element_text(hjust = 0.5))
## 13 would be my selection. Between 10-15 clusters optimal

##Ultimately selected 10 because it was the median number for agglomerative clustering method 
#between the elbow and the silhouette assessments




#### PROBABLE TRAITS ----
#### Distance matrices ----

#select just the traits you want to contribute to ordination
prob.gower.dist <- daisy(probable_traits, metric = c("gower"))

#### Divisive cluster ----
prob.divisive.clust <- diana(as.matrix(prob.gower.dist), 
                              diss = TRUE, keep.diss = TRUE)
plot(prob.divisive.clust, main = "Divisive")

#### Agglomerative cluster ----
#Use "average" or "complete" linkage
#ADD NOTE ON AVE VS. COMPLETE LINKAGE
prob.aggl.clustc <- hclust(prob.gower.dist, method = "complete")
plot(prob.aggl.clustc, main = "Agglomerative, complete linkages")

#### Cluster method assessment ----

#Stats table for divisive method
prob.stats.df.divisive <- cstats.table(prob.gower.dist, prob.divisive.clust, 10)
prob.stats.df.divisive

#Stats table for agglomerative method
prob.stats.df.aggl <- cstats.table(prob.gower.dist, prob.aggl.clustc, 10) 
#complete linkages looks like the most balanced approach
prob.stats.df.aggl

#The average between metric is tighter for agglomerative whilst the average within is tighter 
#for divisive. # MEANING of above statement?
#Conceptually, we want to aggregate prey species and am thus selecting an agglomerative technique.


#### Clusters number selection ----

# Using "Elbow" and "Silhouette" methods to identify the best number of clusters

#Assessment of clustering analyses reveals the minimum optimal clusters = 7, and up to 15.
#Selected --> 10 because of cluster evenness
#I want to choose a reasonable number, based on which I will be able to see basic 
#differences between customer groups as a result

# Elbow method
# Divisive clustering
ggplot(data = data.frame(t(cstats.table(prob.gower.dist, prob.divisive.clust, 20))), 
       aes(x=cluster.number, y=within.cluster.ss)) + 
  geom_point()+
  geom_line()+
  ggtitle("Divisive clustering") +
  labs(x = "Num.of clusters", y = "Within clusters sum of squares (SS)") +
  theme(plot.title = element_text(hjust = 0.5))
# but > 10 captured complexity well enough

# Agglomerative clustering,provides a more ambiguous picture
ggplot(data = data.frame(t(cstats.table(prob.gower.dist, prob.aggl.clustc, 20))), 
       aes(x=cluster.number, y=within.cluster.ss)) + 
  geom_point()+
  geom_line()+
  ggtitle("Agglomerative clustering") +
  labs(x = "Num.of clusters", y = "Within clusters sum of squares (SS)") +
  theme(plot.title = element_text(hjust = 0.5))
# > 7 captured complexity well enough

## Silhouette
#When it comes to silhouette assessment, the rule is you should choose the number that maximizes the 
#silhouette coefficient because you want clusters that are distinctive (far) enough to be considered separate.

# Divisive
ggplot(data = data.frame(t(cstats.table(prob.gower.dist, prob.divisive.clust, 20))), 
       aes(x=cluster.number, y=avg.silwidth)) + 
  geom_point()+
  geom_line()+
  ggtitle("Divisive clustering") +
  labs(x = "Num.of clusters", y = "Average silhouette width") +
  theme(plot.title = element_text(hjust = 0.5))
## Looks like ~11 clusters are optimal

# Agglomerative
ggplot(data = data.frame(t(cstats.table(prob.gower.dist, prob.aggl.clustc, 20))), 
       aes(x=cluster.number, y=avg.silwidth)) + 
  geom_point()+
  geom_line()+
  ggtitle("Agglomerative clustering") +
  labs(x = "Num.of clusters", y = "Average silhouette width") +
  theme(plot.title = element_text(hjust = 0.5))
## Messy > 7-10

##Ultimately selected 10 because it was the median number for agglomerative clustering method 
#between the elbow and the silhouette assessments



######## GRAPHS ########
#### ADULT TRAITS GRAPHS ####
#### Cluster Dendrograms - Adult traits ----

#Using agglomerative hierarchical clustering, k = 10
PNW.pal <- pnw_palette(10, name = "Bay", type = "continuous")
str(PNW.pal)

adult.dendro <- as.dendrogram(adult.aggl.clustc) #241 species

#### Horizontal dendrogram - Adult traits ----

#Horizontal cluster illustration version
adult.dendro.col <- adult.dendro %>%
  #set("branches_k_color", k = 10, value = PNW.pal) %>%
  set("branches_k_color", k = 10, value =   c("#00496F", "#066384", "#0D7E9A", "#59A082", "#BBC45A", 
                                              "#EDC636", "#EDA417", "#EB8203", "#E46113", "#DD4124")) %>%
  set("branches_lwd", 0.6) %>%
  set("labels", adult_species) %>% #NOT VERY LEGIBLE...
  set("labels_colors", 
      value = c("darkslategray")) %>% 
  set("labels_cex", 0.5)
adult.ggd1 <- as.ggdend(adult.dendro.col)
adult.dendro.graph <- ggplot(adult.ggd1, theme = theme_minimal()) +
  labs(x = "Num. observations", y = "Height", title = "Dendrogram, k = 10")
adult.dendro.graph
ggsave('adult.dendro.horz.png', plot=adult.dendro.graph, width=8, height=8, dpi=300)


#### Radial plot - Adult traits ----

# Radial plot looks less cluttered (and cooler)
adult.dendro.rad <- ggplot(adult.ggd1, labels = FALSE) + 
  scale_y_reverse(expand = c(0.2, 0)) +
  coord_polar(theta="x")
adult.dendro.rad 
#No labels on this one, labels were too cluttered/problems
#Save radial dendrogram for chat
ggsave('adult.dendro.rad.png', plot=adult.dendro.rad, width=8, height=8, dpi=300)


##### Vertical dendrogram version ----

#similar to https://stackoverflow.com/questions/38034663/rotate-labels-for-ggplot-dendrogram 

# This is a different way to compute hierarchical clustering and cut the tree
#clus <- hcut(mydist, k = 6, hc_func = 'hclust', hc_method = 'ward.D2', graph = FALSE, isdiss = TRUE)

## For a horizontal version below
#dend <- as.dendrogram(alb.aggl.clustc)
#Use alb.dendro

#Below is problematic0
#labels(dend) <- paste0(paste0(rep('', 3), collapse = ''), speciesO)
#dend <- sort(dend, decreasing = FALSE)
#View(labels(dend))

#Creating df for the dend labels so that we can accurately line them up with the species
dendlabs <- labels(adult.dendro) #Need to create strings of labels to manipulate
dendlabs2 <- as.data.frame(dendlabs) #turn in df

#Join these data so we can relabel the dendrogram
#Use plyr function because it conserves the row order of the left df, which matters for assigning labels here
library(plyr)
dfdend <- join(dendlabs2, prey_adult)

ggd1 <- ggplot(adult.dendro %>%
                 set("branches_k_color", k = 10, value = 
                       c("#00496F", "#066384", "#0D7E9A", "#59A082", "#BBC45A",
                         "#EDC636", "#EDA417", "#EB8203", "#E46113", "#DD4124")) %>%
                 set("branches_lwd", 0.6) %>%
                 set("labels", dfdend$prey_sp) %>% #NOT VERY LEGIBLE...
                 set("labels_colors", 
                     value = c("darkslategray")) %>% 
                 set("labels_cex", 0.5), 
               theme = theme_minimal(),
               horiz = TRUE)

ggd1 <- ggd1 + theme(panel.grid.major = element_blank(),
                     axis.text = element_blank(),
                     axis.title = element_blank())
ggd1 <- ggd1 + ylim(max(get_branches_heights(adult.dendro)), -1)
ggd1

ggsave('adult_dendro_vertlabs.pdf', plot=ggd1, width=5, height=20, dpi=300)

#With labels removed!!!

adult.dendro.vert <- ggplot(adult.ggd1, horiz = TRUE, labels = FALSE) + 
  scale_y_reverse(expand = c(0.2, 0)) #+
#coord_polar(theta="x")
adult.dendro.vert 
#Export as .png
ggsave('adult.dendro.vert2.png', plot=adult.dendro.vert, width=5, height=12, dpi=300)

#### Albacore Cluster Heatmap ----

#### Cluster number - Adult traits ----

#Extract cluster number to trait matrix

# Time for the heatmap
# the 1st step here is to have 1 variable per row
# factors have to be converted to characters in order not to be dropped
adult.clust.num <- cutree(adult.aggl.clustc, k = 10)

#we want to bind the original dataset with the cluster numbers such that each species is assigned a cluster
#can use whole data or just traits use to just look at unique species clusters in relation to traits
#alb.cl <- cbind(ctraitsO, alb.clust.num)
#OR
adult.prey.cl <- cbind(prey_adult, adult.clust.num)
View(adult.prey.cl)

write.csv(adult.prey.cl, "adult.prey.clusternum.csv")

#### Heatmap- Adult traits ----

#Note plyr can mess with this!!
detach("package:plyr", unload=TRUE)

#Create dfs for graphs
adult.clust.long = adult.prey.cl %>%
  dplyr::select(prey_sp, `vertical habitat`:`adult.clust.num`) %>%
  reshape2::melt(id.vars = c("prey_sp", "adult.clust.num"), variable.name = "trait", value.name = "level") %>%
  group_by(adult.clust.num, trait, level) %>%
  mutate(count = n_distinct(prey_sp)) %>%
  distinct(adult.clust.num, trait, level, count) %>% #, percent
  group_by(adult.clust.num, trait) %>%
  mutate(percent = count / sum(count)*100) %>%
  arrange(adult.clust.num)

str(adult.clust.long)

#heatmap.c will be suitable in case you want to go for absolute counts - but it doesn't tell much to my taste
#problem below involves the values of our data being ordinal, therefore they are not unique
levels(adult.clust.long$trait)

#Our data above comes truncated, you would need to truncate the data and re-label clusters depending on which dfs you melt/merge/reshape.
#Example: View(alb.cust.long.q[96:nrow(alb.cust.long.q),])
heatmap.c <- ggplot(adult.clust.long, aes(x = factor(adult.clust.num), y = level)) +
  geom_tile(aes(fill = count))+
  labs(title = "Distribution of characteristics across clusters", x = "Cluster number", y = NULL) +
  scale_fill_viridis(option="magma", begin = 0.2, end = 0.95)+
  facet_grid(trait~. , scales="free_y")
heatmap.c

ggsave('adult_dendro_heatcounts.pdf', plot=heatmap.c, width=8, height=12, dpi=300)


heatmap.p <- ggplot(adult.clust.long, aes(x = factor(adult.clust.num), y = factor(level, ordered = T))) +
  geom_tile(aes(fill = percent), alpha = 0.85)+
  labs(title = "Distribution of characteristics across clusters", x = "Cluster number", y = NULL) +
  #scale_fill_gradient2(low = "darkslategray1", mid = "yellow", high = "turquoise4") +
  scale_fill_viridis(option="magma", begin = 0.2, end = 0.95)+
  facet_grid(trait~., scales="free_y")
heatmap.p

ggsave('adult_dendro_heatpercent.pdf', plot=heatmap.p, width=8, height=12, dpi=300)




#### PROBABLE TRAITS GRAPHS ####
#### Cluster Dendrograms - Probable traits ----

#Using agglomerative hierarchical clustering, k = 10
PNW.pal2 <- pnw_palette(7, name = "Bay", type = "continuous")
str(PNW.pal2) #"#00496F" "#0A718F" "#59A082" "#EDD746" "#EDA417" "#E7720B" "#DD4124"

prob.dendro <- as.dendrogram(prob.aggl.clustc) #241 species

#### Horizontal dendrogram - Probable traits ----

#Horizontal cluster illustration version
prob.dendro.col <- prob.dendro %>%
  #set("branches_k_color", k = 10, value = PNW.pal) %>%
  set("branches_k_color", k = 7, value =   c("#00496F", "#0A718F", "#59A082", "#EDD746", "#EDA417", "#E7720B", "#DD4124")) %>%
  set("branches_lwd", 0.6) %>%
  set("labels", adult_species) %>% #NOT VERY LEGIBLE...
  set("labels_colors", 
      value = c("darkslategray")) %>% 
  set("labels_cex", 0.5)
prob.ggd1 <- as.ggdend(prob.dendro.col)
prob.dendro.graph <- ggplot(prob.ggd1, theme = theme_minimal()) +
  labs(x = "Num. observations", y = "Height", title = "Dendrogram, k = 7")
prob.dendro.graph
ggsave('prob.dendro.horzk7.png', plot=prob.dendro.graph, width=8, height=8, dpi=300)


#### Radial plot - Probable traits ----

# Radial plot looks less cluttered (and cooler)
prob.dendro.rad <- ggplot(prob.ggd1, labels = FALSE) + 
  scale_y_reverse(expand = c(0.2, 0)) +
  coord_polar(theta="x")
prob.dendro.rad 
#No labels on this one, labels were too cluttered/problems
#Save radial dendrogram for chat
ggsave('prob.dendro.radk7.png', plot=prob.dendro.rad, width=8, height=8, dpi=300)


##### Vertical dendrogram version ----

#similar to https://stackoverflow.com/questions/38034663/rotate-labels-for-ggplot-dendrogram 

# This is a different way to compute hierarchical clustering and cut the tree
#clus <- hcut(mydist, k = 6, hc_func = 'hclust', hc_method = 'ward.D2', graph = FALSE, isdiss = TRUE)

## For a horizontal version below
#dend <- as.dendrogram(alb.aggl.clustc)
#Use alb.dendro

#Below is problematic0
#labels(dend) <- paste0(paste0(rep('', 3), collapse = ''), speciesO)
#dend <- sort(dend, decreasing = FALSE)
#View(labels(dend))

#Creating df for the dend labels so that we can accurately line them up with the species
dendlabs <- labels(prob.dendro) #Need to create strings of labels to manipulate
dendlabs2 <- as.data.frame(dendlabs) #turn in df

#Join these data so we can relabel the dendrogram
#Use plyr function because it conserves the row order of the left df, which matters for assigning labels here
library(plyr)
dfdend <- join(dendlabs2, prey_probable)

ggd1 <- ggplot(prob.dendro %>%
                 set("branches_k_color", k = 7, value = 
                       c("#00496F", "#0A718F", "#59A082", "#EDD746", "#EDA417", "#E7720B", "#DD4124")) %>%
                 set("branches_lwd", 0.6) %>%
                 set("labels", dfdend$prey_sp) %>% #NOT VERY LEGIBLE...
                 set("labels_colors", 
                     value = c("darkslategray")) %>% 
                 set("labels_cex", 0.5), 
               theme = theme_minimal(),
               horiz = TRUE)

ggd1 <- ggd1 + theme(panel.grid.major = element_blank(),
                     axis.text = element_blank(),
                     axis.title = element_blank())
ggd1 <- ggd1 + ylim(max(get_branches_heights(prob.dendro)), -1)
ggd1

ggsave('prob_dendro_vertlabs_k7.pdf', plot=ggd1, width=5, height=20, dpi=300)

#With labels removed!!!

prob.dendro.vert <- ggplot(prob.ggd1, horiz = TRUE, labels = FALSE) + 
  scale_y_reverse(expand = c(0.2, 0)) #+
#coord_polar(theta="x")
prob.dendro.vert 
#Export as .png
ggsave('prob.dendro.vert2_k7.png', plot=prob.dendro.vert, width=5, height=12, dpi=300)

#### Albacore Cluster Heatmap ----
#### Cluster number - Probable traits ----

#Extract cluster number to trait matrix

# Time for the heatmap
# the 1st step here is to have 1 variable per row
# factors have to be converted to characters in order not to be dropped
prob.clust.num <- cutree(prob.aggl.clustc, k = 7)

#we want to bind the original dataset with the cluster numbers such that each species is assigned a cluster
#can use whole data or just traits use to just look at unique species clusters in relation to traits
#alb.cl <- cbind(ctraitsO, alb.clust.num)
#OR
prob.prey.cl <- cbind(prey_probable, prob.clust.num)
View(prob.prey.cl)

write.csv(prob.prey.cl, "prob.prey.clusternum_k7.csv")


#### Heatmap - Probable traits ----

#Note plyr can mess with this!!
detach("package:plyr", unload=TRUE)

#Create dfs for graphs
prob.clust.long = prob.prey.cl %>%
  dplyr::select(prey_sp, `vertical habitat`:`prob.clust.num`) %>%
  reshape2::melt(id.vars = c("prey_sp", "prob.clust.num"), variable.name = "trait", value.name = "level") %>%
  group_by(prob.clust.num, trait, level) %>%
  mutate(count = n_distinct(prey_sp)) %>%
  distinct(prob.clust.num, trait, level, count) %>% #, percent
  group_by(prob.clust.num, trait) %>%
  mutate(percent = count / sum(count)*100) %>%
  arrange(prob.clust.num)

str(prob.clust.long)

#heatmap.c will be suitable in case you want to go for absolute counts - but it doesn't tell much to my taste
#problem below involves the values of our data being ordinal, therefore they are not unique
levels(prob.clust.long$trait)

#Our data above comes truncated, you would need to truncate the data and re-label clusters depending on which dfs you melt/merge/reshape.
#Example: View(alb.cust.long.q[96:nrow(alb.cust.long.q),])
heatmap.c <- ggplot(prob.clust.long, aes(x = factor(prob.clust.num), y = level)) +
  geom_tile(aes(fill = count))+
  labs(title = "Distribution of characteristics across clusters", x = "Cluster number", y = NULL) +
  scale_fill_viridis(option="magma", begin = 0.2, end = 0.95)+
  facet_grid(trait~. , scales="free_y")
heatmap.c

ggsave('prob_dendro_heatcounts_k7.pdf', plot=heatmap.c, width=8, height=12, dpi=300)


heatmap.p <- ggplot(prob.clust.long, aes(x = factor(prob.clust.num), y = factor(level, ordered = T))) +
  geom_tile(aes(fill = percent), alpha = 0.85)+
  labs(title = "Distribution of characteristics across clusters", x = "Cluster number", y = NULL) +
  #scale_fill_gradient2(low = "darkslategray1", mid = "yellow", high = "turquoise4") +
  scale_fill_viridis(option="magma", begin = 0.2, end = 0.95)+
  facet_grid(trait~., scales="free_y")
heatmap.p

ggsave('prob_dendro_heatpercent_k7.pdf', plot=heatmap.p, width=8, height=12, dpi=300)


