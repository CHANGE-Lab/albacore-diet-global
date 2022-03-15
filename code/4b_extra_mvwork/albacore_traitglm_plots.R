#### 4th plot ----

a        = max( abs(trait_model1b$fourth.corner) )
colort   = colorRampPalette(c("blue","grey","red")) 
temp = t(as.matrix(trait_model1b$fourth.corner))

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

#### 2nd plot ----


#### SPP plot ----


#### Long data for %FO by Ocean basin ----

prey_long <- read.csv("prey_longx_20191019.csv", header=TRUE)
str(prey_long)
range(prey_long$maxFO)

#issue with refuge and phys_defenses being treated as numeric
prey_long[,14:15] <- sapply(prey_long[,14:15], as.factor)

## vertical habitat 

levels(prey_long$vert_habitat)
prey_long$vert_habitat <- factor(prey_long$vert_habitat, levels = c("benthic", "demersal", 
                                                                "benthopelagic", "epipelagic", 
                                                                "mesopelagic", "bathypelagic"))

prey_long %>% 
  ggplot() +
  geom_boxplot(aes(x=OceanBasinQ, maxFO, fill=vert_habitat)) +
  xlab("Vertical Habitat") + ylab("Frequency of Occurance(%)") + 
  theme_classic() +
  scale_fill_viridis_d(option="D", begin = 0.3, end = 1, name = "Ocean Basin")+
  theme(axis.title=element_text(size=20)) +
  theme(axis.text=element_text(size=20)) +
  theme(legend.text=element_text(size=20)) +
  theme(legend.title=element_text(size=20)) +
  theme(legend.position="top")

dev.copy2pdf(file="traits_FO_verthab2.pdf", width=12, height=5)

## horizontal habitat 

levels(prey_long$horz_habitat)
prey_long$horz_habitat <- factor(prey_long$horz_habitat, levels = c("reef-associated", "coastal",
                                                                    "continental shelf", "continental slope",
                                                                    "oceanic"))

prey_long %>% 
  ggplot() +
  geom_boxplot(aes(x=OceanBasinQ, maxFO, fill=horz_habitat)) +
  xlab("Horizontal Habitat") + ylab("Frequency of Occurance(%)") + 
  theme_classic() + 
  scale_fill_viridis_d(option="D", begin = 0.3, end = 1, name = "Ocean Basin")+
  theme(axis.title=element_text(size=20)) +
  theme(axis.text=element_text(size=20)) +
  theme(legend.text=element_text(size=20)) +
  theme(legend.title=element_text(size=20)) +
  theme(legend.position="top")

dev.copy2pdf(file="traits_FO_horzhab2.pdf", width=12, height=5)

## body shape 

levels(prey_long$body_shape)
prey_long$body_shape <- factor(prey_long$body_shape, levels = c("compressiform", "globiform",
                                                                "fusiform ", "elongated",
                                                                "eel-like"))
prey_long %>% 
  ggplot() +
  geom_boxplot(aes(x=OceanBasinQ, maxFO, fill=body_shape)) +
  xlab("Ocean Basin") + ylab("Frequency of Occurance(%)") + theme_classic() +
  scale_fill_viridis_d(option="D", begin = 0.3, end = 1, name = "Body Shape")+
  theme(axis.title=element_text(size=20)) +
  theme(axis.text=element_text(size=20)) +
  theme(legend.text=element_text(size=20)) +
  theme(legend.title=element_text(size=20)) +
  theme(legend.position="top")

dev.copy2pdf(file="traits_FO_body.pdf", width=12, height=5)

##NOTE: need to merge compressiform and depressiform
##NOTE: need to rm unique species also
##NOTE: need to rm 100% FOs
##Done and re-ran 4th corner models

## diel migrant 
levels(prey_long$diel_migrant)
prey_long$diel_migrant <- factor(prey_long$diel_migrant, levels = c("UN", "0",
                                                                "1"))
prey_long %>% 
  ggplot() +
  geom_boxplot(aes(x=OceanBasinQ, maxFO, fill=diel_migrant)) +
  xlab("Ocean Basin") + ylab("Frequency of Occurance(%)") + theme_classic() +
  scale_fill_viridis_d(option="D", begin = 0.3, end = 1, name = "Diel Migration")+
  theme(axis.title=element_text(size=20)) +
  theme(axis.text=element_text(size=20)) +
  theme(legend.text=element_text(size=20)) +
  theme(legend.title=element_text(size=20)) +
  theme(legend.position="top")

dev.copy2pdf(file="traits_FO_diel.pdf", width=9, height=5)

## refuge

levels(prey_long$refuge)
prey_long$diel_migrant <- factor(prey_long$diel_migrant, levels = c("UN", "0",
                                                                    "1"))
prey_long %>% 
  ggplot() +
  geom_boxplot(aes(x=OceanBasinQ, maxFO, fill=refuge)) +
  xlab("Ocean Basin") + ylab("Frequency of Occurance(%)") + 
  theme_classic() +
  scale_fill_viridis_d(option="D", begin = 1, end = 0.6, name = "Refuge")+
  theme(axis.title=element_text(size=20)) +
  theme(axis.text=element_text(size=20)) +
  theme(legend.text=element_text(size=20)) +
  theme(legend.title=element_text(size=20)) +
  theme(legend.position="top") ##what is going on with the levels of Refuge and Phys Defenses?

dev.copy2pdf(file="traits_FO_refuge.pdf", width=8, height=5)

## physical defenses
prey_long %>% 
  ggplot() +
  geom_boxplot(aes(x=OceanBasinQ, maxFO, fill=phys_defense)) +
  xlab("Ocean Basin") + ylab("Frequency of Occurance(%)") + 
  theme_classic() +
  scale_fill_viridis_d(option="D", begin = 1, end = 0.6, name = "Physical Defenses")+
  theme(axis.title=element_text(size=20)) +
  theme(axis.text=element_text(size=20)) +
  theme(legend.text=element_text(size=20)) +
  theme(legend.title=element_text(size=20)) +
  theme(legend.position="top")

dev.copy2pdf(file="traits_FO_physdef.pdf", width=8, height=5)
