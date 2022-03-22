library(vegan)
data(dune.env)
data(dune)
dune.env$site.totals <- apply(dune,1,sum)
Accum.1 <- accumresult(dune, y=dune.env, scale='site.totals', method='exact', conditioned=TRUE)
Accum.1
accumplot(Accum.1)

Accum.2 <- accumcomp(dune, y=dune.env, factor='Management', method='exact', 
                     legend=FALSE, conditioned=TRUE, scale='site.totals')
## CLICK IN THE GRAPH TO INDICATE WHERE THE LEGEND NEEDS TO BE PLACED FOR
## OPTION WHERE LEGEND=TRUE (DEFAULT).

## Not run: 
# ggplot2 plotting method

data(warcom)
data(warenv)

Accum.3 <- accumcomp(warcom, y=warenv, factor='population', 
                     method='exact', conditioned=F, plotit=F)

accum.long3 <- accumcomp.long(Accum.3, ci=NA, label.freq=5)

library(ggplot2)

# possibly need for extrafont::loadfonts(device="win") to have Arial
# as alternative, use library(ggThemeAssist)
BioR.theme <- theme(
  panel.background = element_blank(),
  #panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.line = element_line("gray25"),
  text = element_text(size = 12, family="Arial"),
  axis.text = element_text(size = 10, colour = "gray25"),
  axis.title = element_text(size = 14, colour = "gray25"),
  legend.title = element_text(size = 14),
  legend.text = element_text(size = 14),
  legend.key = element_blank())

plotgg1 <- ggplot(data=accum.long3, aes(x = Sites, y = Richness, ymax =  UPR, ymin= LWR)) + 
  scale_x_continuous(expand=c(0, 1), sec.axis = dup_axis(labels=NULL, name=NULL)) +
  scale_y_continuous(sec.axis = dup_axis(labels=NULL, name=NULL)) +
  geom_line(aes(colour=Grouping), size=2) +
  geom_point(data=subset(accum.long3, labelit==TRUE), 
             aes(colour=Grouping, shape=Grouping), size=5) +
  geom_ribbon(aes(colour=Grouping), alpha=0.2, show.legend=FALSE) + 
  BioR.theme +
  scale_color_brewer(palette = "Set1") +
  labs(x = "Trees", y = "Loci", colour = "Population", shape = "Population")

plotgg1

## End(Not run) # dontrun