library("sf")
library("cartography")

# data import

pcf <- read.csv("data/vote congres.csv")
depts <- st_read(dsn = "data/depts.shp")
depts <- merge(x = depts, y = pcf, by.x = "DEP", by.y = "id", all.x=T)
countries <- st_read(dsn = "data/others.shp", stringsAsFactors = F)

# config

sizes <- getFigDim(x = depts, width = 1000,mar = c(0,0,1.2,0), res = 100)
#sizes <- c(1000,1000)
xlim <-  c(119047,1200000)
ylim <-  c(6070000,7100000)  


# layout

lay <- function(title){
par(mar = c(0,0,0,0))
plot(st_geometry(depts),xlim = xlim, ylim = ylim, lwd=0.1)
plot(st_geometry(countries), col="#DDDDDD", border="#AAAAAA00", lwd=1, add=T)
plot(st_geometry(depts), col="#CCCCCC", border="#AAAAAA00", lwd=0.5, add=T)
text(x = 90000,y = 7080000,labels = txt, adj=0, cex=1.5,font = 2)
x <- 763033
y <- 6173000

barscale(size = 200, lwd = 1.5, cex = 0.6, pos = c(x,y), style = "pretty")
text(x = x, y=y-40000,adj=0, font=2, label="Vote des 4, 5 et 6 octobre 2018", cex=0.8)
text(x = x, y=y-60000,adj=0, font=1, label="Nicolas LAMBERT, 2018", cex=0.8)
text(x = x, y=y-100000,adj=0,font=3, label="https://2017.pcf.fr/r_sultat_du_vote_sur_la_base_commune", cex=0.7)
}

# MAP 1

png("output/votants.png", width = sizes[1], height = sizes[2], res = 100)
txt <- "Plus de 30 000 communistes\nont choisi leur base commune\nde discussion"
lay(txt)
propSymbolsLayer(depts, var  = "votants", 
                 col = "#a02323", inches = 0.5,
                 border = "white", lwd = .8, legend.pos="bottomleft", legend.style="c", legend.title.txt ="Nombre de votants",legend.frame = TRUE )
labelLayer(depts[depts$exprimés>500,],txt = "Nom", halo=T, cex=0.8, overlap=FALSE,show.lines=F, col="#382222")
dev.off()

# MAP 2

bases <- c("Texte 1 :\nLe communisme est la question du XXIe siècle", "Texte 2 :\nPour un printemps du communisme", "Texte 3 :\nManifeste du Parti communiste du XXIe siècle", "Texte 4 :\nReconstruire le Parti de classe")
cols <- c("#377eb8","#4daf4a","#e41a1c","#984ea3")
depts$type <- NULL
depts[depts$question > depts$manifeste & depts$question > depts$classe & depts$question > depts$printemps,"type"] <- bases[1]
depts[depts$printemps > depts$manifeste & depts$printemps > depts$classe & depts$printemps > depts$question,"type"] <-  bases[2]
depts[depts$manifeste > depts$question & depts$manifeste > depts$classe & depts$manifeste > depts$printemps,"type"] <-  bases[3]
depts[depts$classe > depts$manifeste & depts$classe > depts$question & depts$class > depts$printemps,"type"] <-  bases[4]

png("output/textes.png", width = sizes[1], height = sizes[2], res = 100)
txt <- "Texte arrivé en tête\ndans chaque département"
lay(txt)
typoLayer(x = depts, var = "type",
                     col = cols,
                     legend.values.order = bases,
                     legend.pos = "bottomleft",
                     border="white",legend.values.cex = 0.6, lwd=0.5,
                  legend.title.txt = "Les 4 textes soumis au vote", legend.frame = TRUE, add=T)
for (i in 1:length(depts$DEP))
{
  depts$max[i] <- max(depts$question[i],depts$printemps[i],depts$manifeste[i],depts$classe[i])
 }

labelLayer(depts[depts$max>depts$exprimés/2,],txt = "Nom", halo=T, cex=0.8, overlap=FALSE,show.lines=F, col="#382222")

dev.off()

# MAP 3
inches <- 0.2
fixmax <- 100
t <- c("Texte minoritaire","Texte arrivé en tête","Majorité absolue")
cols <- c("#c6dbef00","#6baed6", "#08519c")
depts$typo <- t[1]
depts[depts$question > depts$manifeste & depts$question > depts$classe & depts$question > depts$printemps,"typo"] <- t[2]
depts[depts$question/depts$exprimés > 0.5,"typo"] <- t[3]


tot <- sum(depts$question)
share <- round(tot/sum(depts$exprimés)*100,1)
txt <- paste0("Le communisme\nest la question du XXIe  siècle\n",tot," voix | ",share,"%")

png("output/texte1.png", width = sizes[1], height = sizes[2], res = 100)
lay(txt)
propSymbolsTypoLayer(x = depts, var = "question", var2 = "typo",
                    symbols = "circle",          
                    col = cols,
                    inches=inches,
                    fixmax = fixmax, 
                    legend.var2.values.order = t,
                    legend.var.pos = "bottomleft", border = "white",
                    legend.var.frame = T, legend.var2.frame = T,
                    legend.var.title.txt = "Nombre de votes\npour le texte 1",
                    legend.var2.title.txt = "Classement")
labelLayer(depts[depts$typo==t[3],],txt = "Nom", halo=T, cex=0.8, overlap=FALSE,show.lines=F, col="#382222")

dev.off()

# MAP 4
t <- c("Texte minoritaire","Texte arrivé en tête","Majorité absolue")
cols <- c("#ccece600","#66c2a4", "#006d2c")
depts$typo <- t[1]
depts[depts$printemps > depts$manifeste & depts$printemps > depts$manifeste & depts$printemps > depts$question,"typo"] <- t[2]
depts[depts$printemps/depts$exprimés > 0.5,"typo"] <- t[3]


tot <- sum(depts$printemps)
share <- round(tot/sum(depts$exprimés)*100,1)
txt <- paste0("Pour un printemps \ndu communisme\n",tot," voix | ",share,"%")

png("output/texte2.png", width = sizes[1], height = sizes[2], res = 100)
lay(txt)
propSymbolsTypoLayer(x = depts, var = "printemps", var2 = "typo",
                     symbols = "circle",          
                     col = cols,
                     inches=inches,
                     fixmax = fixmax, 
                     legend.var2.values.order = t,
                     legend.var.pos = "bottomleft", border = "white",
                     legend.var.frame = T, legend.var2.frame = T,
                     legend.var.title.txt = "Nombre de votes\npour le texte 2",
                     legend.var2.title.txt = "Classement")
labelLayer(depts[depts$typo==t[3],],txt = "Nom", halo=T, cex=0.8, overlap=T,show.lines=F, col="#382222")

dev.off()

# MAP 5

t <- c("Texte minoritaire","Texte arrivé en tête","Majorité absolue")
cols <- c("#fcbba100","#fb6a4a","#db0404")
depts$typo <- t[1]
depts[depts$manifeste > depts$printemps & depts$manifeste > depts$classe & depts$manifeste > depts$question,"typo"] <- t[2]
depts[depts$manifeste/depts$exprimés > 0.5,"typo"] <- t[3]


tot <- sum(depts$manifeste)
share <- round(tot/sum(depts$exprimés)*100,1)
txt <- paste0("Manifeste du Parti\ncommuniste du XXIe siècle\n",tot," voix | ",share,"%")

png("output/texte3.png", width = sizes[1], height = sizes[2], res = 100)
lay(txt)
propSymbolsTypoLayer(x = depts, var = "manifeste", var2 = "typo",
                     symbols = "circle",          
                     col = cols,
                     inches=inches,
                     fixmax = fixmax, 
                     legend.var2.values.order = t,
                     legend.var.pos = "bottomleft", border = "white",
                     legend.var.frame = T, legend.var2.frame = T,
                     legend.var.title.txt = "Nombre de votes\npour le texte 3",
                     legend.var2.title.txt = "Classement")
labelLayer(depts[depts$typo==t[3],],txt = "Nom", halo=T, cex=0.8, overlap=F,show.lines=F, col="#382222")

dev.off()

# MAP 5

t <- c("Texte minoritaire","Texte arrivé en tête","Majorité absolue")
cols <- c("#fcc5c000","#f768a1","#7a0177")
depts$typo <- t[1]
depts[depts$classe > depts$printemps & depts$classe > depts$manifeste & depts$classe > depts$question,"typo"] <- t[2]
depts[depts$classe/depts$exprimés > 0.5,"typo"] <- t[3]

tot <- sum(depts$classe)
share <- round(tot/sum(depts$exprimés)*100,1)
txt <- paste0("Reconstruire le\nParti de classe\n",tot," voix | ",share,"%")

png("output/texte4.png", width = sizes[1], height = sizes[2], res = 100)
lay(txt)
propSymbolsTypoLayer(x = depts, var = "classe", var2 = "typo",
                     symbols = "circle",          
                     col = cols,
                     inches=inches,
                     fixmax = fixmax, 
                     legend.var2.values.order = t,
                     legend.var.pos = "bottomleft", border = "white",
                     legend.var.frame = T, legend.var2.frame = T,
                     legend.var.title.txt = "Nombre de votes\npour le texte 4",
                     legend.var2.title.txt = "Classement")
labelLayer(depts[depts$typo==t[3],],txt = "Nom", halo=T, cex=0.8, overlap=F,show.lines=F, col="#382222")

dev.off()
