# ******************************************
# ** Exercice 2 : cartographier la France **
# ******************************************

setwd("/home/nlambert/ownCloud/ANF2018 - R geoviz")
setwd("C:/MyCore/ANF2018 - R geoviz")

# Charger les packages

library("sf")
library("readxl")
library("cartogram")
library("cartography")

# Import des données
sheet <- "COM_2014"
data <- data.frame(read_excel("data/France/INSEE/pop-act2554-csp-cd-6814.xlsx", skip = 15, sheet = sheet)) 
colnames(data)
data <- data[data$RR == "76",] # Ne prendre que les communes d'Occitanie (code 76)
View(data) # vOIR Si le fichier a correctement été importé. 


# Nouveaux champs
# Création de nouvelles colonnes par aggrégation de CSP (emploi + chômage) + arrondi sans décimale
# affectation d'un nom clair + création d'une colonne total (calcul pourcentage)
colnames(data)

data$id <- paste0(data$DR, data$CR) # Creation du code commune
data[,"agr"]<- round(data[7] + data[8],0) # Actifs + chomeurs par CSP
data[,"art"] <- round(data[9] + data[10],0)
data[,"sup"] <- round(data[11] + data[12],0)
data[,"int"] <- round(data[13] + data[14],0)
data[,"emp"] <- round(data[15] + data[16],0)
data[,"ouv"] <- round(data[17] + data[18],0)
data$total <- data$agr + data$art + data$sup + data$int + data$emp + data$ouv

View(data)


# Import et visualisation du fond de carte
communes <- st_read(dsn = "data/Occitanie/occitanie.shp", stringsAsFactors = F)
plot(st_geometry(communes),col="black", border="white", lwd=0.1)

# Jointure
# Un petit texte décrivant l'importance de la chose. 
communes <- merge(x = communes, y = data, by.x = "INSEE_COM", by.y = "id", all.x=T)

# Nettoyage et mise en forme du tableau
colnames(communes)
fields <- c("INSEE_COM", "LIBELLE", "agr", "art", "sup", "int", "emp", "ouv", "total",  "geometry")
communes <- communes[,fields]
colnames(communes) <-  c("id", "name", "agr", "art", "sup", "int", "emp", "ouv", "total",  "geometry")
View(communes)

# Premiere carte
# On parle de cartography, la doc formidable, la vignette, la cheat sheet 
plot(st_geometry(communes), col="lightblue",border="white", lwd=0.2)
propSymbolsLayer(x = communes, var = "total", 
                 symbols = "circle", col =  "red",
                 legend.pos = "right", border = "grey",
                 legend.title.txt = "Les actifs",
                 legend.style = "c")

top <- sort(data.frame(communes)[,"total"], decreasing = TRUE) 
labelLayer(x = communes[data.frame(communes)[,"total"] %in% top[1:15],], txt = "name", halo=TRUE, cex = 0.6, col= "#000000", bg = "#FFFFFF50", overlap = FALSE) 
title("Région Occitanie")


# Typologie (plus d'ouvriers que de cadres ?)
communes$typo <- "Indéterminé"
communes$r <- communes$ouv / communes$sup
communes$r[is.na(communes$r)] <- 0
communes[communes$r > 1.1,"typo"] <- "Plus d'ouvriers que de cadres"
communes[communes$r < 0.91,"typo"] <- "Plus de cadres que d'ouvriers"


# carte de typologie
# Color picker c'est genial
# Données qualitatives > couleur 
colouv <- "#dd4e44"
colsup <- "#63b269"

typoLayer(communes, var="typo", 
          legend.values.order = c("Plus d'ouvriers que de cadres", 
                                  "Plus de cadres que d'ouvriers",
                                  "Indéterminé" ),
          col = c(colouv, colsup, 'grey'), 
          border = NA, legend.title.txt = "Type dominant") 

# Combiner avec la carte précédente
plot(st_geometry(communes), col="lightblue",border=NA)
propSymbolsTypoLayer(x = communes, var = "total", var2 = "typo",
                     symbols = "circle",
                     inches = 0.4,
                     col = c(colouv, colsup, 'grey'),
                     border = "white", lwd = 0.1,
                     legend.var2.values.order = c("Plus d'ouvriers que de cadres", 
                                             "Plus de cadres que d'ouvriers",
                                             "Indéterminé" ),
                     legend.var.pos = "right", 
                     legend.var.title.txt = "Nombre d'actifs",
                     legend.var2.title.txt = "Type dominant")

labelLayer(x = communes[data.frame(communes)[,"total"] %in% top[1:15],], txt = "name", halo=TRUE, cex = 0.6, col= "#000000", bg = "#FFFFFF50", overlap = FALSE) 
title("Cadres et ouvriers en région Occitanie")


# cartogram de Dorling
actifs <- cartogram_dorling(communes, "total", k = 1, m_weight = 1, itermax = 20)

plot(st_geometry(communes), col="lightblue",border=NA)

typoLayer(actifs, var="typo", 
          legend.values.order = c("Plus d'ouvriers que de cadres", 
                                  "Plus de cadres que d'ouvriers",
                                  "Indéterminé" ),
          col = c(colouv, colsup, 'grey'), 
          border = "white", lwd=0.2, legend.title.txt = "Type dominant",
          add = T) 
labelLayer(x = actifs[data.frame(actifs)[,"total"] %in% top[1:15],], txt = "name", halo=TRUE, cex = 0.6, col= "#000000", bg = "#FFFFFF50", overlap = FALSE) 
title("Ouvriers et cadres en Occitanie, 2014")


# A vous de jouer : Réaliser une carte avec les parametres suivants :
# On s'interesse maintenant au chomage des ouvriers. 
# Typologie : 2 catégories (communes ou le taux de chômage des ouvriers est supérieur/inférieur à 20 %)
# Cercles proportionnels : population ouvrière (actifs + chomeurs)
# Nommer les communes où plus de 500 ouvriers sont au chômage

# Ex / changer pour ouvrier / employé et regarder l'histogramme 

# Solution

sheet <- "COM_2014"
data <- data.frame(read_excel("data/France/INSEE/pop-act2554-csp-cd-6814.xlsx", skip = 15, sheet = sheet))
data <- data[data[,1] == "76",]
data$id <- paste0(data$DR, data$CR)
data[,"ouv_tot"] <- round(data[17] + data[18],0)
data[,"ouv_act"] <- round(data[17],0)
data[,"ouv_chom"] <- round(data[18],0)
communes <- st_read(dsn = "data/Occitanie/occitanie.shp", stringsAsFactors = F)
communes <- merge(x = communes, y = data, by.x = "INSEE_COM", by.y = "id", all.x=T)
fields <- c("INSEE_COM", "LIBELLE", "ouv_tot", "ouv_act", "ouv_chom",  "geometry")
communes <- communes[,fields]
colnames(communes) <-  c("id", "name", "ouv_tot","ouv_act","ouv_chom","geometry")
communes$typo <- "Indéterminé"

communes$r <- communes$ouv_chom / communes$ouv_tot
communes$r[is.na(communes$r)] <- 0
communes[communes$r >= 0.2,"typo"] <- "Supérieur à 20 %"
communes[communes$r < 0.2,"typo"] <- "Inférieur à 20 %"



colsup <- "#dd4e44"
colinf <- "#63b269"

# Plot communes chômage à plus de 20 % et à moins de 20 %, Mais où ?
barplot(communes$typo)

head(communes)

plot(st_geometry(communes), col="lightblue",border="white", lwd=0.2)
propSymbolsTypoLayer(x = communes, var = "ouv_tot", var2 = "typo",
                     symbols = "circle",
                     inches = 0.3,
                     col = c(colsup, colinf), 
                     legend.var2.values.order = c("Supérieur à 20 %", 
                                                  "Inférieur à 20 %"),
                     legend.var.pos = "right", border = "grey",
                     legend.var.title.txt = "Nombre d'ouvriers",
                     legend.var2.title.txt = "Taux de chômage ouvrier en 2014")
labelLayer(x = communes[data.frame(communes)[,"ouv_chom"] > 500,], txt = "name", halo=TRUE, cex = 0.6, col= "#8B0000", bg = "#FFFFFF50", overlap = FALSE) 
title("Chômage des ouvriers en Occitanie")
