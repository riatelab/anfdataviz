# *****************************************
# ** Exercice 3 : cartographier l'Europe **
# *****************************************

# Obj 1 : Telecharger et mettre en forme des donnes depuis Eurostat
# Obj 2 : Faire une carte choroplethe en testant plusieurs methodes de discretisation
# Obj 3 : Introduire les pb liés au MAUP et construire des representations permettant de s'en abstraire
# Obj 4 : Construire un modele cartographique en R. 

# ------------------------
# Template cartographique
# ------------------------

library("sf")
library("rmapshaper") # J'ai generalise le fond de carte initial qui contenait des erreurs topologiques / Étape à shinter

# Fond de carte principal
# Fond avec erreurs topo
shp <- "data/Europe/GREAT/GEOM_EU34_NUTS13/geom_eu34_nuts13.shp"
regions <- st_read(dsn = shp, stringsAsFactors = F) 
regions <- ms_simplify(regions, keep = 0.9999)

# Elements d'habillage
borders <- st_read("data/Europe/GREAT/OTHERS/borders_EU34.shp", stringsAsFactors = F)
countries <- st_read("data/Europe/GREAT/OTHERS/background_countries.shp",stringsAsFactors = F)
coastlines <- st_read("data/Europe/GREAT/OTHERS/coast_wide.shp", stringsAsFactors = F)
cyprus <- st_read("data/Europe/GREAT/OTHERS/cyprus_north_part.shp", stringsAsFactors = F)
remote <- st_read("data/Europe/GREAT/OTHERS/remote_territories_wide.shp", stringsAsFactors = F)
seaboxes <- st_read ("data/Europe/GREAT/OTHERS/remote_area_background_wide.shp",stringsAsFactors = F)
background <- st_read("data/Europe/GREAT/OTHERS/background.shp", stringsAsFactors = F)

# Mise en page du modele cartographique
par(mfrow=c(1,1))
plot(st_geometry(background), col = "#e0ecff", border = NA, add =FALSE)
plot(st_geometry(countries), col = "#e6e6e6", border = NA, add =TRUE)
plot(st_geometry(seaboxes), col = "#f7fcfe", border = NA, add = TRUE)
plot(st_geometry(regions),  col = "#7ccdea", border = "#f7fcfe", lwd = 0.15, add=T) # Les representations cartographiques porteront sur cette couche 
plot (st_geometry(remote), col = "#e6e6e6", border =NA, add=TRUE)
plot (st_geometry(borders), col = "#f7fcfe",lwd = 0.5, add = TRUE) 
plot (st_geometry(cyprus), col = "#f7fcfe", border = NA, add = TRUE)
plot (st_geometry(coastlines), col = "#d2dbef", lwd = 0.15, add = TRUE)
plot(st_geometry(seaboxes), col = NA, border = "#bbbdc0", lwd = 0.15, add = TRUE)


# ------------------------
# Des données sur l'Europe
# ------------------------

library("eurostat")
library("reshape2") # For DataFrame Manipulation

# POPULATION
var <- "demo_r_pjangrp3"
data <- get_eurostat(var, time_format = "num") # Telecharger la table ESTAT
data <- subset(data, data$sex == "T") # Filtre des dimensions de la table
data <- subset(data, data$age == "TOTAL") # Filtre des dimensions de la table
data <- dcast(data, geo ~ time, value.var = "values")
head(data)
colnames(data) <- c("geo","POP_2014","POP_2015","POP_2016","POP_2017")

# SURFACE
var <- "reg_area3"
area <- get_eurostat(var, time_format = "num") # Telecharger la table ESTAT
str(area)
area <- subset(area, area$landuse == "TOTAL") # Filtre des dimensions de la table
area <- dcast(area, geo ~ time, value.var = "values")
colnames(area) <- c("geo","AREA")

# PIB
var <- "nama_10r_3gdp"
gdp <- get_eurostat(var, time_format = "num")
str(gdp)
levels(gdp$unit)
gdp <- subset(gdp, gdp$unit == "MIO_EUR")
gdp <- dcast(gdp, geo ~ time, value.var = "values")
fields <- c("geo", "2014", "2015", "2016") # On ne garde que les valeurs de PIB correspondant aux valeurs de population disoponibles
gdp <- gdp[,fields]
colnames(gdp) <- c("geo","GDP_2014","GDP_2015","GDP_2016")
head(gdp)

# Jointure
regions <- merge(x = regions, y = data, by.x = "id", by.y = "geo", all.x=T) # population
regions <- merge(x = regions, y = area, by.x = "id", by.y = "geo", all.x=T) # surface
regions <- merge(x = regions, y = gdp, by.x = "id", by.y = "geo", all.x=T) # surface

head(regions)

# ------------------------
# CARTOGRAPHIER LA DENSITE DE POPULATION DANS LA MAILLE (NUTS3)
# ------------------------
library(cartography)

# Quelle annee ? Analyse de completude
apply(regions,2,function(x) sum(is.na(x)))

# Calcul du ratio
regions$DENS_2017 <- regions$POP_2017 / regions$AREA

# Choix de la palette
display.carto.all(10)
cols <- carto.pal(pal1 = "red.pal", n1 = 10)

# Methode discretisation (quantiles)
breaks <- getBreaks(v = regions$DENS_2017, nclass = 10, method = "quantile")
hist(regions$DENS_2017, probability = TRUE, breaks = breaks, col = "#F0D9F9")
rug(regions$DENS_2017)
med <- median(regions$DENS_2017)
abline(v = med, col = "blue", lwd = 3)

# Mise en page (back)
par(mar = c(0.5,0.5,1.5,0.5)) 
# Ajuster les marges
plot(st_geometry(background), col = "#e0ecff", border = NA, add =FALSE)
plot(st_geometry(countries), col = "#c6c4c4", border = NA, add =TRUE)
plot(st_geometry(seaboxes), col = "#f7fcfe", border = NA, add = TRUE)

# Cartographie (ratio)
choroLayer(x = regions, 
           var = "DENS_2017",
           method ="quantile",
           nclass = 10,
           col = cols,
           border = NA,
           add = TRUE,
           legend.pos = "topleft",
           legend.title.txt = "Population density, 2017",
           legend.values.rnd = 2)

# Mise en page (top)
plot (st_geometry(remote), col = "#e6e6e6", border =NA, add=TRUE)
plot (st_geometry(borders), col = "#f7fcfe",lwd = 0.5, add = TRUE) 
plot (st_geometry(cyprus), col = "#f7fcfe", border = NA, add = TRUE)
plot (st_geometry(coastlines), col = "#d2dbef", lwd = 0.15, add = TRUE)
plot(st_geometry(seaboxes), col = NA, border = "#bbbdc0", lwd = 0.15, add = TRUE)


# Sources + elements d'habillage
layoutLayer(title = "Population density in Europe Union, EFTA and Candidation Countries",
            author = "Serial Mappers, 2018", sources = "Eurostat, 2018",
            scale = 200, tabtitle = TRUE,
            frame = FALSE,theme = "sand.pal",
            south = TRUE)

#############
# A VOUS DE JOUER / COMPRENDRE L'IMPORTANCE DES DISCRETISATIONS
#############

# Reproduire la carte en utilisant d'autres methodes de discretisation (Intervalles égaux et progression geometrique). Que remarquez-vous ? 
# Solution 
breaks <- getBreaks(v = regions$DENS_2017, nclass = 10, method = "equal")
hist(regions$DENS_2017, probability = TRUE, breaks = breaks, col = "#F0D9F9")
rug(regions$DENS_2017)
med <- median(regions$DENS_2017)
abline(v = med, col = "blue", lwd = 3)

breaks <- getBreaks(v = regions$DENS_2017, nclass = 10, method = "geom")
hist(regions$DENS_2017, probability = TRUE, breaks = breaks, col = "#F0D9F9")
rug(regions$DENS_2017)


# On separe le plot en 4 pour comparer les cartes
par(mfrow=c(2,2))

choroLayer(x = regions, 
           var = "DENS_2017",
           method ="equal",
           nclass = 10,
           col = cols,
           border = NA,
           add = FALSE,
           legend.pos = "topleft",
           legend.title.txt = "Population density, 2017",
           legend.values.rnd = 0)

choroLayer(x = regions, 
           var = "DENS_2017",
           method ="geom",
           nclass = 10,
           col = cols,
           border = NA,
           add = FALSE,
           legend.pos = "topleft",
           legend.title.txt = "Population density, 2017",
           legend.values.rnd = 2)

choroLayer(x = regions, 
           var = "DENS_2017",
           method ="quantile",
           nclass = 10,
           col = cols,
           border = NA,
           add = FALSE,
           legend.pos = "topleft",
           legend.title.txt = "Population density, 2017",
           legend.values.rnd = 2)

choroLayer(x = regions, 
           var = "DENS_2017",
           method ="sd",
           nclass = 10,
           col = cols,
           border = NA,
           add = FALSE,
           legend.pos = "topleft",
           legend.title.txt = "Population density, 2017",
           legend.values.rnd = 2)


#######################################################
# TECHNIQUES POUR EVITER LE MAUP - PASSAGE EN GRILLES
######################################################

# Transformation des donnees dans une grille reguliere de 50 km
mygrid50k <- getGridLayer(x = regions, cellsize = 50000 * 50000, 
                       type = "regular", var = c("POP_2017","POP_2014","GDP_2014"))


# Transformation des donnees dans une grille reguliere de 100 km
mygrid100k <- getGridLayer(x = regions, cellsize = 100000 * 100000, 
                          type = "regular", var = c("POP_2017","POP_2014","GDP_2014"))

# Conversion en km²
mygrid50k$densitykm <- mygrid50k$POP_2017 * 1000 * 1000 / mygrid50k$gridarea 
mygrid100k$densitykm <- mygrid100k$POP_2017 * 1000 * 1000 / mygrid100k$gridarea 


# Cartographie
par(mfrow=c(1,2)) # 2 cartes

cols <- carto.pal(pal1 = "blue.pal", pal2 = "red.pal",  n1 = 5, n2 = 5)  
  
choroLayer(x = mygrid50k, var = "densitykm", 
           border = "grey80",col = cols, method ="quantile",
           nclass = 10, legend.pos = "topright", legend.values.rnd = 1,
           legend.title.txt = "Densité de population, 2017")

layoutLayer(title = "Densité de population dans une grille régulière de 50km",
            author = "Serial Mappers, 2018", sources = "Eurostat, 2018",
            scale = 200,
            frame = TRUE,
            col = "red",
            coltitle = "white",
            south = TRUE)

choroLayer(x = mygrid100k, var = "densitykm", 
           border = "grey80",col = cols, method ="quantile",
           nclass = 10, legend.pos = "topright", legend.values.rnd = 1,
           legend.title.txt = "Densité de population, 2017")

layoutLayer(title = "Densité de population dans une grille régulière de 100km",
            author = "Serial Mappers, 2018", sources = "Eurostat, 2018",
            scale = 200,
            frame = TRUE,
            col = "red",
            coltitle = "white",
            south = TRUE)


##############################
# A VOUS DE JOUER
#############################

# Réaliser une carte du PIB par habitant 2014 sur une grille reguliere de 200 km -
# La cartographie doit être réalisée avec 6 classes en utilisant une palette verte et rouge discretisee selon la methode des quantiles
# Solution
# Creation de la grille
mygrid200k <- getGridLayer(x = regions, cellsize = 200000 * 200000, 
                          type = "regular", var = c("POP_2014","GDP_2014"))

# Calcul PIB par habitant
mygrid200k$GDP_HAB <- mygrid200k$GDP_2014 * 1000000 / mygrid200k$POP_2014 

# Une seule carte
par(mfrow=c(1,1))

# Cartographie

cols <- carto.pal(pal1 = "green.pal", pal2 = "red.pal",  n1 = 3, n2 = 3)  

choroLayer(x = mygrid200k, var = "GDP_HAB", 
           border = "grey80",col = cols, method ="quantile",
           nclass = 6, legend.pos = "topright", legend.values.rnd = 1,
           legend.title.txt = "PIB par habitant, 2014")

layoutLayer(title = "PIB par habitant dans une grille régulière de 100km",
            author = "Serial Mappers, 2018", sources = "Eurostat, 2018",
            scale = 200,
            frame = TRUE,
            col = "red",
            coltitle = "white",
            south = TRUE)


