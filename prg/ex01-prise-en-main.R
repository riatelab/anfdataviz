  # ********************************
  # ** Exercice 1 : prise en main **
  # ********************************



# -----------------------------------------
# IMPORTER DES DONNES GEOMETRIQUES DANS R
# -----------------------------------------

# >>> Définir  votre espace de travail <<< #

setwd("/home/nlambert/ownCloud/ANF2018 - R geoviz")

# Consulter le contenu 

list.dirs()
list.files()
list.files("data")

# import d'une couche SIG dans R avec le package sf
# basé sur GEOS (Geometry Engine Open Source) et GDAL (Geospatial Data Abstraction Library) 

library("sf")

communes <- st_read(dsn = "data/Hérault/communes.shp", stringsAsFactors = F)

# Voir la table atributaire

communes
head(communes)
View(communes)

# Afficher la couche

plot(st_geometry(communes))

# Afficher la couche avec des parametres graphiques

plot(st_geometry(communes), col="#aec8f2", border="darkblue", lwd=1)

# ------------------------------------------------------------------------------------------------------------------
# SIG : Quelles sont les communes qui ont une partie de leur territoire situé à moins de 20 km du centre de Sete ?
# ------------------------------------------------------------------------------------------------------------------

# Extraire la commune de Sete 

macommune <- "SETE"

monpoly <- communes[communes$NOM_COMM == macommune,]
plot(st_geometry(monpoly), col="red", border="purple", lwd=1, add=T)

# Extraitre le centroide de la commune de Sete

moncentre <- st_centroid(x = monpoly)

plot(st_geometry(moncentre), pch=20, col="black", cex=2, add=T)

# Calculer une zone tampon
mydist <- 20000

buff <- st_buffer(x = st_geometry(moncentre), dist=mydist)
plot(buff, col=NA, border="black", lty=2,add=T)

# Recuperer la liste des communes

communes$buff <- st_intersects(st_geometry(communes), st_geometry(buff), sparse = FALSE)
head(communes)

# Récupérer le nombre de communes à moins de 20 km

nb <- dim(communes[communes$buff == T,])[1]

# extraction et affichage des communes

communes20km <- communes[communes$buff == TRUE,]

plot(st_geometry(communes20km), col=NA, lwd=2, border="red", add=T)

# Ajout d'un titre

montitre <- paste0("Il y a ", nb, " communes situées à moins de ", mydist/1000, "km de ",tolower(macommune))

title(montitre)

# export (si besoin)

st_write(obj = communes20km, dsn = "outputs/resultat.shp")


# >>> recommencez avec la commune de votre choix et une autre distance <<<
communes$NOM_COMM


