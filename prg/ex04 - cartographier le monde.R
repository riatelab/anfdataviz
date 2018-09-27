# *****************************************
# ** Exercice 4 : cartographier le Monde **
# *****************************************

# Charger les packages

library("sf")

# -----------------------------------------
# Import et configuration du fond de carte
# -----------------------------------------

# méthode 1 : à la main (après téléchargement sur le site Natural Earth)

countries <- st_read(dsn = "data/world/naturalearth/ne_110m_admin_0_countries.shp", stringsAsFactors = F)  

# Méthode 2 : directement via R

url <- "https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/110m/cultural/ne_110m_admin_0_countries.zip"
download.file(url = url, destfile = "data/download/countries.zip")
list.files("data/download")
unzip("data/download/countries.zip", exdir = "data/download", unzip = "internal")
list.files("data/download")
file.remove("data/download/countries.zip")
list.files("data/download")

countries <- st_read(dsn = "data/download/ne_110m_admin_0_countries.shp", stringsAsFactors = F)


# Méthode 3 : grace au package rnaturalearth

library("rnaturalearth")
countries <- ne_countries(scale = 50, type = "countries", continent = NULL,
                          country = NULL, geounit = NULL, sovereignty = NULL,
                          returnclass = "sf")

# Affichage
plot(st_geometry(countries))

# On ne garde que les champs utiles
head(countries)
countries <- countries[,c("adm0_a3", "admin", "geometry")]
colnames(countries) <- c("id","name","geometry")
head(countries)

# Supprimer l'antarctique
countries <- countries[countries$id != "ATA",]
plot(st_geometry(countries))

# Supprimer les lignes vides
countries <- countries[!is.na(countries$id),]

# --------------------------------
# Import des données attributaires
# --------------------------------


  # Esperance de vie (source : Banque mondiale)

library("readxl")

file <-"data/world/worldbank/API_SP.DYN.LE00.IN_DS2_en_excel_v2_10081535.xls"
sheet <- "Data"

lifexp <- data.frame(read_excel(file, skip = 3, sheet = sheet))

lifexp <- lifexp[,c("Country.Code","Country.Name","X2016")]
colnames(lifexp) <- c("id","name","lifexp2016")
lifexp$lifexp2016 <- as.numeric(as.character(lifexp$lifexp2016))

  # Indice de deveoppement humain (source : Human Development Report)

file <-"data/world/hdr/Human Development Index (HDI).csv"
hdi <- read.csv2(file = file, sep = ",",skip = 1)
head(hdi)
hdi <- hdi[,c("Country","X2016")]

library("countrycode")

hdi$id <- countrycode(hdi$Country, 'country.name', 'iso3c')

hdi <- hdi[,c("id","Country","X2016")]
colnames(hdi) <- c("id","name","hdi2016")
hdi$hdi2016 <- as.numeric(as.character(hdi$hdi2016))

class(hdi$hdi2016)


# -------------
# Cartographie
# -------------

library("cartography")

# Changer la projection du fond de carte (voir le site http://spatialreference.org)

countries <- st_transform(countries, "+proj=cea +lon_0=0 +lat_ts=45 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs ")
plot(st_geometry(countries))

# jointures des deux fichiers de données

countries <- merge(countries, lifexp, by="id", all.x=TRUE)
countries <- merge(countries, hdi, by="id", all.x=TRUE)

countries <- countries[,c("id","name.x","lifexp2016","hdi2016","geometry")]
colnames(countries)[2] <- "name"

# -------------------------------
# Indice de developpement humain
# ------------------------------

# analyse de la distribution
var <- countries$hdi2016
hist(countries$hdi2016, probability = TRUE, nclass = 30)

# discretisation par la méthode des quantiles sans classe centrale

breaks <- getBreaks(v = var, nclass = 6, method = "quantile")
cols <- carto.pal(pal1="blue.pal", n1 = 3, pal2 = "orange.pal", n2 = 3, middle = FALSE, transparency = FALSE)

layoutLayer(extent = countries,
            bg="#EEEEEE",
            title = "Indice de développement humain en 2016",
            scale = NULL,
            sources = "Human development Report, 2018",
            author = "les serial mappers"
            )
choroLayer(x = countries, 
           var = "hdi2016",
           breaks = breaks,
           col = cols,
           border = "white",
           lwd=0.2,
           add = TRUE,
           legend.pos = "topleft",
           legend.title.txt = "IDH, 2016",
           legend.values.rnd = 2)


# Discontinuités

borders <- getBorders(countries)
outer <- getOuterBorders(x = countries, res = 100000, width=500000)

b <- rbind(borders,outer)

discLayer(x = b, df = countries,
          var = "hdi2016", col="black", nclass=4,
          method="quantile", threshold = 0.25, sizemin = 0.2,
          sizemax = 8, type = "abs", 
          legend.title.txt = "Discontinuities\nabsolues",
          legend.pos = "bottomleft", add=TRUE)
