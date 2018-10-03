# ---------------------------
# Exercice x - généralisation
# ---------------------------

setwd("/home/nlambert/Documents/ANF/R")

library("sf")
library("rmapshaper")

countries <- st_read(dsn = "data/world/naturalearth/ne_110m_admin_0_countries.shp", stringsAsFactors = F)

plot(st_geometry(countries))

# Generalisation avec sf (ca marche mal)

countries2 <- st_simplify(x = countries, preserveTopology = FALSE, dTolerance = 1)
plot(st_geometry(countries2), col="red")

countries2 <- st_simplify(x = countries, preserveTopology = TRUE, dTolerance = 2)
plot(st_geometry(countries2), col="red")

# Generalisation avec rmapshaper (ca marche bien)

countries2 <- ms_simplify(countries, keep = 0.1)
plot(st_geometry(countries2), col="red")

