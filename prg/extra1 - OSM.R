# Utiliser les données de OSM

library("sf")
library("cartography")

depts <- st_read(dsn = "data/France/IGN/DEPARTEMENT.shp", stringsAsFactors = F)
colnames(depts)
Paris <- depts[depts$CODE_DEPT == 75,]
plot(st_geometry(Paris))

Paname <- getTiles(x = Paris, type = "osm", crop = TRUE)
Paname <- getTiles(x = Paris, type ="opencycle", crop = TRUE)
tilesLayer(Paname)
plot(st_geometry(Paris), lwd=4, add=T)


library("osmdata")

prj <- st_crs(Paris)

bbox <- st_bbox(st_transform(Paris,4326))

q <- opq(bbox = bbox , timeout = 2000) %>% add_osm_feature(key = 'man_made', value = 'surveillance')
cameras <- osmdata_sf(q)$osm_points
cameras <- st_transform(cameras, prj)

cameras$ok <- st_intersects(st_geometry(cameras), st_geometry(Paris), sparse = FALSE)
cameras <- cameras[cameras$ok == T,]

Paname <- getTiles(x = Paris, type ="cartodark", crop = TRUE)
tilesLayer(Paname)
plot(st_geometry(Paris), lwd=1, border="white", lty=2, add=T)
plot(st_geometry(cameras), pch=20, col="red", add=T)
