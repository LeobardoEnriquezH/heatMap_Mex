# Ejemplo Geographic Heat Map
# Leobardo Enriquez

### Archivos shape files para mapas de M?xico 
# Una opci?n es: Marco Geoestad?stico. Censo de Poblaci?n y Vivienda 2020
# https://www.inegi.org.mx/app/biblioteca/ficha.html?upc=889463807469

# Ejemplo de un mapa a nivel nacional (la informaci?n se obtiene al descargar lo correspondiente a MG_2020_Integrado)
# De la carpeta descargada s?lo usaremos los archivos que inician con 00ent, es decir, queremos los 
# pol?gonos de la capa a nivel entidad federativa
# Presentaremos un mapa con diferentes colores de acuerdo al nivel de pobreza
# similar a http://sistemas.coneval.org.mx/InfoPobreza/Pages/wfrMapaPobreza?pAnio=2018&pTipoPobreza=1&pTipoIndicador=1&pTipoMedicion=2
# La informaci?n de pobreza la tomaremos de
# https://www.coneval.org.mx/Medicion/MP/Paginas/AE_pobreza_2018.aspx

rm(list=ls())
gc()

library(tidyverse)
setwd("~/GitHub/heatMap_Mex")

#New
library(sf)
library(tmap)
EntFedMex <- st_read("00ent.shp")  #read the shapefile 
qtm(EntFedMex) #plot the map 
str(EntFedMex)
head(EntFedMex)

Pob2018=read.csv("Pobreza2018.csv", header=TRUE)
EntFedMex$CVE_ENT=as.numeric(EntFedMex$CVE_ENT)

EntFedMex=EntFedMex %>% left_join(Pob2018, by=c("CVE_ENT"))
str(EntFedMex)


tm_shape(EntFedMex) +
  tm_polygons("Pobreza2018", title = "Pobreza 2018", palette = "Greens", style = "jenks")+ tm_layout(legend.title.size = 0.8) 

ggplot(data = EntFedMex) +
  geom_sf(aes(fill = Pobreza2018)) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt")




### Otra forma (formato un poco m?s pesado)
library(rgdal)

datos <- readOGR("00ent.shp",
                            verbose = FALSE)
str(datos)
plot(datos, main = "Primer mapa", sub = "Est?tico",
     xlab = "Longitud", ylab = "Latitud")


datos@data
datos@data$CVE_ENT=as.numeric(datos@data$CVE_ENT)
datos@data=merge(datos@data, Pob2018, by=c("CVE_ENT"))
head(datos@data)

library(htmlwidgets)
library(leaflet)
datos=datos %>%
  spTransform(CRS("+proj=longlat +datum=WGS84"))
mapa_ent <- leaflet(datos)

pal2 <- colorNumeric(palette = "YlGnBu", domain = datos$Pobreza2018)
MapaEnt=mapa_ent %>% addPolygons(color = ~pal2(Pobreza2018), fillOpacity = 0.5, stroke = FALSE, smoothFactor = 0.2) %>%
                         addLegend("bottomright", pal = pal2, values = ~ Pobreza2018,
                         title = "Pobreza 2018", opacity = 0.7)

saveWidget(MapaEnt, "Map.html", selfcontained = TRUE)



###Otra forma: con leaflet, retomando los datos EntFedMex

EntFedMex2=st_transform(EntFedMex, 4326)
pal3 <- colorNumeric(palette = "YlGnBu", domain = EntFedMex2$Pobreza2018)
MapaEnt2=leaflet(EntFedMex2) %>% addPolygons(color = ~pal3(Pobreza2018), fillOpacity = 0.5, stroke = FALSE, smoothFactor = 0.2) %>%
  addLegend("bottomright", pal = pal3, values = ~ Pobreza2018,
            title = "Pobreza 2018", opacity = 0.7)%>%
  addTiles()

saveWidget(MapaEnt2, "Map2.html", selfcontained = TRUE)
