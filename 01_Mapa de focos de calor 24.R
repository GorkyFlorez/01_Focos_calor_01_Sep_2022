#  https://firms.modaps.eosdis.nasa.gov/active_fire/

library(sf)
library(ggplot2)
library(raster)
library(plyr)
library(tidyverse)
library(RColorBrewer)
library(ggspatial)
# Capas mundiales
mapa_mundo <- map_data("world")

SurAmerica = st_read("SHP/SurAmerica.geojson")  %>% st_as_sf()
SurAmeric  <- st_transform(SurAmerica ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
Peru       <- getData('GADM', country='Peru', level=1) %>%st_as_sf() 
write_sf(Peru, "SHP/Peru.shp")
# Capas de focos de calor 
Foco = st_read("SHP/MODIS_C6_1_South_America_24h.shp")  %>% st_as_sf()
Focos <- st_transform(Foco ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))

Foco_Peru = st_intersection(Focos, Peru)


Resu = Foco_Peru %>%
  as_tibble %>%
  group_by(NAME_1)%>%
  summarize (N_Focos =n(),
             temp = mean(BRIGHT_T31))%>%
  ungroup() %>%
  mutate (temp = temp - 273.15)# para pasar grados F a C

Peru_Focos_Res = inner_join(Peru,  Resu, by = "NAME_1")


col1=c("#FFFFEA","#FFFFBF" ,"#FFFF95","#FFFF6A" ,"#FFFF40",
      "#FFFF15", "#FFFF00", "#FFF200", "#FFE400", "#FFD700", 
      "#FFC900", "#FFBC00", "#FFAE00","#FFA100", "#FF9400" ,
      "#FF8600", "#FF7900", "#FF6B00", "#FF5E00", "#FF5100",
      "#FF4300", "#FF3600", "#FF2800", "#FF1B00", "#FF0D00", 
      "#FF0000"
      )

library(ggrepel)

Peru_Focos_Res_xy <- cbind(Peru_Focos_Res, st_coordinates(st_centroid(Peru_Focos_Res$geometry)))
Foco_Peru_xy <- cbind(Foco_Peru, st_coordinates(st_centroid(Foco_Peru$geometry)))

gg_Focos_peru = ggplot()+
  geom_sf(data = Peru, fill="white", color="black")+
  geom_sf(data = SurAmeric, fill="white", color="black")+
  geom_sf(data = Peru_Focos_Res, aes(fill= N_Focos))+
  scale_fill_gradientn(colours = col1,
                       breaks = c(10,20,30,40,50,60,70,80),)+
  geom_label_repel(data = Peru_Focos_Res_xy, aes(x = X, y = Y, label =NAME_1 ,fill = N_Focos), 
                   family="serif", box.padding = unit(0.9, "lines"), size = 3, face = "bold",color = 'black',
                   point.padding = unit(0.5, "lines")) +
  geom_point(data =Foco_Peru_xy , aes(x = X, y = Y),size=1.5, alpha=0.3, color="black", pch=21, fill="red")+
  coord_sf(xlim = c(-81.3307, -68.65311), ylim = c(-18.3518 ,-0.03747),expand = FALSE)+
  theme_classic()+
  labs(fill="Cantidad de incendios")+
  theme(legend.position = "bottom",
        axis.text.x  = element_text(face="bold", color="black", size=10,
                                    family="serif"),
        axis.text.y  = element_text(angle = 90,face="bold", color="black",
                                    family="serif",size=10),
        panel.background = element_rect(fill = "#a2d2ff"),
        axis.title = element_text(face="bold", color="black"),
        legend.background = element_rect(fill = "white"),
        legend.text=element_text(size=9, family="serif"),
        legend.title = element_text(size=9, family="serif"),
        legend.key.width = unit(2.5,"line"), #ancho de cuadrados de referencia 
        legend.key.size = unit(0.3, "cm"), #alto de cuadrados de referencia
        panel.border = element_rect( color = "grey20", fill = NA, size = 0.5))+
  labs(title = '', fill = 'Cantidad Incendios',  x = 'Longitud', y = 'Latitud')+
  annotation_north_arrow(location="tl",which_north="true",style=north_arrow_fancy_orienteering ())+
  annotation_scale(location = "bl",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")


summ <- Peru_Focos_Res %>%
  mutate(NAME_1= fct_reorder(NAME_1, N_Focos, .desc = TRUE))


col=c("#FF0000" ,"#FF0D00" ,"#FF1B00", "#FF2800", "#FF3600", "#FF4300", "#FF5100", "#FF5E00",
      "#FF6B00", "#FF7900" ,"#FF8600" ,"#FF9400" ,"#FFA100" ,"#FFAE00" ,"#FFBC00", "#FFC900",
      "#FFD700", "#FFE400" ,"#FFF200" ,"#FFFF00", "#FFFF15" ,"#FFFF40", "#FFFF6A" ,"#FFFF95",
       "#FFFFBF" ,"#FFFFEA")

STAT = ggplot(data = summ, aes(x=NAME_1, y=N_Focos, fill=NAME_1))+
  scale_fill_manual(values =  col)+
  geom_col()+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        legend.position = c(0.75, 0.55),
        legend.text =element_text(size=8, family="serif"),
        legend.title = element_text(size=8, family="serif"),
        axis.title.x = element_text(color="black", size=11,family="serif",face="bold"),
        axis.title.y = element_text(color="black", size=11,family="serif",face="bold"),
        axis.text.y  = element_text(color="black", size=10, family="serif",face="bold"),
        axis.text.x  = element_text(color="black", size=10, family="serif",face="bold", hjust=1),
        legend.key.size = unit(0.3, "cm"), #alto de cuadrados de referencia
        legend.key.width = unit(0.6,"cm"), #ancho de cuadrados de referencia 
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 11))+
  geom_text(aes(label = round(N_Focos,1)), vjust = -.5, size = 4, 
            fontface="bold", position = position_dodge(width = .9)) +
  labs(x= "Departamentos",
       y= "Numero de Focos de Calor",fill = 'Cantidad \nIncendios')+
  guides(fill= guide_legend(nrow = 26, ncol=1))+
  coord_flip()


library(cowplot)
Final=ggdraw() +
  coord_equal(xlim = c(0, 29), ylim = c(0, 21), expand = FALSE) +
  draw_plot(gg_Focos_peru , width = 19, height = 19,x = -3, y = 1)+
  draw_plot(STAT, width = 16, height = 18,x = 13, y = 1)+
  
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect( color = "black", fill = NA, size = 1))+
  annotate(geom = "text", x = 8, y = 3, hjust = 0, vjust = 1, angle=45,
           label = "Gorky Florez Castillo            Gorky Florez Castillo        Gorky Florez Castillo",
           size = 7, family="serif", color = "grey20",
           alpha=0.2)+
  annotate(geom = "text", x = 10, y = 20.5, hjust = 0, vjust = 1,
           label = "Número de focos de Calor en el Perú - 24 Horas ",
           size = 5,family="serif", face = "italic", color = "#6a040f")+
  annotate(geom = "text", x = 8, y = 20, hjust = 0, vjust = 1,
           label = "Data: FIRMS Información sobre incendios para el sistema de Gestión de recursos ",
           size = 5,family="serif", color = "#6a040f")
Final
ggsave(plot=Final,"Mapa/Mapa focos de calor 24 Horas Peru.png",units = "cm",width = 29, #alto
       height = 21, #ancho
       dpi=1200)









st_kde <- function(points,cellsize, bandwith, extent = NULL){
  require(MASS)
  require(raster)
  require(sf)
  if(is.null(extent)){
    extent_vec <- st_bbox(points)[c(1,3,2,4)]
  } else{
    extent_vec <- st_bbox(extent)[c(1,3,2,4)]
  }
  
  n_y <- ceiling((extent_vec[4]-extent_vec[3])/cellsize)
  n_x <- ceiling((extent_vec[2]-extent_vec[1])/cellsize)
  
  extent_vec[2] <- extent_vec[1]+(n_x*cellsize)-cellsize
  extent_vec[4] <- extent_vec[3]+(n_y*cellsize)-cellsize
  
  coords <- st_coordinates(points)
  matrix <- kde2d(coords[,1],coords[,2],h = bandwith,n = c(n_x,n_y),lims = extent_vec)
  raster(matrix)
}


points_kd <- st_kde(Foco_Peru,0.01,0.8)
points_kde<- crop(points_kd  ,Peru ) 

Dens_ras   <-  rasterToPoints(points_kde)
Dens_raster   <-  data.frame(Dens_ras)
colnames(Dens_raster ) = c("X", "Y", "den")


Dens_raster %>%
  subset(den <= 1 & den> 0.01) ->Dens_raster_focos

library(ggplot2)
library(ggspatial)

colores<- c( '#40916c', '#80b918',
             '#55a630','#aacc00','#d4d700','#eeef20','#ffff3f','#ff9e00','#ff9100','#ff6d00','#e36414'
             ,'#9a031e')

Den_focos = ggplot()+
  geom_sf(data = Peru, fill="white", color="black")+
  geom_sf(data = SurAmeric, fill="white", color="black")+
  geom_raster(data = Dens_raster_focos, aes(x=X, y=Y, fill=den), show.legend = F)+
  scale_fill_gradientn(colours = colores, 
                       na.value = 'white',
  ) +
  geom_point(data =Foco_Peru_xy , aes(x = X, y = Y),size=1, alpha=0.3, 
             color="black", pch=21, fill="red")+
  geom_sf(data = Peru, fill=NA, color="black")+
  coord_sf(xlim = c(-81.3307, -68.65311), ylim = c(-18.3518 ,-0.03747),expand = FALSE)+
  theme_classic()+
  labs(fill="Cantidad de incendios")+
  theme(legend.position = "bottom",
        axis.text.x  = element_text(face="bold", color="black", size=10,
                                    family="serif"),
        axis.text.y  = element_text(angle = 90,face="bold", color="black",
                                    family="serif",size=10),
        axis.title = element_text(face="bold", color="black"),
        legend.background = element_rect(fill = "#f4f3ee"),
        legend.text=element_text(size=9, family="serif"),
        legend.title = element_text(size=9, family="serif"),
        legend.key.width = unit(3,"line"), #ancho de cuadrados de referencia 
        panel.background = element_rect(fill = "#a2d2ff"),
        panel.border = element_rect( color = "grey20", fill = NA, size = 0.5))+
  labs(title = '', fill = 'Cantidad \nIncendios',  x = 'Longitud', y = 'Latitud')+
  annotation_north_arrow(location="tl",which_north="true",style=north_arrow_fancy_orienteering ())+
  annotation_scale(location = "bl",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")




summ <- Peru_Focos_Res %>%
  mutate(NAME_1= fct_reorder(NAME_1, temp, .desc = TRUE))


STAT_temp = ggplot(data = summ, aes(x=NAME_1, y=temp, fill=NAME_1))+
  scale_fill_manual(values =  col)+
  geom_col()+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        legend.position = c(0.85, 0.55),
        legend.text =element_text(size=8, family="serif"),
        legend.title = element_text(size=8, family="serif"),
        axis.title.x = element_text(color="black", size=11,family="serif",face="bold"),
        axis.title.y = element_text(color="black", size=11,family="serif",face="bold"),
        axis.text.y  = element_text(color="black", size=10, family="serif",face="bold"),
        axis.text.x  = element_text(color="black", size=10, family="serif",face="bold", hjust=1),
        legend.key.size = unit(0.3, "cm"), #alto de cuadrados de referencia
        legend.key.width = unit(0.6,"cm"), #ancho de cuadrados de referencia 
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 11))+
  geom_text(aes(label = round(temp,1)), vjust = -.5, size = 4, 
            fontface="bold", position = position_dodge(width = .9)) +
  labs(x= "Departamentos",
       y= "Temperatura°C",fill = 'Temperatura por\n focos de calor')+
  guides(fill= guide_legend(nrow = 26, ncol=1))+
  coord_flip()+
  ylim(0,50) 
STAT_temp
library(cowplot)
Final1=ggdraw() +
  coord_equal(xlim = c(0, 29), ylim = c(0, 21), expand = FALSE) +
  draw_plot(Den_focos , width = 19, height = 19,x = -3, y = 1)+
  draw_plot(STAT_temp, width = 16, height = 18,x = 13, y = 1)+
  
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect( color = "black", fill = NA, size = 1))+
  annotate(geom = "text", x = 8, y = 3, hjust = 0, vjust = 1, angle=45,
           label = "Gorky Florez Castillo            Gorky Florez Castillo        Gorky Florez Castillo",
           size = 7, family="serif", color = "grey20",
           alpha=0.2)+
  annotate(geom = "text", x = 10, y = 20.5, hjust = 0, vjust = 1,
           label = "Temperatura de los focos de calor en el Perú - 24 Horas ",
           size = 5,family="serif", face = "italic", color = "#6a040f")+
  annotate(geom = "text", x = 8, y = 20, hjust = 0, vjust = 1,
           label = "Data: FIRMS Información sobre incendios para el sistema de Gestión de recursos ",
           size = 5,family="serif", color = "#6a040f")
Final1

ggsave(plot=Final1,"Mapa/Mapa densidad de focos de calor 24 Horas Peru.png",units = "cm",width = 29, #alto
       height = 21, #ancho
       dpi=1200)




