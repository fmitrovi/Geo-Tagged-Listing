library(ggplot2)
library(ggmap)
library(rgdal)
library(ggrepel)
library(cowplot)
library(rgdal)
library(ggspatial)
library(ggpubr)

  
  
  
  tr <- read.csv("C:/Users/mitro/Documents/MICS/MICS6 Countries/MICS6TUV/11 Fieldwork/main_tuvalu_711.csv", comment.char="#")
    colnames(tr)<-c("Team",	"Day",	"HH",	"Cluster Number", "Household Number",	"EA code",	"hh_id",	"hhsize",	"island",	"island code",	"village",	"village code",	"EA code4",	"occupancy",	"dwelling type",	"name",	"family name",	"domain",	"domain5",	"LAT", "LON",	"HH_Head")

    df <- NULL
    df$HH<-tr$HH
    df$HH_Head <- tr$HH_Head
    df$lat<- tr$LAT
    df$lon<- tr$LON
    df$HH_Head1<-paste(df$HH,df$HH_Head, sep=".")
      df<-as.data.frame(df) 
df[complete.cases(df),]
    
df<- na.omit(object = df)


  
    # Plotting the Location of each Household within the selected Cluster
    tuvmapea <- get_map(location = c(lon = mean(df$lon), lat = mean(df$lat)), zoom = 16,
                        source = "google" , maptype = "hybrid", scale = 1)
  
  
  tuvmapea <- ggmap(tuvmapea) +
    
    geom_point(data = df, aes(x = lon, y = lat, color= "white"), color="red", fill= "black", stroke = 1.5, shape = 21, alpha=0.9)+
    
  geom_label_repel(data = df, 
                  aes(x = lon, y = lat, label = HH_Head1), 
                  fontface = "bold",
                  
                  nudge_x = 0.02,
                  
                  direction = "y",
                  box.padding = unit(0.25, 'lines'),
                  hjust = 1,
                  
                  segment.size = 0.1,
                 
                  arrow = arrow(length = unit(0.03, "npc"), type = "closed", ends = "first"),force=10) +
        scale_fill_viridis_c(trans = "sqrt", alpha = .9)+ 

    annotation_north_arrow(location = "bl", which_north = "true", 
                           pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                           style = north_arrow_fancy_orienteering) +
   

    ggtitle("", subtitle = "(Cluster # 711, Team # 2)")+
   
    theme_void() + 
    
    theme(legend.position="none")

  # Plotting the Location of the EA
   
   tuvmapea_small <- get_map(location = c(lon = mean(df$lon), lat = mean(df$lat)), zoom = 16,
                       source = "google" , maptype = "hybrid", scale = 1)
   
   
   tuvmapea_small <- ggmap(tuvmapea_small) +
     
     geom_rect(data = df, aes(xmin = min(df$lon), xmax=max(df$lon), ymin= min(df$lat),ymax = max(df$lat), color= "white"), color="red",alpha=0.125)+
     
  
     
     annotate("text", label = "Cluster Location, Cluster #:711", x = max(df$lon)+0.0052, y = max(df$lat)+0.002, size = 5, colour = "white",fontface =2)+
     
     
     ggtitle("", subtitle = "(Cluster # 711, Team # 2)")+
     
     theme_void() + 
     
     theme(legend.position="none")
  
   # plot(tuvmapea_small)
   
   ggarrange(tuvmapea, tuvmapea_small + rremove("x.text"), 
             labels = c("2019 MICS Tuvalu:Location of Households in EA", "2019 MICS Tuvalu: Location of the EA"),
              
             ncol = 1, nrow = 2,align = "v")
   
   ggsave(filename = "Cluster_map_maintuvalu_711_revised.pdf", plot = last_plot(), device = NULL, path = NULL,
           width = 8.3, height = 11.7,
          dpi = "retina", limitsize = TRUE)
   
   
   