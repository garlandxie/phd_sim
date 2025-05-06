# Credit: Toby Tsang wrote the code

###current map only

current_tgb <- rast("C:/Users/Garland/Documents/R/phd_sim/DSV.tgb/Final/CurrentEM_final_tgb.tif")
current_no_tgb <- rast("C:/Users/Garland/Documents/R/phd_sim/DSV/Final/CurrentEM_final.tif")

library(ggplot2)
library(viridisLite)

plot_df_tgb <- as.data.frame(current_tgb,xy=T)

p_tgb <- ggplot(plot_df_tgb,aes(x=x,y=y,fill=DSV.tgb_EMwmeanByROC_mergedData_mergedRun_mergedAlgo))+
  geom_raster()+
  scale_fill_viridis_c()+
  coord_equal()+
  labs(fill="Suitability")+
  theme_void()+
  theme(legend.position="bottom") #current map with tgb

plot(p_tgb)  

plot_df_no_tgb <- as.data.frame(current_no_tgb,xy=T)
p_no_tgb <- ggplot(plot_df_no_tgb,aes(x=x,y=y,fill=DSV_EMwmeanByROC_mergedData_mergedRun_mergedAlgo))+
  geom_raster()+
  scale_fill_viridis_c()+
  coord_equal()+
  labs(fill="Suitability")+
  theme_void()+
  theme(legend.position="bottom") #current map without tgb
plot(p_no_tgb)  

library(ggpubr)
ggarrange(p_tgb,p_no_tgb,common.legend = T,labels=c("TGB","No TGB"),legend="bottom")

p_diff <- ggplot(plot_df_tgb,aes(x=x,y=y,fill=difference))+
  geom_raster()+
  scale_fill_viridis_c()+
  coord_equal()+
  labs(fill="tgb-no_tgb")+
  theme_void()+
  theme(legend.position="bottom")
plot(p_diff)
