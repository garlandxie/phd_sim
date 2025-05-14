################################################################################
# Accompanying code for the paper: 
#   Can urban green spaces support biological invasions? 
#   A case study in Toronto, Canada
#
# Corresponding authors for this script:  
#   Garland Xie      (1,2)
#   Toby Tsang       (1)
#
# Affiliations: 
#   (1) Department of Biological Sciences, 
#       University of Toronto Scarborough,
#       1265 Military Trail, Toronto, ON, M1C 1A4, Canada
#       email: garlandxie@gmail.com
#   (2) Department of Ecology and Evolutionary Biology,
#       University of Colorado Boulder,
#
# Purpose of this R script: to build and project habitat suitability models 
# that account for sampling bias using the target background approach

### prep the data
library(terra)

dir_sc1 <- "data/input_data/resist_surfaces/converted/"
rast_name <- list.files(dir_sc1)
rast_sc1 <- lapply(rast_name,function(x) rast(paste0(dir_sc1,x)))
rast_sc1 <- rast(rast_sc1)

dir_sc2 <- "data/intermediate_data/ugs_scenarios/sc2_realistic/"
rast_name <- list.files(dir_sc2)
rast_sc2 <- lapply(rast_name,function(x) rast(paste0(dir_sc2,x)))
rast_sc2 <- rast(rast_sc2)

dir_sc3 <- "data/intermediate_data/ugs_scenarios/sc3_extreme/"
rast_name <- list.files(dir_sc3)
rast_sc3 <- lapply(rast_name,function(x) rast(paste0(dir_sc3,x)))
rast_sc3 <- rast(rast_sc3)

names(rast_sc1) <- c("dsv_priority","land_cover","ndvi","slope","clay","soil_pH","soil_sand","soil_water","wind")
names(rast_sc2) <- c("dsv_priority","clay","land_cover","ndvi","soil_pH","soil_sand","slope","soil_water","wind")
names(rast_sc3) <- c("dsv_priority","clay","land_cover","ndvi","soil_pH","soil_sand","slope","soil_water","wind")

### mask NA areas in some rasters
rast_sc1 <- mask(rast_sc1,rast_sc1$ndvi)
rast_sc2 <- mask(rast_sc2,rast_sc2$ndvi)
rast_sc3 <- mask(rast_sc3,rast_sc3$ndvi)
rast_sc1[rast_sc1$land_cover == 80,] <- NA #set aquatic area as NA
rast_sc2[rast_sc2$land_cover == 80,] <- NA
rast_sc3[rast_sc3$land_cover == 80,] <- NA

plot(rast_sc1, colNA="white")
plot(rast_sc2, colNA="white")
plot(rast_sc3, colNA="white")

###Prepare occurrence data
occ <- read.csv("data/intermediate_data/occurrence_records/occ_tidy_utm17n.csv")
colnames(occ)[3:4] <- c("x","y")
occ_wgs84 <- read.csv("data/intermediate_data/occurrence_records/occ_tidy_wgs84.csv")
occ <- cbind(occ,occ_wgs84)
plot(rast_sc1$land_cover)
plot(vect(occ[,c("lon","lat")]),add=T)
occ <- na.omit(occ)
occ$Presence <- 1
occ$sp <- "DSV"
library(spThin)

thinned_occ <- thin(loc.data = occ,
                    lat.col="lat",
                    long.col="lon",
                    spec.col="sp",
                    thin.par=0.1,
                    reps=1,locs.thinned.list.return = T,
                    write.log.file=F,
                    out.dir="C:/Users/Garland/Documents/R/phd_sim/DSV.tbg")

thinned_occ <- as.data.frame(thinned_occ)
occ<- occ[rownames(thinned_occ),]

### biomod2
library(biomod2)
#####alternatively, something like a tbg approach 
grid<-extract(rast_sc1,occ[,c("x","y")],xy=T,cells=TRUE) 
rast_sc1_df <- as.data.frame(rast_sc1,cells=T,xy=T)
rast_sc1_df$presence <- ifelse(rast_sc1_df$cell %in% grid$cell,1,NA)

tgb <- rast("C:/Users/Garland/Documents/R/phd_sim/data/intermediate_data/target_background/trgt_prob_raster.tiff")
tgb <- mask(tgb,rast_sc1$ndvi)
plot(tgb)
tgb_df <- as.data.frame(tgb,xy=T,cells=T)
rast_sc1_df$tgb <-tgb_df[match(paste0(rast_sc1_df$x,rast_sc1_df$y),paste0(tgb_df$x,tgb_df$y)),"layer"]
rast_sc1_df$tgb[is.na(rast_sc1_df$tgb)] <- 0 

myResp_PA_vect <- vect(rast_sc1_df[,c("presence","x","y")],geom=c("x","y")) #grid format
DSVPAtable <- data.frame(PA1 = ifelse(is.na(rast_sc1_df$presence), FALSE, TRUE),
                         PA2 = ifelse(is.na(rast_sc1_df$presence), FALSE, TRUE),
                         PA3 = ifelse(is.na(rast_sc1_df$presence), FALSE, TRUE))

#####select 10000 PA. prob = null = equal probability. can specify the prob argument with values from a bias layer for tbg approach
for (i in 1:ncol(DSVPAtable)) {
  message(i)
  pos <- sample(which(DSVPAtable[, i] == FALSE), 
                prob=rast_sc1_df$tgb[which(DSVPAtable[, i] == FALSE)],
                10000)
  DSVPAtable[pos, i] <- TRUE
}

DSVBiomodData <- BIOMOD_FormatingData(resp.name="DSV_tbg",
                                      resp.var=rast_sc1_df$presence,
                                      resp.xy = rast_sc1_df[,c("x","y")],
                                      expl.var = rast_sc1,
                                      PA.strategy = "user.defined",
                                      PA.user.table=DSVPAtable) #note number is different (296 vs 351) becoz presence with incomplete cases were excluded

### end of tbg approach
### actual model
DSVBiomodModelOut <- BIOMOD_Modeling(bm.format = DSVBiomodData,
                                     modeling.id = 'AllModels',
                                     models = c("MAXNET"),
                                     CV.strategy = 'kfold',
                                     CV.nb.rep = 3,
                                     CV.perc = 0.8,
                                     CV.k = 5,
                                     metric.eval = c("ROC","BOYCE"),
                                     var.import = 3,
                                     seed.val = 1000)

DSVBiomodEM <- BIOMOD_EnsembleModeling(bm.mod = DSVBiomodModelOut,
                                       models.chosen = 'all',
                                       em.by = 'all',
                                       em.algo = c('EMwmean'),
                                       metric.select = c("ROC"),
                                       metric.select.thresh = c(0.7),
                                       metric.eval = c("ROC"),
                                       var.import = 3,
                                       seed.val = 100)

DSVBiomodEMProj1 <- BIOMOD_EnsembleForecasting(bm.em = DSVBiomodEM,
                                               proj.name = 'CurrentEM_tgb',
                                               new.env = rast_sc1,
                                               models.chosen = 'all',
                                               metric.binary = 'all',
                                               metric.filter = 'all')

DSVBiomodEMProj2 <- BIOMOD_EnsembleForecasting(bm.em = DSVBiomodEM,
                                               proj.name = 'FutureEMSc2_tgb',
                                               new.env = rast_sc2,
                                               models.chosen = 'all',
                                               metric.binary = 'all',
                                               metric.filter = 'all')

DSVBiomodEMProj3 <- BIOMOD_EnsembleForecasting(bm.em = DSVBiomodEM,
                                               proj.name = 'FutureEMSc3_tgb',
                                               new.env = rast_sc3,
                                               models.chosen = 'all',
                                               metric.binary = 'all',
                                               metric.filter = 'all')

### Check model output & finetune
sc1 <- rast("C:/Users/Garland/Documents/R/phd_sim/DSV.tbg/proj_CurrentEM_tgb/proj_CurrentEM_tgb_DSV.tbg_ensemble.tif")
plot(sc1)

sc2 <- rast("C:/Users/Garland/Documents/R/phd_sim/DSV.tbg/proj_FutureEMSc2_tgb/proj_FutureEMSc2_tgb_DSV.tbg_ensemble.tif")
plot(sc2)

sc3 <- rast("C:/Users/Garland/Documents/R/phd_sim/DSV.tbg/proj_FutureEMSc3_tgb/proj_FutureEMSc3_tgb_DSV.tbg_ensemble.tif")
plot(sc3)

plot(sc1, colNA="white")
plot(sc2, colNA="white")
plot(sc3, colNA="white")

sc1 <- sc1/1000
sc1 <- mask(sc1,rast_sc1$ndvi) #mask again
plot(sc1)
writeRaster(sc1,"C:/Users/Garland/Documents/R/phd_sim/DSV.tbg/final/Sc1_CurrentEM_final_tgb.tif",overwrite=T)

sc2 <- sc2/1000
sc2 <- mask(sc2,rast_sc1$ndvi) #mask again
plot(sc2)
writeRaster(sc2,"C:/Users/Garland/Documents/R/phd_sim/DSV.tbg/final/Sc2_FutureEM_final_tgb.tif",overwrite=T)

sc3 <- sc3/1000
sc3 <- mask(sc3,rast_sc1$ndvi) #mask again
plot(sc3)
writeRaster(sc3,"C:/Users/Garland/Documents/R/phd_sim/DSV.tbg/final/Sc3_FutureEM_final_tgb.tif",overwrite=T)

## save disk ----

base::saveRDS(DSVBiomodData, file = "C:/Users/Garland/Documents/R/phd_sim/DSV.tbg/RDS_objects/DSVBiomodData.RDS")
base::saveRDS(DSVBiomodModelOut, file = "C:/Users/Garland/Documents/R/phd_sim/DSV.tbg/RDS_objects/DSVBiomodModelOut.RDS")
base::saveRDS(DSVBiomodEM, file = "C:/Users/Garland/Documents/R/phd_sim/DSV.tbg/RDS_objects/DSVBiomodModelEM.RDS")
base::saveRDS(DSVBiomodEMProj1, file = "C:/Users/Garland/Documents/R/phd_sim/DSV.tbg/RDS_objects/DSVBiomodModelEMProj1.RDS")
base::saveRDS(DSVBiomodEMProj2, file = "C:/Users/Garland/Documents/R/phd_sim/DSV.tbg/RDS_objects/DSVBiomodModelEMProj2.RDS")
base::saveRDS(DSVBiomodEMProj3, file = "C:/Users/Garland/Documents/R/phd_sim/DSV.tbg/RDS_objects/DSVBiomodModelEMProj3.RDS")


