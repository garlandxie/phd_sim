### prep the data

dir_sc1 <- "data/input_data/resist_surfaces/converted/"
rast_name <- list.files(dir_sc1)

library(terra)
rast_sc1 <- lapply(rast_name,function(x) rast(paste0(dir_sc1,x)))
rast_sc1 <- rast(rast_sc1)

dir_sc2 <- "data/intermediate_data/ugs_scenarios/"
rast_name <- list.files(dir_sc2)
rast_sc2 <- lapply(rast_name,function(x) rast(paste0(dir_sc2,x)))
rast_sc2 <- rast(rast_sc2)

names(rast_sc1) <- c("dsv_priority","land_cover","ndvi","slope","clay","soil_pH","soil_sand","soil_water","wind")
names(rast_sc2) <- c("dsv_priority","clay","land_cover","ndvi","soil_pH","soil_sand","slope","soil_water","wind")

### mask NA areas in some rasters
rast_sc1<-mask(rast_sc1,rast_sc1$ndvi)
rast_sc2<-mask(rast_sc2,rast_sc2$ndvi)
rast_sc1[rast_sc1$land_cover == 80,] <- NA #set aquatic area as NA
rast_sc2[rast_sc2$land_cover == 80,] <- NA

plot(rast_sc1,colNA="white")
plot(rast_sc2,colNA="white")

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
                    out.dir="C:/Users/Garland/Documents/R/phd_sim/DSV")

thinned_occ <- as.data.frame(thinned_occ)
occ<- occ[rownames(thinned_occ),]

### biomod2
library(biomod2)

#####PA-random (see assumptions of different approaches https://cran.r-project.org/web/packages/biomod2/vignettes/vignette_pseudoAbsences.html#:~:text=Pseudo%2Dabsences%20(sometimes%20also%20referred,presences)%20against%20what%20is%20available.)
DSVBiomodData <- BIOMOD_FormatingData(resp.var = occ$Presence,
                                     expl.var = rast_sc1,
                                     resp.xy = occ[,c("x","y")],
                                     resp.name = "DSV",
                                     PA.nb.rep = 3,
                                     PA.nb.absence = 10000,
                                     PA.strategy = "random")

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
                                              proj.name = 'CurrentEM',
                                              new.env = rast_sc1,
                                              models.chosen = 'all',
                                              metric.binary = 'all',
                                              metric.filter = 'all')

DSVBiomodEMProj2 <- BIOMOD_EnsembleForecasting(bm.em = DSVBiomodEM,
                                              proj.name = 'FutureEM',
                                              new.env = rast_sc2,
                                              models.chosen = 'all',
                                              metric.binary = 'all',
                                              metric.filter = 'all')

### Check model output & finetune
sc1 <- rast("C:/Users/Garland/Documents/R/phd_sim/DSV/proj_CurrentEM/proj_CurrentEM_DSV_ensemble.tif")
plot(sc1)

sc2 <- rast("C:/Users/Garland/Documents/R/phd_sim/DSV/proj_FutureEM/proj_FutureEM_DSV_ensemble.tif")
plot(sc2)

plot(sc1,colNA="white")
plot(sc2,colNA="white")

sc1 <- sc1/1000
sc1 <- mask(sc1,rast_sc1$ndvi) #mask again
plot(sc1)
writeRaster(sc1,"DSV/Final/CurrentEM_final.tif",overwrite=T)

sc2 <- sc2/1000
sc2 <- mask(sc2,rast_sc1$ndvi) #mask again
plot(sc2)
writeRaster(sc2,"DSV/Final/FutureEM_final.tif",overwrite=T)
