##### load libraries 
library (raster)
library (rgdal)
library (dismo)


##### download climatic variables for the last glacial maximum (LGM) and for the present from worldclim.org
## ** = write the directory of the variables

setwd ("**")
LGM_CCSM<- stack (raster ("bio1.bil"), raster ("bio2.bil"),raster ("bio3.bil"),
                  raster ("bio4.bil"),raster ("bio5.bil"),raster ("bio6.bil"),
                  raster ("bio7.bil"),raster ("bio8.bil"),raster ("bio9.bil"),
                  raster ("bio10.bil"),raster ("bio11.bil"),raster ("bio12.bil"),
                  raster ("bio13.bil"),raster ("bio14.bil"),raster ("bio15.bil"),
                  raster ("bio16.bil"),raster ("bio17.bil"),raster ("bio18.bil"),
                  raster ("bio19.bil"), overwrite=TRUE)

setwd ("**")
LGM_MIROC<- stack (raster ("bio1.bil"), raster ("bio2.bil"),raster ("bio3.bil"),
                        raster ("bio4.bil"),raster ("bio5.bil"),raster ("bio6.bil"),
                        raster ("bio7.bil"),raster ("bio8.bil"),raster ("bio9.bil"),
                        raster ("bio10.bil"),raster ("bio11.bil"),raster ("bio12.bil"),
                        raster ("bio13.bil"),raster ("bio14.bil"),raster ("bio15.bil"),
                        raster ("bio16.bil"),raster ("bio17.bil"),raster ("bio18.bil"),
                        raster ("bio19.bil"), overwrite=TRUE)

setwd ("**")
WC<- stack (raster ("bio1.bil"), raster ("bio2.bil"),raster ("bio3.bil"),
                           raster ("bio4.bil"),raster ("bio5.bil"),raster ("bio6.bil"),
                           raster ("bio7.bil"),raster ("bio8.bil"),raster ("bio9.bil"),
                           raster ("bio10.bil"),raster ("bio11.bil"),raster ("bio12.bil"),
                           raster ("bio13.bil"),raster ("bio14.bil"),raster ("bio15.bil"),
                           raster ("bio16.bil"),raster ("bio17.bil"),raster ("bio18.bil"),
                           raster ("bio19.bil"), overwrite=TRUE)

##### load the points of the species
bucanetes<- read.table ("bucanetes.csv", sep=",", header=F)

##### run the model

# witholding a 20% sample for testing
fold <- kfold(bucanetes, k=5)
occtest <- bucanetes[fold == 1, ]
occtrain <- bucanetes[fold != 1, ]

# select 1000 background points
bg <- randomPoints(WC, 1000)

# run the model using the training sample
model_bucanetes <- maxent(WC, occtrain)

#evaluate model results 
model_evaluation <- evaluate(model_bucanetes, p=occtest, a=bg, x=WC)
model_evaluation

# training AUC
model_bucanetes@results [5] 

# plot variable contribution to the model
plot(model_bucanetes)
# response curves
response(model_bucanetes)

# project the model in the present interglacial scenario
present <- predict (model_bucanetes, WC, progress="window")
plot (present, main="Interglacial")

# project the model in the Last Glacial Maximum
CCSM <- predict(model_bucanetes, LGM_CCSM, progress="window")
MIROC <- predict(model_bucanetes, LGM_MIROC, progress="window")

# multiply both predictions to contruct a consensus map
LGM<- CCSM*MIROC

# plot the maps
e <- extent(-20, 80, 10, 60)
map_LGM<- crop (LGM, e)
map_WC<- crop (present, e)
plot (map_LGM, main="Glacial")
plot (map_WC, main="Interglacial")

### contact: Sara Varela, email: svarela@paleobiogeography.org





