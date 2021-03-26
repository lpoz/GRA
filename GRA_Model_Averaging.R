########################################
## Granger-Ramanathan Model Averaging ##
########################################

# Source: Uta Stockman
# Updated by: Liana Pozza
# Date: 26/03/21

# Code used for:  
# Pozza, L.E., Bishop, T.F.A., Stockmann, U. and Birch, G.F., 2020. 
#   Integration of vis-NIR and pXRF spectroscopy for rapid measurement of 
#     soil lead concentrations. Soil Research, 58(3), pp.247-257.

# GRA theory: 
# Granger CWJ, Ramanathan R (1984). Improved methods of combining forecasts.
#   Journal of Forecasting 3(2), 197-204. doi:10.1002/for.3980030207


# In this code averaging with XRF Compton-normalised values with Cubist NIR

rm(list=ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

load("goof2.RData") # Goodness of fit statistics


##### Read in data #####

Model_aver_c = read.csv("calib.csv")
Model_aver_v = read.csv("valid.csv")


##### Granger-Ramanathan Averaging #####

### Calibration ###

soil_c <- (Model_aver_c$ref_Pb)

# linear regression model
mod.1.c <- lm(Model_aver_c$ref_Pb ~ NIR_bag_mean_Pb + ComptonPb, data=Model_aver_c)

# Call up mod.1.v results to obtain the coefficients:
mod.1.c
weight<-mod.1.c$coefficients
wo<-weight[1]
w_nir<-weight[2]
w_xrf<-weight[3]
# (intercept), NIR_bag_mean_Pb, XRF_bag_mean_rawPb
# These are Wo, W_NIR and W_xrf, respectively
# Note W_NIR and W_XRF do not sum to 1.

# Model averaged predicted Y;
# Y = Wo + (W_nir.X_nir)+(W_xrf.X_xrf)
Y_cal= wo + (w_nir*(Model_aver_c$NIR_bag_mean_Pb)) + (w_xrf*(Model_aver_c$ComptonPb))
#write.csv(cbind(Model_aver_c$ref_Pb, Y_cal), file=export_calib)

isrow <- complete.cases(Y_cal) # find which row has complete record (no NA values)
soil_c <- soil_c[isrow] #specify type of spectra to use for prediction
Y_cal <- Y_cal[isrow] # this represents the soil variable without missing values

gfc.mod.1._predict <- goof2(soil_c, Y_cal) 


### Validation ###

#Model
mod.1.v <- lm(Model_aver_v$ref_Pb ~ NIR_bag_mean_Pb+ComptonPb, data=Model_aver_v)
mod.1.v
weight<-mod.1.v$coefficients
wo_v<-weight[1]
w_nir_v<-weight[2]
w_xrf_v<-weight[3]

Y_val= wo_v + (w_nir_v*(Model_aver_v$NIR_bag_mean_Pb)) + (w_xrf_v *(Model_aver_v$ComptonPb))
#write.csv(cbind(Model_aver_v$ref_Pb, Y_val), file=export_valid)

gfv.mod.1._predict <- goof2(Model_aver_v$ref_Pb, Y_val)

gfc.mod.1._predict
gfv.mod.1._predict
