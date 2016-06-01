# Name               : isoclim.R
# Type              : Program
# Object             : Construct a work environment.
#                     Cast programs for data manipulation.
#                     Destruct work environment.
# Input             : See Input section
# Output            : datasets and figures
# Author            : A. Soudant
# R version         : 2.10.1
# Creation date : Jan 2010
# Modified date : June 2015
#__________________________________________________________________Environment
# Project path (TO MODIFY)
.ROOT <- "/home/..."

# Data directory (TO MODIFY)
site <- "site name"  # To Modify depending on the study site


# Paths to the data directory (TO MODIFY)
.DATA <- paste(.ROOT,"data",sep="/")


#_____________________________________________________________________Input

# Selection of the time frame and year to study
 y <- 1997:2009

# settings for the size of the plot window in inches 
X11.options(width = 10*1, height = 1*8, xpos = 0, pointsize = 10)

setwd(.DATA)
# load the different raw isotope time series located in original.data.sets (TO MODIFFY)
iso1 <- read.table("Isotope1.txt",header=T)
iso2 <- read.table("Isotope2.txt",header=T)
iso3 <- read.table("Isotope3.txt",header=T)

# intra-annual sample resolution (microns)
sample.res <- 80

# Study site coordinates
latitude <- 61
longitude <- 24

# Flux tower resolution (typically 48 or 24)
FT.res <- 48

# Select which method to characterise the growing season
#1: default method using PAR and temperature thresholds
#2: alternative method using ET
#3: alternative method using NEE
GS.method <- 1

# tresholds to determine Onset and cessation dates with smoothing splines
#Based on PAR (in micromol.m-2.s-1)
treshold.Rad <- 435
# Based on air temperature (in degree Celcius)
treshold.T <- 10
# Attribute dates for onset and cessation of radial growth according to 2% and 99% of cumulative NEE values reached respectively
treshold.NEE.onset <- 2
treshold.NEE.cessation <- 99
# Attribute dates for onset and cessation of radial growth according to 10% and 95% of cumulative ET values reached respectively
treshold.ET.onset <- 10
treshold.ET.cessation <- 95

# Select which cell phase to study: 
#1: cell division (no cell life time) | 2: cell enlargement | 3: secondary wall thickening | 4: total growth
cellphase <- 4

# Select which feature to divide in 3 sections:
#1: tree-rings
#2: growing season
division <- 1

#_____________________________________________________________________Programs
isoclim <-
function()
{

# Create the plots for raw isotope series
plot.rawseries(iso1,iso2,iso3,sample.res)

#Create the standardised istope time series
interpolation.tech(y,iso1,iso2,iso3)
# type "isotope.data" to see the standardised istope time series and the mean signal

# Atmosptheric corrections for isotope time series 1991 - 2010
atm.correction()
# type "atm.cor" to see the atmospheric correction values

# Create the dataset for the atmospheric corrected isotope time series
corrected.iso(y,isotope.data,atm.cor)
# type "EPS" to see the EPS value
# type "mean.isotope" to see the mean isotope time series
# type "cor.iso" to see the corrected isotope time series

# Create the plots for standardised and the atmospheric corrected  isotope series
plot.standardseries(y,isotope.data,mean.isotope,cor.iso,EPS,sample.res)

# Calculates the potential downwelling shortwave radiation at the surface
# Necessary for growing season calculation
#rad.pot(y,latitude,longitude,FT.res)

# Calculate the growing season dates based on PAR and temperature treshold OR Evapotranspiration OR NEE
gs.calc(y,GS.method,cor.iso,treshold.Rad,treshold.T,treshold.NEE.onset,treshold.NEE.cessation,treshold.ET.onset,treshold.ET.cessation,FT.res)
# type "data.gs" to see onset and cessation dates selected

# Gompertz fitting to obtain intra-annual time resolved stable isotope series
#Gompertz.calc(y,cor.iso,data.gs)
gompertz.calc.example()
# type "gompertz.parameters" to see final parameters beta and keta for the Gompertz curve
# type "final.isotope" to see the isotope time resolved high resolution isotope series

# Set the cell life time for cell formation,enlargement, secondary wall thicknening and total growth
cell.lifetime()

# Perform the matching procedure between the isotope dates and the weather variables year by year
match.climate(y,cellphase,final.isotope,increment.division,increment.enlargement,increment.wallthick,increment.total,FT.res)

# Create the intra-annual dataset containing the matched isotopes - climate data 
intraannual.data(y,final.isotope)
# type "climate.iso" to see the intra-annually matched isotopes - climate data 

# Create the annual dataset object containing the matched isotopes - climate data 
annual.data(y,climate.iso)
# type "annual.dataset" to see annually matched isotopes - climate data 

# Divide the tree-ring or the growing season into three sections for climate analysis
feat.division(y,division,climate.iso)
# type "data.early", "data.mid" or "data.late" to see the splitted dataset


#_____________________________________________________________________ Plotting
#Plotting tools for linear regression between isotope and climate (Temperature, PAR, Precipitation)

# simple linear correlations month by month at the inter-annual resolution  
plot.correlation(annual.dataset,climate.iso) 

# Produce graphics for climate correlation with isotopes at the inter-annual scale
plot.interannual(y,annual.dataset)

# Produce graphics for climate correlation with isotopes at the intra-annual scale
plot.intraannual(y,climate.iso,data.early,data.mid,data.late)
# type lr.prop to see linear regressions properties for each combination of sections and climate variables


}
