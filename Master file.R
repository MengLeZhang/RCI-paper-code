##################################################################
##  Master file: This is the masterfile outlining the steps   ####
##  in the RCI analysis                                       ####
##################################################################
##  Start: 3/2/2017

##  The R packages required and user functions are in this file
source('RCI functions.R')


##  Step one: Dataset management of Census and DWP data-----
##  Prior to the analysis we have to join the data from the census to DWP data. We also have to properly create a look-up table for output areas to their 2001 LSOAs. This is because we have to get working pop. info to LSOA level.

##  These are the files required to create the merged England and Wales LSOA file; to convert population in 2011 to their 2001 LSOAs; and to load in the custom-function needed

source('Census 2001 OA to LSOA.R') #Creates lookup table OA to LSOA 2001
source('Creation of the England and Wales data.R') ## First .shp file creation
source('LSOA 2001 to 2011 conversion by area ratio.R') ## Merging the 2001 data is easy but we need to adapt the 2011 census statistics. First we work out the area of OAs covered by each 2001 LSOA
source('LSOA 2001 and 2011 conversion.R') ## This is the final step that we needed to get the raw data for pop, work pop and DWP data together.

##  Step two: Bayesian inference. We need to get the predicted outcomes from the Bayesian models for the 3 DWP stats and save them. This is because they are used for all calculation of RCI so we are better off saving time this way
source('Running and saving bayes model LA.R') #Runs the models for the LA
source('Running and saving bayes model TTWA.R') #Runs the models for the TTWAs (which now include the greater london area)

##  Step three: Calculating the RCI. We do this by boundaries--LAs and TTWA.
source('RCI analysis LA.R') #This is self explanatory; cover standard RCI and by accessibility


##  Step four: Sensivity analysis






### These are the analysis files: