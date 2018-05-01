## try to read in some outside weather data

install.packages("rnoaa")
library(rnoaa)

?ncdc_stations

## API Code: WZMuuiCHWZfoONOLdblOaDqeRQIBPguI
## this code needs to be the "token" argument in the call....unclear
## yet if this has to be in every single function but I think it does


## this function looks like it finds the stations with a given station id?
## I think it's mostly used to get metadata about stations, after we know
## which stations we want so it might be useful once we are trying to merge 
## the noaa data with the data that we have.
ncdc_stations(datasetid = 'PRECIP_HLY',
  ##locationid = 'FIPS:12017',
  stationid = 'GHCND:USW00014895',
  startdate = '2013-11-11', enddate = '2013-11-12',
  token = 'WZMuuiCHWZfoONOLdblOaDqeRQIBPguI')

## this is the function that gets the data....it looks like
ncdc(datasetid = 'GHCND',
  ##locationid = 'FIPS:12017',
  stationid = 'GHCND:USW00014895',
  startdate = '2009-11-01', enddate = '2009-11-12',
  token = 'WZMuuiCHWZfoONOLdblOaDqeRQIBPguI')



ncdc(datasetid = 'GHCND', stationid = 'GHCND:USW00014895', startdate = '2013-10-01',
  enddate = '2013-12-01', 
  token = 'WZMuuiCHWZfoONOLdblOaDqeRQIBPguI')


## this is how we would search for stations based on latitude and longitude

ncdc_stations(extent = c(47.5204, -122.2047, 47.6139, -122.1065))