## try to read in some outside weather data

## here are the resources I've been using:
## https://recology.info/2015/07/weather-data-with-rnoaa/
## https://ropensci.org/tutorials/rnoaa_tutorial/

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

## example from website
ncdc_stations(extent = c(47.5204, -122.2047, 47.6139, -122.1065),
  token = 'WZMuuiCHWZfoONOLdblOaDqeRQIBPguI')

## trying to find a location that is in Eugene, OR
ncdc_stations(extent = c(44.0500 - 0.03, -123.0830 - 0.03, 44.0500 + 0.03,
  -123.0830 + 0.03),
  token = 'WZMuuiCHWZfoONOLdblOaDqeRQIBPguI')

## some hourly precipitation for Chicago Midway
ncdc_datasets(stationid = 'COOP:111577', 
  token = 'WZMuuiCHWZfoONOLdblOaDqeRQIBPguI')

## no data found at this particular location but not sure why
ncdc(datasetid = 'PRECIP_HLY',
  ##locationid = 'FIPS:12017',
  stationid = 'COOP:111577',
  startdate = '2013-11-01', enddate = '2013-11-12',
  token = 'WZMuuiCHWZfoONOLdblOaDqeRQIBPguI')$data

cpc_prcp(date = "2017-01-15", us = TRUE)

ghcnd_search("AGE00147704", var = "PRCP",
  date_min = "2000-01-01")

## hourly precipitation data is just so spotty
ncdc(datasetid='PRECIP_HLY', locationid='ZIP:28801', datatypeid='HPCP',
  startdate = '2010-05-01', enddate = '2010-05-20',
  token = 'WZMuuiCHWZfoONOLdblOaDqeRQIBPguI')$data

## 15-minute interval data sets are also spotty
ncdc(datasetid='PRECIP_15', datatypeid='QPCP',
  startdate = '2010-05-01', enddate = '2010-05-02',
  token = 'WZMuuiCHWZfoONOLdblOaDqeRQIBPguI')


ncdc_datacats(datasetid=c("ANNUAL", "PRECIP_HLY"),
  token = 'WZMuuiCHWZfoONOLdblOaDqeRQIBPguI')

## precipitation Hourly and Precipitation-15 do not extend past January 1st, 2014
## for any location so that's a bummer.
max(ncdc_stations(datasetid='PRECIP_HLY', startdate='20130101', enddate='20161231',
  token = 'WZMuuiCHWZfoONOLdblOaDqeRQIBPguI')$data[ ,"maxdate"])

ncdc_stations(datasetid='GHCND', startdate='20130101', enddate='20161231',
  token = 'WZMuuiCHWZfoONOLdblOaDqeRQIBPguI')



## ditch NOAA, try package rwunderground

install.packages("rwunderground")
library(rwunderground)

## Erin: here is an API key i got from weather underground: a618a9283b58695a

## here's some data from weather underground. It has a ton of missing values though.
## Maybe some locations would be better than others?
## 
## I'm frustrated with this now so I'm stopping but, in summary, rnoaa is a bit
## of a dead end while weather underground *might have some potential

history_range(set_location(airport_code = "PDX"), "20140131", "20140201")$precip
