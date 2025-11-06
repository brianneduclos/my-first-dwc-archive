# load the dataset
dipnet = read.csv(file.choose(), header = T)
head(dipnet)

library(tidyverse)
library(uuid)
library("devtools")
devtools::install_github("iobis/obistools")
library(obistools)

# map column headings to Darwin Core terms
# batch rename following rename help page example:
# https://dplyr.tidyverse.org/reference/rename.html
dipnet = as_tibble(dipnet)
lookup = c(fieldNumber = "plotID", decimalLatitude = "Latitude", decimalLongitude = "Longitude",
            eventDate = "sample.date", individualCount = "abundance", measurementValue = "Dry.weight.g")
dipnet2 = rename(dipnet, all_of(lookup))
head(dipnet2)

# site and treatment wrangling to populate event table properly
# parent event is the site and date (SA# or SJ#)
# event is the treatment at the site and date (UU, UG, SU, SG)
splitPlots = dipnet2 %>%
  separate(fieldNumber, into = c("site", "treatment"), sep = 3, remove = FALSE)
# count number of parent evetns
parentEvents <- splitPlots %>%
  distinct(site, eventDate)
# 29 parent events
# count sampling events by treatment, site and date
# (these will be duplicated for each combo of site/date for now)
eventsToID <- splitPlots %>%
  distinct(fieldNumber, eventDate)
# 67 sampling events

# add eventID column to dipnet2
# use string_concatenate to combine site and date to make eventIDs
# add occurrenceStatus, occurrenceID, kingdom, and basisOfRecord for occurrence Core table
# add geodeticDatum and countryCode
# add day, month, and year for event Core table
# measurement: type, typeID (P01), unit, unitID (P06)
dipnet2events <- splitPlots %>%
  mutate(parentEventID = str_c(site, "-", eventDate)) %>%
  mutate(eventID = str_c(fieldNumber, "-", eventDate)) %>%
  mutate(occurrenceStatus = "PRESENT") %>%
  mutate(occurrenceID = UUIDgenerate(n=696)) %>%
  mutate(kingdom = "Animalia") %>%
  mutate(basisOfRecord = "MaterialSample") %>%
  mutate(geodeticDatum = "WGS84") %>%
  mutate(countryCode = "US") %>%
  separate(eventDate, into = c("year", "month", "day"), sep = "-", remove = FALSE) %>%
  mutate(measurementType = "Dry weight biomass (in assayed sample) of biological entity specified elsewhere") %>%
  mutate(measurementTypeID = "https://vocab.nerc.ac.uk/collection/P01/current/ODRYBM01/") %>%
  mutate(measurementUnit = "Grams") %>%
  mutate(measurementUnitID = "http://vocab.nerc.ac.uk/collection/P06/current/UGRM/") 
n_distinct(dipnet2events$parentEventID)
n_distinct(dipnet2events$eventID)
# 67 events, 29 parent events!!! I did it!

# to consider: coordinateUncertaintyinMeters, depth, habitat, locality

# QAQC with obistools: https://iobis.github.io/obistools/
# could also use obistools at the beginning for mapping terms 
check_fields(dipnet2events) 
# all there! empty tibble
plot_map(dipnet2events, zoom = TRUE)
# made a nice map!


# now for publishing: make the tables
# event Core, occurrence Core, and measurement or Fact
# event is distinct to eliminate all the duplicates (should have 67 rows)
event = dipnet2events %>%
  distinct(parentEventID, eventID, fieldNumber, eventDate, year, month, day, 
           decimalLatitude, decimalLongitude, geodeticDatum, countryCode)
occurrence = dipnet2events %>%
  select(eventID, occurrenceID, basisOfRecord, occurrenceStatus, kingdom, scientificName, scientificNameID, 
         acceptedNameUsageID, individualCount)
eMOF = dipnet2events %>%
  select(eventID, occurrenceID, measurementType, measurementTypeID, measurementValue,
         measurementUnit, measurementUnitID)

# need to make metadata!

