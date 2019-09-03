library(dplyr)
library(RPostgres)
library(jsonlite)
library(stringi)
weekdays_string_to_numbers <- 
  list("Montag" = 1,
       "Dienstag" = 2,
       "Mittwoch" = 3,
       "Donnerstag" = 4,
       "Freitag" = 5,
       "Samstag" = 6,
       "Sonntag" = 0)

type_table <-
  list("Fahren" = 1,
       "Abbiegen" = 2,
       "Einbiegen/Kreuzen" = 3,
       "Überschreiten" = 4,
       "ruhender Verkehr" = 5,
       "Längsverkehr" = 6,
       "sonstiges" = 7)
