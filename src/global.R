library(dplyr)

# 	# TODO to-be-automatized, best via a database; then delete raw data from repo
unfaelle = read.csv("../data/ms_unfaelle.csv")
locations = read.csv("../data/geolocator.result.csv", col.names = c("id", "latitude", "longitude"), header = FALSE)
accidents = merge(x = unfaelle, y = locations, by = "id", all.y = TRUE)
accidents = accidents[1:200,]

accidents <- accidents %>% 
	mutate(flucht = if_else(flucht == "", FALSE, TRUE)) %>% 
  mutate(sv = ifelse(is.na(sv), 0, sv)) %>% 
  mutate(lv = ifelse(is.na(lv), 0, lv)) %>% 
  mutate(t = ifelse(is.na(t), 0, t))

