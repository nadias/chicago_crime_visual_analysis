library(tidyverse)
library(lubridate)
library(here)
library(rgdal)
library(rgeos)
library(sf)
library(zoo)


##### LOAD CRIMES DATA #####
df <- rbind(read_csv("data/Chicago_Crimes_2001_to_2004.csv"), 
            read_csv("data/Chicago_Crimes_2005_to_2007.csv"), 
            read_csv("data/Chicago_Crimes_2008_to_2011.csv"), 
            read_csv("data/Chicago_Crimes_2012_to_2017.csv"))
df$X1 <- NULL

df <- df %>% filter(!Year %in% c(2001,2017)) %>% distinct()

df <- df[complete.cases(df),] # 7941285 - 7145214 = 796071 rows with missing values

Sys.setlocale("LC_TIME", "English")
# Convert Date from factor to date
df$Date <- mdy_hms(df$Date)
# Extract hour from Date
df$Hour <- substring(df$Date, 12,13)
# Drop time from Date
df$Date <- as.Date(df$Date, format="%m/%d/%Y")
df$Month <- as.factor(months(df$Date))
df$Weekday <- as.factor(weekdays(df$Date))
df$MonthYear <- as.yearmon(paste(substr(df$Month, 1, 3), df$Year), "%b %Y")

df$Weekday <- factor(df$Weekday, levels= c("Sunday", "Monday", 
                                           "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

df$Month <- factor(df$Month, levels= c("January", "February", 
                                       "March", "April", "May", "June", "July",
                                       "August","September","October","November","December"))

df <- df %>% mutate(Arrest = ifelse(Arrest == T, "Yes", "No"))
df <- df %>% mutate(Domestic = ifelse(Domestic == T, "Yes", "No"))


##### LOAD SPATIAL DATA #####
# Needs to be set to the right directory. Each parameter is a folder in the path.
community_areas <- here("data", "Boundaries - Community Areas (current)",
                        "geo_export_0f728e16-26c6-4453-b3ac-456d0b4f48db.shp") %>%
                    st_read() %>%
                    # convert community names to title case
                    mutate(community = str_to_title(community))

##### LOAD SOCIO-INDICATOR DATA ####
indicators <- read_csv("data/Census_Data_-_Selected_socioeconomic_indicators_in_Chicago__2008___2012.csv") %>% na.omit()
indicators$`Community Area Number` <- as.factor(indicators$`Community Area Number`)

df <- df %>% inner_join(indicators[,1:2], by = c("Community Area" = "Community Area Number"))

##### LOAD LANGUAGE DATA ####
languages <- read_csv("data/Census_Data_-_Languages_spoken_in_Chicago__2008___2012.csv") %>% 
                select(`Community Area`, `Community Area Name`, `SPANISH`)
languages <- languages[-which(languages$`Community Area` == 0),]
languages$`Community Area` <- as.factor(languages$`Community Area`)


##### LOAD CRIME OCCURENCES BY COMMUNITY AREA #####
# TODO - CHANGE TO PER CAPITA
ca_primary.type <- df %>% 
                      group_by(`Community Area`, `Primary Type`, `Year`) %>% 
                      na.omit() %>% 
                      summarize(count = n()) %>% 
                      spread(`Primary Type`, count) %>% 
                      ungroup()
ca_primary.type$`Community Area` <- as.factor(ca_primary.type$`Community Area`)


ca_arrest <- df %>% filter(Arrest == "Yes") %>%
  group_by(`Community Area`, `Arrest`, `Year`) %>% 
  na.omit() %>% 
  summarize(count = n()) %>% 
  spread(`Arrest`, count) %>% 
  ungroup() %>% rename(Arrest = Yes)
ca_arrest$`Community Area` <- as.factor(ca_arrest$`Community Area`)

ca_domestic <- df %>% 
  group_by(`Community Area`, `Domestic`, `Year`) %>% 
  filter(Domestic == "Yes") %>%
  na.omit() %>% 
  summarize(count = n()) %>% 
  spread(`Domestic`, count) %>% 
  ungroup() %>% rename(Domestic = Yes)
ca_domestic$`Community Area` <- as.factor(ca_domestic$`Community Area`)

##### LOAD POPULATION DATA ####
pop <- read.table('data/population.txt', header = T, sep = ";", dec = ".",quote = "\"")[,1:3]
pop[,3] <- pop[,3]*1000
pop$Num <- as.factor(pop$Num)


##### COMBINE DATA ####
community_areas <- community_areas %>% 
                    inner_join(indicators, by = c("area_num_1" = "Community Area Number")) %>%
                    inner_join(languages, by = c("area_num_1" = "Community Area")) %>%
                    inner_join(pop, by = c("area_num_1" = "Num")) %>%
                    inner_join(ca_primary.type, by = c("area_num_1" = "Community Area")) %>%
                    inner_join(ca_arrest, by = c("area_num_1" = "Community Area", "Year" = "Year")) %>%
                    inner_join(ca_domestic, by = c("area_num_1" = "Community Area", "Year" = "Year")) %>%
                    select(c(2,8:16,19,21:59))

community_areas[is.na(community_areas)] <- 0


##### FEATURE ENGINEERING #####
community_areas <- community_areas %>% 
  mutate(lat.centroid = st_coordinates(st_centroid(st_transform(community_areas, "+proj=longlat +datum=WGS84")$geometry))[,1]) %>% 
  mutate(lon.centroid = st_coordinates(st_centroid(st_transform(community_areas, "+proj=longlat +datum=WGS84")$geometry))[,2]) %>%
  mutate(`PER CAPITA HISPANICS` = SPANISH / X2010) %>%
  mutate(`POPULATION DENSITY` = (X2010/shape_area)*1000) %>%
  mutate(ALL = rowSums(as.data.frame(community_areas)[,14:47])) %>%
  mutate(`MOST COMMON PRIMARY TYPE` = colnames(as.data.frame(community_areas)[,c(14:47)])[max.col(as.data.frame(community_areas)[,c(14:47)])])



