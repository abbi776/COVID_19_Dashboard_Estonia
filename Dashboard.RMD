---
title: "COVID-19 dashboard for Estonia"
output: 
  flexdashboard::flex_dashboard:
    orientation: columns
    vertical_layout: fill
---

<style>                     
.navbar {
  background-color:orange;
  border-color:black;
}
.navbar-brand {
color:black!important;
}
</style>

```{r setup, include=FALSE}
library(flexdashboard)
library(tidyverse)
library(ggthemes) 
library(plotly)
library(sf)
library(leaflet)
library(gganimate)
library(tmap)
```

# Current Situation

Column {data-width=500}
-----------------------------------------------------------------------

### COVID-19 Daily cases (from start of disease to present)

```{r,fig.width=20, fig.height=20}

#downloading the dataset
url1 = "https://opendata.digilugu.ee/opendata_covid19_test_county_all.csv"
download.file(url1, destfile="1.csv")
covid19_test_county_all <- read_delim("D:/GEM Erasmus/1st Year-University of Tartu/Semester 1/Geospatial Analysis with Python and R (6 ECTS)/R_working_directory/Final project 1 jan 2021/1.csv")

#filtering only covid positive test results
covid19_test_county_all_P <- dplyr:: filter(covid19_test_county_all, ResultValue %in% c("P"))

#deleting the word maakond from county names
covid19_test_county_all_P <- covid19_test_county_all_P %>% 
  mutate(County = gsub(" maakond", "", County))

#filtering na values
covid19_test_county_all_P <- covid19_test_county_all_P %>% 
  filter(!is.na(County))

#ploting it
gg_cov_cases <- ggplot()+
  geom_line(data = covid19_test_county_all_P, aes(x = StatisticsDate, y = DailyCases), size = .25, color= "red")+
  facet_wrap(vars(County), scale = "free_y", ncol = 3)+
  labs(x = "Statistics Date", y= "Number of daily cases") 

plotly::ggplotly(gg_cov_cases)

```

Column {data-width=400}
-----------------------------------------------------------------------

### COVID-19 today cases in all counties

```{r, fig.width=10, fig.height=10}

library(rgdal)

#downloading county shapefile for estonia
download.file("https://geoportaal.maaamet.ee/docs/haldus_asustus/maakond_shp.zip", destfile="maakond_shp.zip")

unzip("maakond_shp.zip")

counties <- st_read("maakond_20220101.shp" , stringsAsFactors=FALSE, quiet=TRUE)

#filtering only covid cases for the latest date i.e today
covid19_test_county_all_latest <- covid19_test_county_all_P %>% 
  select(CountyEHAK, DailyCases, StatisticsDate) %>% 
  filter(StatisticsDate == max(StatisticsDate))

#simplyfing shapefile to smooth the display process
counties <- counties %>% 
  st_simplify(preserveTopology = TRUE, dTolerance = 200) %>% 
  st_cast("MULTIPOLYGON") 


#joining both datasets based on county codes
covid19_test_county_all_latest_sf <- left_join(counties, covid19_test_county_all_latest, by = c("MKOOD" = "CountyEHAK"))

#to display in leaflet, converting in crs 4326
covid19_test_county_all_latest_sf_4326 <- covid19_test_county_all_latest_sf %>% 
  st_transform(4326)

library(RColorBrewer)

#defining color palates
pal <- colorBin(palette = "YlOrRd", 
                domain = covid19_test_county_all_latest_sf_4326$DailyCases, n = 5) 

covid19_test_county_all_latest_sf_cntr <- covid19_test_county_all_latest_sf %>% 
  st_centroid()


covid19_test_county_all_latest_sf_cntr_4326 <- covid19_test_county_all_latest_sf_cntr %>% 
  st_transform(4326)


#ploting it
leaflet() %>% 
  addProviderTiles("Esri.WorldGrayCanvas",options = providerTileOptions(minZoom=7, maxZoom=10))%>% 
  addPolygons(data = covid19_test_county_all_latest_sf_4326, 
              label= ~DailyCases, 
              color = "gray",
              fillColor = ~pal(covid19_test_county_all_latest_sf_4326$DailyCases), 
              weight = 1.0, 
              opacity = 1.0, 
              fillOpacity = 0.8) %>%  
  addLabelOnlyMarkers(data = covid19_test_county_all_latest_sf_cntr_4326,
                      label = ~covid19_test_county_all_latest_sf_cntr_4326$DailyCases,
                      labelOptions = labelOptions(noHide = T))

```



# Past glimpse


Column {data-width=500}
-----------------------------------------------------------------------

### Heatmap of COVID-19 cases by Age Group

```{r,fig.width=20, fig.height=20}

#downloading data
url2 <- "https://opendata.digilugu.ee/opendata_covid19_test_results.csv"
download.file(url2, destfile = "2.csv")
test_results <- read_delim("2.csv")

#filtering only positive cases
positive_cases_only <- dplyr:: filter(test_results, ResultValue %in% c("P"))

#filtering na values
positive_cases_only_N <- positive_cases_only %>% 
  filter(!is.na(County))

library(lubridate)

#selecting data for last 90 days using lubricate library
positive_cases_90_days <- positive_cases_only_N %>%
  select(County, ResultValue, StatisticsDate, AgeGroup) %>%
  filter(StatisticsDate  >= today() - days(90))

positive_cases_90_days_1 <-positive_cases_90_days

#replacing the word p to 1 to perform the sum on values for each day
positive_cases_90_days_1$ResultValue[positive_cases_90_days$ResultValue == "P"] <- 1

#converting string into integer to perfom function
positive_cases_90_days_1$ResultValue <- as.integer(as.character(positive_cases_90_days_1$ResultValue))

#grouping data by age group and date of test and summarizing by sum 
positive_cases_90_days_1_sum <- positive_cases_90_days_1 %>% 
  group_by(AgeGroup,StatisticsDate) %>% 
  summarise(ResultValue = sum(ResultValue))

library(ggplot2)
library(hrbrthemes)
library(plotly)

#ploting it
p <- ggplot(positive_cases_90_days_1_sum, aes(StatisticsDate, AgeGroup, fill= ResultValue)) + 
  geom_tile()+
  scale_fill_gradient(low = "#353436",
                      high = "#f6f805",
                      guide = "colorbar")+
  labs(x = "Statistics Date", y= "Age Group", fill = "Number of covid cases")
ggplotly(p)

```

Column {data-width=500}
-----------------------------------------------------------------------

### COVID-19 last week cases in all counties

```{r, fig.width=10, fig.height=10}

library(rgdal)
library(lubridate)

#selecting data for last week covid cases
covid_lastweek <- covid19_test_county_all_P %>% 
  select(LastStatisticsDate, StatisticsDate, Country, CountryEHAK, County, CountyEHAK, ResultValue, DailyTests, TotalTests, DailyCases, TotalCases) %>% 
  filter(StatisticsDate  >= today() - days(7))

library(dplyr)

#grouping by county and summarizing it by sum
covid_lastweek_sum <- covid_lastweek %>% 
  group_by(CountyEHAK) %>% 
  summarise(DailyCases = sum(DailyCases))

#joining the dataset with shapefile
covid_lastweek_sum_counties_sf <- left_join(counties, covid_lastweek_sum, by = c("MKOOD" = "CountyEHAK"))

#converting to crs 4326 to plot in leaflet
covid_lastweek_sum_counties_sf_4326 <- covid_lastweek_sum_counties_sf %>% 
  st_transform(4326)

library(RColorBrewer)
pal1 <- colorBin(palette = "YlOrRd", 
                domain = covid_lastweek_sum_counties_sf_4326$DailyCases, n = 5) 

covid_lastweek_sum_counties_sf_cntr <- covid_lastweek_sum_counties_sf %>% 
  st_centroid()

covid_lastweek_sum_counties_sf_cntr_4326 <- covid_lastweek_sum_counties_sf_cntr %>% 
  st_transform(4326)

leaflet() %>% 
  addProviderTiles("Esri.WorldGrayCanvas",options = providerTileOptions(minZoom=7, maxZoom=10))%>% 
  addPolygons(data = covid_lastweek_sum_counties_sf_4326, 
              label= ~DailyCases,
              color = "gray", 
              fillColor = ~pal1(covid_lastweek_sum_counties_sf_4326$DailyCases), 
              weight = 1.0, 
              opacity = 1.0, 
              fillOpacity = 0.8) %>%  
  addLabelOnlyMarkers(data = covid_lastweek_sum_counties_sf_cntr_4326,
                      label = ~covid_lastweek_sum_counties_sf_cntr_4326$DailyCases,
                      labelOptions = labelOptions(noHide = T))

```

### COVID-19 second last week cases in all counties

```{r,  fig.width=10, fig.height=10}

#selecting data for secondlast week only
covid_secondlastweek <- covid19_test_county_all_P %>% 
  select(LastStatisticsDate, StatisticsDate, Country, CountryEHAK, County, CountyEHAK, ResultValue, DailyTests, TotalTests, DailyCases, TotalCases) %>% 
  filter(StatisticsDate  >= (today() - days(7) - days(7)))

#the above code give for 2 weeks so selecting first 105 entries that is for secondlast week
covid_secondlastweek_N <- head(covid_secondlastweek, 105)

#grouping by counties
covid_secondlastweek_sum <- covid_secondlastweek_N %>% 
  group_by(CountyEHAK) %>% 
  summarise(DailyCases = sum(DailyCases))

#joining data with shapefile
covid_secondlastweek_sum_counties_sf <- left_join(counties, covid_secondlastweek_sum, by = c("MKOOD" = "CountyEHAK"))

#converting to crs 4326 for displaying in leaflet
covid_secondlastweek_sum_counties_sf_4326 <- covid_secondlastweek_sum_counties_sf %>% 
  st_transform(4326)

library(RColorBrewer)
pal2 <- colorBin(palette = "YlOrRd", 
                domain = covid_secondlastweek_sum_counties_sf_4326$DailyCases, n = 5) 

covid_secondlastweek_sum_counties_sf_cntr <- covid_secondlastweek_sum_counties_sf %>% 
  st_centroid()


covid_secondlastweek_sum_counties_sf_cntr_4326 <- covid_secondlastweek_sum_counties_sf_cntr %>% 
  st_transform(4326)

#ploting it
leaflet() %>% 
  addProviderTiles("Esri.WorldGrayCanvas",options = providerTileOptions(minZoom=7, maxZoom=10))%>% 
  addPolygons(data = covid_secondlastweek_sum_counties_sf_4326, 
              label= ~DailyCases, #mouseover value
              color = "gray", # border color 
              fillColor = ~pal2(covid_secondlastweek_sum_counties_sf_4326$DailyCases), # polygons fill color
              weight = 1.0, # border lines thickness 
              opacity = 1.0, # border lines transparency
              fillOpacity = 0.8) %>%  # polygons fill transparency
  addLabelOnlyMarkers(data = covid_secondlastweek_sum_counties_sf_cntr_4326,
                      label = ~covid_secondlastweek_sum_counties_sf_cntr_4326$DailyCases,
                      labelOptions = labelOptions(noHide = T))

```

# COVID-19 Infected


Column {data-width=500}
-----------------------------------------------------------------------

### Number of infected persons per 100 thousand inhabitants in Estonia

```{r}

#downloading the data
url5 <- "https://opendata.digilugu.ee/opendata_covid19_tests_total.csv"
download.file(url5, destfile = "5.csv")
covid_1000 <- read_delim("5.csv")

#ploting it
gg_cov_per_1000 <- ggplot()+
  geom_line(data = covid_1000, aes(x= StatisticsDate, y = PerPopulation), size= .25, color="red")+
  labs( x = "Statistics Date", y= "Infected persons")

plotly::ggplotly(gg_cov_per_1000)

```

Column {data-width=500}
-----------------------------------------------------------------------

### Number of people hospitalized due to COVID-19

```{r}

#downloading the data
url6 <- "https://opendata.digilugu.ee/opendata_covid19_hospitalization_timeline.csv"
download.file(url6, destfile = "6.csv")
covid_hospital <- read_delim("6.csv")

#ploting it
gg_cov_hospital <- ggplot()+
  geom_line(data = covid_hospital, aes(x= StatisticsDate, y = Hospitalised), size= .25, color="red")+
  labs(x = "Statistics Date", y= "Number of patients")

plotly::ggplotly(gg_cov_hospital)

```



# Vaccine Status


Column {data-width=500}
-----------------------------------------------------------------------

### Percentage of population fully vaccinated at county level

```{r,fig.width=20, fig.height=20}

#downloading the data
url3 <- "https://opendata.digilugu.ee/covid19/vaccination/v2/opendata_covid19_vaccination_location_county.csv"
download.file(url3, destfile = "3.csv")
counties_vaccination <- read_delim("3.csv")
counties_vaccination <- read_delim("opendata_covid19_vaccination_location_county.csv")

#selecting the data of only fully vaccinated
counties_vaccination_fully <- dplyr:: filter(counties_vaccination, MeasurementType %in% c("FullyVaccinated"))

#joining the data with shapefile
counties_vaccination_fully <- counties_vaccination_fully %>% 
  mutate(LocationCounty = gsub(" maakond", "", LocationCounty))

#filtering na values
counties_vaccination_fully <- counties_vaccination_fully %>% 
  filter(!is.na(LocationCounty))

#ploting it
gg_vacc_percentage <- ggplot()+
  geom_line(data = counties_vaccination_fully, aes(x = StatisticsDate, y = PopulationCoverage), size = .25)+
  facet_wrap(vars(LocationCounty), scale = "free_y", ncol = 3)+
  labs( x = "Statistics Date", y= "Population coverage") 

plotly::ggplotly(gg_vacc_percentage)

```

Column {data-width=500}
-----------------------------------------------------------------------

### Vaccination level by population coverage in counties

```{r}

#selecting data of vaccine coverage for last week
vaccine_lastweek <- counties_vaccination_fully %>% 
  select(StatisticsDate, LocationCountry, LocationCountryEHAK, LocationCounty, LocationCountyEHAK, MeasurementType, LocationPopulation, DailyCount, TotalCount, PopulationCoverage) %>% 
  filter(StatisticsDate  >= today() - days(7))

#grouping data based on counties and sumamrzing it by sum
vaccine_lastweek_max <- vaccine_lastweek %>% 
  group_by(LocationCountyEHAK) %>% 
  summarise(PopulationCoverage = max(PopulationCoverage))

#joing the data
vaccine_lastweek_max_counties_sf <- left_join(counties, vaccine_lastweek_max, by = c("MKOOD" = "LocationCountyEHAK"))

#converting in crs 4326 to plot in leaflet
vaccine_lastweek_max_counties_sf_4326 <- vaccine_lastweek_max_counties_sf %>% 
  st_transform(4326)

library(RColorBrewer)
pal3 <- colorBin(palette = "YlOrRd", 
                domain = vaccine_lastweek_max_counties_sf$PopulationCoverage, n = 5) 

vaccine_lastweek_max_counties_sf_cntr <- vaccine_lastweek_max_counties_sf %>% 
  st_centroid()


vaccine_lastweek_max_counties_sf_cntr_4326 <- vaccine_lastweek_max_counties_sf_cntr %>% 
  st_transform(4326)

#ploting it
leaflet() %>% 
  addProviderTiles("Esri.WorldGrayCanvas",options = providerTileOptions(minZoom=7, maxZoom=10))%>%
  addPolygons(data = vaccine_lastweek_max_counties_sf_4326, 
              label= ~PopulationCoverage, #mouseover value
              color = "gray", # border color 
              fillColor = ~pal3(vaccine_lastweek_max_counties_sf_4326$PopulationCoverage), # polygons fill color
              weight = 1.0, # border lines thickness 
              opacity = 1.0, # border lines transparency
              fillOpacity = 0.8) %>%  # polygons fill transparency
  addLabelOnlyMarkers(data = vaccine_lastweek_max_counties_sf_cntr_4326,
                      label = ~vaccine_lastweek_max_counties_sf_cntr_4326$PopulationCoverage,
                      labelOptions = labelOptions(noHide = T))

```


# Vaccinated by Age group


Column {data-width=800}
-----------------------------------------------------------------------

### Fully vaccinated populaion by age group and county 

```{r}

#downloading the data
url4 <- "https://opendata.digilugu.ee/covid19/vaccination/v2/opendata_covid19_vaccination_location_county_agegroup_gender.csv"
download.file(url4, destfile = "4.csv")
vaccinated_men_women <- read_delim("4.csv")

#selecting data of only fully vaccinated
vaccinated_men_women_fully <- dplyr:: filter(vaccinated_men_women, MeasurementType %in% c("FullyVaccinated"))

#selecting data of last date i.e today
vaccinated_men_women_fully_latest <- vaccinated_men_women_fully %>% 
  select(AgeGroup, LocationCounty, PopulationCoverage, StatisticsDate, Gender) %>% 
  filter(StatisticsDate == max(StatisticsDate))

#joining data
vaccinated_men_women_fully_latest <- vaccinated_men_women_fully_latest %>% 
  mutate(LocationCounty = gsub(" maakond", "", LocationCounty))

#filtering na values
vaccinated_men_women_fully_latest <- vaccinated_men_women_fully_latest %>% 
  filter(!is.na(LocationCounty))

#filtering na values
vaccinated_men_women_fully_latest <- vaccinated_men_women_fully_latest %>% 
  filter(!is.na(AgeGroup))

#filtering na values
vaccinated_men_women_fully_latest <- vaccinated_men_women_fully_latest %>% 
  filter(!is.na(PopulationCoverage))

#ploting it
ggplot(vaccinated_men_women_fully_latest,aes(AgeGroup,PopulationCoverage))+
  geom_bar(stat="identity",position="dodge")+
  scale_x_discrete(guide = guide_axis(angle = 90))+
  facet_wrap(vars(LocationCounty), scale = "free_y")+
  labs(x = "Age Group", y= "Population coverage")


```


# Vaccinated Males


Column {data-width=1000}
-----------------------------------------------------------------------

### Fully vaccinated males by age group and county

```{r}

#selecting data of fully vaccinated males
vaccinated_men_fully <- dplyr:: filter(vaccinated_men_women_fully, Gender %in% c("Male"))

#selecting data of last date i.e today
vaccinated_men_fully_latest <- vaccinated_men_fully %>% 
  select(AgeGroup, LocationCounty, PopulationCoverage, StatisticsDate, Gender) %>% 
  filter(StatisticsDate == max(StatisticsDate))

#joining it
vaccinated_men_fully_latest <- vaccinated_men_fully_latest %>% 
  mutate(LocationCounty = gsub(" maakond", "", LocationCounty))

#filtering na values
vaccinated_men_fully_latest <- vaccinated_men_fully_latest %>% 
  filter(!is.na(LocationCounty))

#filtering na values
vaccinated_men_fully_latest <- vaccinated_men_fully_latest %>% 
  filter(!is.na(AgeGroup))

#filtering na values
vaccinated_men_fully_latest <- vaccinated_men_fully_latest %>% 
  filter(!is.na(PopulationCoverage))

#ploting it
ggplot(vaccinated_men_fully_latest,aes(AgeGroup,PopulationCoverage))+
  geom_bar(stat="identity",position="dodge", color="black", fill="lightblue")+
  scale_x_discrete(guide = guide_axis(angle = 90))+
  facet_wrap(vars(LocationCounty), scale = "free_y")+
  labs( x = "Age Group", y= "Population coverage")


```


# Vaccinated Females


Column {data-width=1000}
-----------------------------------------------------------------------

### Fully vaccinated females by age group and county

```{r}

#selecting data of fully vaccinated females
vaccinated_women_fully <- dplyr:: filter(vaccinated_men_women_fully, Gender %in% c("Female"))

#selecting data of last date i.e today
vaccinated_women_fully_latest <- vaccinated_women_fully %>% 
  select(AgeGroup, LocationCounty, PopulationCoverage, StatisticsDate, Gender) %>% 
  filter(StatisticsDate == max(StatisticsDate))

#joining data
vaccinated_women_fully_latest <- vaccinated_women_fully_latest %>% 
  mutate(LocationCounty = gsub(" maakond", "", LocationCounty))

#filtering na values
vaccinated_women_fully_latest <- vaccinated_women_fully_latest %>% 
  filter(!is.na(LocationCounty))

#filtering na values
vaccinated_women_fully_latest <- vaccinated_women_fully_latest %>% 
  filter(!is.na(AgeGroup))

#filtering na values
vaccinated_women_fully_latest <- vaccinated_women_fully_latest %>% 
  filter(!is.na(PopulationCoverage))

#ploting it
ggplot(vaccinated_women_fully_latest,aes(AgeGroup,PopulationCoverage))+
  geom_bar(stat="identity",position="dodge", color="black", fill="pink")+
  scale_x_discrete(guide = guide_axis(angle = 90))+
  facet_wrap(vars(LocationCounty), scale = "free_y")+
  labs(x = "Age Group", y= "Population coverage")


```



# About

Column {.tabset}
-----------------------------------------------------------------------

### Information

#### Author

- This dashboard is created by Abdullah Toqeer 

#### Dataset

- The dataset used in this dashboard is from Estonian health portal (https://www.terviseamet.ee/et/koroonaviirus/avaandmed)

```{r}


```
