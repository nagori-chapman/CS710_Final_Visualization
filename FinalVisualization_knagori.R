# https://www.nytimes.com/interactive/2021/world/covid-vaccinations-tracker.html

# Install Packages
list.of.packages <-c("readr","dplyr","tidyverse","tidyr","countrycode","ggplot2","hrbrthemes","gapminder","shadowtext","rworldmap","RColorBrewer","plotly","viridis")
new.packages<-list.of.packages[!(list.of.packages%in%installed.packages()[,"Package"])] 
if(length(new.packages)) install.packages(new.packages)

# Load required R packages
library (readr)
library (dplyr)
library(tidyverse)
library(tidyr)
library(countrycode)
library(ggplot2)
library(hrbrthemes)
library(gapminder)
library(shadowtext)
library(rworldmap)
library(RColorBrewer)
library(plotly)
library(viridis)

# Read in Covid Data
#urlfile="https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.csv"
#dat<-read.csv(url(urlfile))

# Read in Covid Data
# 215
urlfile="https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/latest/owid-covid-latest.csv"
dat<-read.csv(url(urlfile))
summary(dat)

#204
# remove rows with "OWID_" string
dat2<- dat[- grep("OWID_", dat$iso_code),]

#150
# removed ppl without vacc data
vac <- dat2 %>% filter(!is.na(people_vaccinated_per_hundred))

########################################################################################################
########################################################################################################
options(warn=-1)

#Download map data
world_map <- map_data("world")

#merging in isocode data to the worldmap file using the countrycode package
world_map$isocode <- countrycode(world_map[,"region"], origin = 'country.name', destination = 'iso3c')

#Merge map data with vaccine data
vac.map <- inner_join(vac, world_map, by = c("iso_code" = "isocode"))
#colnames(vac.map)[which(names(vac.map) == "total_vaccinations_per_hundred")] <- "Vaccine_doses_per_100_people"

# compare location and region and identify where they are different
vac.map$compare = ifelse(vac.map$location==vac.map$region,"Yes","No")
Diff <- subset(vac.map, compare=="No")

vac.map$region2 = ifelse(vac.map$location==vac.map$region, vac.map$location, vac.map$region)
vac.map$compare2 = ifelse(vac.map$region2==vac.map$region,"Yes","No")
Diff2 <- subset(vac.map, compare2=="No")

# remove duplicates from vac.map to get unique rows with merged world.map values
#149
vac.map_sh= vac.map %>% 
  # Base the removal on the "Age" column
  distinct(region2, .keep_all = TRUE)
################################################################################################
## VACCINE TYPE 
################################################################################################
urlfile3="https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/locations.csv"
vaccinetype<-read.csv(url(urlfile3))
vaccinetype = vaccinetype[,c("location","iso_code", "vaccines")]

# separate the "," delimited vaccine column into separate rows - so that the data changes from wide format to long format
vaccinetype2 = vaccinetype %>% mutate(vaccines = strsplit(as.character(vaccines), ",")) %>%  unnest(vaccines)
vaccinetype2$vaccines_factor <- factor(vaccinetype2$vaccines)
vaccinetype2$vaccines_factor2 <- factor(trimws(vaccinetype2$vaccines_factor, which=c("left")))

#Merge map data with vaccine type data
vac.type <- inner_join(vaccinetype2, world_map, by = c("iso_code" = "isocode"))



colorPalette <- brewer.pal(1, 'Set2')

#################### Moderna
Moderna <- subset(vac.type, vaccines_factor2=="Moderna")
Moderna$Given <- 1

malMap <- joinCountryData2Map(Moderna, joinCode = "ISO3", nameJoinColumn = "iso_code")

mapCountryData(malMap, nameColumnToPlot="Given", catMethod = "categorical", missingCountryCol = 'light grey', addLegend = FALSE,  mapTitle = "Moderna", colourPalette = colorPalette, borderCol = "white")

#################### Cansino

CanSino <- subset(vac.type, vaccines_factor2=="CanSino")
CanSino$Given <- 1

malMap <- joinCountryData2Map(CanSino, joinCode = "ISO3", nameJoinColumn = "iso_code")

mapCountryData(malMap, nameColumnToPlot="Given", catMethod = "categorical", missingCountryCol = 'light grey', addLegend = FALSE,  mapTitle = "CanSino", colourPalette = colorPalette, borderCol = "white")

#################### Covaxin

Covaxin <- subset(vac.type, vaccines_factor2=="Covaxin")
Covaxin$Given <- 1

malMap <- joinCountryData2Map(Covaxin, joinCode = "ISO3", nameJoinColumn = "iso_code")

mapCountryData(malMap, nameColumnToPlot="Given", catMethod = "categorical", missingCountryCol = 'light grey', addLegend = FALSE,  mapTitle = "Covaxin", colourPalette = colorPalette, borderCol = "white")

#################### EpiVacCorona

EpiVacCorona <- subset(vac.type, vaccines_factor2=="EpiVacCorona")
EpiVacCorona$Given <- 1

malMap <- joinCountryData2Map(EpiVacCorona, joinCode = "ISO3", nameJoinColumn = "iso_code")

mapCountryData(malMap, nameColumnToPlot="Given", catMethod = "categorical", missingCountryCol = 'light grey', addLegend = FALSE,  mapTitle = "EpiVacCorona", colourPalette = colorPalette, borderCol = "white")

#################### Johnson&Johnson

Johnson_Johnson <- subset(vac.type, vaccines_factor2=="Johnson&Johnson")
Johnson_Johnson$Given <- 1

malMap <- joinCountryData2Map(Johnson_Johnson, joinCode = "ISO3", nameJoinColumn = "iso_code")

mapCountryData(malMap, nameColumnToPlot="Given", catMethod = "categorical", missingCountryCol = 'light grey', addLegend = FALSE,  mapTitle = "Johnson&Johnson", colourPalette = colorPalette, borderCol = "white")

#################### Oxford/AstraZeneca

Oxford_AstraZeneca <- subset(vac.type, vaccines_factor2=="Oxford/AstraZeneca")
Oxford_AstraZeneca$Given <- 1

malMap <- joinCountryData2Map(Oxford_AstraZeneca, joinCode = "ISO3", nameJoinColumn = "iso_code")

mapCountryData(malMap, nameColumnToPlot="Given", catMethod = "categorical", missingCountryCol = 'light grey', addLegend = FALSE,  mapTitle = "Oxford/AstraZeneca", colourPalette = colorPalette, borderCol = "white")

#################### Pfizer/BioNTech

Pfizer_BioNTech <- subset(vac.type, vaccines_factor2=="Pfizer/BioNTech")
Pfizer_BioNTech$Given <- 1

malMap <- joinCountryData2Map(Pfizer_BioNTech, joinCode = "ISO3", nameJoinColumn = "iso_code")

mapCountryData(malMap, nameColumnToPlot="Given", catMethod = "categorical", missingCountryCol = 'light grey', addLegend = FALSE,  mapTitle = "Pfizer/BioNTech", colourPalette = colorPalette, borderCol = "white")

#################### Sinopharm/Beijing

Sinopharm_Beijing <- subset(vac.type, vaccines_factor2=="Sinopharm/Beijing")
Sinopharm_Beijing$Given <- 1

malMap <- joinCountryData2Map(Sinopharm_Beijing, joinCode = "ISO3", nameJoinColumn = "iso_code")

mapCountryData(malMap, nameColumnToPlot="Given", catMethod = "categorical", missingCountryCol = 'light grey', addLegend = FALSE,  mapTitle = "Sinopharm/Beijing", colourPalette = colorPalette, borderCol = "white")

#################### Sinopharm/Wuhan

Sinopharm_Wuhan <- subset(vac.type, vaccines_factor2=="Sinopharm/Wuhan")
Sinopharm_Wuhan$Given <- 1

malMap <- joinCountryData2Map(Sinopharm_Wuhan, joinCode = "ISO3", nameJoinColumn = "iso_code")

mapCountryData(malMap, nameColumnToPlot="Given", catMethod = "categorical", missingCountryCol = 'light grey', addLegend = FALSE,  mapTitle = "Sinopharm/Wuhan", colourPalette = colorPalette, borderCol = "white")

#################### Sinovac

Sinovac <- subset(vac.type, vaccines_factor2=="Sinovac")
Sinovac$Given <- 1

malMap <- joinCountryData2Map(Sinovac, joinCode = "ISO3", nameJoinColumn = "iso_code")

mapCountryData(malMap, nameColumnToPlot="Given", catMethod = "categorical", missingCountryCol = 'light grey', addLegend = FALSE,  mapTitle = "Sinovac", colourPalette = colorPalette, borderCol = "white")

#################### Sputnik V

Sputnik_V <- subset(vac.type, vaccines_factor2=="Sputnik V")
Sputnik_V$Given <- 1

malMap <- joinCountryData2Map(Sputnik_V, joinCode = "ISO3", nameJoinColumn = "iso_code")

mapCountryData(malMap, nameColumnToPlot="Given", catMethod = "categorical", missingCountryCol = 'light grey', addLegend = FALSE,  mapTitle = "Sputnik V", colourPalette = colorPalette, borderCol = "white")



#############################################
#GDP by continent and population
#############################################


gdp_vac <- dat2 %>% filter(!is.na(gdp_per_capita))
gdp_vac <- gdp_vac %>% filter(!is.na(population))

gdp_vac2 <- gdp_vac %>% mutate(gdp_quartile = ntile(gdp_per_capita, 4))
summary(gdp_vac2)
gdp_vac2 <- gdp_vac2 %>%
  mutate(gdp_quartile2 = case_when(gdp_quartile == 1 ~"Low",
                                   gdp_quartile == 2 ~ "Lower Middle" ,
                                   gdp_quartile == 3~ "Upper Middle",
                                   TRUE ~ "High"))
table(gdp_vac2$gdp_quartile)
table(gdp_vac2$gdp_quartile2)

#gdp_vac2$z <- with(aa, x + y - 2)

positions3 <- c("High", "Upper Middle", "Lower Middle", "Low")
positions4 <- c("Low", "Lower Middle", "Upper Middle", "High")


g<- ggplot(gdp_vac2, aes(y = total_vaccinations_per_hundred, x =gdp_quartile2))+ 
  geom_point(aes(color = continent, size = population_density), alpha = 0.5,position = position_dodge(width = 0.6)) +
  scale_color_manual(values = c("#E67E22","#58D68D" ,  "#2E86C1","#F4D03F","#F5B7B1" ,"#8E44AD"),name="Continents") +
  scale_size(range = c(3, 25),name="Population Density") +
  coord_flip()+ theme_ipsum(base_size = 14,axis_title_size = 18) + scale_x_discrete(limits=positions4) +labs(x= "GDP Quartiles", y= "Vaccination per Hundred People")
g
