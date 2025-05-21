#Set Working Directory 
#setwd("/cloud/project")

#Loaded libraries 
library(readxl)
library(psych)
library(ggplot2)
library(maps)
library(leaflet)
library(dplyr)
library(readr)
library(sf)
library(stringr)
library(lubridate)  
library(modelsummary)
library(car)

#Functions that counts hospitals by city
hospital_city_numbers <- function(data,subregion)
{
  data%>%
    group_by(!!sym(subregion)) %>%
    summarise(num_hospitals = n(), .groups = "drop")
}

NYHospitals <- read_excel("NYS_ED_loc-2.xlsx")
colnames(NYHospitals) <- str_trim(colnames(NYHospitals))
NYHospitals_2020 <- NYHospitals %>%
  filter(year == "2019")

city_numbers <- hospital_city_numbers(NYHospitals_2020, "subregion")

city_numbers$subregion <- gsub("^\\.", "", city_numbers$subregion)  # Remove leading period
city_numbers$subregion <- gsub("(?i) county", "", city_numbers$subregion)


AreaHealthResourceFiles <- read_excel("AHRF2021_Feb2025.xlsx") %>%
  filter(f00008 == "New York") 

CombinedData <- city_numbers %>%
  left_join(AreaHealthResourceFiles, by = c("subregion" = "f00010"))

NYRegressionData <- CombinedData %>% 
  select(HospitalCounty = subregion,
         NumberOfHospitals = num_hospitals,
         UnemploymentRate = f0679518,
         PIndividualsInPoverty = f1332119, #Percent of individuals in Poverty
         MedianHouseholdIncome2019 = f1322619,
         NumberOfIndividualsWithHealthInsuranceAges18To64in2019 = f1549619,
         Males18To64WithHealthInsurance2019 = f1550019,
         Females18To64WithHealthInsurance2019 = f1550419,
         NumIndividualsWithoutHealthInsurance18to64 = f1549719, #Number of Individuals Without Health Insurance Ages 18 To 64 in 2019
         MenWithoutHealthInsurance18to64 = f1550119, #Men 18 to 64 Without Health Insurance
         WomenWithoutHealthInsurance18to64= f1550519, #Women 18 to 64 Without Health Insurance
         CivilianLaborForceOver16In2019 = f0679219,
         NumberOfIndividualsEmployedOver16In2019 = f0679319,
         NumberOfIndividualsUnemployedOver16In2019 = f0679419,
         UnemploymentRateOver16In2019 = f0679519,
         PerCapitaPersonalIncomeIn2019 = f0978119,
         PopulationEstimateOver65 = f1408319,
         IndividualsLessThan65WithoutHealthInsurance = f1547319, #%Individuals Less Than 65 Without Health Insurance
         ) 

NYRegressionData <- NYRegressionData %>%
  mutate(
    UnemploymentRate = UnemploymentRate,
    PIndividualsInPoverty = PIndividualsInPoverty,
    PerCapitaPersonalIncomeIn2019 = PerCapitaPersonalIncomeIn2019 / 1000,  # $1,000s
    MedianHouseholdIncome2019 = MedianHouseholdIncome2019 / 1000,          # $1,000s
    NumberOfIndividualsUnemployedOver16In2019 = NumberOfIndividualsUnemployedOver16In2019 / 1000,  # thousands
    NumIndividualsWithoutHealthInsurance18to64 = NumIndividualsWithoutHealthInsurance18to64 / 1000,
    PopulationEstimateOver65 = PopulationEstimateOver65 / 1000
  )

NYRegressionData <- NYRegressionData %>%
  mutate(PIndividualsOver65WithoutHealthInsurance = 100 - IndividualsLessThan65WithoutHealthInsurance ) #%Individuals Older Than 65 Without HealthInsurance


Regression1 <- lm(NumberOfHospitals ~ UnemploymentRate, data = NYRegressionData)
Regression2 <- lm(NumberOfHospitals ~ UnemploymentRate + PIndividualsInPoverty, data = NYRegressionData)
Regression3 <-lm(NumberOfHospitals ~ UnemploymentRate + PIndividualsInPoverty + PerCapitaPersonalIncomeIn2019 , data = NYRegressionData)
Regression4 <-lm(NumberOfHospitals ~ UnemploymentRate + PIndividualsInPoverty + PerCapitaPersonalIncomeIn2019 + PIndividualsOver65WithoutHealthInsurance, data = NYRegressionData)
Regression5 <- lm(NumberOfHospitals ~ UnemploymentRate + PIndividualsInPoverty + PerCapitaPersonalIncomeIn2019 + PIndividualsOver65WithoutHealthInsurance + NumberOfIndividualsUnemployedOver16In2019 , data = NYRegressionData)
Regression6 <- lm(NumberOfHospitals ~ UnemploymentRate + PIndividualsInPoverty + PerCapitaPersonalIncomeIn2019 + PIndividualsOver65WithoutHealthInsurance + NumberOfIndividualsUnemployedOver16In2019 + MedianHouseholdIncome2019 , data = NYRegressionData)
Regression7 <- lm(NumberOfHospitals ~ UnemploymentRate + PIndividualsInPoverty + PerCapitaPersonalIncomeIn2019 + PIndividualsOver65WithoutHealthInsurance + NumberOfIndividualsUnemployedOver16In2019 + MedianHouseholdIncome2019 + NumIndividualsWithoutHealthInsurance18to64 , data = NYRegressionData)
Regression8 <- lm(NumberOfHospitals ~ UnemploymentRate + PIndividualsInPoverty + PerCapitaPersonalIncomeIn2019 + PIndividualsOver65WithoutHealthInsurance + NumberOfIndividualsUnemployedOver16In2019 + MedianHouseholdIncome2019 + NumIndividualsWithoutHealthInsurance18to64 + PopulationEstimateOver65, data = na.omit(NYRegressionData))


model_list <- list(
  "Model 1" = Regression1,
  "Model 2" = Regression2,
  "Model 3" = Regression3,
  "Model 4" = Regression4,
  "Model 5" = Regression5,
  "Model 6" = Regression6,
  "Model 7" = Regression7,
  "Model 8" = Regression8
)


modelsummary(
  model_list,
  title = "Regression Results: Number of Hospitals by Socioeconomic Factors",
  stars = TRUE
)


