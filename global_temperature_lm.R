library(tidyverse)
global_temperature <- read.csv('Dataset/GlobalTemperatures.csv')
global_temperature$year <- substring(global_temperature$dt,1,4)
global_temperature$year <- as.numeric(global_temperature$year)
global_LandAndOcean_temp <- global_temperature %>% 
  filter(year >=1850) %>% group_by(year) %>% summarize(global_LandAndOcean_temp = mean(LandAndOceanAverageTemperature)) 

CO2_global <- read.csv('Dataset/CO2_global.csv')
CH4_global <- read.csv('Dataset/CH4_global.csv')
Fgas_global <- read.csv('Dataset/F-gas_global.csv')
N20_global <- read.csv('Dataset/N20_global.csv')


global_temp_lm_dataset <- inner_join(global_LandAndOcean_temp,CO2_global,by = c('year'='Year')) 
global_temp_lm_dataset <- inner_join(global_temp_lm_dataset,CH4_global, by = c('year'='Year'))
global_temp_lm_dataset <- inner_join(global_temp_lm_dataset,Fgas_global, by = c('year'='Year'))
global_temp_lm_dataset <- inner_join(global_temp_lm_dataset,N20_global, by = c('year'='Year'))

glimpse(global_temp_lm_dataset)

write.csv(global_temp_lm_dataset, "m1.csv")


m1 <- lm(global_LandAndOcean_temp ~ C02_Energy + CO2_Industrial_Processes_and_Product_Use + CO2_Agriculture + CO2_Waste +
           CH4_Energy + CH4_Industrial_Processes_and_Product_Use + CH4_Agriculture + CH4_Waste + 
           Fgas + N20_Energy + N20_Industrial_Processes_and_Product_Use + N20_Agriculture + N20_Waste, data = global_temp_lm_dataset)
summary(m1)

#==============================
library(MASS)
step_m1 <- stepAIC(m1, direction = 'both', trace = FALSE)
summary(step_m1)
# we were able to reduce the parameter without losing precise level (adj-R2)
#==============================
# try cross-validation
# R2, RMSE and MAE are used to measure the regression model performance during cross-validation.
library(caret)
train.control <- trainControl(method = "cv", number = 10)

# Linear least squares 
model_lm <- train(global_LandAndOcean_temp ~ 
                    C02_Energy + CO2_Industrial_Processes_and_Product_Use + CO2_Agriculture + CO2_Waste +
                    CH4_Industrial_Processes_and_Product_Use + CH4_Agriculture + CH4_Waste + 
                    Fgas + N20_Energy + N20_Agriculture, 
                    data = global_temp_lm_dataset, method = "lm", trControl = train.control
                  )
# Summarize the results
print(model_lm)

# Random Forest
model_rf <- train(global_LandAndOcean_temp ~ 
                    C02_Energy + CO2_Industrial_Processes_and_Product_Use + CO2_Agriculture + CO2_Waste +
                    CH4_Industrial_Processes_and_Product_Use + CH4_Agriculture + CH4_Waste + 
                    Fgas + N20_Energy + N20_Agriculture, 
                    data = global_temp_lm_dataset, method = "rf", trControl = train.control
                  )
print(model_rf)

model_rf_full <- train(global_LandAndOcean_temp ~ C02_Energy + CO2_Industrial_Processes_and_Product_Use + CO2_Agriculture + CO2_Waste +
                         CH4_Energy + CH4_Industrial_Processes_and_Product_Use + CH4_Agriculture + CH4_Waste + 
                         Fgas + N20_Energy + N20_Industrial_Processes_and_Product_Use + N20_Agriculture + N20_Waste, 
                  data = global_temp_lm_dataset, method = "rf", trControl = train.control
)
print(model_rf_full)

library(randomForest)
m1_rf_result = randomForest(global_LandAndOcean_temp ~ C02_Energy + CO2_Industrial_Processes_and_Product_Use + CO2_Agriculture + CO2_Waste +
                              CH4_Energy + CH4_Industrial_Processes_and_Product_Use + CH4_Agriculture + CH4_Waste + 
                              Fgas + N20_Energy + N20_Industrial_Processes_and_Product_Use + N20_Agriculture + N20_Waste, 
               data = global_temp_lm_dataset, mtry = 2)

summary(m1_rf_result)

m1_rf_result$importance
m1_rf_result$localImportance

varImpPlot(m1_rf_result)



# Generalized linear model 
model_glm <- train(global_LandAndOcean_temp ~ 
                     C02_Energy + CO2_Industrial_Processes_and_Product_Use + CO2_Agriculture + CO2_Waste +
                     CH4_Industrial_Processes_and_Product_Use + CH4_Agriculture + CH4_Waste + 
                     Fgas + N20_Energy + N20_Agriculture, 
                     data = global_temp_lm_dataset, method = "glm", trControl = train.control
                   )
print(model_glm)

# Boosted regression models
model_glmboost <- train(global_LandAndOcean_temp ~ 
                          C02_Energy + CO2_Industrial_Processes_and_Product_Use + CO2_Agriculture + CO2_Waste +
                          CH4_Industrial_Processes_and_Product_Use + CH4_Agriculture + CH4_Waste + 
                          Fgas + N20_Energy + N20_Agriculture, 
                          data = global_temp_lm_dataset, method = "glmboost", trControl = train.control
                        )

print(model_glmboost)
# 
model_glmStepAIC <- train(global_LandAndOcean_temp ~ C02_Energy + CO2_Industrial_Processes_and_Product_Use + CO2_Agriculture + CO2_Waste +
                          CH4_Energy + CH4_Industrial_Processes_and_Product_Use + CH4_Agriculture + CH4_Waste + 
                          Fgas + N20_Energy + N20_Industrial_Processes_and_Product_Use + N20_Agriculture + N20_Waste, 
                        data = global_temp_lm_dataset, method = "glmStepAIC", trControl = train.control)
print(model_glmStepAIC)

#==============================

#All part above is the linear regression model for global temperature and greenhouse gases by sectors
#The model is full model without predictors selection, please use AIC or BIC to do a stepwise selection. 

USA_population <- read.csv('Dataset/USA_population_1950-2016.csv')
China_population <- read.csv('Dataset/China_population_1950-2016.csv')
China_population$year <- substring(China_population$year,7,10)
China_population$year <- as.integer(China_population$year)

global_temperature_country <- read.csv('Dataset/GlobalLandTemperaturesByCountry.csv')
global_temperature_country$year <- substring(global_temperature_country$dt,1,4)
global_temperature_country$year <- as.numeric(global_temperature_country$year)
USA_CHINA_TEMP <- global_temperature_country %>% 
  filter(year >= 1949) %>%
  filter(Country %in% c('China','United States')) %>% 
  group_by(Country,year) %>% 
  summarize(avg_temp = mean(AverageTemperature))

China_USA_CH4 <- read.csv('Dataset/China_USA_CH4.csv')
China_USA_CO2 <- read.csv('Dataset/China_USA_CO2.csv')
China_USA_Fgas <- read.csv('Dataset/China_USA_Fgas.csv')
China_USA_N20 <- read.csv('Dataset/China_USA_N20.csv')

USA_CHINA_TEMP$DIFF <- (USA_CHINA_TEMP$avg_temp - lag(USA_CHINA_TEMP$avg_temp,1))*100/(USA_CHINA_TEMP$avg_temp)
USA_CHINA_temp_lm_dataset <- USA_CHINA_TEMP %>% 
  inner_join(China_USA_CH4, by = c('year' = 'Year')) %>% 
  inner_join(China_USA_CO2, by = c('year' = 'Year')) %>%
  inner_join(China_USA_Fgas, by = c('year' = 'Year')) %>%
  inner_join(China_USA_N20, by = c('year' = 'Year')) 

USA_temp_lm_dataset <- USA_CHINA_temp_lm_dataset %>% 
  filter(Country == 'United States') %>% 
  inner_join(USA_population, by = c('year' = 'year')) %>% 
  mutate(United_States_CH4_per_capita = United_States_CH4 / USA_population,
         United_States_CO2_per_capita = United_States_CO2 / USA_population,
         United_States_Fgas_per_capita =  United_States_Fgas / USA_population,
         United_States_N20_per_capita = United_States_N20 / USA_population) %>% 
  slice(-1)


China_temp_lm_dataset <- USA_CHINA_temp_lm_dataset %>% 
  filter(Country == 'China') %>%
  inner_join(China_population, by = c('year' = 'year')) %>%
  mutate(China_CH4_per_capita = China_CH4 / China_Population,
         China_CO2_per_capita = China_CO2 / China_Population,
         China_Fgas_per_capita = China_Fgas / China_Population,
         China_N20_per_capita = China_N20 / China_Population) %>% 
  slice(-1)

m2 <- lm(DIFF ~ United_States_CH4_per_capita+United_States_CO2_per_capita+United_States_Fgas_per_capita+United_States_N20_per_capita, data = USA_temp_lm_dataset)
m3 <- lm(DIFF ~ China_CH4_per_capita+China_CO2_per_capita+China_Fgas_per_capita+China_N20_per_capita, data = China_temp_lm_dataset)
summary(m2)
summary(m3)

step_m2 <- stepAIC(m2, direction = 'both', trace = FALSE)
summary(step_m2)

step_m3 <- stepAIC(m3, direction = 'both', trace = FALSE)
summary(step_m3)

#The part above is the two linear regression models, one for China, one for USA
#In each of the model, it captures the PERCENTAGE CHANGE in temperature in this country and 
#the PERCENTAGE CHANGE in its country's greenhouse gases WITHOUT sector classification PER CAPITA
#Both model are full model without predictors selection, please use AIC or BIC to do a stepwise selection. 

USA_CO2_sector <- read.csv('Dataset/USA_CO2_sector.csv')
USA_CH4_sector <- read.csv('Dataset/USA_CH4_sector.csv')
USA_Fgas_sector <- read.csv('Dataset/USA_Fgas_sector.csv')
USA_N20_sector <- read.csv('Dataset/USA_N20_sector.csv')

USA_temp_sector_lm_dataset <- USA_CHINA_TEMP %>% 
  filter(Country == 'United States') %>%
  inner_join(USA_CO2_sector, by = c('year' = 'Year')) %>% 
  inner_join(USA_CH4_sector, by = c('year' = 'Year')) %>%
  inner_join(USA_Fgas_sector, by = c('year' = 'Year')) %>%
  inner_join(USA_N20_sector, by = c('year' = 'Year')) %>% 
  inner_join(USA_population, by = c('year' = 'year')) %>% 
  mutate(C02_Bunker_Fuels_per_capita = C02_Bunker_Fuels / USA_population,
         CO2_Industrial_Processes_per_caipta = CO2_Industrial_Processes / USA_population,
         CO2_Land.Use_Change_and_Forestry_per_capita = CO2_Land.Use_Change_and_Forestry / USA_population,
         CO2_Building_per_capita = CO2_Building / USA_population,
         CO2_Electricity.Heatg_per_capita = CO2_Electricity.Heat / USA_population,
         CO2_Fugitive_Emissions_per_capita = CO2_Fugitive_Emissions / USA_population,
         CO2_Manufacturing.Construction_per_capita = CO2_Manufacturing.Construction / USA_population,
         CO2_Other.Fuel_Combustion_per_capita = CO2_Other.Fuel_Combustion / USA_population,
         CO2_Transportation_per_capita = CO2_Transportation / USA_population,
         CH4_Agriculture_per_capita = CH4_Agriculture / USA_population,
         CH4_Industrial_Processes_per_capita = CH4_Industrial_Processes / USA_population,
         CH4_Land.Use_Change_and_Forestry_per_capita = CH4_Land.Use_Change_and_Forestry / USA_population,
         CH4_Waste_per_capita = CH4_Waste / USA_population,
         CH4_Fugitive_Emissions_per_capita = CH4_Fugitive_Emissions / USA_population,
         CH4_Other_Fuel_Combustion_per_capita = CH4_Other_Fuel_Combustion / USA_population,
         Fgas_Industrial_Processes_per_capita = Fgas_Industrial_Processes / USA_population,
         N20_Agriculture_per_capita = N20_Agriculture / USA_population,
         N20_Industrial_Processes_per_capita = N20_Industrial_Processes / USA_population,
         N20_Land.Use_Change_and_Forestry_per_capita = N20_Land.Use_Change_and_Forestry / USA_population,
         N20_Waste_per_capita = N20_Waste / USA_population,
         N20_Other_Fuel_Combustion_per_capita = N20_Other_Fuel_Combustion / USA_population)




m4 <- lm(DIFF ~ C02_Bunker_Fuels_per_capita+CO2_Industrial_Processes_per_caipta+CO2_Land.Use_Change_and_Forestry_per_capita+CO2_Building_per_capita+
           CO2_Electricity.Heatg_per_capita+CO2_Fugitive_Emissions_per_capita+CO2_Manufacturing.Construction_per_capita+CO2_Other.Fuel_Combustion_per_capita+
           CO2_Transportation_per_capita+CH4_Agriculture_per_capita+CH4_Industrial_Processes_per_capita+CH4_Land.Use_Change_and_Forestry_per_capita+
           CH4_Waste_per_capita+CH4_Fugitive_Emissions_per_capita+CH4_Other_Fuel_Combustion_per_capita+Fgas_Industrial_Processes_per_capita+
           N20_Agriculture_per_capita+N20_Industrial_Processes_per_capita+N20_Land.Use_Change_and_Forestry_per_capita+N20_Waste+N20_Other_Fuel_Combustion_per_capita,
         data = USA_temp_sector_lm_dataset)
summary(m4)

step_m4 <- stepAIC(m4, direction = 'both', trace = FALSE)
summary(step_m4)

#The part above is a linear regression model for USA
#In the model, it captures the PERCENTAGE CHANGE in temperature in this country and 
#the PERCENTAGE CHANGE in its country's greenhouse gases PER CAPITA WITH sector classification. 
#The model is a full model without predictors selection, please use AIC or BIC to do a stepwise selection. 

USA_N20_sector_tot <- read.csv('Dataset/USA_N20_sector_tot.csv')
colnames(USA_N20_sector_tot) <- colnames(USA_N20_sector)
USA_Fgas_sector_tot <- read.csv('Dataset/USA_Fgas_sector_tot.csv')
colnames(USA_Fgas_sector_tot) <- colnames(USA_Fgas_sector)
USA_CO2_sector_tot <- read.csv('Dataset/USA_CO2_sector_tot.csv')
colnames(USA_CO2_sector_tot) <- colnames(USA_CO2_sector)
USA_CH4_sector_tot <- read.csv('Dataset/USA_CH4_sector_tot.csv')
colnames(USA_CH4_sector_tot) <- colnames(USA_CH4_sector)

USA_temp_sector_tot_lm_dataset <- USA_CHINA_TEMP %>% 
  filter(Country == 'United States') %>%
  inner_join(USA_CO2_sector_tot, by = c('year' = 'Year')) %>% 
  inner_join(USA_CH4_sector_tot, by = c('year' = 'Year')) %>%
  inner_join(USA_Fgas_sector_tot, by = c('year' = 'Year')) %>%
  inner_join(USA_N20_sector_tot, by = c('year' = 'Year')) %>%
  inner_join(USA_population, by = c('year' = 'year')) %>% 
  mutate(C02_Bunker_Fuels_per_capita = C02_Bunker_Fuels / USA_population,
         CO2_Industrial_Processes_per_caipta = CO2_Industrial_Processes / USA_population,
         CO2_Land.Use_Change_and_Forestry_per_capita = CO2_Land.Use_Change_and_Forestry / USA_population,
         CO2_Building_per_capita = CO2_Building / USA_population,
         CO2_Electricity.Heatg_per_capita = CO2_Electricity.Heat / USA_population,
         CO2_Fugitive_Emissions_per_capita = CO2_Fugitive_Emissions / USA_population,
         CO2_Manufacturing.Construction_per_capita = CO2_Manufacturing.Construction / USA_population,
         CO2_Other.Fuel_Combustion_per_capita = CO2_Other.Fuel_Combustion / USA_population,
         CO2_Transportation_per_capita = CO2_Transportation / USA_population,
         CH4_Agriculture_per_capita = CH4_Agriculture / USA_population,
         CH4_Industrial_Processes_per_capita = CH4_Industrial_Processes / USA_population,
         CH4_Land.Use_Change_and_Forestry_per_capita = CH4_Land.Use_Change_and_Forestry / USA_population,
         CH4_Waste_per_capita = CH4_Waste / USA_population,
         CH4_Fugitive_Emissions_per_capita = CH4_Fugitive_Emissions / USA_population,
         CH4_Other_Fuel_Combustion_per_capita = CH4_Other_Fuel_Combustion / USA_population,
         Fgas_Industrial_Processes_per_capita = Fgas_Industrial_Processes / USA_population,
         N20_Agriculture_per_capita = N20_Agriculture / USA_population,
         N20_Industrial_Processes_per_capita = N20_Industrial_Processes / USA_population,
         N20_Land.Use_Change_and_Forestry_per_capita = N20_Land.Use_Change_and_Forestry / USA_population,
         N20_Waste_per_capita = N20_Waste / USA_population,
         N20_Other_Fuel_Combustion_per_capita = N20_Other_Fuel_Combustion / USA_population)

m5 <- lm(avg_temp ~ C02_Bunker_Fuels_per_capita+CO2_Industrial_Processes_per_caipta+CO2_Land.Use_Change_and_Forestry_per_capita+CO2_Building_per_capita+
           CO2_Electricity.Heatg_per_capita+CO2_Fugitive_Emissions_per_capita+CO2_Manufacturing.Construction_per_capita+CO2_Other.Fuel_Combustion_per_capita+
           CO2_Transportation_per_capita+CH4_Agriculture_per_capita+CH4_Industrial_Processes_per_capita+CH4_Land.Use_Change_and_Forestry_per_capita+
           CH4_Waste_per_capita+CH4_Fugitive_Emissions_per_capita+CH4_Other_Fuel_Combustion_per_capita+Fgas_Industrial_Processes_per_capita+
           N20_Agriculture_per_capita+N20_Industrial_Processes_per_capita+N20_Land.Use_Change_and_Forestry_per_capita+N20_Waste+N20_Other_Fuel_Combustion_per_capita,
         data = USA_temp_sector_tot_lm_dataset)
summary(m5)

lm(avg_temp ~ C02_Bunker_Fuels_per_capita+CO2_Land.Use_Change_and_Forestry_per_capita+
     CO2_Fugitive_Emissions_per_capita+CO2_Manufacturing.Construction_per_capita+CO2_Other.Fuel_Combustion_per_capita+
     CO2_Transportation_per_capita+CH4_Agriculture_per_capita+CH4_Industrial_Processes_per_capita+CH4_Land.Use_Change_and_Forestry_per_capita+
     CH4_Waste_per_capita+CH4_Fugitive_Emissions_per_capita+CH4_Other_Fuel_Combustion_per_capita+Fgas_Industrial_Processes_per_capita+
     N20_Agriculture_per_capita+N20_Industrial_Processes_per_capita+N20_Land.Use_Change_and_Forestry_per_capita+N20_Other_Fuel_Combustion_per_capita,
   data = USA_temp_sector_tot_lm_dataset)


step_m5 <- stepAIC(m5, direction = 'both', trace = FALSE)
summary(step_m5)

m5_rf_train <- train(avg_temp ~ C02_Bunker_Fuels_per_capita+CO2_Land.Use_Change_and_Forestry_per_capita+
                 CO2_Fugitive_Emissions_per_capita+CO2_Manufacturing.Construction_per_capita+CO2_Other.Fuel_Combustion_per_capita+
                 CO2_Transportation_per_capita+CH4_Agriculture_per_capita+CH4_Industrial_Processes_per_capita+CH4_Land.Use_Change_and_Forestry_per_capita+
                 CH4_Waste_per_capita+CH4_Fugitive_Emissions_per_capita+CH4_Other_Fuel_Combustion_per_capita+Fgas_Industrial_Processes_per_capita+
                 N20_Agriculture_per_capita+N20_Industrial_Processes_per_capita+N20_Land.Use_Change_and_Forestry_per_capita+N20_Other_Fuel_Combustion_per_capita,
               data = USA_temp_sector_tot_lm_dataset, method = "rf", trControl = train.control)
print(m5_rf_train)

m5_rf <- randomForest::randomForest(avg_temp ~ C02_Bunker_Fuels_per_capita+CO2_Land.Use_Change_and_Forestry_per_capita+
                                      CO2_Fugitive_Emissions_per_capita+CO2_Manufacturing.Construction_per_capita+CO2_Other.Fuel_Combustion_per_capita+
                                      CO2_Transportation_per_capita+CH4_Agriculture_per_capita+CH4_Industrial_Processes_per_capita+CH4_Land.Use_Change_and_Forestry_per_capita+
                                      CH4_Waste_per_capita+CH4_Fugitive_Emissions_per_capita+CH4_Other_Fuel_Combustion_per_capita+Fgas_Industrial_Processes_per_capita+
                                      N20_Agriculture_per_capita+N20_Industrial_Processes_per_capita+N20_Land.Use_Change_and_Forestry_per_capita+N20_Other_Fuel_Combustion_per_capita,
                                    data = USA_temp_sector_tot_lm_dataset)
summary(m5_rf)
#The part above is a linear regression model for USA
#In the model, it captures the average temperature in this country and 
#the total in its country's greenhouse gases PER CAPITA WITH sector classification. 
#The model is a full model without predictors selection, please use AIC or BIC to do a stepwise selection. 

USA_temp_sector_tot_lm_dataset_1 <- USA_CHINA_TEMP %>% 
  filter(Country == 'United States') %>%
  inner_join(USA_CO2_sector_tot, by = c('year' = 'Year')) %>% 
  inner_join(USA_CH4_sector_tot, by = c('year' = 'Year')) %>%
  inner_join(USA_Fgas_sector_tot, by = c('year' = 'Year')) %>%
  inner_join(USA_N20_sector_tot, by = c('year' = 'Year')) %>%
  inner_join(USA_population, by = c('year' = 'year'))

m6 <- lm(avg_temp ~ C02_Bunker_Fuels+CO2_Industrial_Processes+CO2_Land.Use_Change_and_Forestry+CO2_Building+
           CO2_Electricity.Heat+CO2_Fugitive_Emissions+CO2_Manufacturing.Construction+CO2_Other.Fuel_Combustion+
           CO2_Transportation+CH4_Agriculture+CH4_Industrial_Processes+CH4_Land.Use_Change_and_Forestry+
           CH4_Waste+CH4_Fugitive_Emissions+CH4_Other_Fuel_Combustion+Fgas_Industrial_Processes+
           N20_Agriculture+N20_Industrial_Processes+N20_Land.Use_Change_and_Forestry+N20_Waste+N20_Other_Fuel_Combustion,
         data = USA_temp_sector_tot_lm_dataset_1)

summary(m6)

#Fugitive emissions are emissions of gases or vapors from pressurized equipment due to leaks and other unintended or irregular releases of gases, mostly from industrial activities. As well as the economic cost of lost commodities, fugitive emissions contribute to air pollution.


step_m6 <- stepAIC(m6, direction = 'both', trace = FALSE)
summary(step_m6)

glimpse(USA_temp_sector_tot_lm_dataset_1)
