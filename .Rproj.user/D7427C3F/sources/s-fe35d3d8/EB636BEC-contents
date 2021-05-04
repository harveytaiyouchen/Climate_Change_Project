# Load the data
data("swiss")
# Inspect the data
sample_n(swiss, 3)


set.seed(123) 
train.control <- trainControl(method = "cv", number = 10)
# Linear least squares 
model_lm <- train(global_LandAndOcean_temp ~ C02_Energy + CO2_Industrial_Processes_and_Product_Use + CO2_Agriculture + CO2_Waste +
                 CH4_Energy + CH4_Industrial_Processes_and_Product_Use + CH4_Agriculture + CH4_Waste + 
                 Fgas + N20_Energy + N20_Industrial_Processes_and_Product_Use + N20_Agriculture + N20_Waste, data = global_temp_lm_dataset, method = "lm",
                 trControl = train.control)
# Summarize the results
print(model_lm)

# random forest
model_rf <- train(global_LandAndOcean_temp ~ C02_Energy + CO2_Industrial_Processes_and_Product_Use + CO2_Agriculture + CO2_Waste +
              CH4_Energy + CH4_Industrial_Processes_and_Product_Use + CH4_Agriculture + CH4_Waste + 
              Fgas + N20_Energy + N20_Industrial_Processes_and_Product_Use + N20_Agriculture + N20_Waste, 
              data = global_temp_lm_dataset, method = "rf", trControl = train.control)
print(model_rf)

# Generalized linear model 
model_glm <- train(global_LandAndOcean_temp ~ C02_Energy + CO2_Industrial_Processes_and_Product_Use + CO2_Agriculture + CO2_Waste +
                    CH4_Energy + CH4_Industrial_Processes_and_Product_Use + CH4_Agriculture + CH4_Waste + 
                    Fgas + N20_Energy + N20_Industrial_Processes_and_Product_Use + N20_Agriculture + N20_Waste, 
                  data = global_temp_lm_dataset, method = "glm", trControl = train.control)
print(model_glm)



model_glmStepAIC <- train(global_LandAndOcean_temp ~ C02_Energy + CO2_Industrial_Processes_and_Product_Use + CO2_Agriculture + CO2_Waste +
                     CH4_Energy + CH4_Industrial_Processes_and_Product_Use + CH4_Agriculture + CH4_Waste + 
                     Fgas + N20_Energy + N20_Industrial_Processes_and_Product_Use + N20_Agriculture + N20_Waste, 
                   data = global_temp_lm_dataset, method = "glmStepAIC", trControl = train.control)
print(model_glmStepAIC)
