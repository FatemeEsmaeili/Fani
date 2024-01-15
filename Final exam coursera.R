# @hidden_cell
# The project token is an authorization token that is used to access project resources like data sources, connections, and used by platform APIs.
from project_lib import Project
project = Project(project_id='YOUR_PROJECT_ID', project_access_token='YOUR_PROJECT_TOKEN')
pc = project.project_context
#Install tidymodels if you haven't done so
install.packages("rlang")
install.packages("tidymodels")
library(tidymodels)
library(tidyverse)
library(ggplot2)
# url where the data is located
url <- "https://dax-cdn.cdn.appdomain.cloud/dax-noaa-weather-data-jfk-airport/1.1.4/noaa-weather-sample-data.tar.gz"

# download the file
download.file(url, destfile = "lax_to_jfk.tar.gz")

# untar the file so we can get the csv only
# if you run this on your local machine, then can remove tar = "internal" 
untar("lax_to_jfk.tar.gz", tar = "internal")
weather_data <- read_csv("noaa-weather-sample-data/jfk_weather_sample.csv",
                         col_types = cols('DATE' = col_number(), 
                                          'HOURLYPrecip' = col_number()))

head(weather_data)
glimpse(weather_data)
wea_data <- weather_data %>% 
  select(c(HOURLYRelativeHumidity, 
           HOURLYDRYBULBTEMPF, 
           HOURLYPrecip, 
           HOURLYWindSpeed, 
           HOURLYStationPressure))
head(wea_data,10)
unique(wea_data$HOURLYPrecip)
unique(wea_data$HOURLYPrecip)
as.numeric(wea_data$HOURLYPrecip)
typeof(wea_data$HOURLYPrecip)
wea_data3 <- wea_data %>%
  replace_na(list(HOURLYPrecip = 0)) %>%
  rename("relative_humidity"="HOURLYRelativeHumidity",
         'dry_bulb_temp_f'='HOURLYDRYBULBTEMPF',
         'precip'='HOURLYPrecip',
         'wind_speed'='HOURLYWindSpeed',
         'station_pressure'='HOURLYStationPressure')

head(wea_data3)

wea_data3 <- wea_data3 %>%
  replace_na(list(relative_humidity = 0,
                  dry_bulb_temp_f = 0,
                  precip = 0,
                  wind_speed = 0,
                  station_pressure = 0))
head(wea_data3)
map(wea_data3, ~sum(is.na(.)))
glimpse(wea_data3)
is.na(wea_data3)
set.seed(1234)
wea_split <- initial_split(wea_data3, prop = 0.8)  
train_data <- training(wea_split)
test_data <- testing(wea_split)
train_hist1 <-ggplot(train_data, aes(x=relative_humidity)) + 
  geom_histogram()
train_hist1 

train_hist2 <-ggplot(train_data, aes(x=dry_bulb_temp_f)) + 
  geom_histogram()
train_hist2

train_hist3 <-ggplot(train_data, aes(x=precip)) + 
  geom_histogram()
train_hist3

train_hist4 <-ggplot(train_data, aes(x=wind_speed)) + 
  geom_histogram()
train_hist4

train_hist5 <-ggplot(train_data, aes(x=station_pressure)) + 
  geom_histogram()
train_hist5

# Pick linear regression
lm_spec <- linear_reg() %>%
  # Set engine
  set_engine(engine = "lm")
train_fit1 <- lm_spec %>% 
  fit(precip ~ relative_humidity, data = train_data)
train_fit1

train_fit2 <- lm_spec %>% 
  fit(precip ~ dry_bulb_temp_f, data = train_data)
train_fit2

train_fit3 <- lm_spec %>% 
  fit(precip ~ wind_speed, data = train_data)
train_fit3

train_fit4 <- lm_spec %>% 
  fit(precip ~ station_pressure, data = train_data)
train_fit4

#train_fit1
train_results1 <- train_fit1 %>%
  # Make the predictions and save the predicted values
  predict(new_data = train_data) %>%
  # Create a new column to save the true values
  mutate(truth = train_data$precip)
head(train_results1)

#R2 for train_fit1
rsq(train_results1, truth = truth,
    estimate = .pred)

#train_fit2
train_results2 <- train_fit2 %>%
  # Make the predictions and save the predicted values
  predict(new_data = train_data) %>%
  # Create a new column to save the true values
  mutate(truth = train_data$precip)
head(train_results2)
#R2 for train_fit2
rsq(train_results2, truth = truth,
    estimate = .pred)

#train_fit3
train_results3 <- train_fit3 %>%
  # Make the predictions and save the predicted values
  predict(new_data = train_data) %>%
  # Create a new column to save the true values
  mutate(truth = train_data$precip)
head(train_results3)

#R2 for train_fit3
rsq(train_results3, truth = truth,
    estimate = .pred)

#train_fit4
train_results4 <- train_fit4 %>%
  # Make the predictions and save the predicted values
  predict(new_data = train_data) %>%
  # Create a new column to save the true values
  mutate(truth = train_data$precip)
head(train_results4)

#R2 for train_fit4
rsq(train_results4, truth = truth,
    estimate = .pred)

train_plot1 <- ggplot(train_data, aes(x=relative_humidity, y=precip)) + 
  geom_point()
train_plot1

train_plot2 <- ggplot(train_data, aes(x=dry_bulb_temp_f, y=precip)) + 
  geom_point()
train_plot2

train_plot3 <- ggplot(train_data, aes(x=wind_speed, y=precip)) + 
  geom_point()
train_plot3

train_plot4 <- ggplot(train_data, aes(x=station_pressure, y=precip)) + 
  geom_point()
train_plot4

#Multiple Linear Regression 
train_fit5 <- lm_spec %>% 
  fit(precip ~ ., data = train_data)
train_fit5

train_results5 <- train_fit5 %>%
  # Make the predictions and save the predicted values
  predict(new_data = train_data) %>%
  # Create a new column to save the true values
  mutate(truth = train_data$precip)
head(train_results5)

#R2 for multiple linear regression
rsq(train_results5, truth = truth,
    estimate = .pred)

#L2
recipe1 <-
  recipe(precip ~ ., data = train_data)
ridge_spec <- linear_reg(penalty = 0.1, mixture = 0) %>%
  set_engine("glmnet")
ridge_wf <- workflow() %>%
  add_recipe(recipe1)
ridge_fit <- ridge_wf %>%
  add_model(ridge_spec) %>%
  fit(data = train_data)
ridge_fit %>%
  pull_workflow_fit() %>%
  tidy()

train_results6 <- ridge_fit %>%
  # Make the predictions and save the predicted values
  predict(new_data = train_data) %>%
  # Create a new column to save the true values
  mutate(truth = train_data$precip)
head(train_results6)

#R2 for Ridge (L2) regularization
rsq(train_results6, truth = truth,
    estimate = .pred)

#Grid Search
tune_spec <- linear_reg(penalty = tune(), mixture = 0.2) %>% 
  set_engine("glmnet")

el_wf <- workflow() %>%
  add_recipe(recipe1)
cvfolds1 <- vfold_cv(train_data)
lambda_grid <- grid_regular(levels = 50,
                            penalty(range = c(-3, 0.3)))
el_grid <- tune_grid(
  el_wf %>% add_model(tune_spec), 
  resamples = cvfolds1, 
  grid = lambda_grid)

show_best(el_grid, metric = "rmse")
#Elastic Net (L1 and L2) Regularization
recipe1 <-
  recipe(precip ~ ., data = train_data) 
#The dot . in the formula is a special character that tells R to use all the variables in train_data
el_spec <- linear_reg(penalty = 0.001363622, mixture = 0.2) %>%
  set_engine("glmnet")

el_wf <- workflow() %>%
  add_recipe(recipe1)

el_fit <- el_wf %>%
  add_model(el_spec) %>%
  fit(data = train_data)

el_fit %>%
  pull_workflow_fit() %>%
  tidy()

#Elastic Net (L1 and L2) Regularization and optimal penalty
train_results7 <- el_fit %>%
  # Make the predictions and save the predicted values
  predict(new_data = train_data) %>%
  # Create a new column to save the true values
  mutate(truth = train_data$precip)
head(train_results7)

#R2 for Ridge (L2) regularization
rsq(train_results7, truth = truth,
    estimate = .pred)

model_names <- c("precip ~ relative_humidity","precip ~ dry_bulb_temp_f","precip ~ wind_speed","precip ~ station_pressure","Multiple Linear Regression", "Ridge (L2) regularization", "Elastic Net Regularization")
sqr <- c("0.02471808","0.0002962591","0.003871889","0.0002849881","0.03841775", "0.03022052", "0.03805498")
comparison_df <- data.frame(model_names, sqr)
comparison_df