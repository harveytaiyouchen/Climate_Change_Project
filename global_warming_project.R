library(tidyverse)
library(reshape2)
library(ggplot2)

library(plotly)
library(countrycode)
library(gganimate)

library(hrbrthemes)
library(viridis)
global_temperature <- read.csv('GlobalLandTemperatures/GlobalTemperatures.csv')
global_temperature$dt <- as.Date(global_temperature$dt)
str(global_temperature)
global_temperature$year <- substring(global_temperature$dt,1,4)
str(global_temperature)


global_temp_mean <- global_temperature %>% 
  group_by(year) %>% 
  summarize(global_temp_mean = mean(LandAverageTemperature)) %>%
  filter(global_temp_mean != 'NA')
global_temp_uncertainty <- global_temperature %>% 
  group_by(year) %>% 
  summarize(global_temp_uncertainty = mean(LandAverageTemperatureUncertainty)) %>%
  filter(global_temp_uncertainty != 'NA')
dt1 <- data.frame('Year' = global_temp_mean$year, 'global_temp_mean' = global_temp_mean$global_temp_mean, 'global_temp_uncertainty' = global_temp_uncertainty$global_temp_uncertainty)

dt1$high <- dt1$global_temp_mean + dt1$global_temp_uncertainty
dt1$low <- dt1$global_temp_mean - dt1$global_temp_uncertainty


accumulate_by <- function(dat, var) {
  var <- lazyeval::f_eval(var, dat)
  lvls <- plotly:::getLevels(var)
  dats <- lapply(seq_along(lvls), function(x) {
    cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
  })
  dplyr::bind_rows(dats)
}

dt1 <- dt1 %>% accumulate_by(~Year)
fig <- plot_ly(dt1, x = ~Year, y = ~high, name = 'Uncertainty top', type = 'scatter', mode = 'lines',line = list(color = 'rgba(0,255,255,1)'),frame = ~frame)
fig <- fig %>% add_trace(y = ~low, name = 'Uncertainty bottom', type = 'scatter', mode = 'lines',fill = 'tonexty', fillcolor = 'rgba(168, 216, 234, 0.5)',line = list(color = 'rgba(0,255,255,1)'),frame = ~frame)
fig <- fig %>% add_trace(y = ~global_temp_mean, name = 'Average Temperature', type = 'scatter', mode = 'lines',line = list(color = 'rgba(199,121,093)'),frame = ~frame)
fig <- fig %>% layout(title = 'Global Temperature',
                      yaxis = list(title = 'Average Temperature, °C',
                                   range = c(0,15),
                                   zeroline = F),
                      xaxis = list(
                        title = 'Year',
                        zeroline = F,
                        type = 'date',
                        range = c('1753','2015')
                      ))

fig <- fig %>% animation_opts(
  frame = 100, 
  transition = 0, 
  redraw = FALSE
)
fig <- fig %>% animation_slider(
  currentvalue = list(
    prefix = "Year "
  )
)


global_temperature_country <- read.csv('GlobalLandTemperatures/GlobalLandTemperaturesByCountry.csv')
global_temperature_country$dt <- as.Date(global_temperature_country$dt)
global_temperature_country$year <- substring(global_temperature_country$dt,1,4)


global_temperature_country <- global_temperature_country %>%
  filter(!Country %in% c('Denmark', 'Antarctica', 'France', 'Europe', 'Netherlands','United Kingdom', 'Africa', 'South America'))


global_temperature_country$Country[global_temperature_country$Country == 'Denmark (Europe)'] <- 'Denmark'
global_temperature_country$Country[global_temperature_country$Country == 'France (Europe)'] <- 'France'
global_temperature_country$Country[global_temperature_country$Country == 'Netherlands (Europe)'] <- 'Netherlands'
global_temperature_country$Country[global_temperature_country$Country == 'United Kingdom (Europe)'] <- 'United Kingdom'

global_temperature_country <- global_temperature_country %>% 
  filter(!Country %in% c('Asia', 'Baker Island', 'Kingman Reef', 'North America', 'Oceania', 'Palmyra Atoll', 'Saint Martin', 'Virgin Islands') )

global_temperature_country <- global_temperature_country %>% 
  mutate(country_code = countrycode(Country,origin = 'country.name', destination = 'iso3c'))

global_temperature_country <- na.omit(global_temperature_country)

global_temperature_country <- global_temperature_country %>% 
  mutate(continent = countrycode(Country,origin = 'country.name', destination = 'continent'))

global_temperature_country <- global_temperature_country %>% filter(!continent %in% NA) %>% 
  mutate(decades = as.numeric(year) - as.numeric(year) %% 10)

dt2 <- global_temperature_country %>% 
  group_by(Country) %>% 
  summarize(avg_temp = mean(AverageTemperature))

dt2 <- dt2 %>% 
  mutate(code = countrycode(Country,origin = 'country.name', destination = 'iso3c'))

fig2 <- dt2 %>% 
  plot_ly(z = ~avg_temp, type = 'choropleth', locations = ~code, text = ~Country, color = ~avg_temp, colors = 'Purples')

g <- list(
  fitbounds = "locations",
  visible = FALSE
)

fig2 <- fig2 %>% 
  colorbar(title = 'Average Temperature °C') %>% 
  layout(geo = g,
         title = 'Global Average Temperature By Countries')




N2O_country <- read.csv('N2O_country.csv')
N2O_country$SUM <- rowSums(N2O_country[,c(2:169)]) 
N2O_country_dataset <- N2O_country %>% select(Country,SUM) %>%  na.omit() %>%
  mutate(CODE = countrycode(Country,origin = 'country.name', destination = 'iso3c')) %>% na.omit()

N2O_fig <- N2O_country_dataset %>% 
  plot_ly(z = ~SUM, type = 'choropleth', locations = ~CODE, text = ~Country, color = ~SUM, colors = 'Purples')

g <- list(
  fitbounds = "locations",
  visible = FALSE
)

N2O_fig <- N2O_fig %>% 
  colorbar(title = 'Cumulated N2O Emission By Countires') %>% 
  layout(geo = g,
         title = 'Cumulated N2O Emission By Countires')
