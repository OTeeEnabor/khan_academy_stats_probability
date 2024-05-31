# This script uses data from World bank to explain the relationships between two
# variables. From the data, draw a scatter plot and find the equation of the line. 
# Extrapolate and find the future value of the dependent variable. Explain what
# the slope and y-intercept mean.

# import libraries
library(ggplot2)
library(ggpubr)
library(dplyr)
library(tidyverse)
library(janitor)
library(here)
library(gt)
library(gtsummary)
library(knitr)
library(DT)
library(maps)
library(sf)
library(leaflet)

# get the current working directory
current_directory <- getwd()
#import the data set - World Bank GDP per capita
file_path <- "exploring_bivariate_data/data/raw/World_bank_GDP_Per_Capita.csv"
# load the csv into memory
wb_gdp <- read_csv(paste(current_directory,file_path,sep = "/"))
# dataset is currently in wide format - change to long format and filter for
# South Africa

# country code
country_code = "ZAF"
country_gdp <- wb_gdp |> 
  filter(`Country Code` == country_code) |>
  pivot_longer(
    cols = `1960`:`2022`,
    names_to = "year",
    values_to ="gdp_per_capita"
  ) |>
  mutate(
    year = parse_number(year)
)
# clean the dataframe to have ASCII standard names
country_gdp = clean_names(country_gdp)

# get country based on filtered data frame
country = country_gdp$country_name[1]
# plot the South African GDP per capita growth 
ggplot(
  country_gdp,
  aes(
    x=year,
    y=gdp_per_capita
  )
)+ 
  geom_point()+
  geom_line()+
  geom_smooth(method="lm",se=FALSE)+
  labs(
    title=sprintf("%s GDP per capita between 1960 and 2023", country),
    x = "Year",
    y = "GDP per capita ($)"
  )+
  stat_regline_equation(label.y = 5000,
                         aes(
                           label=..eq.label..
                         ))

# get line equation - y = -240000 + 120 x
slope <- 120
y_intercept <- -240000

# to get a prediction for GDP in future
future_year <- 2023
future_gdp <- slope * future_year + y_intercept
future_gdp

# create another data frame based on phone data
# client has collected data on how long they spend on their phone
# compared to how much battery life was remaining (in hours) throughout
# the day
# independent variable
time_on_phone <- c(1, 2, 3.5, 4, 6, 7, 8, 9)
# dependent variable
battery_remaining <- c(8, 7, 7, 5.5, 5 , 3.5, 2.5, 2.5)

# create dataframe
df <- data.frame(time_on_phone, battery_remaining)
# view head of the data frame
head(df)
# plot data and show regression equation
ggplot(
  data=df,
  aes(
    x=time_on_phone,
      y=battery_remaining
    )
)+
  geom_smooth(method = "lm")+
  geom_point() +
  stat_regline_equation(
    label.x = 1,
    label.y=12
  )
# line variables
# y-intercept
y_intercept <- 8.8
# slope
slope <- -0.72
# predict the batter remaining in hours 
time_spent <- 4
battery_remaining_predict <- slope*time_spent + y_intercept