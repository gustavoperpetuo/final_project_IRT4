
# Sucess Rate of terrorist attacks in different Political Regimes



# Loading Libraries
library(tidyverse)
library(readxl)
library("plm") # library that contains the panel models
library("writexl")
library(stargazer)
library(openxlsx) 


# Loading Datasets
gdt_data <- read_xlsx("/Users/gustavoperpetuo/Desktop/IRT\ Research/Data/gdt/gdt_data_final.xlsx") #Base filtrada 
polity <- read_xlsx("/Users/gustavoperpetuo/Desktop/IRT\ Research/Data/Polity\ 5/Polity\ 5.xlsx")

# Cleaning GDT
gdt_tiny <- gdt_data %>%
  select(country_iso3,
         iyear,
         success) %>% 
  rename(year = iyear) %>% 
  group_by(country_iso3, year) %>% 
  summarise(sucess_rate = sum(success)/n()) %>% 
  ungroup() 
view(gdt_tiny)

# Cleaning Polity
polity_tiny <- polity %>% 
  select(country_iso3, 
         year,
         polity,
         polity_bin) %>% 
  filter(year >= 1970, year <= 2019) # filtering years to match the GDT range
view(polity_tiny)

# Merging Data
merged_data <-
  inner_join(gdt_tiny, 
             polity_tiny, 
             by = c("country_iso3","year")) %>% 
  drop_na() %>% 
  as.data.frame() 

view(merged_data)

# Checking for Duplicates
table <- table(merged_data$country_iso3, merged_data$year) 
which(table >= 2, arr.ind=TRUE) # found two

# Excluding Duplicates
merged_data <- merged_data %>% 
  filter(!country_iso3 == c("DEU", "SSD"))


# PLM MODELS

# Fixed-Effects Model
fe_model <- plm(sucess_rate ~ polity, 
                data = merged_data, 
                index = c("country_iso3", "year"), 
                model = "within")

# Random-Effects Model
random_model <- plm(sucess_rate ~ polity,
                    data = merged_data, 
                    index = c("country_iso3", "year"), 
                    model = "random")

# First-Differences Model
fd_model <- plm(sucess_rate ~ polity, 
                data = merged_data, 
                index = c("country_iso3", "year"), 
                model = "fd")

# Pooled-OLS Model
pooled_model <- plm(sucess_rate ~ polity, 
                    data = merged_data, 
                    index = c("country_iso3", "year"), 
                    model = "pooling")

# Final Stargazer Plot
stargazer(fe_model, random_model, fd_model, pooled_model, 
          title="Results", 
          align=TRUE, 
          type = "text")
