options(scipen = 10)
set.seed(1818)

# load packages
library(partykit)
library(tidyverse)
library(titanic)
library(PerformanceAnalytics)
library(rpart)       
library(rpart.plot)  
library(caret)

# load data
#crash_report <- read_csv("C:/Users/kebar/OneDrive/Documents/MGSC_310/datasets/CrashReport.csv")
                         
crash_report <- read_csv("C:/Users/kebar/OneDrive/Documents/MGSC_310/datasets/CrashReport.csv",                       
                         col_select = c("Weather", "Surface Condition", "Vehicle Body Type", 
                                        "Speed Limit", "Vehicle Damage Extent", "Driver Substance Abuse"))
# looking at data
summary(crash_report)
sapply(crash_report, function(x) sum(is.na(x)))

# making Speed Limit an int and not a character
crash_report$`Speed Limit` <- as.numeric(crash_report$`Speed Limit`)

# cleaning data:
# making all values lowercase to keep consistency
crash_report$Weather <- tolower(crash_report$Weather)
crash_report$`Surface Condition` <- tolower(crash_report$`Surface Condition`)
crash_report$`Vehicle Body Type` <- tolower(crash_report$`Vehicle Body Type`)
crash_report$`Vehicle Damage Extent` <- tolower(crash_report$`Vehicle Damage Extent`)

# making string 'n/a' values = NA
crash_report[] <- lapply(crash_report, function(x) gsub("^n/a$", NA, x))
crash_report[] <- lapply(crash_report, function(x) gsub("^unknown$", NA, x))
crash_report[] <- lapply(crash_report, function(x) gsub("^other$", NA, x))
crash_report[] <- lapply(crash_report, function(x) gsub("^vehicle not at scene$", NA, x))

# removing all null values
crash_report <- na.omit(crash_report)
sapply(crash_report, function(x) sum(is.na(x)))

# combining categories for each variable.......:

# all unique values of vehicle damage extent (what we are going to predict)
unique(crash_report$'Vehicle Damage Extent')
# Combine categories in 'Vehicle Damage Extent'
crash_report$`Vehicle Damage Extent` <- recode(crash_report$`Vehicle Damage Extent`,
                                               "minor damage" = "minor damage/no damage",
                                               "no damage" = "minor damage/no damage",
                                               "superficial" = "minor damage/no damage",
                                               "functional" = "minor damage/no damage",
                                               "disabling" = "destroyed")
unique(crash_report$'Vehicle Damage Extent')

table(crash_report$`Vehicle Damage Extent`)

# driver substance abuse:
unique(crash_report$'Driver Substance Abuse')

consolidate_substance_use <- function(substance) {
  case_when(
    substance %in% c("ALCOHOL CONTRIBUTED", "ALCOHOL PRESENT", 
                     "COMBINATION CONTRIBUTED", "COMBINED SUBSTANCE PRESENT", 
                     "ILLEGAL DRUG CONTRIBUTED", "ILLEGAL DRUG PRESENT", 
                     "MEDICATION CONTRIBUTED", "MEDICATION PRESENT", 
                     "Suspect of Alcohol Use, Not Suspect of Drug Use", 
                     "Suspect of Alcohol Use, Unknown", 
                     "Not Suspect of Alcohol Use, Suspect of Drug Use", 
                     "Unknown, Suspect of Drug Use", 
                     "Suspect of Alcohol Use, Suspect of Drug Use", 
                     "Not Suspect of Alcohol Use, Unknown") ~ "Yes",
    substance %in% c("NONE DETECTED", "N/A", "Not Suspect of Alcohol Use, Not Suspect of Drug Use") ~ "No",
    TRUE ~ NA_character_ # Removes any remaining unknown values
  )
}

crash_report$'Driver Substance Abuse' <- consolidate_substance_use(crash_report$'Driver Substance Abuse')
crash_report <- na.omit(crash_report)
sapply(crash_report, function(x) sum(is.na(x)))
unique(crash_report$'Driver Substance Abuse')




# weather:
consolidate_weather <- function(Weather) {
  case_when(
    Weather %in% c("clear") ~ "clear",
    Weather %in% c("cloudy", "foggy", "fog, smog, smoke") ~ "cloudy",
    Weather %in% c("raining", "rain", "freezing rain or freezing drizzle") ~ "rainy",
    Weather %in% c("snow", "blowing snow", "wintry mix", "sleet", "sleet or hail") ~ "snow",
    Weather %in% c("severe winds", "severe crosswinds", "blowing sand, soil, dirt") ~ "windy",
    TRUE ~ "other" # Catch any unexpected values
  )
}
crash_report$Weather <- consolidate_weather(crash_report$Weather)
unique(crash_report$'Weather')

# surface condition:
consolidate_surface_condition <- function(condition) {
  case_when(
    condition %in% c("dry") ~ "dry",
    condition %in% c("wet", "water(standing/moving)", "water (standing, moving)", "oil", "snow", "ice", "ice/frost", "slush") ~ "wet/snow",
    condition %in% c("mud, dirt, gravel", "sand") ~ "loose material",
    TRUE ~ "other" # Catch any unexpected values
  )
}
crash_report$`Surface Condition` <- consolidate_surface_condition(crash_report$`Surface Condition`)
unique(crash_report$'Surface Condition')


#body type:
consolidate_vehicle_body <- function(body_type) {
  case_when(
    body_type %in% c(
      "passenger car", "station wagon", "limousine", 
      "low speed vehicle", "autocycle"
    ) ~ "sedan",
    
    body_type %in% c(
      "(sport) utility vehicle", "sport utility vehicle", 
      "pickup truck", "pickup"
    ) ~ "SUV",
    
    body_type %in% c(
      "school bus", "transit bus", "cross country bus", 
      "bus - school", "bus - transit", "bus - mini", 
      "bus - other type", 
      "other light trucks (10,000lbs (4,536kg) or less)", 
      "cargo van/light truck 2 axles (over 10,000lbs (4,536 kg))"
    ) ~ "large truck/bus",
    
    body_type %in% c(
      "police vehicle/emergency", "police vehicle/non emergency", 
      "ambulance/emergency", "ambulance/non emergency", 
      "fire vehicle/emergency", "fire vehicle/non emergency"
    ) ~ "emergency vehicle",
    
    body_type %in% c(
      "motorcycle", "moped", "moped or motorized bicycle", 
      "motorcycle - 2 wheeled", "motorcycle - 3 wheeled"
    ) ~ "motorcycle",
    
    body_type %in% c(
      "all terrain vehicle (atv)", 
      "all-terrain vehicle/all-terrain cycle (atv/atc)", 
      "recreational vehicle", "recreational off-highway vehicles (rov)", 
      "snowmobile"
    ) ~ "recreational vehicle",
    
    TRUE ~ "other" # Catch any unexpected values
  )
}
crash_report$`Vehicle Body Type` <- consolidate_vehicle_body(crash_report$`Vehicle Body Type`)
unique(crash_report$'Vehicle Body Type')



# list of variables to convert to factors (all except "Speed Limit")
factor_vars <- c("Weather", "Surface Condition", "Vehicle Body Type",
                 "Vehicle Damage Extent")

# convert specified columns to factors
crash_report[factor_vars] <- lapply(crash_report[factor_vars], as.factor)

# verify 
str(crash_report)

# some plots to explore:
#plot 1:
# Stacked bar plot for Weather and Vehicle Damage Extent
ggplot(crash_report, aes(x = Weather, fill = `Vehicle Damage Extent`)) +
  geom_bar(position = "fill") + # Use 'fill' for proportionate stacking
  theme_minimal() +
  labs(title = "Proportion of Vehicle Damage Extent by Weather Condition", 
       x = "Weather", 
       y = "Proportion") 


#decision tree

# split into train and test
train_index <- sample(1:nrow(crash_report), nrow(crash_report)*0.80)

# train 
train_set <- crash_report[train_index, ]
str(train_set)

# test
test_set <- crash_report[-train_index, ]
str(test_set)
summary(train_set)

table(train_set$`Vehicle Damage Extent`) / nrow(train_set)
table(test_set$`Vehicle Damage Extent`) / nrow(test_set)

# tree
crash_tree <- rpart(`Vehicle Damage Extent` ~ ., 
                    data = train_set, 
                    method = "class",
                    control = list(cp = 0.00001, 
                                   minsplit = 100,
                                   minbucket = 1000,
                                   maxdepth = 6))

# look at tree
rpart.plot(crash_tree, main = "Decision Tree for Crash Report Dataset")

crash_tree$cptable

# plot the relationship between tree complexity (depth and cp)
# and CV error
plotcp(crash_tree)
printcp(crash_tree)

# test set
# predictions
predictions <- predict(crash_tree, test_set, type = "class")
predictions

# evaluate the model
cm <- confusionMatrix(predictions, test_set$'Vehicle Damage Extent')
cm

accuracy <- mean(predictions == test_set$"Vehicle Damage Extent")
print(paste("Accuracy:", accuracy))


# train set
# predictions
predictions <- predict(crash_tree, train_set, type = "class")
predictions

# evaluate the model
library(caret)
cm <- confusionMatrix(predictions, train_set$'Vehicle Damage Extent')
cm

accuracy <- mean(predictions == train_set$"Vehicle Damage Extent")
print(paste("Accuracy:", accuracy))



